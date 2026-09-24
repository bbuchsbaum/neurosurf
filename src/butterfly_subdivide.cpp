#include <Rcpp.h>
#include <cmath>
#include <cstdint>
#include <unordered_map>
#include <vector>

using namespace Rcpp;

namespace {

inline std::uint64_t edge_key(int a, int b) {
  if (a > b) std::swap(a, b);
  return (static_cast<std::uint64_t>(a) << 32) | static_cast<std::uint32_t>(b);
}

// Ordered one-ring of every vertex, or an empty ring for boundary and
// non-manifold vertices.
std::vector<std::vector<int>> one_rings(const std::vector<int>& f, int nvert) {
  const int nface = static_cast<int>(f.size() / 3);
  std::vector<std::vector<std::pair<int, int>>> fans(nvert);
  for (int t = 0; t < nface; ++t) {
    for (int k = 0; k < 3; ++k) {
      const int v = f[3 * t + k];
      fans[v].emplace_back(f[3 * t + (k + 1) % 3], f[3 * t + (k + 2) % 3]);
    }
  }
  std::vector<std::vector<int>> rings(nvert);
  for (int v = 0; v < nvert; ++v) {
    const auto& fan = fans[v];
    if (fan.size() < 3) continue;
    std::unordered_map<int, int> next;
    bool ok = true;
    for (const auto& p : fan) {
      if (!next.emplace(p.first, p.second).second) {
        ok = false;
        break;
      }
    }
    if (!ok) continue;
    std::vector<int> ring;
    int cur = fan[0].first;
    for (std::size_t i = 0; i < fan.size(); ++i) {
      ring.push_back(cur);
      auto it = next.find(cur);
      if (it == next.end()) {
        ok = false;
        break;
      }
      cur = it->second;
    }
    if (ok && cur == fan[0].first) rings[v] = ring;
  }
  return rings;
}

inline int ring_index(const std::vector<int>& ring, int v) {
  for (std::size_t i = 0; i < ring.size(); ++i) {
    if (ring[i] == v) return static_cast<int>(i);
  }
  return -1;
}

// Modified-butterfly stencil for extraordinary vertex `v` of valence k, with
// the ring rotated to start at the edge's other endpoint (Zorin et al. 1996).
void extraordinary_weights(int k, std::vector<double>& s) {
  s.assign(k, 0.0);
  if (k == 3) {
    s[0] = 5.0 / 12.0;
    s[1] = s[2] = -1.0 / 12.0;
  } else if (k == 4) {
    s[0] = 3.0 / 8.0;
    s[2] = -1.0 / 8.0;
  } else {
    for (int j = 0; j < k; ++j) {
      const double a = 2.0 * M_PI * j / k;
      s[j] = (0.25 + std::cos(a) + 0.5 * std::cos(2.0 * a)) / k;
    }
  }
}

} // namespace

// One level of interpolating modified-butterfly subdivision.
// faces are 1-based (F x 3); attrs holds one row per vertex. New values in
// `clamp_cols` (0-based columns) are clamped to the range of the edge's
// endpoints so scalar fields gain no new extrema. Edges touching a boundary
// or non-manifold vertex, or with non-finite stencil values, fall back to
// the edge midpoint.
// [[Rcpp::export]]
Rcpp::List cpp_butterfly_subdivide(const IntegerMatrix& faces,
                                   const NumericMatrix& attrs,
                                   const IntegerVector& clamp_cols) {
  const int nvert = attrs.nrow();
  const int ncol = attrs.ncol();
  const int nface = faces.nrow();
  std::vector<int> f(3 * nface);
  for (int t = 0; t < nface; ++t) {
    for (int k = 0; k < 3; ++k) {
      const int v = faces(t, k) - 1;
      if (v < 0 || v >= nvert) stop("faces contain an out-of-range vertex index");
      f[3 * t + k] = v;
    }
  }
  const auto rings = one_rings(f, nvert);
  std::vector<bool> clamp(ncol, false);
  for (int c : clamp_cols) {
    if (c >= 0 && c < ncol) clamp[c] = true;
  }

  std::unordered_map<std::uint64_t, int> edge_vertex;
  edge_vertex.reserve(static_cast<std::size_t>(nface) * 2);
  std::vector<std::pair<int, int>> new_edges;
  for (int t = 0; t < nface; ++t) {
    for (int k = 0; k < 3; ++k) {
      const int a = f[3 * t + k];
      const int b = f[3 * t + (k + 1) % 3];
      if (edge_vertex.emplace(edge_key(a, b), nvert + new_edges.size()).second) {
        new_edges.emplace_back(a, b);
      }
    }
  }

  const int nnew = nvert + static_cast<int>(new_edges.size());
  NumericMatrix out(nnew, ncol);
  IntegerMatrix parents(nnew, 2);
  for (int v = 0; v < nvert; ++v) {
    for (int c = 0; c < ncol; ++c) out(v, c) = attrs(v, c);
    parents(v, 0) = parents(v, 1) = v + 1;
  }

  std::vector<int> ids;
  std::vector<double> w, s;
  for (std::size_t e = 0; e < new_edges.size(); ++e) {
    const int a = new_edges[e].first;
    const int b = new_edges[e].second;
    const int row = nvert + static_cast<int>(e);
    parents(row, 0) = a + 1;
    parents(row, 1) = b + 1;

    ids.clear();
    w.clear();
    const auto& ra = rings[a];
    const auto& rb = rings[b];
    const int ia = ra.empty() ? -1 : ring_index(ra, b);
    const int ib = rb.empty() ? -1 : ring_index(rb, a);
    if (ia >= 0 && ib >= 0) {
      const int ka = static_cast<int>(ra.size());
      const int kb = static_cast<int>(rb.size());
      if (ka == 6 && kb == 6) {
        const int c = ra[(ia + 1) % 6], d = ra[(ia + 5) % 6];
        ids = {a, b, c, d, ra[(ia + 2) % 6], ra[(ia + 4) % 6],
               rb[(ib + 2) % 6], rb[(ib + 4) % 6]};
        w = {0.5, 0.5, 0.125, 0.125, -0.0625, -0.0625, -0.0625, -0.0625};
      } else {
        // Average the extraordinary stencils (a single one when only one
        // endpoint is extraordinary).
        const bool use_a = ka != 6;
        const bool use_b = kb != 6;
        const double share = (use_a && use_b) ? 0.5 : 1.0;
        if (use_a) {
          extraordinary_weights(ka, s);
          ids.push_back(a);
          w.push_back(0.75 * share);
          for (int j = 0; j < ka; ++j) {
            ids.push_back(ra[(ia + j) % ka]);
            w.push_back(s[j] * share);
          }
        }
        if (use_b) {
          extraordinary_weights(kb, s);
          ids.push_back(b);
          w.push_back(0.75 * share);
          for (int j = 0; j < kb; ++j) {
            ids.push_back(rb[(ib + j) % kb]);
            w.push_back(s[j] * share);
          }
        }
      }
    }

    for (int c = 0; c < ncol; ++c) {
      const double va = attrs(a, c);
      const double vb = attrs(b, c);
      double value = 0.0;
      bool finite = !ids.empty();
      for (std::size_t i = 0; i < ids.size() && finite; ++i) {
        const double x = attrs(ids[i], c);
        if (!std::isfinite(x)) finite = false;
        value += w[i] * x;
      }
      if (!finite) {
        value = (std::isfinite(va) && std::isfinite(vb)) ? 0.5 * (va + vb)
                                                         : NA_REAL;
      } else if (clamp[c]) {
        value = std::max(std::min(va, vb), std::min(std::max(va, vb), value));
      }
      out(row, c) = value;
    }
  }

  IntegerMatrix new_faces(4 * nface, 3);
  for (int t = 0; t < nface; ++t) {
    const int v0 = f[3 * t], v1 = f[3 * t + 1], v2 = f[3 * t + 2];
    const int m01 = edge_vertex[edge_key(v0, v1)];
    const int m12 = edge_vertex[edge_key(v1, v2)];
    const int m20 = edge_vertex[edge_key(v2, v0)];
    const int tri[4][3] = {{v0, m01, m20}, {v1, m12, m01},
                           {v2, m20, m12}, {m01, m12, m20}};
    for (int q = 0; q < 4; ++q) {
      for (int k = 0; k < 3; ++k) new_faces(4 * t + q, k) = tri[q][k] + 1;
    }
  }
  return List::create(_["faces"] = new_faces, _["attrs"] = out,
                      _["parents"] = parents);
}
