#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <map>
#include <unordered_map>
#include <set>
#include <deque>

using namespace Rcpp;

struct Edge {
  int u, v;
  bool operator<(const Edge& other) const {
    if (u != other.u) return u < other.u;
    return v < other.v;
  }
  bool operator==(const Edge& other) const {
    return u == other.u && v == other.v;
  }
};

// Simple boundary edge (v1 < v2, 0-based vertex indices)
struct BoundaryEdge {
  int v1;
  int v2;
};

// [[Rcpp::export]]
List find_roi_boundaries_cpp(const IntegerMatrix& faces, const IntegerVector& vertex_id) {
  const int n_faces = faces.nrow();
  if (n_faces == 0 || vertex_id.size() == 0) {
    return List::create(_["boundary_verts"] = List(),
                        _["boundary_roi_id"] = IntegerVector(),
                        _["roi_components"] = IntegerVector());
  }

  // ROI -> edges along its boundary
  std::map<int, std::vector<Edge>> roi_edges;
  roi_edges.clear();
  roi_edges.swap(roi_edges); // ensure empty capacity? optional

  for (int i = 0; i < n_faces; ++i) {
    int a = faces(i, 0) - 1; // zero-based
    int b = faces(i, 1) - 1;
    int c = faces(i, 2) - 1;

    int r1 = vertex_id[a];
    int r2 = vertex_id[b];
    int r3 = vertex_id[c];

    // skip internal faces
    if (r1 == r2 && r2 == r3) continue;

    auto add_edge = [&](int v1, int v2, int roi) {
      if (v1 > v2) std::swap(v1, v2);
      roi_edges[roi].push_back({v1, v2});
    };

    if (r1 == r2) add_edge(a, b, r1);
    if (r2 == r3) add_edge(b, c, r2);
    if (r3 == r1) add_edge(c, a, r3);
  }

  List out_boundary_verts;
  IntegerVector out_boundary_roi;
  std::map<int, int> comp_counts;

  for (auto& kv : roi_edges) {
    int roi = kv.first;
    std::vector<Edge>& edges = kv.second;
    if (edges.empty()) {
      comp_counts[roi] = 0;
      continue;
    }

    std::sort(edges.begin(), edges.end());
    edges.erase(std::unique(edges.begin(), edges.end()), edges.end());

    // adjacency list (sparse: boundary vertices only)
    std::map<int, std::vector<int>> adj;
    for (const auto& e : edges) {
      adj[e.u].push_back(e.v);
      adj[e.v].push_back(e.u);
    }

    int comps = 0;

    while (!adj.empty()) {
      int start = adj.begin()->first;
      std::vector<int> path;
      path.reserve(adj.size());
      int current = start;
      int prev = -1;

      while (true) {
        path.push_back(current);
        auto it = adj.find(current);
        if (it == adj.end()) break;
        std::vector<int>& nbrs = it->second;
        // pick a neighbor not equal to prev
        int next = -1;
        for (size_t k = 0; k < nbrs.size(); ++k) {
          if (nbrs[k] != prev) { next = nbrs[k]; nbrs.erase(nbrs.begin() + k); break; }
        }
        if (nbrs.empty()) adj.erase(it);
        if (next == -1) break;

        // remove back-edge
        auto it2 = adj.find(next);
        if (it2 != adj.end()) {
          std::vector<int>& nbrs2 = it2->second;
          for (size_t k = 0; k < nbrs2.size(); ++k) {
            if (nbrs2[k] == current) { nbrs2.erase(nbrs2.begin() + k); break; }
          }
          if (nbrs2.empty()) adj.erase(it2);
        }

        prev = current;
        current = next;
        if (current == start) break; // closed loop
      }

      if (path.size() >= 3) {
        comps++;
        IntegerVector loop(path.size());
        for (size_t k = 0; k < path.size(); ++k) loop[k] = path[k] + 1; // back to 1-based
        out_boundary_verts.push_back(loop);
        out_boundary_roi.push_back(roi);
      }
    }

    comp_counts[roi] = comps;
  }

  // roi_components aligned to sorted unique vertex ids
  std::vector<int> distinct = as<std::vector<int>>(vertex_id);
  std::sort(distinct.begin(), distinct.end());
  distinct.erase(std::unique(distinct.begin(), distinct.end()), distinct.end());

  IntegerVector roi_components(distinct.size());
  for (size_t i = 0; i < distinct.size(); ++i) {
    auto it = comp_counts.find(distinct[i]);
    roi_components[i] = (it != comp_counts.end()) ? it->second : 0;
  }

  return List::create(_["boundary_verts"] = out_boundary_verts,
                      _["boundary_roi_id"] = out_boundary_roi,
                      _["roi_components"] = roi_components);
}

// [[Rcpp::export]]
List roi_boundary_loops_cpp(const NumericMatrix& vertices,
                            const IntegerMatrix& faces,
                            const IntegerVector& vertex_id) {

  const int nVerts = vertices.nrow();
  const int nFaces = faces.nrow();

  if (nVerts == 0 || nFaces == 0) {
    return List::create(
      _["boundary"]        = List::create(),
      _["boundary_roi_id"] = IntegerVector(),
      _["roi_components"]  = IntegerVector(),
      _["boundary_verts"]  = List::create()
    );
  }

  // ---- ROI ids & mapping: sort(unique(vertex_id)) ----
  std::set<int> roi_set;
  for (int i = 0; i < nVerts; ++i) {
    roi_set.insert(vertex_id[i]);
  }
  std::vector<int> roi_ids(roi_set.begin(), roi_set.end());
  const int nRoi = static_cast<int>(roi_ids.size());

  std::unordered_map<int, int> roi_to_index;
  roi_to_index.reserve(nRoi * 2);
  for (int i = 0; i < nRoi; ++i) {
    roi_to_index[roi_ids[i]] = i;
  }

  std::vector<int> roi_components(nRoi, 0);

  // ---- Step 1: collect boundary edges from mixed faces only ----
  std::vector<BoundaryEdge> edges_raw;
  edges_raw.reserve(static_cast<std::size_t>(nFaces) * 3u);

  for (int f = 0; f < nFaces; ++f) {
    // faces are 1-based in R; convert to 0-based
    int a = faces(f, 0) - 1;
    int b = faces(f, 1) - 1;
    int c = faces(f, 2) - 1;

    // basic sanity (optional; R-side checks normally handle this)
    if (a < 0 || a >= nVerts ||
        b < 0 || b >= nVerts ||
        c < 0 || c >= nVerts) {
      continue;
    }

    int ra = vertex_id[a];
    int rb = vertex_id[b];
    int rc = vertex_id[c];

    // Optimization: skip interior faces
    // if the face is entirely inside a single ROI, it cannot contribute a boundary
    if (ra == rb && rb == rc) {
      continue;
    }

    // The face is "mixed": at least one vertex has different ROI.
    // For such a face, an edge (u,v) is a boundary *for its ROI* iff:
    //   - vertex_id[u] == vertex_id[v] == K
    //   - the third vertex has a different ROI (guaranteed by this 'mixed' face block).
    // So we only keep edges whose endpoints share an ROI.
    int vs[3][2] = { {a, b}, {b, c}, {c, a} };

    for (int e = 0; e < 3; ++e) {
      int v1 = vs[e][0];
      int v2 = vs[e][1];

      if (v1 == v2) {
        continue;
      }

      if (vertex_id[v1] != vertex_id[v2]) {
        continue; // endpoints in different ROIs, not a boundary *segment* for any ROI
      }

      if (v1 > v2) {
        std::swap(v1, v2);
      }

      BoundaryEdge ed;
      ed.v1 = v1;
      ed.v2 = v2;
      edges_raw.push_back(ed);
    }
  }

  if (edges_raw.empty()) {
    // No boundary edges at all
    return List::create(
      _["boundary"]        = List::create(),
      _["boundary_roi_id"] = IntegerVector(),
      _["roi_components"]  = IntegerVector(nRoi), // all zeros
      _["boundary_verts"]  = List::create()
    );
  }

  // ---- Step 2: sort & deduplicate edges ----
  std::sort(edges_raw.begin(), edges_raw.end(),
            [](const BoundaryEdge& a, const BoundaryEdge& b) {
              if (a.v1 != b.v1) return a.v1 < b.v1;
              return a.v2 < b.v2;
            });

  edges_raw.erase(
    std::unique(edges_raw.begin(), edges_raw.end(),
                [](const BoundaryEdge& a, const BoundaryEdge& b) {
                  return a.v1 == b.v1 && a.v2 == b.v2;
                }),
    edges_raw.end()
  );

  const std::vector<BoundaryEdge>& edges = edges_raw; // all are boundary edges now

  if (edges.empty()) {
    return List::create(
      _["boundary"]        = List::create(),
      _["boundary_roi_id"] = IntegerVector(),
      _["roi_components"]  = IntegerVector(nRoi), // all zeros
      _["boundary_verts"]  = List::create()
    );
  }

  // ---- Step 3: build adjacency for boundary edges ----
  std::vector< std::vector<int> > adj(nVerts);
  adj.shrink_to_fit();

  for (const BoundaryEdge& e : edges) {
    const int v1 = e.v1;
    const int v2 = e.v2;
    adj[v1].push_back(v2);
    adj[v2].push_back(v1);
  }

  std::vector<int> deg(nVerts);
  for (int v = 0; v < nVerts; ++v) {
    deg[v] = static_cast<int>(adj[v].size());
  }

  // ---- Step 4: BFS connected components & trace loops per component ----
  std::vector<int> comp_id(nVerts, -1);
  int current_comp = 0;

  std::vector< std::vector<int> > loops_vertices;
  std::vector<int>                loops_roi;
  loops_vertices.reserve(edges.size());

  std::vector<int> stack;

  for (int v0 = 0; v0 < nVerts; ++v0) {
    if (adj[v0].empty()) continue;   // not on any boundary edge
    if (comp_id[v0] != -1) continue; // already assigned

    const int roi_val = vertex_id[v0];
    auto it_roi = roi_to_index.find(roi_val);
    if (it_roi == roi_to_index.end()) {
      // ROI not recognized in our mapping; skip
      continue;
    }
    const int roi_idx = it_roi->second;
    roi_components[roi_idx]++; // new component for this ROI

    // BFS to collect all vertices in this component
    stack.clear();
    stack.push_back(v0);
    comp_id[v0] = current_comp;

    std::vector<int> comp_vertices;
    comp_vertices.reserve(64);
    comp_vertices.push_back(v0);

    while (!stack.empty()) {
      int v = stack.back();
      stack.pop_back();
      for (int u : adj[v]) {
        if (comp_id[u] == -1) {
          comp_id[u] = current_comp;
          stack.push_back(u);
          comp_vertices.push_back(u);
        }
      }
    }

    // classify degrees inside this component
    bool all_deg2 = true;
    std::vector<int> deg2_vertices;
    deg2_vertices.reserve(comp_vertices.size());

    for (int v : comp_vertices) {
      if (deg[v] == 2) {
        deg2_vertices.push_back(v);
      } else {
        all_deg2 = false;
      }
    }

    // If there are no degree-2 vertices, the original R code would also skip;
    // there is nothing that looks like a polygon loop.
    if (deg2_vertices.empty()) {
      ++current_comp;
      continue;
    }

    std::vector<int> path;

    if (all_deg2) {
      // ---- Simple cycle: every vertex has degree 2 ----
      int start   = comp_vertices[0];
      int current = start;
      int prev    = -1;

      const int max_steps = static_cast<int>(comp_vertices.size()) + 5;
      int steps = 0;

      path.clear();
      path.push_back(start);

      while (true) {
        const std::vector<int>& nbrs = adj[current];
        if (nbrs.empty()) break;

        int next = -1;
        if (prev == -1) {
          // take any neighbor
          next = nbrs[0];
        } else {
          // choose neighbor that is not 'prev'
          if (nbrs[0] != prev) next = nbrs[0];
          else if (nbrs.size() > 1) next = nbrs[1];
        }

        if (next < 0) break;

        prev    = current;
        current = next;
        path.push_back(current);
        ++steps;

        if (current == start || steps > max_steps) break;
      }

      if (path.back() != start) {
        path.push_back(start); // close loop explicitly
      }

    } else {
      // ---- Non-simple: break one edge and find a path (BFS) like original code ----
      int start = deg2_vertices[0];
      const std::vector<int>& nbrs_start = adj[start];
      if (nbrs_start.empty()) {
        ++current_comp;
        continue;
      }
      int end = nbrs_start[0];

      std::vector<int> parent(nVerts, -1);
      std::deque<int>  q;

      parent[start] = start;
      q.push_back(start);

      while (!q.empty()) {
        int v = q.front();
        q.pop_front();
        if (v == end) break;

        for (int u : adj[v]) {
          // ignore the direct start <-> end edge to "break" the cycle
          if ((v == start && u == end) || (v == end && u == start))
            continue;
          if (parent[u] == -1) {
            parent[u] = v;
            q.push_back(u);
          }
        }
      }

      if (parent[end] == -1) {
        ++current_comp;
        continue; // no path; degenerate
      }

      // reconstruct path from end back to start
      std::vector<int> rev;
      for (int v = end; ; ) {
        rev.push_back(v);
        if (v == start) break;
        v = parent[v];
      }

      path.assign(rev.rbegin(), rev.rend());
      path.push_back(start); // close loop
    }

    loops_vertices.push_back(path);
    loops_roi.push_back(roi_val);

    ++current_comp;
  }

  // ---- Step 5: pack results into R structures ----
  const std::size_t nLoops = loops_vertices.size();

  List boundary(nLoops);
  List boundary_verts(nLoops);
  IntegerVector boundary_roi_id(nLoops);

  for (std::size_t i = 0; i < nLoops; ++i) {
    const std::vector<int>& loop = loops_vertices[i];
    const int L = static_cast<int>(loop.size());

    NumericMatrix coords(L, 3);
    IntegerVector vloop(L);

    for (int k = 0; k < L; ++k) {
      const int v = loop[k];   // 0-based
      vloop[k] = v + 1;        // convert back to 1-based for R
      coords(k, 0) = vertices(v, 0);
      coords(k, 1) = vertices(v, 1);
      coords(k, 2) = vertices(v, 2);
    }

    boundary[i]        = coords;
    boundary_verts[i]  = vloop;
    boundary_roi_id[i] = loops_roi[i];
  }

  IntegerVector roi_components_R(nRoi);
  for (int i = 0; i < nRoi; ++i) {
    roi_components_R[i] = roi_components[i];
  }

  return List::create(
    _["boundary"]        = boundary,
    _["boundary_roi_id"] = boundary_roi_id,
    _["roi_components"]  = roi_components_R,
    _["boundary_verts"]  = boundary_verts,
    _["roi_ids"]         = wrap(roi_ids) // optional convenience; you can ignore in R
  );
}
