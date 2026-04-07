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

  const int nV = vertices.nrow();
  const int nF = faces.nrow();

  List empty = List::create(
    _["boundary"]        = List::create(),
    _["boundary_roi_id"] = IntegerVector(),
    _["roi_components"]  = IntegerVector(),
    _["boundary_verts"]  = List::create(),
    _["roi_ids"]         = IntegerVector()
  );

  if (nV == 0 || nF == 0) return empty;

  // ---------------------------------------------------------------
  // Step 1: collect boundary edges, grouped by ROI.
  //
  // A face is "mixed" when its three vertices don't all share the
  // same ROI label.  For each edge of a mixed face whose two
  // endpoints belong to the *same* ROI, that edge is a boundary
  // segment for that ROI.  We collect canonical (v1 < v2) pairs
  // into per-ROI sets so duplicates are automatically removed.
  // ---------------------------------------------------------------
  std::map<int, std::set<std::pair<int,int>>> roi_edges;

  for (int f = 0; f < nF; ++f) {
    int a = faces(f, 0) - 1;
    int b = faces(f, 1) - 1;
    int c = faces(f, 2) - 1;
    if (a < 0 || b < 0 || c < 0 || a >= nV || b >= nV || c >= nV) continue;

    int ra = vertex_id[a], rb = vertex_id[b], rc = vertex_id[c];
    if (ra == rb && rb == rc) continue; // interior face – skip

    int ev[3][2] = {{a, b}, {b, c}, {c, a}};
    int er[3][2] = {{ra, rb}, {rb, rc}, {rc, ra}};
    for (int e = 0; e < 3; ++e) {
      if (er[e][0] == er[e][1]) { // both endpoints in the same ROI
        int v1 = ev[e][0], v2 = ev[e][1];
        if (v1 > v2) std::swap(v1, v2);
        roi_edges[er[e][0]].insert({v1, v2});
      }
    }
  }

  // ---------------------------------------------------------------
  // Step 2: sorted unique ROI ids for the roi_components output.
  // ---------------------------------------------------------------
  std::vector<int> all_rois;
  {
    std::set<int> s(vertex_id.begin(), vertex_id.end());
    all_rois.assign(s.begin(), s.end());
  }
  const int nRoi = static_cast<int>(all_rois.size());
  std::unordered_map<int,int> roi_idx_map;
  roi_idx_map.reserve(nRoi * 2);
  for (int i = 0; i < nRoi; ++i) roi_idx_map[all_rois[i]] = i;
  std::vector<int> roi_components(nRoi, 0);

  // ---------------------------------------------------------------
  // Step 3: trace boundary loops per ROI using directed half-edge
  //         consumption with angle-based pairing at T-junctions.
  //
  // Every undirected edge (u, v) yields two directed half-edges:
  // (u→v) and (v→u).  We consume half-edges one at a time, always
  // choosing the continuation whose outgoing direction is most
  // collinear with the incoming direction (smallest turning angle).
  // This naturally resolves T-junctions (degree > 2) into separate
  // geometrically consistent loops without the single-loop collapse
  // bug of the previous BFS-per-component approach.
  // ---------------------------------------------------------------
  List out_boundary;
  List out_verts;
  IntegerVector out_roi;

  for (auto& kv : roi_edges) {
    const int roi      = kv.first;
    const auto& edges  = kv.second; // canonical (v1 < v2) pairs, deduplicated
    if (edges.empty()) continue;

    // Directed adjacency: for each undirected edge add both directions.
    std::map<int, std::vector<int>> adj;
    for (const auto& e : edges) {
      adj[e.first].push_back(e.second);
      adj[e.second].push_back(e.first);
    }
    // Sort neighbour lists for determinism.
    for (auto& a : adj) std::sort(a.second.begin(), a.second.end());

    // Track consumed directed half-edges.
    std::set<std::pair<int,int>> used;

    int n_loops = 0;

    // Iterate over every directed half-edge; start a new loop whenever
    // we find one that has not yet been consumed.
    for (const auto& e : edges) {
      for (int dir = 0; dir < 2; ++dir) {
        const int start = (dir == 0) ? e.first  : e.second;
        const int first = (dir == 0) ? e.second : e.first;

        if (used.count({start, first})) continue;

        // --- trace one loop beginning with half-edge start → first ---
        used.insert({start, first});

        std::vector<int> path;
        path.reserve(32);
        path.push_back(start);

        int prev = start;
        int cur  = first;
        bool closed = false;

        // Safety bound: a simple polygon can have at most 2*|edges| steps.
        const int max_steps = static_cast<int>(edges.size()) * 2 + 4;

        for (int step = 0; step < max_steps; ++step) {
          // Closure check before appending.
          if (cur == start) { closed = true; break; }

          path.push_back(cur);

          // Incoming direction vector at cur (from prev).
          double ix = vertices(cur, 0) - vertices(prev, 0);
          double iy = vertices(cur, 1) - vertices(prev, 1);
          double iz = vertices(cur, 2) - vertices(prev, 2);
          double ilen = std::sqrt(ix*ix + iy*iy + iz*iz);
          const bool has_dir = (ilen > 1e-12);
          if (has_dir) { ix /= ilen; iy /= ilen; iz /= ilen; }

          // Pick unused outgoing half-edge whose direction is most
          // collinear with the incoming direction (dot product most
          // negative = straightest continuation, least turning).
          // Backtracking to prev is explicitly excluded.
          int  best     = -1;
          double best_dot =  2.0; // sentinal; lower is better

          const auto it = adj.find(cur);
          if (it == adj.end()) break;
          for (int nbr : it->second) {
            if (nbr == prev) continue;
            if (used.count({cur, nbr})) continue;

            if (!has_dir) { best = nbr; break; } // no direction info yet

            double ox = vertices(nbr, 0) - vertices(cur, 0);
            double oy = vertices(nbr, 1) - vertices(cur, 1);
            double oz = vertices(nbr, 2) - vertices(cur, 2);
            double olen = std::sqrt(ox*ox + oy*oy + oz*oz);
            if (olen < 1e-12) { best = nbr; break; }
            ox /= olen; oy /= olen; oz /= olen;

            double dot = ix*ox + iy*oy + iz*oz;
            if (dot < best_dot) { best_dot = dot; best = nbr; }
          }

          if (best == -1) break; // dead end – open/degenerate chain

          used.insert({cur, best});
          prev = cur;
          cur  = best;
        }

        // Require at least 3 unique vertices (triangle is the minimum
        // closed polygon) before emitting this loop.
        if (!closed || path.size() < 3) continue;

        // Mark all reverse half-edges as used so the same ring is not
        // traced again in the opposite direction.
        {
          const int K = static_cast<int>(path.size());
          for (int k = 0; k + 1 < K; ++k)
            used.insert({path[k + 1], path[k]});
          // reverse of the closing half-edge (path[K-1] → start)
          used.insert({path[0], path[K - 1]});
        }

        ++n_loops;

        // Build output: path[0..K-1] are unique vertices; append path[0]
        // as the closing point so rgl::lines3d() draws a closed polygon.
        const int K = static_cast<int>(path.size());
        const int L = K + 1; // +1 for closing vertex
        NumericMatrix coords(L, 3);
        IntegerVector vloop(L);

        for (int k = 0; k < K; ++k) {
          const int v = path[k]; // 0-based
          vloop[k]      = v + 1; // 1-based for R
          coords(k, 0)  = vertices(v, 0);
          coords(k, 1)  = vertices(v, 1);
          coords(k, 2)  = vertices(v, 2);
        }
        // Closing vertex (repeat of path[0]).
        {
          const int v   = path[0];
          vloop[K]      = v + 1;
          coords(K, 0)  = vertices(v, 0);
          coords(K, 1)  = vertices(v, 1);
          coords(K, 2)  = vertices(v, 2);
        }

        out_boundary.push_back(coords);
        out_verts.push_back(vloop);
        out_roi.push_back(roi);
      }
    }

    roi_components[roi_idx_map[roi]] = n_loops;
  }

  IntegerVector roi_comp_R(nRoi);
  for (int i = 0; i < nRoi; ++i) roi_comp_R[i] = roi_components[i];

  return List::create(
    _["boundary"]        = out_boundary,
    _["boundary_roi_id"] = out_roi,
    _["roi_components"]  = roi_comp_R,
    _["boundary_verts"]  = out_verts,
    _["roi_ids"]         = wrap(all_rois)
  );
}
