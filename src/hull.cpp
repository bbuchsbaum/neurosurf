#include <RcppArmadillo.h>
#include <roptim.h>
#include <vector>
#include <cmath>

// [[Rcpp::depends(RcppArmadillo, roptim)]]
using namespace Rcpp;

namespace {

inline int idx3d(int i, int j, int k, int nx, int ny) {
  return i + nx * j + nx * ny * k;
}

inline double trilinear_sample(const double* vol,
                               int nx, int ny, int nz,
                               double gi, double gj, double gk,
                               double outside_val) {
  if (gi < 1.0 || gj < 1.0 || gk < 1.0 ||
      gi > nx  || gj > ny  || gk > nz) {
    return outside_val;
  }

  double x = gi - 1.0;
  double y = gj - 1.0;
  double z = gk - 1.0;

  int i0 = static_cast<int>(std::floor(x));
  int j0 = static_cast<int>(std::floor(y));
  int k0 = static_cast<int>(std::floor(z));
  int i1 = i0 + 1;
  int j1 = j0 + 1;
  int k1 = k0 + 1;

  if (i0 < 0 || j0 < 0 || k0 < 0 ||
      i1 >= nx || j1 >= ny || k1 >= nz) {
    return outside_val;
  }

  double dx = x - i0;
  double dy = y - j0;
  double dz = z - k0;

  auto at = [nx, ny, vol](int ii, int jj, int kk) {
    return vol[idx3d(ii, jj, kk, nx, ny)];
  };

  double c000 = at(i0, j0, k0);
  double c100 = at(i1, j0, k0);
  double c010 = at(i0, j1, k0);
  double c110 = at(i1, j1, k0);
  double c001 = at(i0, j0, k1);
  double c101 = at(i1, j0, k1);
  double c011 = at(i0, j1, k1);
  double c111 = at(i1, j1, k1);

  double c00 = c000 * (1.0 - dx) + c100 * dx;
  double c01 = c001 * (1.0 - dx) + c101 * dx;
  double c10 = c010 * (1.0 - dx) + c110 * dx;
  double c11 = c011 * (1.0 - dx) + c111 * dx;

  double c0 = c00 * (1.0 - dy) + c10 * dy;
  double c1 = c01 * (1.0 - dy) + c11 * dy;

  return c0 * (1.0 - dz) + c1 * dz;
}

class SDFAlign : public roptim::Functor {
public:
  const arma::mat hull_;
  const double* sdf_;
  const int nx_, ny_, nz_;
  const arma::mat world2grid_;
  const double outside_penalty_;

  SDFAlign(const arma::mat& hull_world,
           Rcpp::NumericVector sdf,
           Rcpp::IntegerVector dims,
           const arma::mat& world2grid,
           double outside_penalty)
    : hull_(hull_world),
      sdf_(sdf.begin()),
      nx_(dims[0]),
      ny_(dims[1]),
      nz_(dims[2]),
      world2grid_(world2grid),
      outside_penalty_(outside_penalty) {}

  double sample_sdf(const arma::vec& world_p) const {
    arma::vec4 w;
    w.head(3) = world_p;
    w(3) = 1.0;
    arma::vec4 g = world2grid_ * w;
    return trilinear_sample(sdf_, nx_, ny_, nz_, g(0), g(1), g(2),
                            outside_penalty_);
  }

  arma::mat params_to_matrix(const arma::vec& p) const {
    double tx = p(0), ty = p(1), tz = p(2);
    double rx = p(3), ry = p(4), rz = p(5);

    double cx = std::cos(rx), sx = std::sin(rx);
    double cy = std::cos(ry), sy = std::sin(ry);
    double cz = std::cos(rz), sz = std::sin(rz);

    arma::mat Rx = {{1, 0, 0},
                    {0, cx,-sx},
                    {0, sx, cx}};
    arma::mat Ry = {{ cy, 0, sy},
                    {  0, 1,  0},
                    {-sy, 0, cy}};
    arma::mat Rz = {{cz,-sz, 0},
                    {sz, cz, 0},
                    { 0,  0, 1}};

    arma::mat R = Rz * Ry * Rx;

    arma::mat T = arma::eye<arma::mat>(4, 4);
    T.submat(0, 0, 2, 2) = R;
    T(0, 3) = tx;
    T(1, 3) = ty;
    T(2, 3) = tz;
    return T;
  }

  double operator()(const arma::vec& par) override {
    arma::mat T = params_to_matrix(par);
    const int N = hull_.n_rows;
    double sumsq = 0.0;

    for (int i = 0; i < N; ++i) {
      arma::vec4 pw;
      pw.head(3) = hull_.row(i).t();
      pw(3) = 1.0;
      arma::vec4 qw = T * pw;
      double d = sample_sdf(qw.head(3));
      sumsq += d * d;
    }
    return sumsq / static_cast<double>(N);
  }

  void Gradient(const arma::vec& par, arma::vec& gr) override {
    const double eps = 1e-4;
    const int m = par.n_elem;
    gr.set_size(m);
    double f0 = (*this)(par);
    arma::vec tmp = par;
    for (int k = 0; k < m; ++k) {
      tmp(k) += eps;
      double fk = (*this)(tmp);
      gr(k) = (fk - f0) / eps;
      tmp(k) = par(k);
    }
  }
};

} // namespace

//' Compute boundary hull points in world space (C++)
//'
//' @param vol flattened numeric array (dim = dims)
//' @param dims integer vector length 3 (nx, ny, nz)
//' @param grid2world 4x4 matrix mapping voxel indices to world coords
//' @param thresh intensity threshold
//' @param n_points max number of hull points (approximate)
//' @param mask optional logical vector same length as vol; if empty, ignored
//'
//' @return numeric matrix (n_points x 3) of (x,y,z) in world coords
// [[Rcpp::export]]
NumericMatrix compute_hull_world_cpp(NumericVector vol,
                                     IntegerVector dims,
                                     NumericMatrix grid2world,
                                     double thresh,
                                     int n_points,
                                     LogicalVector mask) {
  if (dims.size() != 3) {
    stop("dims must have length 3");
  }
  const int nx = dims[0];
  const int ny = dims[1];
  const int nz = dims[2];
  const int nvox = nx * ny * nz;

  if (vol.size() != nvox) {
    stop("vol length does not match dims product");
  }

  const bool use_mask = (mask.size() == nvox);

  std::vector<char> inside(static_cast<std::size_t>(nvox), 0);
  for (int k = 0; k < nz; ++k) {
    for (int j = 0; j < ny; ++j) {
      for (int i = 0; i < nx; ++i) {
        int idx = idx3d(i, j, k, nx, ny);
        double v = vol[idx];
        bool in = !R_IsNA(v) && (v > thresh);
        if (use_mask) {
          in = in && (mask[idx] == TRUE);
        }
        inside[idx] = in ? 1 : 0;
      }
    }
  }

  std::vector<double> xs;
  std::vector<double> ys;
  std::vector<double> zs;
  xs.reserve(8192);
  ys.reserve(8192);
  zs.reserve(8192);

  const int dx[6] = { 1, -1, 0, 0, 0, 0 };
  const int dy[6] = { 0, 0,  1, -1, 0, 0 };
  const int dz[6] = { 0, 0,  0, 0,  1, -1 };

  for (int k = 1; k < nz - 1; ++k) {
    for (int j = 1; j < ny - 1; ++j) {
      for (int i = 1; i < nx - 1; ++i) {
        int idx = idx3d(i, j, k, nx, ny);
        if (!inside[idx]) continue;

        bool boundary = false;
        for (int n = 0; n < 6; ++n) {
          int ii = i + dx[n];
          int jj = j + dy[n];
          int kk = k + dz[n];
          int idxn = idx3d(ii, jj, kk, nx, ny);
          if (!inside[idxn]) {
            boundary = true;
            break;
          }
        }
        if (!boundary) continue;

        double ix = i + 1.0;
        double iy = j + 1.0;
        double iz = k + 1.0;

        double x = grid2world(0,0) * ix +
                   grid2world(0,1) * iy +
                   grid2world(0,2) * iz +
                   grid2world(0,3);
        double y = grid2world(1,0) * ix +
                   grid2world(1,1) * iy +
                   grid2world(1,2) * iz +
                   grid2world(1,3);
        double z = grid2world(2,0) * ix +
                   grid2world(2,1) * iy +
                   grid2world(2,2) * iz +
                   grid2world(2,3);

        xs.push_back(x);
        ys.push_back(y);
        zs.push_back(z);
      }
    }
  }

  int n_hull = static_cast<int>(xs.size());
  if (n_hull == 0) {
    return NumericMatrix(0, 3);
  }

  if (n_points > 0 && n_hull > n_points) {
    NumericMatrix out(n_points, 3);
    double stride = static_cast<double>(n_hull) /
                    static_cast<double>(n_points);
    for (int m = 0; m < n_points; ++m) {
      int idx = static_cast<int>(std::floor(m * stride));
      if (idx >= n_hull) idx = n_hull - 1;
      out(m, 0) = xs[idx];
      out(m, 1) = ys[idx];
      out(m, 2) = zs[idx];
    }
    return out;
  }

  NumericMatrix out(n_hull, 3);
  for (int i = 0; i < n_hull; ++i) {
    out(i, 0) = xs[static_cast<std::size_t>(i)];
    out(i, 1) = ys[static_cast<std::size_t>(i)];
    out(i, 2) = zs[static_cast<std::size_t>(i)];
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::List estimate_sdf_rigid_cpp(const arma::mat& hull_world,
                                  Rcpp::NumericVector sdf,
                                  Rcpp::IntegerVector sdf_dim,
                                  const arma::mat& world2grid_sdf,
                                  arma::vec init_par,
                                  double outside_penalty = 20.0,
                                  std::string method = "L-BFGS-B") {
  if (hull_world.n_cols != 3) {
    stop("hull_world must have 3 columns");
  }
  if (sdf_dim.size() != 3) {
    stop("sdf_dim must have length 3");
  }
  if (sdf.size() != sdf_dim[0] * sdf_dim[1] * sdf_dim[2]) {
    stop("sdf size does not match sdf_dim");
  }

  SDFAlign fun(hull_world, sdf, sdf_dim, world2grid_sdf, outside_penalty);
  roptim::Roptim<SDFAlign> opt(method);
  opt.control.trace = 0;

  arma::vec par = init_par;
  opt.minimize(fun, par);

  arma::mat T = fun.params_to_matrix(par);
  return Rcpp::List::create(
    Rcpp::Named("par")   = par,
    Rcpp::Named("T")     = T,
    Rcpp::Named("value") = opt.value()
  );
}

