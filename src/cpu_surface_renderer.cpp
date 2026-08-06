#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <limits>
#include <vector>

using namespace Rcpp;

namespace {

inline double edge(double ax, double ay, double bx, double by,
                   double px, double py) {
  return (px - ax) * (by - ay) - (py - ay) * (bx - ax);
}

inline double clamp01(double x) {
  return std::max(0.0, std::min(1.0, x));
}

inline bool finite3(double a, double b, double c) {
  return std::isfinite(a) && std::isfinite(b) && std::isfinite(c);
}

inline void palette_color(const NumericMatrix& palette, double value,
                          double lo, double hi, double& r, double& g,
                          double& b) {
  double t = (hi > lo) ? clamp01((value - lo) / (hi - lo)) : 0.5;
  double pos = t * (palette.nrow() - 1);
  int i0 = static_cast<int>(std::floor(pos));
  int i1 = std::min(i0 + 1, palette.nrow() - 1);
  double f = pos - i0;
  r = palette(i0, 0) * (1.0 - f) + palette(i1, 0) * f;
  g = palette(i0, 1) * (1.0 - f) + palette(i1, 1) * f;
  b = palette(i0, 2) * (1.0 - f) + palette(i1, 2) * f;
}

inline bool passes_threshold(double value, double threshold, int tail) {
  if (!std::isfinite(value)) return false;
  if (tail == 1) return value >= threshold;
  if (tail == -1) return value <= -threshold;
  return std::abs(value) >= threshold;
}

} // namespace

// Deterministic scalar-first software rasterizer.
// projected coordinates are in output-pixel units and faces are 1-based.
// [[Rcpp::export]]
Rcpp::List cpp_rasterize_surface_scalar(
    const NumericMatrix& projected,
    const IntegerMatrix& faces,
    const NumericVector& values,
    const NumericVector& anatomy,
    const LogicalVector& cortex_mask,
    int width,
    int height,
    double threshold,
    int tail,
    const NumericVector& limits,
    const NumericMatrix& palette,
    double overlay_alpha,
    double alpha_ramp,
    double base_low,
    double base_high,
    int medial_wall_policy,
    const NumericVector& background,
    int supersample,
    bool return_buffers) {

  const int nvert = projected.nrow();
  if (projected.ncol() != 3 || faces.ncol() != 3) {
    stop("projected and faces must have three columns");
  }
  if (values.size() != nvert || anatomy.size() != nvert ||
      cortex_mask.size() != nvert) {
    stop("vertex values, anatomy, and cortex_mask must match projected rows");
  }
  if (width < 1 || height < 1 || supersample < 1) {
    stop("width, height, and supersample must be positive");
  }
  if (limits.size() != 2 || palette.ncol() < 3 || palette.nrow() < 2 ||
      background.size() < 3) {
    stop("invalid limits, palette, or background");
  }

  const int sw = width * supersample;
  const int sh = height * supersample;
  const std::size_t npix = static_cast<std::size_t>(sw) * sh;
  const float neg_inf = -std::numeric_limits<float>::infinity();
  std::vector<float> zbuffer(npix, neg_inf);
  std::vector<float> tie_scalar(npix, std::numeric_limits<float>::infinity());
  std::vector<float> scalar(npix, std::numeric_limits<float>::quiet_NaN());
  std::vector<std::uint8_t> rgba(npix * 4);
  std::vector<std::uint8_t> coverage(npix, 0);
  std::vector<std::uint8_t> cortex_coverage(npix, 0);
  std::vector<std::uint8_t> overlay_a(npix, 0);

  const std::uint8_t br = static_cast<std::uint8_t>(std::round(255 * clamp01(background[0])));
  const std::uint8_t bg = static_cast<std::uint8_t>(std::round(255 * clamp01(background[1])));
  const std::uint8_t bb = static_cast<std::uint8_t>(std::round(255 * clamp01(background[2])));
  for (std::size_t p = 0; p < npix; ++p) {
    rgba[p * 4] = br;
    rgba[p * 4 + 1] = bg;
    rgba[p * 4 + 2] = bb;
    rgba[p * 4 + 3] = 255;
  }

  const double eps = 1e-10;
  for (int fi = 0; fi < faces.nrow(); ++fi) {
    const int i0 = faces(fi, 0) - 1;
    const int i1 = faces(fi, 1) - 1;
    const int i2 = faces(fi, 2) - 1;
    if (i0 < 0 || i1 < 0 || i2 < 0 || i0 >= nvert || i1 >= nvert || i2 >= nvert) {
      stop("faces contain an out-of-range vertex index");
    }

    const double x0 = projected(i0, 0) * supersample;
    const double y0 = projected(i0, 1) * supersample;
    const double z0 = projected(i0, 2);
    const double x1 = projected(i1, 0) * supersample;
    const double y1 = projected(i1, 1) * supersample;
    const double z1 = projected(i1, 2);
    const double x2 = projected(i2, 0) * supersample;
    const double y2 = projected(i2, 1) * supersample;
    const double z2 = projected(i2, 2);
    if (!finite3(x0, y0, z0) || !finite3(x1, y1, z1) ||
        !finite3(x2, y2, z2)) continue;

    const double area = edge(x0, y0, x1, y1, x2, y2);
    if (!std::isfinite(area) || std::abs(area) <= eps) continue;

    const int xmin = std::max(0, static_cast<int>(std::floor(std::min({x0, x1, x2}))));
    const int xmax = std::min(sw - 1, static_cast<int>(std::ceil(std::max({x0, x1, x2}))));
    const int ymin = std::max(0, static_cast<int>(std::floor(std::min({y0, y1, y2}))));
    const int ymax = std::min(sh - 1, static_cast<int>(std::ceil(std::max({y0, y1, y2}))));
    const bool face_cortex = cortex_mask[i0] == TRUE &&
      cortex_mask[i1] == TRUE && cortex_mask[i2] == TRUE;
    if (!face_cortex && medial_wall_policy == 1) continue;

    for (int py = ymin; py <= ymax; ++py) {
      const double sy = py + 0.5;
      for (int px = xmin; px <= xmax; ++px) {
        const double sx = px + 0.5;
        const double w0 = edge(x1, y1, x2, y2, sx, sy) / area;
        const double w1 = edge(x2, y2, x0, y0, sx, sy) / area;
        const double w2 = 1.0 - w0 - w1;
        if (w0 < -eps || w1 < -eps || w2 < -eps) continue;

        const double z = w0 * z0 + w1 * z1 + w2 * z2;
        const double sv = w0 * values[i0] + w1 * values[i1] + w2 * values[i2];
        const std::size_t pos = static_cast<std::size_t>(py) * sw + px;
        const bool nearer = z > zbuffer[pos] + eps;
        const bool tie = std::abs(z - zbuffer[pos]) <= eps;
        const float tie_value = std::isfinite(sv) ? static_cast<float>(sv) :
          std::numeric_limits<float>::infinity();
        if (!nearer && !(tie && tie_value < tie_scalar[pos])) continue;

        zbuffer[pos] = static_cast<float>(z);
        tie_scalar[pos] = tie_value;
        scalar[pos] = static_cast<float>(sv);
        coverage[pos] = 1;
        cortex_coverage[pos] = face_cortex ? 1 : 0;

        double av = w0 * anatomy[i0] + w1 * anatomy[i1] + w2 * anatomy[i2];
        if (!std::isfinite(av)) av = 0.5;
        double base = base_low + clamp01(av) * (base_high - base_low);
        if (!face_cortex) base = 0.78;
        double rr = base, gg = base, bl = base;
        double aa = 0.0;

        if (face_cortex && passes_threshold(sv, threshold, tail)) {
          aa = overlay_alpha;
          if (alpha_ramp > 0) {
            aa *= clamp01((std::abs(sv) - threshold) / alpha_ramp);
          }
          double pr, pg, pb;
          palette_color(palette, sv, limits[0], limits[1], pr, pg, pb);
          // Premultiplied-alpha source-over composition onto opaque anatomy.
          rr = pr * aa + rr * (1.0 - aa);
          gg = pg * aa + gg * (1.0 - aa);
          bl = pb * aa + bl * (1.0 - aa);
        }

        rgba[pos * 4] = static_cast<std::uint8_t>(std::round(255 * clamp01(rr)));
        rgba[pos * 4 + 1] = static_cast<std::uint8_t>(std::round(255 * clamp01(gg)));
        rgba[pos * 4 + 2] = static_cast<std::uint8_t>(std::round(255 * clamp01(bl)));
        rgba[pos * 4 + 3] = 255;
        overlay_a[pos] = static_cast<std::uint8_t>(std::round(255 * clamp01(aa)));
      }
    }
  }

  RawVector out_rgba(static_cast<R_xlen_t>(width) * height * 4);
  RawVector out_overlay(static_cast<R_xlen_t>(width) * height);
  LogicalMatrix out_coverage(height, width);
  LogicalMatrix out_cortex_coverage(height, width);
  NumericMatrix out_scalar;
  NumericMatrix out_depth;
  if (return_buffers) {
    out_scalar = NumericMatrix(height, width);
    out_depth = NumericMatrix(height, width);
    std::fill(out_scalar.begin(), out_scalar.end(), NA_REAL);
    std::fill(out_depth.begin(), out_depth.end(), R_NegInf);
  }

  for (int oy = 0; oy < height; ++oy) {
    for (int ox = 0; ox < width; ++ox) {
      unsigned int sums[4] = {0, 0, 0, 0};
      unsigned int alpha_sum = 0;
      bool covered = false;
      bool cortex_covered = false;
      double scalar_sum = 0.0;
      double depth_max = R_NegInf;
      int scalar_n = 0;
      for (int ay = 0; ay < supersample; ++ay) {
        for (int ax = 0; ax < supersample; ++ax) {
          const int ix = ox * supersample + ax;
          const int iy = oy * supersample + ay;
          const std::size_t hp = static_cast<std::size_t>(iy) * sw + ix;
          for (int c = 0; c < 4; ++c) sums[c] += rgba[hp * 4 + c];
          alpha_sum += overlay_a[hp];
          covered = covered || coverage[hp];
          cortex_covered = cortex_covered || cortex_coverage[hp];
          if (std::isfinite(scalar[hp])) {
            scalar_sum += scalar[hp];
            ++scalar_n;
          }
          depth_max = std::max(depth_max, static_cast<double>(zbuffer[hp]));
        }
      }
      const unsigned int denom = supersample * supersample;
      const std::size_t op = static_cast<std::size_t>(oy) +
        static_cast<std::size_t>(height) * ox;
      for (int c = 0; c < 4; ++c) {
        const std::size_t channel_pos = op +
          static_cast<std::size_t>(height) * width * c;
        out_rgba[channel_pos] = static_cast<Rbyte>(
          std::round(static_cast<double>(sums[c]) / denom)
        );
      }
      out_overlay[op] = static_cast<Rbyte>(std::round(static_cast<double>(alpha_sum) / denom));
      out_coverage(oy, ox) = covered;
      out_cortex_coverage(oy, ox) = cortex_covered;
      if (return_buffers) {
        if (scalar_n) out_scalar(oy, ox) = scalar_sum / scalar_n;
        out_depth(oy, ox) = depth_max;
      }
    }
  }

  out_rgba.attr("dim") = IntegerVector::create(height, width, 4);
  out_overlay.attr("dim") = IntegerVector::create(height, width);
  return List::create(
    _["rgba"] = out_rgba,
    _["coverage"] = out_coverage,
    _["cortex_coverage"] = out_cortex_coverage,
    _["overlay_alpha"] = out_overlay,
    _["scalar"] = return_buffers ? static_cast<SEXP>(out_scalar) : R_NilValue,
    _["depth"] = return_buffers ? static_cast<SEXP>(out_depth) : R_NilValue
  );
}
