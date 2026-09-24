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

inline void normalize3(double& x, double& y, double& z) {
  const double len = std::sqrt(x * x + y * y + z * z);
  if (len > 0) {
    x /= len;
    y /= len;
    z /= len;
  }
}

// Two-light Blinn-Phong shading in view space (x right, y up, z toward the
// viewer). Returns the diffuse multiplier and the additive specular term.
struct Lighting {
  bool enabled = false;
  double ambient = 1.0, key = 0.0, fill = 0.0, specular = 0.0, shininess = 1.0;
  double overlay_shading = 1.0, sky = 0.0, reference = 1.0;
  double lx = 0, ly = 0, lz = 1, fx = 0, fy = 0, fz = 1, hx = 0, hy = 0, hz = 1;

  void shade(double nx, double ny, double nz, double& diffuse,
             double& spec) const {
    normalize3(nx, ny, nz);
    // Interpolated normals can tip slightly away at the silhouette; light
    // the visible side.
    if (nz < 0) {
      nx = -nx;
      ny = -ny;
      nz = -nz;
    }
    const double nl = std::max(0.0, nx * lx + ny * ly + nz * lz);
    const double nf = std::max(0.0, nx * fx + ny * fy + nz * fz);
    const double nh = std::max(0.0, nx * hx + ny * hy + nz * hz);
    // Hemispheric ambient: surfaces facing up receive more sky light.
    diffuse = ambient * (1.0 - sky + sky * ny) + key * nl + fill * nf;
    spec = specular * std::pow(nh, shininess);
  }
};

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
    bool return_buffers,
    Rcpp::Nullable<Rcpp::NumericMatrix> normals = R_NilValue,
    Rcpp::Nullable<Rcpp::NumericVector> lighting = R_NilValue,
    Rcpp::Nullable<Rcpp::NumericVector> contour = R_NilValue) {

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

  // Optional per-vertex view-space normals and lighting parameters:
  // ambient, key, fill, specular, shininess, key direction (3), fill
  // direction (3), the fraction of the shading applied to overlay colour, and
  // the hemispheric (sky) ambient fraction.
  Lighting light;
  NumericMatrix nrm;
  if (normals.isNotNull() && lighting.isNotNull()) {
    nrm = NumericMatrix(normals.get());
    NumericVector lp(lighting.get());
    if (nrm.nrow() != nvert || nrm.ncol() != 3 || lp.size() != 13) {
      stop("normals must be nvert x 3 and lighting must have 13 values");
    }
    light.overlay_shading = lp[11];
    light.sky = lp[12];
    light.enabled = true;
    light.ambient = lp[0];
    light.key = lp[1];
    light.fill = lp[2];
    light.specular = lp[3];
    light.shininess = lp[4];
    light.lx = lp[5]; light.ly = lp[6]; light.lz = lp[7];
    light.fx = lp[8]; light.fy = lp[9]; light.fz = lp[10];
    normalize3(light.lx, light.ly, light.lz);
    normalize3(light.fx, light.fy, light.fz);
    light.hx = light.lx;
    light.hy = light.ly;
    light.hz = light.lz + 1.0;
    normalize3(light.hx, light.hy, light.hz);
    // Shading of a surface facing the viewer; overlay colour is modulated
    // relative to it so camera-facing overlay matches the colour bar.
    double ref_spec;
    light.shade(0.0, 0.0, 1.0, light.reference, ref_spec);
    if (light.reference <= 0) light.reference = 1.0;
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
        double diffuse = 1.0, spec = 0.0;
        if (light.enabled) {
          light.shade(w0 * nrm(i0, 0) + w1 * nrm(i1, 0) + w2 * nrm(i2, 0),
                      w0 * nrm(i0, 1) + w1 * nrm(i1, 1) + w2 * nrm(i2, 1),
                      w0 * nrm(i0, 2) + w1 * nrm(i1, 2) + w2 * nrm(i2, 2),
                      diffuse, spec);
        }
        const double lit_base = base * diffuse + spec;
        double rr = lit_base, gg = lit_base, bl = lit_base;
        double aa = 0.0;

        if (face_cortex && passes_threshold(sv, threshold, tail)) {
          aa = overlay_alpha;
          if (alpha_ramp > 0) {
            aa *= clamp01((std::abs(sv) - threshold) / alpha_ramp);
          }
          double pr, pg, pb;
          palette_color(palette, sv, limits[0], limits[1], pr, pg, pb);
          // Overlay colour receives a reduced share of the shading so its
          // hue and value stay readable against the colour bar.
          const double k = light.overlay_shading;
          // The modulation is bounded so a constant value keeps a readable
          // colour at grazing incidence: lighting conveys form, the palette
          // conveys value.
          const double rel = std::max(0.85, std::min(1.08, diffuse / light.reference));
          const double od = 1.0 + (rel - 1.0) * k;
          const double os = std::min(spec, 0.05) * k;
          // Source-over composition onto the lit anatomy.
          rr = (pr * od + os) * aa + rr * (1.0 - aa);
          gg = (pg * od + os) * aa + gg * (1.0 - aa);
          bl = (pb * od + os) * aa + bl * (1.0 - aa);
        }

        rgba[pos * 4] = static_cast<std::uint8_t>(std::round(255 * clamp01(rr)));
        rgba[pos * 4 + 1] = static_cast<std::uint8_t>(std::round(255 * clamp01(gg)));
        rgba[pos * 4 + 2] = static_cast<std::uint8_t>(std::round(255 * clamp01(bl)));
        rgba[pos * 4 + 3] = 255;
        overlay_a[pos] = static_cast<std::uint8_t>(std::round(255 * clamp01(aa)));
      }
    }
  }

  // Outer silhouette contour at sample resolution: covered samples within
  // `radius` samples of background connected to the image border. Enclosed
  // holes are not exterior. Drawn before the box filter, so it is
  // anti-aliased. contour = (r, g, b, alpha, width in output pixels).
  if (contour.isNotNull()) {
    NumericVector cp(contour.get());
    if (cp.size() != 5) stop("contour must have 5 values");
    const int radius = std::max(1, static_cast<int>(std::round(cp[4] * supersample)));
    std::vector<std::uint8_t> exterior(npix, 0);
    std::vector<std::size_t> queue;
    queue.reserve(npix / 4);
    auto push = [&](int x, int y) {
      if (x < 0 || y < 0 || x >= sw || y >= sh) return;
      const std::size_t q = static_cast<std::size_t>(y) * sw + x;
      if (coverage[q] || exterior[q]) return;
      exterior[q] = 1;
      queue.push_back(q);
    };
    for (int x = 0; x < sw; ++x) { push(x, 0); push(x, sh - 1); }
    for (int y = 0; y < sh; ++y) { push(0, y); push(sw - 1, y); }
    for (std::size_t head = 0; head < queue.size(); ++head) {
      const int x = static_cast<int>(queue[head] % sw);
      const int y = static_cast<int>(queue[head] / sw);
      push(x - 1, y); push(x + 1, y); push(x, y - 1); push(x, y + 1);
    }
    // Distance (in 4-neighbour steps) from the exterior, up to `radius`.
    std::vector<std::uint8_t> near(exterior);
    for (int step = 0; step < radius; ++step) {
      std::vector<std::uint8_t> grown(near);
      for (int y = 0; y < sh; ++y) {
        for (int x = 0; x < sw; ++x) {
          const std::size_t q = static_cast<std::size_t>(y) * sw + x;
          if (near[q]) continue;
          if ((x > 0 && near[q - 1]) || (x < sw - 1 && near[q + 1]) ||
              (y > 0 && near[q - sw]) || (y < sh - 1 && near[q + sw])) {
            grown[q] = 1;
          }
        }
      }
      near.swap(grown);
    }
    const double a = clamp01(cp[3]);
    for (std::size_t q = 0; q < npix; ++q) {
      if (!coverage[q] || !near[q]) continue;
      for (int c = 0; c < 3; ++c) {
        const double v = rgba[q * 4 + c] / 255.0;
        rgba[q * 4 + c] = static_cast<std::uint8_t>(
          std::round(255 * clamp01(cp[c] * a + v * (1.0 - a))));
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

// Per-pixel geometry buffer for deferred shading. Returns, for each pixel of a
// width x height raster (column-major, height rows), the nearest visible face
// (1-based; 0 where nothing is covered) and the barycentric weights of that
// face's first two vertices. Pixel centres are sampled at (x + 0.5, y + 0.5);
// callers supersample by projecting into a proportionally larger raster.
// [[Rcpp::export]]
Rcpp::List cpp_rasterize_surface_gbuffer(const NumericMatrix& projected,
                                         const IntegerMatrix& faces,
                                         int width,
                                         int height) {
  const int nvert = projected.nrow();
  if (projected.ncol() != 3 || faces.ncol() != 3) {
    stop("projected and faces must have three columns");
  }
  if (width < 1 || height < 1) {
    stop("width and height must be positive");
  }
  const std::size_t npix = static_cast<std::size_t>(width) * height;
  IntegerVector face(npix, 0);
  NumericVector w0v(npix, NA_REAL);
  NumericVector w1v(npix, NA_REAL);
  std::vector<double> zbuffer(npix, -std::numeric_limits<double>::infinity());
  const double eps = 1e-12;

  for (int fi = 0; fi < faces.nrow(); ++fi) {
    const int i0 = faces(fi, 0) - 1;
    const int i1 = faces(fi, 1) - 1;
    const int i2 = faces(fi, 2) - 1;
    if (i0 < 0 || i1 < 0 || i2 < 0 || i0 >= nvert || i1 >= nvert || i2 >= nvert) {
      stop("faces contain an out-of-range vertex index");
    }
    const double x0 = projected(i0, 0), y0 = projected(i0, 1), z0 = projected(i0, 2);
    const double x1 = projected(i1, 0), y1 = projected(i1, 1), z1 = projected(i1, 2);
    const double x2 = projected(i2, 0), y2 = projected(i2, 1), z2 = projected(i2, 2);
    if (!finite3(x0, y0, z0) || !finite3(x1, y1, z1) ||
        !finite3(x2, y2, z2)) continue;
    const double area = edge(x0, y0, x1, y1, x2, y2);
    if (!std::isfinite(area) || std::abs(area) <= eps) continue;

    const int xmin = std::max(0, static_cast<int>(std::floor(std::min({x0, x1, x2}))));
    const int xmax = std::min(width - 1, static_cast<int>(std::ceil(std::max({x0, x1, x2}))));
    const int ymin = std::max(0, static_cast<int>(std::floor(std::min({y0, y1, y2}))));
    const int ymax = std::min(height - 1, static_cast<int>(std::ceil(std::max({y0, y1, y2}))));
    for (int py = ymin; py <= ymax; ++py) {
      const double sy = py + 0.5;
      for (int px = xmin; px <= xmax; ++px) {
        const double sx = px + 0.5;
        const double w0 = edge(x1, y1, x2, y2, sx, sy) / area;
        const double w1 = edge(x2, y2, x0, y0, sx, sy) / area;
        const double w2 = 1.0 - w0 - w1;
        if (w0 < -1e-9 || w1 < -1e-9 || w2 < -1e-9) continue;
        const double z = w0 * z0 + w1 * z1 + w2 * z2;
        const std::size_t pos = static_cast<std::size_t>(py) +
          static_cast<std::size_t>(height) * px;
        if (z <= zbuffer[pos]) continue;
        zbuffer[pos] = z;
        face[pos] = fi + 1;
        w0v[pos] = w0;
        w1v[pos] = w1;
      }
    }
  }
  face.attr("dim") = IntegerVector::create(height, width);
  w0v.attr("dim") = IntegerVector::create(height, width);
  w1v.attr("dim") = IntegerVector::create(height, width);
  return List::create(_["face"] = face, _["w0"] = w0v, _["w1"] = w1v);
}
