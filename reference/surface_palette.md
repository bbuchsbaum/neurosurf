# Colour palettes for surface maps

The palettes neurosurf uses for statistical surface maps, as colour
vectors for any renderer:
[`render_surface_rgba`](https://bbuchsbaum.github.io/neurosurf/reference/render_surface_rgba.md)
and
[`surface_figure`](https://bbuchsbaum.github.io/neurosurf/reference/surface_figure.md)
use them by default, and they can be passed to
[`view_surface`](https://bbuchsbaum.github.io/neurosurf/reference/view_surface.md)
(`cmap`) or
[`surface_layer`](https://bbuchsbaum.github.io/neurosurf/reference/surface_layer.md)
(`colormap`) so interactive and static figures share one colour
language. The two-sided palette runs from dark blue through light blue
and light orange to dark red. It has no white or grey centre, so values
just past a threshold stay distinct from the grey anatomy.

## Usage

``` r
surface_palette(
  type = c("two_sided", "positive", "negative"),
  n = 256L,
  limits = NULL,
  threshold = NULL
)
```

## Arguments

- type:

  `"two_sided"` for signed maps, `"positive"` or `"negative"` for
  one-tailed maps.

- n:

  Number of colours. `NULL` returns the palette's anchor colours.

- limits, threshold:

  Optional colour limits and threshold magnitude. With both, the result
  is the lookup table the CPU renderer uses: `n` colours evenly spaced
  over `limits`, with each tail's ramp starting at the threshold. Pass
  it with the same limits to a viewer (for example
  [`surface_layer`](https://bbuchsbaum.github.io/neurosurf/reference/surface_layer.md))
  to reproduce a static figure's colours exactly.

## Value

A character vector of hex colours.

## Examples

``` r
surface_palette("two_sided", n = 7)
#> [1] "#12397B" "#2A64AB" "#5B92CF" "#BF9BA1" "#E47553" "#C0352C" "#790A1F"
surface_palette("positive")
#>   [1] "#F7A540" "#F6A440" "#F6A340" "#F6A240" "#F6A23F" "#F6A13F" "#F6A03F"
#>   [8] "#F59F3F" "#F59F3E" "#F59E3E" "#F59D3E" "#F59C3E" "#F59C3D" "#F59B3D"
#>  [15] "#F49A3D" "#F4993D" "#F4993C" "#F4983C" "#F4973C" "#F4963C" "#F3963B"
#>  [22] "#F3953B" "#F3943B" "#F3933B" "#F3923A" "#F3923A" "#F3913A" "#F2903A"
#>  [29] "#F28F39" "#F28F39" "#F28E39" "#F28D39" "#F28C38" "#F18C38" "#F18B38"
#>  [36] "#F18A38" "#F18937" "#F18937" "#F18837" "#F08737" "#F08636" "#F08536"
#>  [43] "#F08536" "#F08436" "#EF8335" "#EF8235" "#EF8235" "#EF8135" "#EF8034"
#>  [50] "#EF7F34" "#EE7E34" "#EE7E34" "#EE7D33" "#EE7C33" "#EE7B33" "#ED7A33"
#>  [57] "#ED7A32" "#ED7932" "#ED7832" "#ED7732" "#EC7631" "#EC7631" "#EC7531"
#>  [64] "#EC7431" "#EC7330" "#EC7230" "#EB7230" "#EB7130" "#EB702F" "#EB6F2F"
#>  [71] "#EB6E2F" "#EA6D2F" "#EA6D2F" "#EA6C2E" "#EA6B2E" "#EA6A2E" "#E9692E"
#>  [78] "#E9682D" "#E9682D" "#E9672D" "#E9662D" "#E8652C" "#E8642C" "#E8632C"
#>  [85] "#E8622C" "#E7612C" "#E7612C" "#E7602C" "#E6602C" "#E65F2C" "#E55F2C"
#>  [92] "#E55E2C" "#E45D2C" "#E45D2C" "#E35C2C" "#E35C2C" "#E35B2C" "#E25A2C"
#>  [99] "#E25A2C" "#E1592C" "#E1592C" "#E0582C" "#E0572C" "#DF572C" "#DF562C"
#> [106] "#DF562C" "#DE552C" "#DE542C" "#DD542C" "#DD532C" "#DC532C" "#DC522C"
#> [113] "#DB512C" "#DB512C" "#DB502C" "#DA4F2C" "#DA4F2C" "#D94E2C" "#D94E2C"
#> [120] "#D84D2C" "#D84C2C" "#D74C2C" "#D74B2C" "#D74A2C" "#D64A2C" "#D6492C"
#> [127] "#D5492C" "#D5482C" "#D4472C" "#D4472C" "#D3462C" "#D3452C" "#D3452C"
#> [134] "#D2442C" "#D2432C" "#D1432C" "#D1422C" "#D0412C" "#D0412C" "#CF402C"
#> [141] "#CF3F2C" "#CE3F2C" "#CE3E2C" "#CE3D2C" "#CD3D2C" "#CD3C2C" "#CC3B2C"
#> [148] "#CC3B2C" "#CB3A2C" "#CB392C" "#CA392C" "#CA382C" "#CA372C" "#C9362C"
#> [155] "#C9362C" "#C8352C" "#C8342C" "#C7342C" "#C7332C" "#C6322C" "#C6312C"
#> [162] "#C6312C" "#C5302C" "#C52F2C" "#C42E2C" "#C42E2C" "#C32D2C" "#C32C2C"
#> [169] "#C22B2C" "#C22A2C" "#C12A2C" "#C1292B" "#C0292B" "#BF282B" "#BE282B"
#> [176] "#BD282B" "#BC272B" "#BB272A" "#BA272A" "#BA262A" "#B9262A" "#B8252A"
#> [183] "#B7252A" "#B6252A" "#B52429" "#B42429" "#B42429" "#B32329" "#B22329"
#> [190] "#B12329" "#B02228" "#AF2228" "#AE2128" "#AD2128" "#AD2128" "#AC2028"
#> [197] "#AB2028" "#AA2027" "#A91F27" "#A81F27" "#A71F27" "#A71E27" "#A61E27"
#> [204] "#A51D27" "#A41D26" "#A31D26" "#A21C26" "#A11C26" "#A11C26" "#A01B26"
#> [211] "#9F1B25" "#9E1A25" "#9D1A25" "#9C1A25" "#9B1925" "#9B1925" "#9A1925"
#> [218] "#991824" "#981824" "#971824" "#961724" "#961724" "#951624" "#941623"
#> [225] "#931623" "#921523" "#911523" "#911523" "#901423" "#8F1423" "#8E1322"
#> [232] "#8D1322" "#8C1322" "#8C1222" "#8B1222" "#8A1222" "#891121" "#881121"
#> [239] "#871021" "#871021" "#861021" "#850F21" "#840F21" "#830E20" "#820E20"
#> [246] "#820E20" "#810D20" "#800D20" "#7F0C20" "#7E0C1F" "#7E0C1F" "#7D0B1F"
#> [253] "#7C0B1F" "#7B0A1F" "#7A0A1F" "#790A1F"
# Matches render_surface_rgba(limits = c(-5, 5), threshold = 2)
lut <- surface_palette("two_sided", limits = c(-5, 5), threshold = 2)
```
