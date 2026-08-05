# Write a standalone portable surface report

Write a standalone portable surface report

## Usage

``` r
write_surface_scene(scene, path, self_contained = FALSE, title = scene@id)
```

## Arguments

- scene:

  A \`SurfaceScene\`.

- path:

  Output directory.

- self_contained:

  If \`TRUE\`, inline both assets and the browser runtime. Otherwise,
  write the runtime and SHA-addressed assets beside \`index.html\`.

- title:

  HTML document title.

## Value

The path to \`index.html\`, invisibly.
