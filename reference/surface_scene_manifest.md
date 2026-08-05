# Serialize a SurfaceScene as a surfview.scene.v1 manifest

Serialize a SurfaceScene as a surfview.scene.v1 manifest

## Usage

``` r
surface_scene_manifest(scene, asset_mode = scene@asset_mode, asset_dir = NULL)
```

## Arguments

- scene:

  A \`SurfaceScene\`.

- asset_mode:

  \`"inline"\` or \`"directory"\`.

- asset_dir:

  Directory for adjacent binary assets. Required for directory mode.

## Value

A JSON-compatible named list. Directory mode also writes canonical
content-addressed assets to \`asset_dir\`.
