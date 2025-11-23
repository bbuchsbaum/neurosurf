# Remesh a SurfaceGeometry object

This function applies uniform remeshing to a SurfaceGeometry object,
creating a new mesh with more regular face sizes and improved quality.

## Usage

``` r
remeshSurface(surfgeom, voxel_size = 2, ...)
```

## Arguments

- surfgeom:

  A `SurfaceGeometry` object to be remeshed.

- voxel_size:

  Numeric value specifying the target edge length in the remeshed
  output. Smaller values create finer meshes with more faces.

- ...:

  Additional arguments to pass to
  [`Rvcg::vcgUniformRemesh`](https://rdrr.io/pkg/Rvcg/man/vcgUniformRemesh.html).

## Value

A new `SurfaceGeometry` object with the remeshed surface.

## Details

Remeshing is a process that reconstructs the mesh to improve its quality
and/or adjust its resolution. Uniform remeshing creates a new mesh where
the edge lengths are approximately equal throughout the surface, which
is often desirable for analysis and visualization.

The `voxel_size` parameter controls the resolution of the output mesh:

- Smaller values create denser meshes with more vertices and faces

- Larger values create coarser meshes with fewer vertices and faces

Common reasons to remesh a surface include:

- Simplifying high-resolution meshes for faster processing

- Creating more uniform triangle sizes for better numerical stability

- Preparing meshes for specific analyses that require regular structures

- Fixing mesh defects and improving the overall quality

This function uses the VCG library (via the Rvcg package) to perform the
remeshing operation, which is a robust and widely-used algorithm for
mesh processing.

## See also

[`SurfaceGeometry`](SurfaceGeometry.md),
[`vcgUniformRemesh`](https://rdrr.io/pkg/Rvcg/man/vcgUniformRemesh.html)

## Examples

``` r
# \donttest{
# Create a simple cube mesh
vertices <- matrix(c(
  0, 0, 0,  # vertex 1
  1, 0, 0,  # vertex 2
  1, 1, 0,  # vertex 3
  0, 1, 0,  # vertex 4
  0, 0, 1,  # vertex 5
  1, 0, 1,  # vertex 6
  1, 1, 1,  # vertex 7
  0, 1, 1   # vertex 8
), ncol = 3, byrow = TRUE)

# Define faces (12 triangular faces making a cube)
# Note indices are 0-based
faces <- matrix(c(
  # bottom face (z=0)
  0, 1, 2,
  0, 2, 3,
  # top face (z=1)
  4, 5, 6,
  4, 6, 7,
  # front face (y=0)
  0, 1, 5,
  0, 5, 4,
  # back face (y=1)
  2, 3, 7,
  2, 7, 6,
  # left face (x=0)
  0, 3, 7,
  0, 7, 4,
  # right face (x=1)
  1, 2, 6,
  1, 6, 5
), ncol = 3, byrow = TRUE)

# Create the SurfaceGeometry object
surf_geom <- SurfaceGeometry(vertices, faces, "lh")

# Remesh with a coarse voxel size - fewer triangles
coarse_remesh <- remeshSurface(surf_geom, voxel_size = 0.5)
#>      Resampling mesh using a volume of 2 x 2 x 2
#>      VoxelSize is 0.500000, offset is 0.000000
#>      Mesh Box is 1.000000 1.000000 1.000000

# Remesh with a fine voxel size - more triangles
fine_remesh <- remeshSurface(surf_geom, voxel_size = 0.2)
#>      Resampling mesh using a volume of 6 x 6 x 6
#>      VoxelSize is 0.200000, offset is 0.000000
#>      Mesh Box is 1.000000 1.000000 1.000000

# Visualize the meshes if rgl is available
if(requireNamespace("rgl", quietly = TRUE)) {
  # Original mesh
  rgl::open3d()
  rgl::shade3d(surf_geom@mesh, col = "red")
  rgl::title3d(main = "Original Mesh")

  # Coarse remesh
  rgl::open3d()
  rgl::shade3d(coarse_remesh@mesh, col = "green")
  rgl::title3d(main = "Coarse Remesh (voxel_size = 0.5)")

  # Fine remesh
  rgl::open3d()
  rgl::shade3d(fine_remesh@mesh, col = "blue")
  rgl::title3d(main = "Fine Remesh (voxel_size = 0.2)")

  # Compare the number of faces in each mesh
  cat("Original mesh faces:", ncol(surf_geom@mesh$it), "\n")
  cat("Coarse remesh faces:", ncol(coarse_remesh@mesh$it), "\n")
  cat("Fine remesh faces:", ncol(fine_remesh@mesh$it), "\n")
}
#> Original mesh faces: 12 
#> Coarse remesh faces: 17 
#> Fine remesh faces: 373 
# }
```
