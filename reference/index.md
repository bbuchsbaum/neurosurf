# Package index

## Read surfaces

Read surface geometry and data files into R.

- [`read_surf()`](https://bbuchsbaum.github.io/neurosurf/reference/read_surf.md)
  : Read Surface Data from a File

- [`read_surf_data()`](https://bbuchsbaum.github.io/neurosurf/reference/read_surf_data.md)
  :

  load surface data and link to `SurfaceGeometry`

- [`read_surf_data_seq()`](https://bbuchsbaum.github.io/neurosurf/reference/read_surf_data_seq.md)
  : Read Surface Data Sequence

- [`read_freesurfer_annot()`](https://bbuchsbaum.github.io/neurosurf/reference/read_freesurfer_annot.md)
  : Read Freesurfer Annotation File

- [`read_surf_geometry()`](https://bbuchsbaum.github.io/neurosurf/reference/read_surf_geometry.md)
  : Read Surface Geometry from File

- [`write_surf_data()`](https://bbuchsbaum.github.io/neurosurf/reference/write_surf_data.md)
  : Write Surface Data to File

## Interactive surface reports

Build bilateral, multi-map viewers and portable offline reports.

- [`surface_scene()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_scene.md)
  : Construct a validated portable surface scene
- [`surface_layer()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_layer.md)
  : Define a named scalar layer for a surface scene
- [`show(`*`<SurfaceScene>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceScene-class.md)
  : A portable cortical surface scene
- [`surfwidget()`](https://bbuchsbaum.github.io/neurosurf/reference/surfwidget-methods.md)
  : Create an interactive surface viewer
- [`show_surface_widget()`](https://bbuchsbaum.github.io/neurosurf/reference/show_surface_widget.md)
  : Show an interactive surface widget
- [`write_surface_scene()`](https://bbuchsbaum.github.io/neurosurf/reference/write_surface_scene.md)
  : Write a standalone portable surface report
- [`surface_scene_manifest()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_scene_manifest.md)
  : Serialize a SurfaceScene as a surfview.scene.v1 manifest
- [`debug_surfwidget()`](https://bbuchsbaum.github.io/neurosurf/reference/debug_surfwidget.md)
  : Debugging Helper for surfwidget

## Surface-based operations

Transform surface data and geometry.

- [`smooth()`](https://bbuchsbaum.github.io/neurosurf/reference/smooth-methods.md)
  : Generic Function for Smoothing a Surface or Associated Data
- [`cluster_threshold()`](https://bbuchsbaum.github.io/neurosurf/reference/cluster_threshold-methods.md)
  : Apply Cluster-Extent Threshold to Surface Data
- [`curvature()`](https://bbuchsbaum.github.io/neurosurf/reference/curvature-methods.md)
  : Compute Surface Curvature Vector

## Extractors

Extract information from surface data structures.

- [`vertices()`](https://bbuchsbaum.github.io/neurosurf/reference/vertices-methods.md)
  : Extract Vertices from a Surface Object

- [`nodes()`](https://bbuchsbaum.github.io/neurosurf/reference/nodes-methods.md)
  : Extract Surface Node Numbers

- [`geometry()`](https://bbuchsbaum.github.io/neurosurf/reference/geometry-methods.md)
  : Extract Geometry from Surface Object

- [`graph()`](https://bbuchsbaum.github.io/neurosurf/reference/graph-methods.md)
  :

  extract `igraph` object

- [`left()`](https://bbuchsbaum.github.io/neurosurf/reference/left-methods.md)
  : Get Left Hemisphere

- [`right()`](https://bbuchsbaum.github.io/neurosurf/reference/right-methods.md)
  : Get Right Hemisphere

- [`adjacency()`](https://bbuchsbaum.github.io/neurosurf/reference/adjacency-methods.md)
  : Get Adjacency Graph

- [`laplacian()`](https://bbuchsbaum.github.io/neurosurf/reference/laplacian-methods.md)
  : Compute Graph Laplacian

## Complete reference

Catch-all index to include any remaining topics.

- [`AFNISurfaceFileDescriptor-class`](https://bbuchsbaum.github.io/neurosurf/reference/AFNISurfaceFileDescriptor-class.md)
  : AFNISurfaceFileDescriptor

- [`Arith(`*`<NeuroSurface>`*`,`*`<NeuroSurface>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/Arith-NeuroSurface-method.md)
  [`Arith(`*`<NeuroSurface>`*`,`*`<numeric>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/Arith-NeuroSurface-method.md)
  [`Arith(`*`<numeric>`*`,`*`<NeuroSurface>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/Arith-NeuroSurface-method.md)
  [`Arith(`*`<NeuroSurface>`*`,`*`<NeuroSurfaceVector>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/Arith-NeuroSurface-method.md)
  : Arithmetic Operations for NeuroSurface Objects

- [`Arith(`*`<NeuroSurfaceVector>`*`,`*`<NeuroSurfaceVector>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/Arith-NeuroSurfaceVector-method.md)
  [`Arith(`*`<NeuroSurfaceVector>`*`,`*`<numeric>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/Arith-NeuroSurfaceVector-method.md)
  [`Arith(`*`<numeric>`*`,`*`<NeuroSurfaceVector>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/Arith-NeuroSurfaceVector-method.md)
  [`Arith(`*`<NeuroSurfaceVector>`*`,`*`<NeuroSurface>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/Arith-NeuroSurfaceVector-method.md)
  : Arithmetic Operations for NeuroSurfaceVector Objects

- [`BilatNeuroSurfaceVector-class`](https://bbuchsbaum.github.io/neurosurf/reference/BilatNeuroSurfaceVector-class.md)
  : Bilateral NeuroSurface Vector Class

- [`ColorMappedNeuroSurface-class`](https://bbuchsbaum.github.io/neurosurf/reference/ColorMappedNeuroSurface-class.md)
  : ColorMappedNeuroSurface

- [`ColorMappedNeuroSurface()`](https://bbuchsbaum.github.io/neurosurf/reference/ColorMappedNeuroSurface.md)
  : ColorMappedNeuroSurface

- [`Compare(`*`<NeuroSurface>`*`,`*`<NeuroSurface>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/Compare-NeuroSurface-method.md)
  : Comparison Operations for NeuroSurface Objects

- [`Compare(`*`<NeuroSurface>`*`,`*`<numeric>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/Compare-NeuroSurface-numeric-method.md)
  : Comparison Operations for NeuroSurface Objects

- [`Compare(`*`<NeuroSurfaceVector>`*`,`*`<NeuroSurfaceVector>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/Compare-NeuroSurfaceVector-method.md)
  [`Compare(`*`<NeuroSurfaceVector>`*`,`*`<numeric>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/Compare-NeuroSurfaceVector-method.md)
  [`Compare(`*`<numeric>`*`,`*`<NeuroSurfaceVector>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/Compare-NeuroSurfaceVector-method.md)
  : Comparison Operations for NeuroSurfaceVector Objects

- [`FreesurferSurfaceGeometryMetaInfo-class`](https://bbuchsbaum.github.io/neurosurf/reference/FreeSurferSurfaceGeometryMetaInfo-class.md)
  : FreesurferSurfaceGeometryMetaInfo Class

- [`FreesurferAsciiSurfaceFileDescriptor-class`](https://bbuchsbaum.github.io/neurosurf/reference/FreesurferAsciiSurfaceFileDescriptor-class.md)
  : FresurferAsciiSurfaceFileDescriptor

- [`FreesurferBinarySurfaceFileDescriptor-class`](https://bbuchsbaum.github.io/neurosurf/reference/FreesurferBinarySurfaceFileDescriptor-class.md)
  : FresurferBinarySurfaceFileDescriptor

- [`GIFTISurfaceDataMetaInfo-class`](https://bbuchsbaum.github.io/neurosurf/reference/GIFTISurfaceDataMetaInfo-class.md)
  : GIFTISurfaceDataMetaInfo

- [`GIFTISurfaceFileDescriptor-class`](https://bbuchsbaum.github.io/neurosurf/reference/GIFTISurfaceFileDescriptor-class.md)
  : GIFTISurfaceFileDescriptor

- [`GIFTISurfaceGeometryMetaInfo-class`](https://bbuchsbaum.github.io/neurosurf/reference/GIFTISurfaceGeometryMetaInfo-class.md)
  : GIFTISurfaceGeometryMetaInfo

- [`LabeledNeuroSurface-class`](https://bbuchsbaum.github.io/neurosurf/reference/LabeledNeuroSurface-class.md)
  : LabeledNeuroSurface Class

- [`NIMLSurfaceDataMetaInfo-class`](https://bbuchsbaum.github.io/neurosurf/reference/NIMLSurfaceDataMetaInfo-class.md)
  : NIMLSurfaceDataMetaInfo

- [`NIMLSurfaceFileDescriptor-class`](https://bbuchsbaum.github.io/neurosurf/reference/NIMLSurfaceFileDescriptor-class.md)
  : NIMLSurfaceFileDescriptor

- [`NeuroSurface-class`](https://bbuchsbaum.github.io/neurosurf/reference/NeuroSurface-class.md)
  : NeuroSurface

- [`NeuroSurface()`](https://bbuchsbaum.github.io/neurosurf/reference/NeuroSurface.md)
  : Construct a NeuroSurface Object

- [`NeuroSurfaceSource()`](https://bbuchsbaum.github.io/neurosurf/reference/NeuroSurfaceSource-class.md)
  : NeuroSurfaceSource Class

- [`NeuroSurfaceVector-class`](https://bbuchsbaum.github.io/neurosurf/reference/NeuroSurfaceVector-class.md)
  : NeuroSurfaceVector Class

- [`NeuroSurfaceVector()`](https://bbuchsbaum.github.io/neurosurf/reference/NeuroSurfaceVector.md)
  : NeuroSurfaceVector

- [`NeuroSurfaceVectorSource-class`](https://bbuchsbaum.github.io/neurosurf/reference/NeuroSurfaceVectorSource-class.md)
  : NeuroSurfaceVectorSource

- [`ROISurface-class`](https://bbuchsbaum.github.io/neurosurf/reference/ROISurface-class.md)
  : ROISurface

- [`ROISurface()`](https://bbuchsbaum.github.io/neurosurf/reference/ROISurface.md)
  :

  Create an instance of class `ROISurface`

- [`ROISurfaceVector-class`](https://bbuchsbaum.github.io/neurosurf/reference/ROISurfaceVector-class.md)
  : ROISurfaceVector

- [`ROISurfaceVector()`](https://bbuchsbaum.github.io/neurosurf/reference/ROISurfaceVector.md)
  :

  Create an instance of class `ROISurfaceVector`

- [`RandomSurfaceSearchlight()`](https://bbuchsbaum.github.io/neurosurf/reference/RandomSurfaceSearchlight.md)
  : Create a Random Searchlight iterator for surface mesh

- [`SurfaceDataMetaInfo-class`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceDataMetaInfo-class.md)
  : SurfaceDataMetaInfo

- [`SurfaceDisk()`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceDisk.md)
  : Create a Region on Surface

- [`SurfaceGeometry-class`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometry-class.md)
  : SurfaceGeometry Class

- [`SurfaceGeometry()`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometry.md)
  : Create a SurfaceGeometry Object

- [`SurfaceGeometryMetaInfo-class`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometryMetaInfo-class.md)
  : SurfaceGeometryMetaInfo Class

- [`SurfaceGeometrySource-class`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometrySource-class.md)
  : SurfaceGeometrySource Class

- [`show(`*`<SurfaceScene>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceScene-class.md)
  : A portable cortical surface scene

- [`SurfaceSearchlight()`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceSearchlight.md)
  : SurfaceSearchlight

- [`geometry(`*`<SurfaceSet>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceSet-class.md)
  [`vertices(`*`<SurfaceSet>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceSet-class.md)
  [`faces(`*`<SurfaceSet>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceSet-class.md)
  [`nodes(`*`<SurfaceSet>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceSet-class.md)
  [`graph(`*`<SurfaceSet>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceSet-class.md)
  [`curvature(`*`<SurfaceSet>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceSet-class.md)
  : SurfaceSet: bundle multiple surface variants for one hemisphere

- [`VertexColoredNeuroSurface-class`](https://bbuchsbaum.github.io/neurosurf/reference/VertexColoredNeuroSurface-class.md)
  : VertexColoredNeuroSurface

- [`VertexColoredNeuroSurface()`](https://bbuchsbaum.github.io/neurosurf/reference/VertexColoredNeuroSurface.md)
  : VertexColoredNeuroSurface

- [`VertexData-class`](https://bbuchsbaum.github.io/neurosurf/reference/VertexData-class.md)
  : VertexData

- [`add_atlas_outline()`](https://bbuchsbaum.github.io/neurosurf/reference/add_atlas_outline.md)
  : Add an atlas outline layer to a surface plot

- [`add_surface_layer()`](https://bbuchsbaum.github.io/neurosurf/reference/add_surface_layer.md)
  : Add a data layer to a surface plot

- [`add_vector_layer()`](https://bbuchsbaum.github.io/neurosurf/reference/add_vector_layer.md)
  : Add a vector field overlay

- [`adjacency()`](https://bbuchsbaum.github.io/neurosurf/reference/adjacency-methods.md)
  : Get Adjacency Graph

- [`apply_surface_sampler()`](https://bbuchsbaum.github.io/neurosurf/reference/apply_surface_sampler.md)
  : Apply a precomputed surface sampler to a volume

- [`as`](https://bbuchsbaum.github.io/neurosurf/reference/as-methods.md)
  : Coercion Methods for NeuroSurface Objects

- [`as.matrix(`*`<ROISurfaceVector>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/as.matrix-methods.md)
  [`as.matrix(`*`<NeuroSurfaceVector>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/as.matrix-methods.md)
  [`as.matrix(`*`<BilatNeuroSurfaceVector>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/as.matrix-methods.md)
  : Convert Surface Data to Matrix

- [`as.vector(`*`<NeuroSurface>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/as.vector-methods.md)
  : Convert Surface Data to Vector

- [`clear_geodesic_cache()`](https://bbuchsbaum.github.io/neurosurf/reference/clear_geodesic_cache.md)
  : Clear geodesic cache

- [`cluster_threshold()`](https://bbuchsbaum.github.io/neurosurf/reference/cluster_threshold-methods.md)
  : Apply Cluster-Extent Threshold to Surface Data

- [`compute_hull_world_cpp()`](https://bbuchsbaum.github.io/neurosurf/reference/compute_hull_world_cpp.md)
  : Compute boundary hull points in world space (C++)

- [`conn_comp(`*`<NeuroSurfaceVector>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/conn_comp-methods.md)
  [`conn_comp(`*`<NeuroSurface>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/conn_comp-methods.md)
  : Compute Connected Components on a Surface

- [`coords(`*`<ROISurface>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/coords-methods.md)
  [`coords(`*`<SurfaceGeometry>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/coords-methods.md)
  [`coords(`*`<igraph>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/coords-methods.md)
  [`coords(`*`<NeuroSurfaceVector>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/coords-methods.md)
  [`coords(`*`<NeuroSurface>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/coords-methods.md)
  : Extract Vertex Coordinates

- [`curv_cols()`](https://bbuchsbaum.github.io/neurosurf/reference/curv_cols.md)
  : Convert Curvature Values to Binary Colors for Visualization

- [`curv_cols_smooth()`](https://bbuchsbaum.github.io/neurosurf/reference/curv_cols_smooth.md)
  : Convert Curvature Values to Smooth Gradient Colors

- [`curvature()`](https://bbuchsbaum.github.io/neurosurf/reference/curvature-methods.md)
  : Compute Surface Curvature Vector

- [`data_reader(`*`<SurfaceGeometryMetaInfo>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/data_reader-methods.md)
  [`data_reader(`*`<NIMLSurfaceDataMetaInfo>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/data_reader-methods.md)
  : Create a Column Reader for Surface Data

- [`debug_surfwidget()`](https://bbuchsbaum.github.io/neurosurf/reference/debug_surfwidget.md)
  : Debugging Helper for surfwidget

- [`draw_surface_plot()`](https://bbuchsbaum.github.io/neurosurf/reference/draw_surface_plot.md)
  : Draw a static multi-panel surface figure

- [`faces()`](https://bbuchsbaum.github.io/neurosurf/reference/faces-methods.md)
  : Extract Faces from a Surface Object

- [`findBoundaries()`](https://bbuchsbaum.github.io/neurosurf/reference/findBoundaries-methods.md)
  : Find Boundaries Between Regions on a Surface

- [`find_all_neighbors()`](https://bbuchsbaum.github.io/neurosurf/reference/find_all_neighbors.md)
  : Find Node Neighbors in a Surface Mesh

- [`find_nearest_vertex()`](https://bbuchsbaum.github.io/neurosurf/reference/find_nearest_vertex.md)
  : Find the nearest surface vertex to a 3D point

- [`find_roi_boundaries()`](https://bbuchsbaum.github.io/neurosurf/reference/find_roi_boundaries.md)
  : Find boundaries of ROIs on a surface mesh

- [`gaussian_splat()`](https://bbuchsbaum.github.io/neurosurf/reference/gaussian_splat.md)
  [`gaussian_splat_vertex()`](https://bbuchsbaum.github.io/neurosurf/reference/gaussian_splat.md)
  [`gaussian_splat_multi()`](https://bbuchsbaum.github.io/neurosurf/reference/gaussian_splat.md)
  : Gaussian splats on surface meshes

- [`geodesic_distance_matrix()`](https://bbuchsbaum.github.io/neurosurf/reference/geodesic_distance_matrix.md)
  : All-pairs geodesic distance matrix (chunked)

- [`geodesic_distances()`](https://bbuchsbaum.github.io/neurosurf/reference/geodesic_distances.md)
  : Geodesic distances from sources to targets

- [`geometry()`](https://bbuchsbaum.github.io/neurosurf/reference/geometry-methods.md)
  : Extract Geometry from Surface Object

- [`get_surface()`](https://bbuchsbaum.github.io/neurosurf/reference/get_surface.md)
  : Retrieve a geometry from a SurfaceSet

- [`graph()`](https://bbuchsbaum.github.io/neurosurf/reference/graph-methods.md)
  :

  extract `igraph` object

- [`indices(`*`<ROISurface>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/indices-methods.md)
  [`indices(`*`<ROISurfaceVector>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/indices-methods.md)
  [`indices(`*`<NeuroSurfaceVector>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/indices-methods.md)
  [`indices(`*`<NeuroSurface>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/indices-methods.md)
  : Extract Vertex Indices

- [`laplacian()`](https://bbuchsbaum.github.io/neurosurf/reference/laplacian-methods.md)
  : Compute Graph Laplacian

- [`left()`](https://bbuchsbaum.github.io/neurosurf/reference/left-methods.md)
  : Get Left Hemisphere

- [`length(`*`<ROISurface>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/length-methods.md)
  : Get Length of Surface Object

- [`loadFSSurface()`](https://bbuchsbaum.github.io/neurosurf/reference/loadFSSurface.md)
  : load Freesurfer ascii surface

- [`loadGIFTISurface()`](https://bbuchsbaum.github.io/neurosurf/reference/loadGIFTISurface.md)
  : Load GIFTI surface geometry

- [`load_data(`*`<NeuroSurfaceVectorSource>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/load_data-methods.md)
  [`load_data(`*`<NeuroSurfaceSource>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/load_data-methods.md)
  [`load_data(`*`<FreesurferSurfaceGeometryMetaInfo>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/load_data-methods.md)
  [`load_data(`*`<GIFTISurfaceGeometryMetaInfo>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/load_data-methods.md)
  [`load_data(`*`<SurfaceGeometrySource>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/load_data-methods.md)
  : load_data

- [`load_fsaverage()`](https://bbuchsbaum.github.io/neurosurf/reference/load_fsaverage.md)
  : Fetch fsaverage surfaces

- [`load_fsaverage_bundle()`](https://bbuchsbaum.github.io/neurosurf/reference/load_fsaverage_bundle.md)
  : Load a bundle of fsaverage surface variants as a SurfaceSet

- [`load_fsaverage_std8()`](https://bbuchsbaum.github.io/neurosurf/reference/load_fsaverage_std8.md)
  : Load fsaverage std.8 surfaces packaged with neurosurf

- [`map_values(`*`<NeuroSurface>`*`,`*`<list>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/map_values-NeuroSurface-list-method.md)
  : Map Values for NeuroSurface with List Lookup

- [`map_values(`*`<NeuroSurface>`*`,`*`<matrix>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/map_values-NeuroSurface-matrix-method.md)
  : Map Values for NeuroSurface with Matrix Lookup

- [`meshToGraph()`](https://bbuchsbaum.github.io/neurosurf/reference/meshToGraph.md)
  : Construct a Graph from Mesh Vertices and Faces

- [`neighbor_graph()`](https://bbuchsbaum.github.io/neurosurf/reference/neighbor_graph-methods.md)
  : Construct Neighborhood Graph from Surface Mesh

- [`neurosurf-package`](https://bbuchsbaum.github.io/neurosurf/reference/neurosurf.md)
  [`neurosurf`](https://bbuchsbaum.github.io/neurosurf/reference/neurosurf.md)
  : neurosurf: Data structures and IO for surface-based neuroimaging
  data.

- [`neurosurf_download_testdata()`](https://bbuchsbaum.github.io/neurosurf/reference/neurosurf_download_testdata.md)
  : Download optional test data for neurosurf

- [`nodes()`](https://bbuchsbaum.github.io/neurosurf/reference/nodes-methods.md)
  : Extract Surface Node Numbers

- [`parcel_boundary_contact()`](https://bbuchsbaum.github.io/neurosurf/reference/parcel_boundary_contact.md)
  : Parcel boundary contact matrix

- [`parcel_geodesic_centroid()`](https://bbuchsbaum.github.io/neurosurf/reference/parcel_geodesic_centroid.md)
  : Parcel centroids using geodesic medoids

- [`parcel_geodesic_distance_matrix()`](https://bbuchsbaum.github.io/neurosurf/reference/parcel_geodesic_distance_matrix.md)
  : Parcel-to-parcel geodesic distances

- [`plot(`*`<SurfaceGeometry>`*`,`*`<missing>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/plot-methods.md)
  [`plot(`*`<NeuroSurface>`*`,`*`<missing>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/plot-methods.md)
  [`plot(`*`<LabeledNeuroSurface>`*`,`*`<missing>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/plot-methods.md)
  [`plot(`*`<ColorMappedNeuroSurface>`*`,`*`<missing>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/plot-methods.md)
  [`plot(`*`<VertexColoredNeuroSurface>`*`,`*`<missing>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/plot-methods.md)
  : Plot a Surface

- [`plot(`*`<SurfaceGeometry>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/plot.SurfaceGeometry.md)
  : Plot method for SurfaceGeometry objects

- [`plot(`*`<SurfaceSet>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/plot.SurfaceSet.md)
  : Plot method for SurfaceSet objects

- [`plot(`*`<neurosurf_plot>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/plot.neurosurf_plot.md)
  : Plot method for neurosurf_plot objects

- [`plot_js()`](https://bbuchsbaum.github.io/neurosurf/reference/plot_js-methods.md)
  : Plot Surface as an HTMLWidget

- [`print(`*`<Searchlight>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/print.Searchlight.md)
  : Print Method for Searchlight Iterator

- [`projectCoordinates()`](https://bbuchsbaum.github.io/neurosurf/reference/projectCoordinates.md)
  : Project 3D Coordinates onto a Surface and Smooth the Values

- [`read_freesurfer_annot()`](https://bbuchsbaum.github.io/neurosurf/reference/read_freesurfer_annot.md)
  : Read Freesurfer Annotation File

- [`read_meta_info(`*`<AFNISurfaceFileDescriptor>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/read_meta_info-methods.md)
  [`read_meta_info(`*`<NIMLSurfaceFileDescriptor>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/read_meta_info-methods.md)
  [`read_meta_info(`*`<FreesurferAsciiSurfaceFileDescriptor>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/read_meta_info-methods.md)
  [`read_meta_info(`*`<FreesurferBinarySurfaceFileDescriptor>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/read_meta_info-methods.md)
  [`read_meta_info(`*`<GIFTISurfaceFileDescriptor>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/read_meta_info-methods.md)
  : Read Meta Information

- [`read_surf()`](https://bbuchsbaum.github.io/neurosurf/reference/read_surf.md)
  : Read Surface Data from a File

- [`read_surf_data()`](https://bbuchsbaum.github.io/neurosurf/reference/read_surf_data.md)
  :

  load surface data and link to `SurfaceGeometry`

- [`read_surf_data_seq()`](https://bbuchsbaum.github.io/neurosurf/reference/read_surf_data_seq.md)
  : Read Surface Data Sequence

- [`read_surf_geometry()`](https://bbuchsbaum.github.io/neurosurf/reference/read_surf_geometry.md)
  : Read Surface Geometry from File

- [`remeshSurface()`](https://bbuchsbaum.github.io/neurosurf/reference/remeshSurface.md)
  : Remesh a SurfaceGeometry object

- [`render_surface_plot()`](https://bbuchsbaum.github.io/neurosurf/reference/render_surface_plot.md)
  : Render a neurosurf plot using rgl

- [`right()`](https://bbuchsbaum.github.io/neurosurf/reference/right-methods.md)
  : Get Right Hemisphere

- [`sampler_to_triplets()`](https://bbuchsbaum.github.io/neurosurf/reference/sampler_to_triplets.md)
  : Extract sparse matrix triplets from a surface sampler

- [`series(`*`<NeuroSurfaceVector>`*`,`*`<numeric>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/series-methods.md)
  [`series(`*`<NeuroSurfaceVector>`*`,`*`<integer>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/series-methods.md)
  [`series(`*`<NeuroSurfaceVector>`*`,`*`<ROISurface>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/series-methods.md)
  [`series(`*`<NeuroSurface>`*`,`*`<numeric>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/series-methods.md)
  : Extract Time Series from Surface Vector

- [`series_roi(`*`<NeuroSurfaceVector>`*`,`*`<numeric>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/series_roi-methods.md)
  [`series_roi(`*`<NeuroSurfaceVector>`*`,`*`<ROISurface>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/series_roi-methods.md)
  : Extract ROI Time Series from Surface Vector

- [`show(`*`<SurfaceGeometryMetaInfo>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/show-methods.md)
  [`show(`*`<SurfaceDataMetaInfo>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/show-methods.md)
  [`show(`*`<ROISurface>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/show-methods.md)
  [`show(`*`<SurfaceGeometry>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/show-methods.md)
  [`show(`*`<NeuroSurfaceVector>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/show-methods.md)
  [`show(`*`<NeuroSurface>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/show-methods.md)
  : show

- [`show_surface_plot()`](https://bbuchsbaum.github.io/neurosurf/reference/show_surface_plot.md)
  : Show a surface plot in one step

- [`show_surface_widget()`](https://bbuchsbaum.github.io/neurosurf/reference/show_surface_widget.md)
  : Show an interactive surface widget

- [`smooth(`*`<NeuroSurface>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/smooth-NeuroSurface-method.md)
  : Smooth Data on a NeuroSurface Object

- [`smooth()`](https://bbuchsbaum.github.io/neurosurf/reference/smooth-methods.md)
  : Generic Function for Smoothing a Surface or Associated Data

- [`snapshot_surface()`](https://bbuchsbaum.github.io/neurosurf/reference/snapshot_surface.md)
  : Snapshot a surface to a PNG

- [`` `[`( ``*`<NeuroSurfaceVector>`*`,`*`<missing>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/sub-NeuroSurfaceVector-missing-missing-ANY-method.md)
  : Extract All Data from NeuroSurfaceVector

- [`` `[`( ``*`<NeuroSurfaceVector>`*`,`*`<missing>`*`,`*`<numeric>`*`,`*`<ANY>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/sub-NeuroSurfaceVector-missing-numeric-ANY-method.md)
  : Subset NeuroSurfaceVector by Column

- [`` `[`( ``*`<NeuroSurfaceVector>`*`,`*`<numeric>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/sub-NeuroSurfaceVector-numeric-missing-ANY-method.md)
  : Subset NeuroSurfaceVector by Row

- [`` `[`( ``*`<NeuroSurfaceVector>`*`,`*`<numeric>`*`,`*`<numeric>`*`,`*`<ANY>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/sub-NeuroSurfaceVector-numeric-numeric-ANY-method.md)
  : Subset NeuroSurfaceVector

- [`` `[`( ``*`<ROISurface>`*`,`*`<numeric>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/sub-ROISurface.md)
  : Subset an ROISurface Object

- [`` `[[`( ``*`<NeuroSurfaceVector>`*`,`*`<numeric>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/sub-sub-NeuroSurfaceVector-numeric-method.md)
  : Extract Data from NeuroSurfaceVector

- [`surf_to_world()`](https://bbuchsbaum.github.io/neurosurf/reference/surf_to_world-methods.md)
  : Get Surface-to-World Transform

- [`` `surf_to_world<-`() ``](https://bbuchsbaum.github.io/neurosurf/reference/surf_to_world-set-methods.md)
  : Set Surface-to-World Transform

- [`surface_labels()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_labels.md)
  : List available surface labels

- [`surface_layer()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_layer.md)
  : Define a named scalar layer for a surface scene

- [`surface_montage()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_montage.md)
  : Arrange multiple surface views into a single montage figure

- [`surface_plot()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_plot.md)
  : Create a surface plot specification

- [`surface_sampler()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_sampler.md)
  : Build a reusable surface sampler for multi-frame volumes

- [`surface_scene()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_scene.md)
  : Construct a validated portable surface scene

- [`surface_scene_manifest()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_scene_manifest.md)
  : Serialize a SurfaceScene as a surfview.scene.v1 manifest

- [`surface_set()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_set.md)
  : Construct a SurfaceSet

- [`surfwidget()`](https://bbuchsbaum.github.io/neurosurf/reference/surfwidget-methods.md)
  : Create an interactive surface viewer

- [`updateColorMap()`](https://bbuchsbaum.github.io/neurosurf/reference/updateColorMap.md)
  : Update Surface Color Map

- [`values(`*`<ROISurface>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/values-methods.md)
  [`values(`*`<ROISurfaceVector>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/values-methods.md)
  [`values(`*`<NeuroSurface>`*`)`](https://bbuchsbaum.github.io/neurosurf/reference/values-methods.md)
  : Extract Data Values from Surface Objects

- [`vertices()`](https://bbuchsbaum.github.io/neurosurf/reference/vertices-methods.md)
  : Extract Vertices from a Surface Object

- [`view_surface()`](https://bbuchsbaum.github.io/neurosurf/reference/view_surface.md)
  : Display a 3D Brain Surface using RGL

- [`vol_to_surf()`](https://bbuchsbaum.github.io/neurosurf/reference/vol_to_surf.md)
  : Map values from a 3D volume to a surface in the same coordinate
  space

- [`vol_to_surf_sdf()`](https://bbuchsbaum.github.io/neurosurf/reference/vol_to_surf_sdf.md)
  : Map a volume to surface after SDF-based rigid alignment

- [`write_surf_data()`](https://bbuchsbaum.github.io/neurosurf/reference/write_surf_data.md)
  : Write Surface Data to File

- [`write_surface_scene()`](https://bbuchsbaum.github.io/neurosurf/reference/write_surface_scene.md)
  : Write a standalone portable surface report
