# MapIcosahedron generated spec file
#History: [bradley@comp94.neuroinfo.rri: Wed Jan  4 15:20:58 2017] MapIcosahedron -spec 1001_rh.spec -ld 40 -dset_map rh.thickness.gii.dset -dset_map rh.curv.gii.dset -dset_map rh.sulc.gii.dset -prefix std.40.

#define the group
	Group = 1001

#define various States
	StateDef = std.smoothwm
	StateDef = std.pial
	StateDef = std.inflated
	StateDef = std.sphere
	StateDef = std.white
	StateDef = std.sphere.reg

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ./std.40.rh.smoothwm.asc
	LocalDomainParent = ./SAME
	LabelDset = ./std.40.rh.aparc.a2009s.annot.niml.dset
	SurfaceState = std.smoothwm
	EmbedDimension = 3
	Anatomical = Y
	LocalCurvatureParent = ./SAME

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ./std.40.rh.pial.asc
	LocalDomainParent = ./std.40.rh.smoothwm.asc
	SurfaceState = std.pial
	EmbedDimension = 3
	Anatomical = Y
	LocalCurvatureParent = ./std.40.rh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ./std.40.rh.inflated.asc
	LocalDomainParent = ./std.40.rh.smoothwm.asc
	SurfaceState = std.inflated
	EmbedDimension = 3
	Anatomical = N
	LocalCurvatureParent = ./std.40.rh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ./std.40.rh.sphere.asc
	LocalDomainParent = ./std.40.rh.smoothwm.asc
	SurfaceState = std.sphere
	EmbedDimension = 3
	Anatomical = N
	LocalCurvatureParent = ./std.40.rh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ./std.40.rh.white.asc
	LocalDomainParent = ./std.40.rh.smoothwm.asc
	SurfaceState = std.white
	EmbedDimension = 3
	Anatomical = Y
	LocalCurvatureParent = ./std.40.rh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ./std.40.rh.sphere.reg.asc
	LocalDomainParent = ./std.40.rh.smoothwm.asc
	SurfaceState = std.sphere.reg
	EmbedDimension = 3
	Anatomical = N
	LocalCurvatureParent = ./std.40.rh.smoothwm.asc
