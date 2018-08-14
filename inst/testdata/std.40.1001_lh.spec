# MapIcosahedron generated spec file
#History: [bradley@comp94.neuroinfo.rri: Wed Jan  4 15:20:45 2017] MapIcosahedron -spec 1001_lh.spec -ld 40 -dset_map lh.thickness.gii.dset -dset_map lh.curv.gii.dset -dset_map lh.sulc.gii.dset -prefix std.40.

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
	SurfaceName = ./std.40.lh.smoothwm.asc
	LocalDomainParent = ./SAME
	LabelDset = ./std.40.lh.aparc.a2009s.annot.niml.dset
	SurfaceState = std.smoothwm
	EmbedDimension = 3
	Anatomical = Y
	LocalCurvatureParent = ./SAME

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ./std.40.lh.pial.asc
	LocalDomainParent = ./std.40.lh.smoothwm.asc
	SurfaceState = std.pial
	EmbedDimension = 3
	Anatomical = Y
	LocalCurvatureParent = ./std.40.lh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ./std.40.lh.inflated.asc
	LocalDomainParent = ./std.40.lh.smoothwm.asc
	SurfaceState = std.inflated
	EmbedDimension = 3
	Anatomical = N
	LocalCurvatureParent = ./std.40.lh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ./std.40.lh.sphere.asc
	LocalDomainParent = ./std.40.lh.smoothwm.asc
	SurfaceState = std.sphere
	EmbedDimension = 3
	Anatomical = N
	LocalCurvatureParent = ./std.40.lh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ./std.40.lh.white.asc
	LocalDomainParent = ./std.40.lh.smoothwm.asc
	SurfaceState = std.white
	EmbedDimension = 3
	Anatomical = Y
	LocalCurvatureParent = ./std.40.lh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ./std.40.lh.sphere.reg.asc
	LocalDomainParent = ./std.40.lh.smoothwm.asc
	SurfaceState = std.sphere.reg
	EmbedDimension = 3
	Anatomical = N
	LocalCurvatureParent = ./std.40.lh.smoothwm.asc
