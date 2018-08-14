
#define the group
	Group = 1001

#define various States
	StateDef = std.smoothwm
	StateDef = std.pial
	StateDef = std.inflated_lh
	StateDef = std.sphere_lh
	StateDef = std.white
	StateDef = std.sphere.reg_lh
	StateDef = std.inflated_rh
	StateDef = std.sphere_rh
	StateDef = std.sphere.reg_rh

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ././std.40.lh.smoothwm.asc
	LocalDomainParent = ././SAME
	LabelDset = ././std.40.lh.aparc.a2009s.annot.niml.dset
	SurfaceState = std.smoothwm
	EmbedDimension = 3
	Anatomical = Y
	LocalCurvatureParent = ././SAME

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ././std.40.lh.pial.asc
	LocalDomainParent = ././std.40.lh.smoothwm.asc
	SurfaceState = std.pial
	EmbedDimension = 3
	Anatomical = Y
	LocalCurvatureParent = ././std.40.lh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ././std.40.lh.inflated.asc
	LocalDomainParent = ././std.40.lh.smoothwm.asc
	SurfaceState = std.inflated_lh
	EmbedDimension = 3
	Anatomical = N
	LocalCurvatureParent = ././std.40.lh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ././std.40.lh.sphere.asc
	LocalDomainParent = ././std.40.lh.smoothwm.asc
	SurfaceState = std.sphere_lh
	EmbedDimension = 3
	Anatomical = N
	LocalCurvatureParent = ././std.40.lh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ././std.40.lh.white.asc
	LocalDomainParent = ././std.40.lh.smoothwm.asc
	SurfaceState = std.white
	EmbedDimension = 3
	Anatomical = Y
	LocalCurvatureParent = ././std.40.lh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ././std.40.lh.sphere.reg.asc
	LocalDomainParent = ././std.40.lh.smoothwm.asc
	SurfaceState = std.sphere.reg_lh
	EmbedDimension = 3
	Anatomical = N
	LocalCurvatureParent = ././std.40.lh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ././std.40.rh.smoothwm.asc
	LocalDomainParent = ././SAME
	LabelDset = ././std.40.rh.aparc.a2009s.annot.niml.dset
	SurfaceState = std.smoothwm
	EmbedDimension = 3
	Anatomical = Y
	LocalCurvatureParent = ././SAME

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ././std.40.rh.pial.asc
	LocalDomainParent = ././std.40.rh.smoothwm.asc
	SurfaceState = std.pial
	EmbedDimension = 3
	Anatomical = Y
	LocalCurvatureParent = ././std.40.rh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ././std.40.rh.inflated.asc
	LocalDomainParent = ././std.40.rh.smoothwm.asc
	SurfaceState = std.inflated_rh
	EmbedDimension = 3
	Anatomical = N
	LocalCurvatureParent = ././std.40.rh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ././std.40.rh.sphere.asc
	LocalDomainParent = ././std.40.rh.smoothwm.asc
	SurfaceState = std.sphere_rh
	EmbedDimension = 3
	Anatomical = N
	LocalCurvatureParent = ././std.40.rh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ././std.40.rh.white.asc
	LocalDomainParent = ././std.40.rh.smoothwm.asc
	SurfaceState = std.white
	EmbedDimension = 3
	Anatomical = Y
	LocalCurvatureParent = ././std.40.rh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ././std.40.rh.sphere.reg.asc
	LocalDomainParent = ././std.40.rh.smoothwm.asc
	SurfaceState = std.sphere.reg_rh
	EmbedDimension = 3
	Anatomical = N
	LocalCurvatureParent = ././std.40.rh.smoothwm.asc
