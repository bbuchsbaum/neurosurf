# MapIcosahedron generated spec file
#History: [bbuchsbaum@Fantail.local: Wed May  1 09:23:02 2019] MapIcosahedron -spec fsaverage_lh.spec -ld 8 -prefix std.8_

#define the group
	Group = fsaverage

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
	SurfaceName = ./std.8_lh.smoothwm.asc
	LocalDomainParent = ./SAME
	SurfaceState = std.smoothwm
	EmbedDimension = 3
	Anatomical = Y
	LocalCurvatureParent = ./SAME

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ./std.8_lh.pial.asc
	LocalDomainParent = ./std.8_lh.smoothwm.asc
	SurfaceState = std.pial
	EmbedDimension = 3
	Anatomical = Y
	LocalCurvatureParent = ./std.8_lh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ./std.8_lh.inflated.asc
	LocalDomainParent = ./std.8_lh.smoothwm.asc
	SurfaceState = std.inflated
	EmbedDimension = 3
	Anatomical = N
	LocalCurvatureParent = ./std.8_lh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ./std.8_lh.sphere.asc
	LocalDomainParent = ./std.8_lh.smoothwm.asc
	SurfaceState = std.sphere
	EmbedDimension = 3
	Anatomical = N
	LocalCurvatureParent = ./std.8_lh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ./std.8_lh.white.asc
	LocalDomainParent = ./std.8_lh.smoothwm.asc
	SurfaceState = std.white
	EmbedDimension = 3
	Anatomical = Y
	LocalCurvatureParent = ./std.8_lh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ./std.8_lh.sphere.reg.asc
	LocalDomainParent = ./std.8_lh.smoothwm.asc
	SurfaceState = std.sphere.reg
	EmbedDimension = 3
	Anatomical = N
	LocalCurvatureParent = ./std.8_lh.smoothwm.asc
