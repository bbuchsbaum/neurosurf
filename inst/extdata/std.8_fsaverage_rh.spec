# MapIcosahedron generated spec file
#History: [bbuchsbaum@Fantail.local: Wed May  1 09:23:17 2019] MapIcosahedron -spec fsaverage_rh.spec -ld 8 -prefix std.8_

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
	SurfaceName = ./std.8_rh.smoothwm.asc
	LocalDomainParent = ./SAME
	SurfaceState = std.smoothwm
	EmbedDimension = 3
	Anatomical = Y
	LocalCurvatureParent = ./SAME

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ./std.8_rh.pial.asc
	LocalDomainParent = ./std.8_rh.smoothwm.asc
	SurfaceState = std.pial
	EmbedDimension = 3
	Anatomical = Y
	LocalCurvatureParent = ./std.8_rh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ./std.8_rh.inflated.asc
	LocalDomainParent = ./std.8_rh.smoothwm.asc
	SurfaceState = std.inflated
	EmbedDimension = 3
	Anatomical = N
	LocalCurvatureParent = ./std.8_rh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ./std.8_rh.sphere.asc
	LocalDomainParent = ./std.8_rh.smoothwm.asc
	SurfaceState = std.sphere
	EmbedDimension = 3
	Anatomical = N
	LocalCurvatureParent = ./std.8_rh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ./std.8_rh.white.asc
	LocalDomainParent = ./std.8_rh.smoothwm.asc
	SurfaceState = std.white
	EmbedDimension = 3
	Anatomical = Y
	LocalCurvatureParent = ./std.8_rh.smoothwm.asc

NewSurface
	SurfaceFormat = ASCII
	SurfaceType = FreeSurfer
	SurfaceName = ./std.8_rh.sphere.reg.asc
	LocalDomainParent = ./std.8_rh.smoothwm.asc
	SurfaceState = std.sphere.reg
	EmbedDimension = 3
	Anatomical = N
	LocalCurvatureParent = ./std.8_rh.smoothwm.asc
