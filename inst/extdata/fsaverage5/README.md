# fsaverage5

FreeSurfer fsaverage5 template surfaces (10,242 vertices per hemisphere):
inflated (`infl_*`) and white (`white_*`) surfaces and sulcal depth
(`sulc_*`), as gzipped GIFTI.

Source: nilearn, `nilearn/datasets/data/fsaverage5/`, commit
e5faab66dc2fe72d2cb50a8aaf2f44813aba4a51 (2023-03-28), files unmodified.
The fsaverage template is part of FreeSurfer and is redistributed under the
FreeSurfer Software License: https://surfer.nmr.mgh.harvard.edu/fswiki/FreeSurferSoftwareLicense

Load with `load_fsaverage("fsaverage5", "inflated")` and
`load_fsaverage_sulc("fsaverage5")`.
