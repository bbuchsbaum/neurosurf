# fsaverage5

FreeSurfer fsaverage5 template surfaces (10,242 vertices per hemisphere):
inflated (`infl_*`), white (`white_*`) and pial (`pial_*`) surfaces and
sulcal depth (`sulc_*`), as gzipped GIFTI.

Source: nilearn, `nilearn/datasets/data/fsaverage5/`, commit
e5faab66dc2fe72d2cb50a8aaf2f44813aba4a51 (2023-03-28), files unmodified.
The fsaverage template is part of FreeSurfer and is redistributed under the
FreeSurfer Software License: https://surfer.nmr.mgh.harvard.edu/fswiki/FreeSurferSoftwareLicense

The Destrieux 2009 parcellation
(`tpl-fsaverage_hemi-{L,R}_den-10k_atlas-Destrieux2009_dseg.label.gii`) comes
unmodified from TemplateFlow `tpl-fsaverage`, commit
8e53ba4f2e438758f69d11436fe0cd291a28bec6, converted there from FreeSurfer 7
`aparc.a2009s.annot`. It supplies the cortex domain (everything except the
medial wall).

Load with `load_fsaverage("fsaverage5", "inflated")` (or `"white"`,
`"pial"`), `load_fsaverage_sulc("fsaverage5")` and
`load_fsaverage_cortex("fsaverage5")`.
