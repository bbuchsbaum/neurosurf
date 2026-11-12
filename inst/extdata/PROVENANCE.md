# Example template provenance

## fsaverage5

The eight gzipped GIFTI files in `fsaverage5/` are unmodified files from
nilearn commit `e5faab66dc2fe72d2cb50a8aaf2f44813aba4a51` (2023-03-28),
`nilearn/datasets/data/fsaverage5/`. They are FreeSurfer templates,
redistributed under `LICENSE-FreeSurfer.txt`. Each was verified against the
pinned source; exact URLs and hashes are in `PROVENANCE.json`.

### fsaverage5 Destrieux parcellation and cortex domain

`fsaverage5/tpl-fsaverage_hemi-{L,R}_den-10k_atlas-Destrieux2009_dseg.label.gii`
are unmodified files from TemplateFlow `tpl-fsaverage`, commit
`8e53ba4f2e438758f69d11436fe0cd291a28bec6` (2024-03-26). TemplateFlow
converted them with `mris_convert` from FreeSurfer 7
`subjects/fsaverage5/label/{l,r}h.aparc.a2009s.annot`. Their bytes match the
git-annex MD5 keys recorded at that commit, and their vertex order matches the
nilearn fsaverage5 surfaces above (identical white-surface coordinates).
`load_fsaverage_cortex()` reads the cortex domain from them: every vertex not
labelled `Medial_wall` or `Unknown`. Cite Destrieux et al. (2010),
NeuroImage, DOI: https://doi.org/10.1016/j.neuroimage.2010.06.010.

## std.8 (modified FreeSurfer templates)

The `std.8_*.asc` meshes are resampled derivatives of FreeSurfer fsaverage,
not the original full-resolution meshes. Bradley Buchsbaum produced them
with AFNI SUMA `MapIcosahedron -ld 8`; the command and timestamps (2019-05-01)
are preserved in the two bundled `.spec` files. The original FreeSurfer
release identifier was not recorded. The mesh content used by this package
is pinned by the SHA256 values in `PROVENANCE.json`.

The `smoothwm`, `white`, `pial`, `inflated`, `sphere` and `sphere.reg` variants
use the same decimated topology. These modified versions retain all terms in
`LICENSE-FreeSurfer.txt` and are explicitly identified here as derivatives.

## Schaefer-200

`Schaefer2018_200Parcels_7Networks_order_FSLMNI152_1mm.nii.gz` comes from
ThomasYeoLab/CBIG, Schaefer2018_LocalGlobal/Parcellations/MNI, at commit
`c4034588d99b80c5cabf7fc1071c5766e4688ce9`. Its uncompressed NIfTI bytes
are identical to the upstream file. The CBIG MIT license is preserved in
`LICENSE-CBIG.txt` (license revision
`ef8e1feba9ee1d7cb8940c4ec27ce3d34941ad62`). Cite Schaefer et al. (2018),
Cerebral Cortex, DOI: https://doi.org/10.1093/cercor/bhx179.

## License terms

All or portions of this licensed product (such portions are the "Software")
have been obtained under license from The General Hospital Corporation and
are subject to the terms and conditions in `LICENSE-FreeSurfer.txt`.
The root neurosurf GPL statement does not replace the template/data terms.
