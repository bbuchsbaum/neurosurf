#!/usr/bin/env python3
"""Audit the built CRAN tarball, not the developer checkout."""
import argparse
import hashlib
import io
import json
from pathlib import Path
import tarfile

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("tarball", type=Path)
args = parser.parse_args()
size = args.tarball.stat().st_size
assert size <= 10_000_000, f"Source tarball exceeds 10 MB: {size}"
with tarfile.open(args.tarball) as tar:
    files = {m.name: m for m in tar.getmembers() if m.isfile()}
    root = next(iter(files)).split("/", 1)[0] + "/"

    def read(path):
        return tar.extractfile(root + path).read()

    docs = sum(m.size for name, m in files.items()
               if name.startswith(root + "inst/doc/"))
    assert docs <= 5_000_000, f"Built documentation exceeds 5 MB: {docs}"
    description = read("DESCRIPTION").decode()
    assert "Remotes:" not in description
    assert "neuroim2 (>= 0.19.1)" in description
    assert all("/fonts/" not in name for name in files)
    assert all("/htmlwidgets/neurosurface/" not in name for name in files)
    assert all("/htmlwidgets/lib/three/" not in name for name in files)
    assert all("rscan01" not in name for name in files)
    runtime = "inst/htmlwidgets/lib/neurosurface/"
    pin = dict(line.split("=", 1) for line in
               read(runtime + "surfview.embed.commit").decode().splitlines())
    bundle = read(runtime + "surfview.embed.iife.js")
    source = read(runtime + "surfview-source.tar.xz")
    assert hashlib.sha256(bundle).hexdigest() == pin["sha256"]
    assert hashlib.sha256(source).hexdigest() == pin["source_sha256"]
    assert bundle.startswith(b"/*!\nBundled surfview runtime")
    notices = read(runtime + "THIRD-PARTY-NOTICES.txt").decode()
    assert notices.encode() in bundle
    with tarfile.open(fileobj=io.BytesIO(source), mode="r:xz") as archive:
        members = archive.getmembers()
        assert all(m.isfile() and m.mode == 0o644 for m in members)
        assert all(not m.name.endswith((".so", ".node", ".dll", ".exe"))
                   for m in members)
        assert "surfview/src/embed.ts" in archive.getnames()
        dependencies = json.load(archive.extractfile(
            "surfview/BUNDLED-MODULES.json"))
        assert "three" in dependencies
        for name, version in dependencies.items():
            assert f"{name} {version}" in notices
    for name in ["COPYRIGHTS", "extdata/LICENSE-FreeSurfer.txt",
                 "extdata/LICENSE-CBIG.txt", "extdata/PROVENANCE.md"]:
        assert read("inst/" + name)
    assets = json.loads(read("inst/extdata/PROVENANCE.json"))["assets"]
    for asset in assets:
        assert hashlib.sha256(read("inst/extdata/" + asset["file"])).hexdigest() == (
            asset["sha256"]), f"Data hash mismatch: {asset['file']}"
    assert b"snapshot3d" not in read("inst/doc/displaying-surfaces.R")
print(json.dumps({"tarball": args.tarball.name, "source_bytes": size,
                  "documentation_bytes": docs,
                  "sha256": hashlib.sha256(args.tarball.read_bytes()).hexdigest(),
                  "runtime_source_packages": dependencies}, indent=2))
