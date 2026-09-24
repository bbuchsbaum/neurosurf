#!/usr/bin/env python3
"""Rebuild neurosurf's pinned browser bundle without a dirty working tree.

Usage:
  python3 tools/rebuild-surfview-runtime.py /path/to/surfviewjs
  python3 tools/rebuild-surfview-runtime.py /path/to/surfviewjs --repin [COMMIT]

Without --repin, rebuild the recorded commit with the recorded runtime patch
and install the bundle only if it matches the recorded SHA-256.

With --repin, build COMMIT (default: the checkout's HEAD) with the current
runtime patch, install the bundle, and rewrite the provenance marker.

Requires git, npm, Node, and the source commit in that checkout.
"""
from pathlib import Path
import argparse
import hashlib
import json
import subprocess
import tarfile
import tempfile

parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
parser.add_argument("surfviewjs", type=Path, help="surfviewjs git checkout")
parser.add_argument(
    "--repin",
    nargs="?",
    const="HEAD",
    metavar="COMMIT",
    help="build COMMIT (default HEAD) and record a new pin",
)
args = parser.parse_args()

root = Path(__file__).resolve().parents[1]
dest = root / "inst/htmlwidgets/lib/neurosurface"
stamp = dest / "surfview.embed.commit"
pin = dict(line.split("=", 1) for line in stamp.read_text().splitlines())
patch = root / pin["patch"]
digest = lambda p: hashlib.sha256(p.read_bytes()).hexdigest()

if args.repin:
    commit = subprocess.run(
        [
            "git",
            "-C",
            str(args.surfviewjs),
            "rev-parse",
            "--verify",
            args.repin + "^{commit}",
        ],
        check=True,
        capture_output=True,
        text=True,
    ).stdout.strip()
else:
    commit = pin["commit"]
    if digest(patch) != pin["patch_sha256"]:
        raise SystemExit("Runtime patch does not match the recorded SHA-256.")

with tempfile.TemporaryDirectory(prefix="neurosurf-runtime-") as tmp:
    tmp = Path(tmp)
    archive = tmp / "source.tar"
    subprocess.run(
        ["git", "-C", str(args.surfviewjs), "archive", "-o", str(archive), commit],
        check=True,
    )
    work = tmp / "source"
    work.mkdir()
    with tarfile.open(archive) as tar:
        tar.extractall(work, filter="data")

    def run(*cmd):
        subprocess.run(cmd, cwd=work, check=True)

    run("git", "apply", str(patch))
    run(
        "npm",
        "ci",
        "--ignore-scripts",
        "--no-audit",
        "--no-fund",
        "--cache",
        str(tmp / "npm-cache"),
    )
    run("npx", "--no-install", "tsc", "--noEmit")
    run(
        "npx",
        "--no-install",
        "vitest",
        "run",
        "tests/unit/surface-contrast.test.ts",
        "tests/unit/style-presets.test.ts",
        "tests/unit/scene-mount-lifecycle.test.ts",
        "tests/unit/report-scene-control-target.test.ts",
    )
    run("npx", "--no-install", "vite", "build", "--config", "vite.config.embed.js")
    bundle = work / "dist/surfview.embed.iife.js"
    sha = digest(bundle)

    if not args.repin:
        if sha != pin["sha256"]:
            raise SystemExit(
                "Rebuilt bundle differs from the recorded SHA-256; not installed."
            )
        (dest / bundle.name).write_bytes(bundle.read_bytes())
        print("Verified and rebuilt", sha)
    else:
        read_json = lambda p: json.loads((work / p).read_text())
        three = read_json("node_modules/three/package.json")["version"]
        pin.update(
            commit=commit,
            patch_sha256=digest(patch),
            version=read_json("package.json")["version"],
            sha256=sha,
            three_revision=three.split(".")[1],
        )
        (dest / bundle.name).write_bytes(bundle.read_bytes())
        stamp.write_text("".join(f"{k}={v}\n" for k, v in pin.items()))
        print("Repinned surfviewjs", commit[:12], "->", sha)
