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
import io

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
    tmp = Path(tmp).resolve()
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

    # An empty patch means the pinned commit already carries every runtime
    # change upstream; `git apply` rejects empty input, so skip it.
    if patch.read_bytes().strip():
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
    # Collect the actual runtime module graph without changing bundle output.
    # This prevents a source/license inventory based only on package.json from
    # overlooking transitive code or shipping unused development dependencies.
    (work / "vite.config.neurosurf.js").write_text("""
import config from './vite.config.embed.js';
import fs from 'node:fs';
export default {
  ...config,
  plugins: [...(config.plugins || []), {
    name: 'neurosurf-runtime-sources',
    generateBundle(options, output) {
      // Rolldown may leave OutputChunk.modules empty. The module graph API
      // also works there, and a source superset is preferable to omissions.
      const modules = Array.from(this.getModuleIds());
      fs.writeFileSync('runtime-modules.json', JSON.stringify(modules));
    }
  }]
};
""")
    run("npx", "--no-install", "vite", "build", "--config",
        "vite.config.neurosurf.js")
    bundle = work / "dist/surfview.embed.iife.js"
    upstream_sha = digest(bundle)

    sources = {}
    for path in sorted((work / "src").rglob("*")):
        if path.is_file():
            sources[str(path.relative_to(work))] = path.read_bytes()
    for name in ["LICENSE", "package.json", "package-lock.json",
                 "tsconfig.json", "vite.config.embed.js"]:
        sources[name] = (work / name).read_bytes()
    packages = {}
    for module in json.loads((work / "runtime-modules.json").read_text()):
        if module.startswith("\0"):
            continue
        path = Path(module.split("?", 1)[0]).resolve()
        if not path.is_file() or not path.is_relative_to(work):
            continue
        relative = str(path.relative_to(work))
        sources[relative] = path.read_bytes()
        if "node_modules/" not in relative:
            continue
        parts = Path(relative.split("node_modules/", 1)[1]).parts
        name = "/".join(parts[:2]) if parts[0].startswith("@") else parts[0]
        package_root = work / "node_modules" / name
        metadata = json.loads((package_root / "package.json").read_text())
        packages[name] = metadata
        sources[str((package_root / "package.json").relative_to(work))] = (
            package_root / "package.json").read_bytes()
        for license_file in package_root.iterdir():
            if license_file.is_file() and license_file.name.lower().startswith(
                    ("license", "licence", "copying", "notice")):
                sources[str(license_file.relative_to(work))] = (
                    license_file.read_bytes())

    notices = ["Bundled surfview runtime\n",
               "Source: https://github.com/bbuchsbaum/surfviewjs\n",
               "Commit: " + commit + "\n\n", sources["LICENSE"].decode()]
    if "three" not in packages:
        raise SystemExit("Runtime source graph does not include Three.js.")
    print("Runtime source packages:", ", ".join(sorted(packages)))
    for name, metadata in sorted(packages.items()):
        prefix = "node_modules/" + name + "/"
        licenses = [value.decode(errors="strict")
                    for key, value in sorted(sources.items())
                    if key.startswith(prefix) and "/" not in key[len(prefix):]
                    and Path(key).name.lower().startswith(
                        ("license", "licence", "copying", "notice"))]
        if not licenses:
            raise SystemExit("No license text found for runtime package " + name)
        notices += ["\n\n" + name + " " + metadata["version"] + "\n",
                    "\n".join(licenses)]
    notice_text = "".join(notices)
    if "*/" in notice_text:
        raise SystemExit("A license contains a JS comment terminator.")
    output_bytes = ("/*!\n" + notice_text + "\n*/\n").encode() + bundle.read_bytes()
    sha = hashlib.sha256(output_bytes).hexdigest()
    source_archive = tmp / "surfview-source.tar.xz"
    sources["BUNDLED-MODULES.json"] = json.dumps(
        {name: metadata["version"] for name, metadata in sorted(packages.items())},
        indent=2).encode() + b"\n"
    with tarfile.open(source_archive, "w:xz", preset=9) as tar:
        for name, data in sorted(sources.items()):
            info = tarfile.TarInfo("surfview/" + name)
            info.size = len(data)
            info.mode = 0o644
            info.mtime = 0
            tar.addfile(info, io.BytesIO(data))
    source_sha = digest(source_archive)

    if not args.repin:
        if sha != pin["sha256"] or source_sha != pin["source_sha256"]:
            raise SystemExit(
                "Rebuilt bundle differs from the recorded SHA-256; not installed."
            )
        (dest / bundle.name).write_bytes(output_bytes)
        (dest / source_archive.name).write_bytes(source_archive.read_bytes())
        (dest / "THIRD-PARTY-NOTICES.txt").write_text(notice_text)
        print("Verified and rebuilt", sha)
    else:
        read_json = lambda p: json.loads((work / p).read_text())
        three = read_json("node_modules/three/package.json")["version"]
        pin.update(
            commit=commit,
            patch_sha256=digest(patch),
            version=read_json("package.json")["version"],
            sha256=sha,
            upstream_sha256=upstream_sha,
            source_sha256=source_sha,
            three_revision=three.split(".")[1],
        )
        (dest / bundle.name).write_bytes(output_bytes)
        (dest / source_archive.name).write_bytes(source_archive.read_bytes())
        (dest / "THIRD-PARTY-NOTICES.txt").write_text(notice_text)
        stamp.write_text("".join(f"{k}={v}\n" for k, v in pin.items()))
        print("Repinned surfviewjs", commit[:12], "->", sha)
