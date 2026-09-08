#!/usr/bin/env python3
"""Rebuild neurosurf's pinned browser bundle without a dirty working tree.

Usage: python3 tools/rebuild-surfview-runtime.py /path/to/surfviewjs
Requires git, npm, Node, and the recorded base revision in that checkout.
"""
from pathlib import Path
import hashlib
import subprocess
import sys
import tarfile
import tempfile

root = Path(__file__).resolve().parents[1]
dest = root / 'inst/htmlwidgets/lib/neurosurface'
stamp = dest / 'surfview.embed.commit'
pin = dict(line.split('=', 1) for line in stamp.read_text().splitlines())
patch = root / pin['patch']
digest = lambda p: hashlib.sha256(p.read_bytes()).hexdigest()
if digest(patch) != pin['patch_sha256']:
    raise SystemExit('Runtime patch does not match the recorded SHA-256.')
with tempfile.TemporaryDirectory(prefix='neurosurf-runtime-') as tmp:
    tmp = Path(tmp)
    archive = tmp / 'source.tar'
    subprocess.run(['git', '-C', sys.argv[1], 'archive', '-o', str(archive),
                    pin['commit']], check=True)
    work = tmp / 'source'
    work.mkdir()
    with tarfile.open(archive) as tar:
        tar.extractall(work, filter='data')
    def run(*args):
        subprocess.run(args, cwd=work, check=True)
    run('git', 'apply', str(patch))
    run('npm', 'ci', '--ignore-scripts', '--no-audit', '--no-fund',
        '--cache', str(tmp / 'npm-cache'))
    run('npx', '--no-install', 'tsc', '--noEmit')
    run('npx', '--no-install', 'vitest', 'run',
        'tests/unit/surface-contrast.test.ts', 'tests/unit/style-presets.test.ts',
        'tests/unit/scene-mount-lifecycle.test.ts',
        'tests/unit/report-scene-control-target.test.ts')
    run('npx', '--no-install', 'vite', 'build', '--config', 'vite.config.embed.js')
    bundle = work / 'dist/surfview.embed.iife.js'
    if digest(bundle) != pin['sha256']:
        raise SystemExit('Rebuilt bundle differs from the recorded SHA-256; not installed.')
    (dest / bundle.name).write_bytes(bundle.read_bytes())
    print('Verified and rebuilt', pin['sha256'])
