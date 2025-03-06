const gulp = require('gulp');
const rollup = require('rollup');
const { nodeResolve } = require('@rollup/plugin-node-resolve');
const commonjs = require('@rollup/plugin-commonjs');
const { terser } = require('rollup-plugin-terser');
const path = require('path');

// Correct output paths
const neurosurfaceDestPath = path.join(__dirname, '..', 'lib', 'neurosurface');
const threeDestPath = path.join(__dirname, '..', 'lib', 'three');

gulp.task('build-js', async function() {
  const bundle = await rollup.rollup({
    input: 'src/index.js',
    plugins: [
      nodeResolve(),
      commonjs()
    ]
  });

  await bundle.write({
    file: path.join(neurosurfaceDestPath, 'neurosurface.js'),
    format: 'iife',
    name: 'neurosurface'
  });

  await bundle.write({
    file: path.join(neurosurfaceDestPath, 'neurosurface.min.js'),
    format: 'iife',
    name: 'neurosurface',
    plugins: [terser()]
  });
});

gulp.task('copy-three', function() {
  return gulp.src('node_modules/three/build/three.min.js')
    .pipe(gulp.dest(threeDestPath));
});

gulp.task('copy-orbit-controls', function() {
  return gulp.src('node_modules/three/examples/js/controls/OrbitControls.js')
    .pipe(gulp.dest(threeDestPath));
});

gulp.task('default', gulp.series('build-js', 'copy-three', 'copy-orbit-controls'));
