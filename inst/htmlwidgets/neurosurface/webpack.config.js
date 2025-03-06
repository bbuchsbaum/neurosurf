const path = require('path');
const CopyPlugin = require('copy-webpack-plugin');

module.exports = {
  entry: './src/index.js',
  output: {
    filename: 'neurosurface.js',
    path: path.resolve(__dirname, '../lib/neurosurface'),
    library: 'neurosurface',
    libraryTarget: 'umd',
    globalObject: 'this'
  },
  mode: 'development',
  devtool: 'source-map',
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: ['@babel/preset-env']
          }
        }
      }
    ]
  },
  resolve: {
    extensions: ['.js']
  },
  externals: {
    three: 'THREE'
  },
  performance: {
    hints: false
  },
  plugins: [
    new CopyPlugin({
      patterns: [
        { 
          from: path.resolve(__dirname, 'node_modules/three/build/three.module.js'),
          to: path.resolve(__dirname, '../lib/three')
        },
        { 
          from: path.resolve(__dirname, 'node_modules/three/examples/jsm/controls/OrbitControls.js'),
          to: path.resolve(__dirname, '../lib/three/examples/jsm/controls')
        }
      ],
    }),
  ],
};