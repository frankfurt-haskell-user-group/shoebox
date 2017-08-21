var webpack = require('webpack');
var path = require('path');

module.exports = {
  entry: [
    './index'
  ],
  module: {
    loaders: [
      { test: /\.js?$/, loader: 'babel', exclude: /node_modules/ , query: { presets: ['es2015', 'react'] }},
      { test: /\.s?css$/, loader: 'style!css!sass' },
    ]
  },
  htmlLoader: { minimize: false },
  resolve: {
    extensions: ['', '.js']
  },
  output: {
    path: path.join(__dirname, '/dist'),
    publicPath: '/',
    filename: 'bundle.js'
  },
  plugins: [
    new webpack.optimize.OccurenceOrderPlugin(),
    new webpack.HotModuleReplacementPlugin(),
    new webpack.NoErrorsPlugin()
  ],
  target: 'node' // we can use node.js modules after adding this configuration 
};
