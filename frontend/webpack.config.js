var webpack = require('webpack');
var path = require('path');

module.exports = {

  entry: [
    './index'
  ],

  debug: true,

  devtool: 'eval',

  devServer: {
    contentBase: './dist',
    port: 4008,
    stats: 'errors-only'
  },

  output: {
    path: path.join(__dirname, '/dist'),
    publicPath: '/',
    filename: 'bundle.js'
  },

  module: {
    loaders: [
      {
        test: /\.purs$/,
        loader: 'purs-loader',
        exclude: /node_modules/,
        query: {
          src: [
            'bower_components/purescript-*/src/**/*.purs',
            './*.purs',
            'components/*.purs'
          ]
        }
      },
      { test: /\.js?$/, loader: 'babel', exclude: /node_modules/ , query: { presets: ['es2015', 'react'] }},
      { test: /\.s?css$/, loader: 'style!css!sass' }
    ]
  },

  htmlLoader: { minimize: false },

  resolve: {
    modulesDirectories: [
      'node_modules',
      'bower_components'
    ],
    extensions: [ '', '.purs', '.js']
  },

//  plugins: [
//    new webpack.optimize.OccurenceOrderPlugin(),
//    new webpack.HotModuleReplacementPlugin(),
//    new webpack.NoErrorsPlugin()
//  ],
  target: 'node' // we can use node.js modules after adding this configuration 
};
