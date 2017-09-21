var path = require('path');
const webpack = require('webpack');

module.exports = {
  entry: {
    bundle: [
      './src/index.js'
    ]
  },
  output : {
    path: path.resolve(__dirname, 'dist'),
    filename: '[name].js',
    publicPath: '/elm-tetris/dist/',
  },
  module: {
    rules: [{
      test: /\.elm$/,
      exclude: [/elm-stuff/, /node_modules/, /Stylesheets\.elm$/],
      use: 'elm-webpack-loader'
    },
    {
      test: /Stylesheets\.elm$/,
      use: ['style-loader', 'css-loader', 'elm-css-webpack-loader']
    },]
  },
  plugins: [
    new webpack.optimize.UglifyJsPlugin({
      sourceMap: true
    }),
    new webpack.DefinePlugin({
      'process.env': {
        'NODE_ENV': JSON.stringify('production')
      }
    })
  ],
  devtool: 'source-map',
}
