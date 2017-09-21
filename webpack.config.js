const path = require('path');

module.exports = {
  entry: './src/index.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'bundle.js',
    publicPath: '/elm-tetris/dist/'
  },
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: /Stylesheets\.elm$/,
        use: 'elm-webpack-loader?debug=true'
      },
      {
        test: /Stylesheets\.elm$/,
        use: ['style-loader', 'css-loader', 'elm-css-webpack-loader']
      }
    ]
  },
  devtool: 'eval-source-map',
  devServer: {
    contentBase: path.join(__dirname, 'dist'),
    compress: true,
    historyApiFallback: true
  },
  stats: {
    colors: true
  }
}
