// Import Stylesheets
require('./Stylesheets.elm');

// Boot app
const Elm = require('./Main.elm');
const app = Elm.Main.fullscreen({ randomSeed: Math.floor(Math.random()*0xFFFFFFFF) });
