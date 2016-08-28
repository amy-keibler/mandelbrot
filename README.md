# Mandelbrot Image Generator
[![Build Status](https://travis-ci.org/matt-keibler/mandelbrot.svg?branch=master)](https://travis-ci.org/matt-keibler/mandelbrot)

The application takes a single commandline parameter that is the path to a JSON file. That file contains all of the necessary configuration parameters to generate an image containing a region of the Mandelbrot Fractal. Examples of interesting regions can be found in the `images/` folder, with `images/default.json` being the well known example.

## Dev
This program relies on the Stack tool to manage it. Upon cloning the repository, run `stack test` to build and verify everything is correctly set up.
