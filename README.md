# minesweeper

A menu-less version of the classic Microsoft Minesweeper game written in Clojure.

Beware, this is my first Clojure project! Lots of room for improvement within.

## Dependencies

* Java 8+
* [Leiningen](https://leiningen.org/) Clojure build tool

## Usage

A single optional command line argument to `lein run` indicates the game mode: 
small, medium, or large.

```
lein run [small | medium | large]
```

## Tools

A small python module `raster.py` in `tools` is responsible for rasterizing PNG images from source SVG files
using Inkscape CLI.

All image assets were recreated from scratch using screenshots available online. 