mc2obj v0.1
===========

mc2obj is a tool to convert part of a Minecraft world to a .obj file, which
can be imported into a 3D package such as 3ds max or Blender to make a
rendering of that world.

mc2obj is similar to mcobj (https://github.com/quag/mcobj), with the following
differences:

* It automatically generates texture coordinates and applies textures
* It generates correct geometry, not just blocks (NOTE: not implemented yet)

mc2obj is still in the early stages of development, so it is not yet usable
unless you know Haskell. Currently supported:

* Loading a region file
* Converting a single chunk to .obj format
* Materials for naturally occurring blocks
* Geometry for naturally occurring full blocks

Below is a teaser shot of a single chunk:

![mc2obj teaser](http://3.bp.blogspot.com/-v7Px1nLat_U/TlBXUT5zg6I/AAAAAAAAASE/8u9xC4uFZwI/s640/hmcobj1.jpg "mc2obj teaser")