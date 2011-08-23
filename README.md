mc2obj v0.2
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

* Converting a group of chunks to .obj format
* Materials for naturally occurring blocks
* Geometry for naturally occurring full blocks
* A few options (min and max y height, show/hide sides and bottoms of chunks)

Below are two work in progress shots showing a section of terrain of 32x32
chunks. All the pink blocks are ones I have yet to implement the material for.

![mc2obj teaser 3](http://3.bp.blogspot.com/-W-140s7T_qk/TlQ1mxWPHLI/AAAAAAAAASM/GTOqD58Kqqk/s640/hmcobj3.jpg "mc2obj teaser3")

![mc2obj teaser 2](http://4.bp.blogspot.com/-r-YzJHGxzNY/TlQ1fo_M9LI/AAAAAAAAASI/5hxsklnCm8Y/s640/hmcobj2.jpg "mc2obj teaser2")