mc2obj v0.3
===========

mc2obj is a tool to convert part of a Minecraft world to a .obj file, which
can be imported into a 3D package such as 3ds max or Blender to make a
rendering of that world.

mc2obj is similar to mcobj (https://github.com/quag/mcobj), with the following
differences:

* It automatically generates texture coordinates and applies textures
* It generates the full geometry, not just blocks
* Support for custom texture packs

Below is an example rendering of a world exported with mc2obj.

[![example export](http://2.bp.blogspot.com/-LvgUDF3sSZA/TmQDKXXhzFI/AAAAAAAAASU/9VdrdtFhLLE/s640/mc2obj_alpha.jpg "example export")](http://2.bp.blogspot.com/-LvgUDF3sSZA/TmQDKXXhzFI/AAAAAAAAASU/9VdrdtFhLLE/s1600/mc2obj_alpha.jpg)

User manual
-----------

Extract the archive you downloaded somewhere. Open a command line and go to the
mc2obj folder. Type the following (replacing myworld with the name of your save file
and the output folder with whatever you want):

    mc2obj "%appdata%\.minecraft\saves\world" -o "e:\output" --circle=0,0,7 --miny=63

This will output a circle of 15 chunks wide (2 * radius + 1) centered around the chunk
with coordinates (x=0,z=0). Let's look at some of the other options by exporting part
of the Nether:

    mc2obj "%appdata%\.minecraft\saves\world" -o "e:\output" --rect=-3,-3,3,3 -n -s -b

This will export the rectangle from (-3,-3) to (3,3) of the nether (-n) with sides (-s)
and top and bottom (-b) enabled.

After exporting, start 3ds max or whatever other 3D package you have and import the .obj
file in the output folder. For 3ds max, use the following settings:

![3ds max import settings](https://lh5.googleusercontent.com/-pm8T_hmlIso/TmQFe9mZsaI/AAAAAAAAASY/Xjeeg0jFtLI/s720/settings.jpg "3ds max import settings")

FAQ
---

__I'm running something other than 64-bit Windows. How do I use your program?__  
Since Haskell code only runs on the platform you created the binary on I need
to set up some virtual machines to do this. In the meantime, you can install the
[Haskell platform](http://hackage.haskell.org/platform) and build the code
yourself. For now you'll need to install the dependencies manually. I'll put the
next version on Hackage to automate this.

__How do I run this program/what do I do with these .hs files?__  
If you just want to run the program, make sure you're getting the binary release
(click the big Downloads button in the top right of the Github page) isntead of
the source code.

__I've found a bug__  
Please check the Known Bugs section below to see if it's in there.
If not, please let me know so I can fix it. Just add a new issue on
https://github.com/FalconNL/mc2obj/issues (assuming the issue hasn't
been reported before).

__Why is the exporter so slow?__  
Unfortunately, generating the correct geometry takes more time than just generating
cubes like mcobj does. To speed up exporting, you can increase the minY argument to
something like 63 so all the underground caves are skipped. 
Since mc2obj is still in alpha there are probably still some ways to speed up the
program. If you have any suggestions, please let me know. See the FAQ section for
more details.

__I'm using Blender/Cinema 4D/some other 3D package. How do I import the .obj file?__  
I only use 3ds max, so I'm not familiar with the other packages. The important
setting is to use the z-axis as the up direction. Aside from that, use the normals,
texture coordinates, etc. from the file rather than recalculating them. If you have
other issues, please seek a forum or other help service specific to that package.

__Why are all the textures so blurry in the rendering?__  
Unlike Minecraft, 3ds max (and probably other 3D packages as well) apply texture
filtering by default. This is particularly noticeable when doing closeup renderings.
You can usually turn this off in your renderer. Note that this will cause moire
patterns on overview shots. For 3ds max, the setting can be found at:

* Scanline renderer: Renderer -> Antialiasing -> Filter maps
* V-ray: V-Ray -> Global switches -> Materials -> Filter maps

__What render settings should I use?__  
Whatever you want. I'd recommend some type of global illumination but given the
amount of packages, render engines and possible settings I can't go into more detail.
Again, seek the help of someone with more experience with your 3D package/render engine.

__How do I use a custom texture pack?__  
Make sure you have [ImageMagick](http://www.imagemagick.org/script/binary-releases.php)
installed. Put the required texture files (terrain.png, fire.png, portal.png and sign.png)
in the texsplit folder. Run texsplit.bat. The next time you export a world it will
use the new texture pack. Alternatively, replace the tex folder in your output directory
with the one in the texplit folder to avoid having to recompile.

__Texsplit doesn't work on Linux/Mac__  
Again, I've yet to set up the virtual machines so for the time being you'll have to
transform texplit.bat into the appropriate format yourself. This shouldn't be too
difficult though.

Known bugs
----------
Major:

* Performance still leaves a lot to be desired. For those interested: according to the
  profiler the two slowest functions are indexString and addFace in ObjExport.hs.
  Suggestions for performance improvement are welcome.
* Water and lava flows are stepped rather than sloped and have no side faces.
  Code change required.
* Redstone wire does not display correctly. Code change required.

Minor:

* Signs have no text. I have yet to come up with a good way of doing this.
* Chests may not always orient correctly
* Fence texture coordinates aren't entirely correct
* Some minor player interaction-related geometry effects like pressed pressure
  plates and lit up redstone ore are ignored