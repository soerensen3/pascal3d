![Logo](https://github.com/soerensen3/pascal3d/blob/master/art/icons/mipmap-xxxhdpi/ic_launcher.png)
# What is Pascal3D
## Pascal3D (p3d) is a open source game engine for Free Pascal/Lazarus


# Features
* Object oriented
* Game data is organized in datablocks similar to blender
    * Saving and loading
    * Blender export script (Python)
    * Garbage collector
    * Scenes that can be rendered separately
    * Instancing
    * Properties will be saved automatically
* Materials
    * Base Materials with Blinn/Phong Shading
    * Custom Shader Materials with GLSL
    * Node Based Materials using P3D Markdown with GLSL
* Graphical User Interface
    * A GUI similar to Lazarus' LCL
* Bitmap and Native fonts
    * Signed distance field for Bitmap Fonts
    * Fonts can be easily converted to Bitmap Fonts
* Works with OpenGL 2.1+

# Pascal3D License #

Pascal3D is published under the MIT License

## The MIT License (MIT) ##

Copyright (c) [2017] [Johannes Rosleff Soerensen]

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

# Dependencies
## SDL2
Pascal3D uses SDL2. 
You can get SDL2 from the repositories (Linux) or from here (Windows, OSX):

* https://www.libsdl.org/download-2.0.php
* https://www.libsdl.org/projects/SDL_image/
* https://www.libsdl.org/projects/SDL_ttf/

### Linux
Install sdl2, sdl2-ttf, sdl2-image including developer version in some distros (Fedora, Ubuntu, ...) using your package manager.
### Windows
Download the files from above and copy them to the system32 folder or to the target folder of each application you want to compile. 

### OSX
Download the files from above and install the dmg files.

## Lazarus

You can get Lazarus from http://www.lazarus-ide.org/ or (Linux) from the repositories of your distro. This website offers a simplified installer suitable for all platforms: https://www.getlazarus.org/

## Pascal3DMath

For Pascal3D you need Math3D which is a opensource math library you can find here:
https://bitbucket.org/soerensen3/math3d

## dglOpenGL

Pascal3D comes with dglOpenGL because the original version did not have a package. However you can download the original version from: https://wiki.delphigl.com/index.php/dglOpenGL.pas

# Building from source

## Cloning the repository
You can clone the repository with the following git command:

    git clone https://soerensen3@bitbucket.org/soerensen3/pascal3d.git

## Register the packages
Before you can compile any Pascal3D you have to first register the packages in Lazarus. You can do this by opening the packages in the IDE. You need to open the following packages:
* <the math3d package>
* ./external/dglopengl/pl_opengl.lpk
* ./external/libsdl2/libsdl2.lpk
* ./src/pascal3d.lpk
* ./src/p3dgui/pascal3d_gui.lpk

## Compiling
The easiest way is to open the project in Lazarus and compile there. You can however also compile from commandline using lazbuild. You will find the project for the official p3dscene editor in ./editor/p3dscene.lpi

# TODO List
* Object inspector
  * dynamic pages that uses property editors and categories (partly implemented)
  * make new spatial object, that can be used by other classes than TP3DActor (for example for joints and pose joints) and be selected and edited in the viewer

