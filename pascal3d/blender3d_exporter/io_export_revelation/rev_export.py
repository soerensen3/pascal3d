import bpy
import struct
from . rev_helper import *
from . rev_export_material import *
from . rev_export_mesh import *
from . rev_export_armature import *

export_type = {
    "MESH",
    "ARMATURE"
}

def ExportRevelation(Config):
    Config.Whitespace = 0
    print("----------\nExporting to {}".format(Config.FilePath))
    if Config.Verbose:
        print("Opening File...")
    Config.File = open(Config.FilePath, "w")
    if Config.Verbose:
        print("Done")

    if Config.Verbose:
        print("Generating Object list for export... (Root parents only)")

    if Config.Verbose:
        for Object in Config.context.scene.objects:
            print(Object)
            print(Object.type)
            print(Object.parent)
    if Config.ExportMode == 1:
        Config.ExportList = [Object for Object in Config.context.scene.objects
                             if Object.type in export_type
                             and Object.parent is None
                             and not Object.hide]
    
    Config.File.write("{}world\n".format("  " * Config.Whitespace))
    Config.Whitespace += 1

    WriteWorld(Config)

    Config.Whitespace -= 1
    Config.File.write("{}end;\n".format("  " * Config.Whitespace))
    
    
    ExportObjects(Config, Config.ExportList)
    ExportMaterials(Config)
    CloseFile(Config)
    print("Finished")

def GetObjectChildren(Parent):
    return [Object for Object in Parent.children
            if Object.type in export_type]


def ExportObjects(Config, ObjectList):
    for Object in ObjectList:
        if ( not Object.type in export_type ):
            exit
        if Object.type == 'MESH':
            Config.File.write("{}object {}\n".format("  " * Config.Whitespace, LegalName(Object.name)))
            Config.Whitespace += 1
            ExportMesh(Config, Object)
        if Object.type == 'ARMATURE':
            Config.File.write("{}armature {}\n".format("  " * Config.Whitespace, LegalName(Object.name)))
            Config.Whitespace += 1
            ExportArmature(Config, Object)

        if len( GetObjectChildren(Object)):
            Config.File.write("{}children\n".format("  " * Config.Whitespace))
            Config.Whitespace += 1

            ExportObjects(Config,GetObjectChildren(Object))

            Config.Whitespace -= 1
            Config.File.write("{}end;\n".format("  " * Config.Whitespace))

        Config.Whitespace -= 1
        Config.File.write("{}end;\n".format("  " * Config.Whitespace))

## MATERIALS

def WriteWorld(Config):
    world = bpy.data.scenes[0].world
    if world:
        val=world.ambient_color
        Config.File.write("{}ambient {:6f}, {:6f}, {:6f}\n".format("  " * Config.Whitespace, *val))  # Ambient, uses mirror color,
        val=world.horizon_color
        Config.File.write("{}horizon {:6f}, {:6f}, {:6f}\n".format("  " * Config.Whitespace, *val))  # Horizon
        
## END

def CloseFile(Config):
    if Config.Verbose:
        print("Closing File...")
    Config.File.close()
    if Config.Verbose:
        print("Done")
