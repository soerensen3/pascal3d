import bpy
import struct
from . rev_helper import *
from . rev_export_material import *
from . rev_export_mesh import *
from . rev_export_armature import *

from xml.etree import cElementTree as et

export_type = {
    "MESH",
    "ARMATURE"
}

def ExportRevelation(Config):
    Config.Whitespace = 0
    print("----------\nExporting to {}".format(Config.FilePath))
#    if Config.Verbose:
#        print("Opening File...")
#    Config.File = open(Config.FilePath, "w")
#    if Config.Verbose:
#        print("Done")
    Config.FileRoot = et.Element('p3dscene')
    Config.DocStack = [ Config.FileRoot ]

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

    WriteWorld(Config)


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
            meshEl = et.Element("mesh")
            Config.DocStack[ -1 ].append( meshEl )
            Config.DocStack.append( meshEl )
            meshEl.attrib['name'] = LegalName(Object.name)            
#            Config.File.write("{}object {}\n".format("  " * Config.Whitespace, LegalName(Object.name)))
            ExportMesh(Config, Object)
#        if Object.type == 'ARMATURE':
#            Config.File.write("{}armature {}\n".format("  " * Config.Whitespace, LegalName(Object.name)))
#            ExportArmature(Config, Object)

#        if len( GetObjectChildren(Object)):
#            Config.File.write("{}children\n".format("  " * Config.Whitespace))
#            Config.Whitespace += 1

            ExportObjects(Config,GetObjectChildren(Object))
            Config.DocStack.pop()

## MATERIALS

def WriteWorld(Config):
    worldEl = et.Element("world")
    Config.DocStack[ -1 ].append( worldEl )
    Config.DocStack.append( worldEl )
    world = bpy.data.scenes[0].world
    if world:
        worldEl.attrib['ambient'] = "{:6f}, {:6f}, {:6f}".format(*world.ambient_color)  # Ambient, uses mirror color,
        worldEl.attrib['horizon'] = "{:6f}, {:6f}, {:6f}".format(*world.horizon_color)  # Horizon
    Config.DocStack.pop()
        
## END
        
# PRETTY PRINT
# function to pretty print the XML code
def prettyPrint(element, level=0):
    '''
    Printing in elementTree requires a little massaging
    Function taken from elementTree site:
    http://effbot.org/zone/element-lib.htm#prettyprint

    '''
    indent = '\n' + level * '  '
    if len(element):
        if not element.text or not element.text.strip():
            element.text = indent + '  '

        if not element.tail or not element.tail.strip():
            element.tail = indent

        for element in element:
            prettyPrint(element, level + 1)

        if not element.tail or not element.tail.strip():
            element.tail = indent

    else:
        if level and (not element.tail or not element.tail.strip()):
            element.tail = indent

    return element

def CloseFile(Config):
    if Config.Verbose:
        print("Closing File...")
    fileObject = open(Config.FilePath, 'w')
    prettyPrint( Config.FileRoot )
    xmlText = et.tostring(Config.FileRoot)
    fileObject.write('<?xml version="1.0" ?>\n' + xmlText.decode('utf8'))
    fileObject.close()
    if Config.Verbose:
        print("Done")