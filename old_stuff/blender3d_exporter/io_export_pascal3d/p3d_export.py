import bpy
import struct
from . p3d_helper import *
from . p3d_export_material import *
from . p3d_export_mesh import *
from . p3d_export_armature import *
from . p3d_export_misc import *

from xml.etree import cElementTree as et

export_type = {
    "MESH",
    "ARMATURE",
    "LAMP",
    "CAMERA"
}

def ExportP3DScene(Config):
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
    elif Config.ExportMode == 2:
        Config.ExportList = [Object for Object in Config.context.selected_objects
                             if Object.type in export_type
                             #and Object.parent is None
                             and not Object.hide]

    WriteWorld(Config)


    ExportObjects(Config, Config.ExportList)
    ExportData(Config)
    CloseFile(Config)
    print("Finished")

def GetObjectChildren(Parent):
    return [Object for Object in Parent.children
            if Object.type in export_type]


def ExportObjects(Config, ObjectList):
    print("Exporting Objects")
    
    for Object in ObjectList:
        if ( not Object.type in export_type ):
            exit
            
        objEl = et.Element("object")
        Config.DocStack[ -1 ].append( objEl )
        Config.DocStack.append( objEl )           
        ExportObject(Config, Object)

        ExportObjects(Config,GetObjectChildren(Object))

        Config.DocStack.pop()
        
        if Object.type == 'MESH':
            print("mesh")
            global globalMeshes
            globalMeshes.add( Object ) #modifiers can't be applied if data is exported
            objEl.attrib['data'] = 'mesh_' + LegalName(Object.name)
        
        if Object.type == 'LAMP':
            print("lamp")
            global globalLamps
            globalLamps.add( Object.data )
            objEl.attrib['data'] = 'light_' + LegalName(Object.name)

        if Object.type == 'CAMERA':
            print("cam")
            global globalCameras
            globalCameras.add( Object.data )
            objEl.attrib['data'] = 'camera_' + LegalName(Object.name)
            
    print("Finished Objects")

def ExportData(Config):
    print("Exporting Data")
    
    ExportMeshes(Config)
    ExportLights(Config)
    ExportCameras(Config)
    ExportMaterials(Config)
    print("Finished Data")

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
        
        
'''        if Object.type == 'MESH':
            objEl = et.Element("mesh")
            Config.DocStack[ -1 ].append( objEl )
            Config.DocStack.append( objEl )
            objEl.attrib['name'] = LegalName(Object.name)            
            ExportMesh(Config, Object)
        
        if Object.type == 'LAMP':
            objEl = et.Element("light")
            Config.DocStack[ -1 ].append( objEl )
            Config.DocStack.append( objEl )
            objEl.attrib['name'] = LegalName(Object.name)            
            ExportLight(Config, Object)

        if Object.type == 'CAMERA':
            objEl = et.Element("camera")
            Config.DocStack[ -1 ].append( objEl )
            Config.DocStack.append( objEl )
            objEl.attrib['name'] = LegalName(Object.name)            
            ExportCamera(Config, Object)
            
        if Object.type == 'ARMATURE':
            objEl = et.Element("armature")
            Config.DocStack[ -1 ].append( objEl )
            Config.DocStack.append( objEl )
            objEl.attrib['name'] = LegalName(Object.name)  
#            ExportArmature(Config, Object)'''
