bl_info = {
    "name": "Revelation Model Format (.model)",
    "author": "Johannes Rosleff Sörensen",
    "version": (1, 0, 0),
    "blender": (2, 63, 0),
    "location": "File > Export > Revelation Model (.model)",
    "description": "Export Revelation Model Format (.model)",
    "warning": "",
    "wiki_url": "",
    "tracker_url": "",
    "category": "Import-Export"}

import os
from math import radians

import bpy
from mathutils import *

class RevelationExporterSettings:
    def __init__(self,
                 context,
                 FilePath,
                 CoordinateSystem=1,
                 RotateX=True,
                 FlipNormals=False,
                 ApplyModifiers=False,
                 IncludeFrameRate=False,
                 ExportTextures=True,
                 ExportArmatures=False,
                 ExportAnimation=0,
                 ExportMode=1,
                 Verbose=False):
        self.context = context
        self.FilePath = FilePath
        self.CoordinateSystem = int(CoordinateSystem)
        self.RotateX = RotateX
        self.FlipNormals = FlipNormals
        self.ApplyModifiers = ApplyModifiers
        self.IncludeFrameRate = IncludeFrameRate
        self.ExportTextures = ExportTextures
        self.ExportArmatures = ExportArmatures
        self.ExportAnimation = int(ExportAnimation)
        self.ExportMode = int(ExportMode)
        self.Verbose = Verbose

def LegalName(Name):

    def ReplaceSet(String, OldSet, NewChar):
        for OldChar in OldSet:
            String = String.replace(OldChar, NewChar)
        return String

    import string

    NewName = ReplaceSet(Name, string.punctuation + " ", "_")
    if NewName[0].isdigit() or NewName in ["ARRAY",
                                           "DWORD",
                                           "UCHAR",
                                           "BINARY",
                                           "FLOAT",
                                           "ULONGLONG",
                                           "BINARY_RESOURCE",
                                           "SDWORD",
                                           "UNICODE",
                                           "CHAR",
                                           "STRING",
                                           "WORD",
                                           "CSTRING",
                                           "SWORD",
                                           "DOUBLE",
                                           "TEMPLATE"]:
        NewName = "_" + NewName
    return NewName

export_type = {
    "MESH"
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
                             and Object.parent is None]
    ExportObjects(Config, Config.ExportList)
    CloseFile(Config)
    print("Finished")

def GetObjectChildren(Parent):
    return [Object for Object in Parent.children
            if Object.type in export_type]


def ExportObjects(Config, ObjectList):
    for Object in ObjectList:
        Config.File.write("{}object {}\n".format("  " * Config.Whitespace, LegalName(Object.name)))
        Config.Whitespace += 1

        if Object.type == 'MESH':
            ExportMesh(Config, Object)

        if len( GetObjectChildren(Object)):
            Config.File.write("{}children\n".format("  " * Config.Whitespace))
            Config.Whitespace += 1

            ExportObjects(Config,GetObjectChildren(Object))

            Config.Whitespace -= 1
            Config.File.write("{}end;\n".format("  " * Config.Whitespace))

        Config.Whitespace -= 1
        Config.File.write("{}end;\n".format("  " * Config.Whitespace))

def ExportMesh(Config, Object):
    print("Exporting Mesh")

    if Config.ApplyModifiers:
        if Config.ExportArmatures:
            #Create a copy of the object and remove all armature modifiers so an unshaped
            #mesh can be created from it.
            Object2 = Object.copy()
            for Modifier in [Modifier for Modifier in Object2.modifiers if Modifier.type == "ARMATURE"]:
                Object2.modifiers.remove(Modifier)
            Mesh = Object2.to_mesh(bpy.context.scene, True, "PREVIEW")
        else:
            Mesh = Object.to_mesh(bpy.context.scene, True, "PREVIEW")
    else:
        Mesh = Object.to_mesh(bpy.context.scene, False, "PREVIEW")

    Config.File.write("{}vertices\n".format("  " * Config.Whitespace))
    Config.Whitespace += 1

    WriteMeshVertices(Config, Mesh)

    Config.Whitespace -= 1
    Config.File.write("{}end;\n".format("  " * Config.Whitespace))
    
    Config.File.write("{}normals\n".format("  " * Config.Whitespace))
    Config.Whitespace += 1

    WriteMeshNormals(Config, Mesh)

    Config.Whitespace -= 1
    Config.File.write("{}end;\n".format("  " * Config.Whitespace))  

    Config.File.write("{}faces\n".format("  " * Config.Whitespace))
    Config.Whitespace += 1

    WriteMeshFaces(Config, Mesh)

    Config.Whitespace -= 1
    Config.File.write("{}end;\n".format("  " * Config.Whitespace))
'''
def WriteMeshVertices(Config, Mesh):
    for Polygon in Mesh.polygons:
        Vertices = list(Polygon.vertices)

        if Config.CoordinateSystem == 1:
            Vertices = Vertices[::-1]

        for Vertex in [Mesh.vertices[Vertex] for Vertex in Vertices]:
            Position = Vertex.co            
            Config.File.write("{}{:9f},{:9f},{:9f};\n".format("  " * Config.Whitespace, Position[0], Position[1], Position[2]))
'''

def veckey3d(v):
    return round(v.x, 6), round(v.y, 6), round(v.z, 6)

def veckey2d(v):
    return round(v[0], 6), round(v[1], 6)

def WriteMeshVertices(Config, Mesh):
    for Vertex in Mesh.vertices:
#        Vertices = list(Polygon.vertices)

#        if Config.CoordinateSystem == 1:
#            Vertices = Vertices[::-1]

#        for Vertex in [Mesh.vertices[Vertex] for Vertex in Vertices]:
        Position = Vertex.co            
        Config.File.write("{}{:9f},{:9f},{:9f};\n".format("  " * Config.Whitespace, Position[0], Position[1], Position[2]))

globalNormals = {}        

def WriteMeshNormals(Config, Mesh):
    global globalNormals
    Mesh.calc_normals()
    totno = 0
    globalNormals = {}
    for f in Mesh.polygons:
        if f.use_smooth:
            for v_idx in f.vertices:
                v = Mesh.vertices[v_idx]
                noKey = veckey3d(v.normal)
                if noKey not in globalNormals:
                    globalNormals[noKey] = totno
                    totno += 1
                    Config.File.write("  " * Config.Whitespace + '%.9f, %.9f, %.9f;\n' % noKey)
        else:
            # Hard, 1 normal from the face.
            noKey = veckey3d(f.normal)
            if noKey not in globalNormals:
                globalNormals[noKey] = totno
                totno += 1
                Config.File.write("  " * Config.Whitespace + '%.9f, %.9f, %.9f;\n' % noKey)    
    
def WriteMeshFaces(Config, Mesh):
    for Polygon in Mesh.polygons:    
        s = "{}".format("  " * Config.Whitespace)
        for Vertex in Polygon.vertices:
            s += "{}/{}, ".format(Vertex, globalNormals[veckey3d(Polygon.normal)])
        Config.File.write(s[:-2] + "\n")

def CloseFile(Config):
    if Config.Verbose:
        print("Closing File...")
    Config.File.close()
    if Config.Verbose:
        print("Done")

CoordinateSystems = (
    ("1", "Left-Handed", ""),
    ("2", "Right-Handed", ""),
    )


AnimationModes = (
    ("0", "None", ""),
    ("1", "Keyframes Only", ""),
    ("2", "Full Animation", ""),
    )

ExportModes = (
    ("1", "All Objects", ""),
    ("2", "Selected Objects", ""),
    )

from bpy.props import StringProperty, EnumProperty, BoolProperty

class RevelationExporter(bpy.types.Operator):
    """Export to the Revelation model format (.model)"""

    bl_idname = "export.revelation"
    bl_label = "Export Revelation Model"

    filepath = StringProperty(subtype='FILE_PATH')

    #Coordinate System
    CoordinateSystem = EnumProperty(
        name="System",
        description="Select a coordinate system to export to",
        items=CoordinateSystems,
        default="1")

    #General Options
    RotateX = BoolProperty(
        name="Rotate X 90 Degrees",
        description="Rotate the entire scene 90 degrees around the X axis so Y is up",
        default=True)
    FlipNormals = BoolProperty(
        name="Flip Normals",
        description="",
        default=False)
    ApplyModifiers = BoolProperty(
        name="Apply Modifiers",
        description="Apply object modifiers before export",
        default=False)
    IncludeFrameRate = BoolProperty(
        name="Include Frame Rate",
        description="Include the AnimTicksPerSecond template which is used by " \
                    "some engines to control animation speed",
        default=False)
    ExportTextures = BoolProperty(
        name="Export Textures",
        description="Reference external image files to be used by the model",
        default=True)
    ExportArmatures = BoolProperty(
        name="Export Armatures",
        description="Export the bones of any armatures to deform meshes",
        default=False)
    ExportAnimation = EnumProperty(
        name="Animations",
        description="Select the type of animations to export. Only object " \
                    "and armature bone animations can be exported. Full " \
                    "Animation exports every frame",
        items=AnimationModes,
        default="0")

    #Export Mode
    ExportMode = EnumProperty(
        name="Export",
        description="Select which objects to export. Only Mesh, Empty, " \
                    "and Armature objects will be exported",
        items=ExportModes,
        default="1")

    Verbose = BoolProperty(
        name="Verbose",
        description="Run the exporter in debug mode. Check the console for output",
        default=False)

    def execute(self, context):
        #Append .model
        FilePath = bpy.path.ensure_ext(self.filepath, ".model")

        Config = RevelationExporterSettings(context,
                                         FilePath,
                                         CoordinateSystem=self.CoordinateSystem,
                                         RotateX=self.RotateX,
                                         FlipNormals=self.FlipNormals,
                                         ApplyModifiers=self.ApplyModifiers,
                                         IncludeFrameRate=self.IncludeFrameRate,
                                         ExportTextures=self.ExportTextures,
                                         ExportArmatures=self.ExportArmatures,
                                         ExportAnimation=self.ExportAnimation,
                                         ExportMode=self.ExportMode,
                                         Verbose=self.Verbose)

        ExportRevelation(Config)
        return {'FINISHED'}

    def invoke(self, context, event):
        if not self.filepath:
            self.filepath = bpy.path.ensure_ext(bpy.data.filepath, ".model")
        WindowManager = context.window_manager
        WindowManager.fileselect_add(self)
        return {"RUNNING_MODAL"}


def menu_func(self, context):
    self.layout.operator(RevelationExporter.bl_idname, text="Revelation (.model)")


def register():
    bpy.utils.register_module(__name__)

    bpy.types.INFO_MT_file_export.append(menu_func)


def unregister():
    bpy.utils.unregister_module(__name__)

    bpy.types.INFO_MT_file_export.remove(menu_func)


if __name__ == "__main__":
    register()
