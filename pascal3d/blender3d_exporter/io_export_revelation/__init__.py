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

#__all__ = [ "rev_export", "rev_helper" ]
if "bpy" in locals():
    import imp
    if "rev_export" in locals():
        imp.reload( rev_export )
    #if "rev_helper" in locals():
    #    imp.reload( rev_helper )
else:        
    from . import rev_export

import bpy

from bpy.utils import (
        register_module,
        unregister_module,
        )

class RevelationExporterSettings:
    def __init__(self,
                 context,
                 FilePath,
                 CoordinateSystem=1,
                 RotateX=True,
                 FlipNormals=False,
                 ApplyModifiers=False,
                 IncludeFrameRate=False,
                 ExportBinaryData=True,  
                 ExportTextures=True,
                 ExportArmatures=False,
                 ExportAnimation=0,
                 PathMode=2,
                 ExportMode=1,
                 Verbose=False):
        self.context = context
        self.FilePath = FilePath
        self.CoordinateSystem = int(CoordinateSystem)
        self.RotateX = RotateX
        self.FlipNormals = FlipNormals
        self.ApplyModifiers = ApplyModifiers
        self.IncludeFrameRate = IncludeFrameRate
        self.ExportBinaryData = ExportBinaryData
        self.ExportTextures = ExportTextures
        self.ExportArmatures = ExportArmatures
        self.ExportAnimation = int(ExportAnimation)
        self.ExportMode = int(ExportMode)
        self.PathMode = int(PathMode)
        self.Verbose = Verbose


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
    
PathModes = (
    ("0", "Absolute Paths", ""),
    ("1", "Relative Paths", ""),
    ("2", "Filenames only", ""),
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
    ExportBinaryData = BoolProperty(
        name="Export Data in Binary Mode",
        description="Export the meshes data binary instead of text",
        default=True)
    PathMode = EnumProperty(
        name="Path Mode",
        description="Export links to external files like textures and objects in relative or absolute mode or just export file names",
        items=PathModes,
        default="2")
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
        default=True)
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
                                         ExportBinaryData=self.ExportBinaryData,
                                         ExportTextures=self.ExportTextures,
                                         ExportArmatures=self.ExportArmatures,
                                         ExportAnimation=self.ExportAnimation,
                                         PathMode=self.PathMode,
                                         ExportMode=self.ExportMode,
                                         Verbose=self.Verbose)

        rev_export.ExportRevelation(Config)
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
