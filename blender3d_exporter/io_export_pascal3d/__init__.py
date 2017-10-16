bl_info = {
  'name': 'Pascal3D Scene Exporter (.p3d)',
  'author': 'Johannes Rosleff Sörensen',
  'version': ( 1, 0, 0 ),
  'blender': ( 2, 76, 0 ),
  'location': 'File > Export > Pascal3D Scene (.p3d)',
  'description': 'Export Pascal3D Scene (.p3d)',
  'warning': '',
  'wiki_url': '',
  'tracker_url': '',
  'category': 'Import-Export' }

if "bpy" in locals():
    import importlib
    for lib in [ p3dfiles, p3ddata, p3ddatablock, p3dexporthelper, p3daction, p3dactor, p3darmature, p3dcamera, p3djoint, p3dlight, p3dmaterial, p3dmesh, p3dscene, p3dtexture ]:
        importlib.reload( lib )
    print("Reloaded multifiles")
else:
    from . import p3dfiles, p3ddata, p3ddatablock, p3dexporthelper, p3daction, p3dactor, p3darmature, p3dcamera, p3djoint, p3dlight, p3dmaterial, p3dmesh, p3dscene, p3dtexture

    print("Imported multifiles")

import bpy, os

from bpy.props import StringProperty, EnumProperty, BoolProperty

PathModes = (
    ( '0', 'Absolute Paths', '' ),
    ( '1', 'Relative Paths', '' ),
    ( '2', 'Filenames only', '' ),
    )

class P3DExporter( bpy.types.Operator ):
    binfile = None
    bl_idname = 'export.p3d'
    bl_label = 'Export Pascal3D Scene'

    filepath = StringProperty( subtype='FILE_PATH' )

    # supported_types = [ 'MESH', 'LAMP', 'CAMERA', 'ARMATURE' ]
    supported_types = []
    # CONFIG ------------------------------------------------------------------------
    Verbose = BoolProperty(
      name = 'Verbose',
      description = 'Run the exporter in debug mode. Check the console for output',
      default = True )

    ApplyModifiers = BoolProperty(
      name = 'Apply Modifiers',
      description = 'Apply object modifiers before export. When used together with Export Armatures the Armature modifier is not applied.',
      default = True )

    ExportCameras = BoolProperty(
      name = 'Export Cameras',
      description = 'Select wether to export camera objects.',
      default = False )

    ExportLamps = BoolProperty(
      name = 'Export Lamps',
      description = 'Select wether to export lamp objects.',
      default = True )

    ExportMeshes = BoolProperty(
      name = 'Export Meshes',
      description = 'Select wether to export mesh objects.',
      default = True )

    ExportArmatures = BoolProperty(
      name = 'Export Armatures',
      description = 'Select wether to export armatures.',
      default = True )

    ExportAnimations = BoolProperty(
      name = 'Export Animations',
      description = 'Select wether to export animations.',
      default = True )

    PathMode = EnumProperty(
      name = 'Path Mode',
      description = 'Export links to external files like textures and objects in relative or absolute mode or just export file names',
      items = PathModes,
      default = '2' )

    ExportSceneCamera = BoolProperty(
      name = 'Export scene camera',
      description = 'The exporter can set the scene''s camera to match the blender scene. This might however be undesired in most cases so this is disabled by default.This setting has no effect if Export Cameras is set to False.',
      default = False )

    ExportVisibleOnly = BoolProperty(
      name = 'Export visible only',
      description = 'Only visible objects will be exported.',
      default = True )

    SaveTextures = BoolProperty(
      name = 'Copy textures',
      description = 'Copies the textures to the target location.',
      default = True )

    # -------------------------------------------------------------------------------

    def execute( self, context ):
        self.ExportFile()
        return { 'FINISHED' }

    def invoke( self, context, event ):
        if not self.filepath:
            self.filepath = os.path.splitext( bpy.data.filepath )[ 0 ] + '.p3d'
        WindowManager = context.window_manager
        WindowManager.fileselect_add( self )
        return { 'RUNNING_MODAL' }

    def ExportPath( self, path ):
        if ( self.PathMode == '1' ):
            return os.path.relpath( path, os.path.dirname( self.filepath ))
        if ( self.PathMode == '2' ):
            return os.path.basename( path )

            return os.path.abspath( path )
    # EXPORTING FILE -----------------------------------------------------------------

    def ExportFile( self ):
        if ( self.Verbose ):
            self.report({ 'INFO' }, 'Exporting to ' + self.filepath )

        self.supported_types = []
        if self.ExportMeshes:
            self.supported_types.append( 'MESH' )
        if self.ExportLamps:
            self.supported_types.append( 'LAMP' )
        if self.ExportCameras:
            self.supported_types.append( 'CAMERA' )
        if self.ExportAnimations:
            self.supported_types.append( 'ACTION' )

        Data = p3ddata.P3DData( filename=self.filepath )
        Data.Exporter = self
        for scene in bpy.data.scenes:
            p3dexporthelper.export_data_root( scene, Data )
        Data.toJSONFile()
        del Data

def menu_func( self, context ):
    self.layout.operator( P3DExporter.bl_idname, text='Pascal3D Scene (.p3d)' )


def register():
    bpy.utils.register_module( __name__ )

    bpy.types.INFO_MT_file_export.append( menu_func )


def unregister():
    bpy.utils.unregister_module( __name__ )

    bpy.types.INFO_MT_file_export.remove( menu_func )


if __name__ == '__main__':
    register()
