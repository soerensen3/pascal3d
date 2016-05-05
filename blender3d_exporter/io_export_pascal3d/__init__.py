bl_info = {
  'name': 'Pascal3D Scene (.p3d)',
  'author': 'Johannes Rosleff Sörensen',
  'version': (1, 0, 0),
  'blender': (2, 63, 0),
  'location': 'File > Export > Pascal3D Scene (.p3d)',
  'description': 'Export Pascal3D Scene (.p3d)',
  'warning': '',
  'wiki_url': '',
  'tracker_url': '',
  'category': 'Import-Export' }

import bpy, os, struct

from bpy.props import StringProperty, EnumProperty, BoolProperty

from xml.etree import cElementTree as et

PathModes = (
    ('0', 'Absolute Paths', ''),
    ('1', 'Relative Paths', ''),
    ('2', 'Filenames only', ''),
    )    

## P3DXMLFile ------------------------------------------------------------------

class P3DXMLFile:
  meshes = set()
  lamps = set()
  cameras = set()
  materials = set()
  root = et.Element( 'p3dfile' )
  docstack = [ root ]

  def __init__( self, fname ):
    self.fname = fname
    root = et.Element( 'p3dfile' )
    docstack = [ root ]

  def push( self, name ):
    el = et.Element( name )
    self.docstack[ -1 ].append( el )
    self.docstack.append( el )
    return el

  def pop( self ):
    self.docstack.pop()

  def tostring( self, element, level=0 ):
    indent = level * '  '
    result = indent + '<' + element.tag 
    if ( len( element.attrib )):
      for attrib in element.attrib:
        result += '\n%s  %s = "%s"'%( indent, attrib, element.attrib[ attrib ])
    result += '>' + '\n'
    if len( element ):
      for el in element:
        result += self.tostring( el, level + 1 )
    result += indent + '</' + element.tag + '>' + '\n'
    return result

  def write( self ):
    file = open( self.fname, 'w')

    xml_text = self.tostring( self.root )

    file.write('<?xml version="1.0" ?>\n' + xml_text )
    file.close()
#---------------------------------------------------------------------------------


class P3DBinaryFile:
  LoopVertex = {}
  def __init__( self, fname ):
    self.fname = fname
    self.file = open( fname, 'wb' )

  def close( self ):
    self.file.close()
    
  def writevec( self, vec ):
    bin = struct.pack( 'f' * len( vec ), *vec)
    self.file.write( bin )


class P3DExporter( bpy.types.Operator ):
  bl_idname = 'export.p3d'
  bl_label = 'Export Pascal3D Scene'

  filepath = StringProperty( subtype='FILE_PATH' )

  supported_types = [ 'MESH', 'LAMP', 'CAMERA' ]

## CONFIG ------------------------------------------------------------------------
  Verbose = BoolProperty(
    name = 'Verbose',
    description = 'Run the exporter in debug mode. Check the console for output',
    default = True )
    
  ApplyModifiers = BoolProperty(
    name = 'Apply Modifiers',
    description = 'Apply object modifiers before export',
    default = True )
    
  PathMode = EnumProperty(
    name = 'Path Mode',
    description = 'Export links to external files like textures and objects in relative or absolute mode or just export file names',
    items = PathModes,
    default = '2' )
## -------------------------------------------------------------------------------

  def execute( self, context ):
    self.ExportFile()
    return {'FINISHED'}

  def invoke( self, context, event ):
    if not self.filepath:
      self.filepath = os.path.splitext( bpy.data.filepath )[ 0 ] + '.p3d'
    WindowManager = context.window_manager
    WindowManager.fileselect_add(self)
    return {'RUNNING_MODAL'}
    
  def ExportPath( self, path ):
    if ( self.PathMode == '1' ):
      return os.path.relpath( path, os.path.dirname( self.filepath ))
    if ( self.PathMode == '2' ):
      return os.path.basename( path )
    return os.path.abspath( path )

##EXPORTING TRANSFORM ------------------------------------------------------------
  def ExportTransform( self, obj ):
    transform = self.file.push( 'transform' )
    transform.attrib['position'] = '{:9f},{:9f},{:9f}'.format( *obj.location )
    quat = obj.matrix_world.to_quaternion()
    transform.attrib['quaternion'] = '{:9f},{:9f},{:9f},{:9f}'.format( quat[ 1 ], quat[ 2 ], quat[ 3 ], quat[ 0 ])
    transform.attrib['scale'] = '{:9f},{:9f},{:9f}'.format( *obj.scale )
    self.file.pop()
##--------------------------------------------------------------------------------

##EXPORTING FILE -----------------------------------------------------------------

  def ExportFile( self ):
    if ( self.Verbose ):
      self.report({ 'INFO' }, 'Exporting to ' + self.filepath )

    self.file = P3DXMLFile( self.filepath )

    for scene in bpy.data.scenes:
      self.ExportScene( scene )
      
    for data in self.file.meshes:
      self.ExportMesh( data )

    for data in self.file.lamps:
      self.ExportLamp( data )

    for data in self.file.cameras:
      self.ExportCamera( data )
      
    for data in self.file.materials:
      self.ExportMaterial( data )

    self.file.write()

    if ( self.Verbose ):
      self.report({ 'INFO' }, 'Written file' )

    del self.file

##--------------------------------------------------------------------------------

##EXPORTING SCENE ----------------------------------------------------------------

  def ExportScene( self, scene ):
    if ( self.Verbose ):
      self.report({ 'INFO' }, 'Exporting scene ' + scene.name )
    el = self.file.push( 'scene' )
    el.attrib[ 'name' ] = scene.name
    for obj in scene.objects:
      self.ExportObject( obj )
    self.file.pop()

##--------------------------------------------------------------------------------

##EXPORTING OBJECT ---------------------------------------------------------------

  def ExportObject( self, obj ):
    if ( self.Verbose ):
      self.report({ 'INFO' }, 'Exporting object ' + obj.name )
    
    el = self.file.push( 'object' )
    el.attrib[ 'name' ] = obj.name
    el.attrib[ 'type' ] = str( obj.type ).lower()
    if ( obj.data != None and obj.type in self.supported_types ):
      type = str( obj.type ).lower()
      el.attrib[ 'data' ] = type + '_' + obj.data.name
      if ( type == 'mesh' ):
        self.file.meshes.add( obj ) #use object instead of data
      elif ( type == 'lamp' ):
        self.file.lamps.add( obj.data )
      elif ( type == 'camera' ):
        self.file.cameras.add( obj.data )
    
    self.ExportTransform( obj )
    
    self.file.pop()

##--------------------------------------------------------------------------------

##EXPORTING MESH HELPERS ---------------------------------------------------------
  def ExportVertices( self, file ):
    totno = 0
    for vertex in file.mesh.vertices:
        totno += 1
        file.writevec( vertex.co )
    return totno

  def ExportNormals( self, file ):
    file.mesh.calc_normals()
    totno = 0
    for vertex in file.mesh.vertices:
      totno += 1
      file.writevec( vertex.normal )
    return totno

  def ExportTangents( self, file ):
    file.mesh.calc_tangents()
    totno = 0
    file.LoopVertex = {}
    for l in file.mesh.loops:
      file.LoopVertex[ l.vertex_index ] = l.index
      totno += 1
      file.writevec( l.tangent )
    return totno

  def ExportCotangents( self, file ):
    totno = 0
    for l in file.mesh.loops:
      totno += 1
      file.writevec( l.bitangent )
    return totno

  def ExportUVs( self, file ):
    totno = 0
    for uv in file.mesh.uv_layers:
      for uvloop in uv.data:
        totno += 1  
        file.writevec([ uvloop.uv[ 0 ], 1 - uvloop.uv[ 1 ]])
    return totno

  def ExportFaces( self, file ):
    cur_matidx = 0;
    materials = {}
    materials[ 0 ] = {}
    materials[ 0 ][ 'start' ] = 0
    
    for polygon in file.mesh.polygons:
      v_idx = 0
      
      if not ( polygon.material_index == cur_matidx ):
        materials[ cur_matidx ][ 'end' ] = polygon.index - 1
        cur_matidx = polygon.material_index
        materials[ cur_matidx ] = { 'start' : polygon.index }

      bin = struct.pack( '2i', int( len( polygon.vertices )), int( len( file.mesh.uv_layers )))
      file.file.write( bin )


      for vertex in polygon.vertices:
        if ( len( file.LoopVertex )):
          tangent = file.LoopVertex[ vertex ]
        else:
          tangent = -1

        bin = struct.pack( 'i', vertex )
        file.file.write( bin )
        bin = struct.pack( 'i', vertex )
        file.file.write( bin )
        bin = struct.pack( 'i', tangent )
        file.file.write( bin )  
        bin = struct.pack( 'i', tangent )
        file.file.write( bin )
        for uv in file.mesh.uv_layers:
          uv_idx = v_idx + polygon.loop_start
          bin = struct.pack( 'i', uv_idx )
          file.file.write( bin )
          v_idx += 1

    materials[ cur_matidx ][ 'end' ] = polygon.index
    return len( file.mesh.polygons ), materials

##--------------------------------------------------------------------------------

##EXPORTING MESH -----------------------------------------------------------------

  def ExportMesh( self, obj ):
    if self.ApplyModifiers:
      mesh = obj.to_mesh( bpy.context.scene, True, 'PREVIEW', True )
    else:
      mesh = obj.to_mesh( bpy.context.scene, False, 'PREVIEW', True )
      
    if ( self.Verbose ):
      self.report({ 'INFO' }, 'Exporting data ' + obj.data.name )

    el = self.file.push( 'mesh' )
    el.attrib[ 'name' ] = 'mesh_' + obj.data.name

    file = P3DBinaryFile( os.path.splitext( self.file.fname )[ 0 ] + '.mesh_' + obj.data.name + '.p3dmesh' ) #filename without extension as base
    file.mesh = mesh

  
    el.attrib[ 'binary' ] = self.ExportPath( file.fname )
    el.attrib[ 'vertices' ] = self.ExportVertices( file )

    el.attrib[ 'normals' ] = self.ExportNormals( file )

    n = self.ExportUVs( file )
    if ( n > 0 ):
      el.attrib[ 'texcoords' ] = str( n )
      el.attrib[ 'tangents' ] = self.ExportTangents( file )
      el.attrib[ 'cotangents' ] = self.ExportCotangents( file )

    el.attrib['faces'], materials = self.ExportFaces( file )

    file.close()

    for matidx in materials:
      mat = mesh.materials[ matidx ]
      el = self.file.push( 'material' )
      mat_offset = materials[ matidx ]
      el.attrib[ 'name' ] = 'material_' + mat.name
      el.attrib[ 'start' ] = str( mat_offset[ 'start' ])
      el.attrib[ 'end' ] = str( mat_offset[ 'end' ])

      self.file.materials.add( mat )
      self.file.pop()

    self.file.pop()
    del mesh

##--------------------------------------------------------------------------------

##EXPORTING TEXTURE --------------------------------------------------------------

  def ExportTexture( self, tex ):
    filepath = tex.texture.image.filepath
    if not filepath:  # may be '' for generated images
      return
    if ( self.Verbose ):
      self.report({ 'INFO' }, 'Exporting data ' + tex.name )

    el = self.file.push( 'texture' )
    

    # write relative image path
    #filepath = bpy_extras.io_utils.path_reference(filepath, , Config.FilePath,
    #                                  'AUTO', '', copy_set, face_img.library)        

    el.attrib[ 'file' ] = self.ExportPath( filepath )

    if ( tex.use_map_color_diffuse ):
      el.attrib[ 'diffuse' ] = str( tex.diffuse_color_factor )

    if ( tex.use_map_diffuse ):
      el.attrib[ 'diffuse_intensity' ] = str( tex.diffuse_factor )

    if ( tex.use_map_normal ):
      el.attrib[ 'normal' ] = str( tex.normal_factor )

    if ( tex.use_map_color_spec ):
      el.attrib[ 'specular' ] = str( tex.specular_color_factor )

    if ( tex.use_map_specular ):
      el.attrib[ 'specular_intensity' ] = str( tex.specular_factor )

    if ( tex.use_map_alpha ):
      el.attrib[ 'alpha' ] = str( tex.alpha_factor )

    el.attrib[ 'mode' ] = str( tex.blend_type.lower())
    
    if ( tex.uv_layer != '' ):
      el.attrib[ 'layer' ] = Mesh.uv_layers.find( tex.uv_layer )

    self.file.pop()

##--------------------------------------------------------------------------------


##EXPORTING MATERIAL -------------------------------------------------------------

  def ExportMaterial( self, material ):
    if ( self.Verbose ):
      self.report({ 'INFO' }, 'Exporting data ' + material.name )

    el = self.file.push( 'material' )
    el.attrib[ 'name' ] = 'material_' + material.name
    
    el.attrib['diffuse'] = '{:6f}, {:6f}, {:6f}'.format(*( material.diffuse_color * material.diffuse_intensity ))
    el.attrib['specular'] = '{:6f}, {:6f}, {:6f}, {:6f}'.format( material.specular_hardness, *( material.specular_color * material.specular_intensity ))  # Specular        
    
    if ( material.use_shadeless ):
      el.attrib[ 'unlit' ] = 'yes';        
    texlist = (tex for tex in material.texture_slots if ( not ( tex is None )) and tex.use and ( tex.texture.type == 'IMAGE' ) and ( not ( tex.texture.image is None )))
    
    for tex in texlist:
      self.ExportTexture( tex )

    self.file.pop()

##--------------------------------------------------------------------------------

##EXPORTING LAMP ---------------------------------------------------------------

  def ExportLamp( self, lamp ):
    if ( self.Verbose ):
      self.report({ 'INFO' }, 'Exporting data ' + lamp.name )

    el = self.file.push( 'lamp' )
    el.attrib[ 'name' ] = 'lamp_' + lamp.name
    
    el.attrib[ 'type' ] = str( lamp.type.lower())
    el.attrib[ 'color' ] = str( '{:6f}, {:6f}, {:6f}'.format( *lamp.color ))
    el.attrib[ 'energy' ] = str( lamp.energy ) 

    self.file.pop()

##--------------------------------------------------------------------------------

##EXPORTING CAMERA ---------------------------------------------------------------

  def ExportCamera( self, cam ):
    if ( self.Verbose ):
      self.report({ 'INFO' }, 'Exporting data ' + cam.name )

    el = self.file.push( 'camera' )
    el.attrib[ 'name' ] = 'camera_' + cam.name
    
    el.attrib[ 'near' ] = str( cam.clip_start )
    el.attrib[ 'far' ] = str( cam.clip_end )
    el.attrib[ 'fov' ] = str( cam.angle )    

    self.file.pop()

##--------------------------------------------------------------------------------
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
