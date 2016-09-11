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
from mathutils import Vector

from bpy.props import StringProperty, EnumProperty, BoolProperty

from xml.etree import cElementTree as et

PathModes = (
    ('0', 'Absolute Paths', ''),
    ('1', 'Relative Paths', ''),
    ('2', 'Filenames only', ''),
    )

## P3DXMLFile ------------------------------------------------------------------

class P3DXMLFile:
  def __init__( self, fname ):
    self.fname = fname
    self.root = et.Element( 'p3dfile' )
    self.docstack = [ self.root ]
    self.meshes = set()
    self.lamps = set()
    self.cameras = set()
    self.materials = set()
    self.armatures = set()
    self.textures = set()    
    self.actions = set()

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
    
  def writeint( self, i ):
    bin = struct.pack( 'i', i )
    self.file.write( bin )      

  def writeintvec( self, ivec ):
    bin = struct.pack( 'i' * len( ivec ), *ivec )
    self.file.write( bin )      

class P3DExporter( bpy.types.Operator ):
  bl_idname = 'export.p3d'
  bl_label = 'Export Pascal3D Scene'

  filepath = StringProperty( subtype='FILE_PATH' )

  #supported_types = [ 'MESH', 'LAMP', 'CAMERA', 'ARMATURE' ]
  supported_types = []
## CONFIG ------------------------------------------------------------------------
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
    default = False )    
    
  ExportAnimations = BoolProperty(
    name = 'Export Animations',
    description = 'Select wether to export animations.',
    default = False )        

  PathMode = EnumProperty(
    name = 'Path Mode',
    description = 'Export links to external files like textures and objects in relative or absolute mode or just export file names',
    items = PathModes,
    default = '2' )
    
  ExportSceneCamera = BoolProperty(
    name = 'Export scene camera',
    description = 'The exporter can set the scene''s camera to match the blender scene. This might however be undesired in most cases so this is disabled by default.This setting has no effect if Export Cameras is set to False.',
    default = False )
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
 
    self.supported_types = []    
    if self.ExportMeshes:
      self.supported_types.append( 'MESH' )
    if self.ExportArmatures:
      self.supported_types.append( 'ARMATURE' )
    if self.ExportLamps:
      self.supported_types.append( 'LAMP' )
    if self.ExportCameras:
      self.supported_types.append( 'CAMERA' )
    if self.ExportAnimations:
      self.supported_types.append( 'ACTION' )

    if 'ARMATURE' in self.supported_types:
        # This is needed so applying modifiers dosnt apply the armature deformation, its also needed
        # ...so mesh objects return their rest worldspace matrix when joint-parents are exported as weighted meshes.
        # set every armature to its rest, backup the original values so we done mess up the scene
        self.ob_arms_orig_rest = [arm.pose_position for arm in bpy.data.armatures]

        for arm in bpy.data.armatures:
            arm.pose_position = 'REST'

        if self.ob_arms_orig_rest:
            for ob_base in bpy.data.objects:
                if ob_base.type == 'ARMATURE':
                    ob_base.update_tag()
        scene = bpy.context.scene
        scene.frame_set(scene.frame_current)
        # This causes the makeDisplayList command to effect the mesh      

            
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
      
    for data in self.file.textures:
      self.ExportTexture( data )
      
    for data in self.file.armatures:
      self.ExportArmature( data )
      
    if 'ARMATURE' in self.supported_types:
        # This is needed so applying modifiers dosnt apply the armature deformation, its also needed
        # ...so mesh objects return their rest worldspace matrix when joint-parents are exported as weighted meshes.
        # set every armature to its rest, backup the original values so we done mess up the scene
        self.ob_arms_orig_rest = [arm.pose_position for arm in bpy.data.armatures]

        for arm in bpy.data.armatures:
            arm.pose_position = 'POSE'

        if self.ob_arms_orig_rest:
            for ob_base in bpy.data.objects:
                if ob_base.type == 'ARMATURE':
                    ob_base.update_tag()
        scene = bpy.context.scene
        scene.frame_set(scene.frame_current)
        # This causes the makeDisplayList command to effect the mesh     
      
    for data in self.file.actions:
      self.ExportAction( data )      

    self.file.write()

    if ( self.Verbose ):
      self.report({ 'INFO' }, 'Written file' )
      
    '''if 'ARMATURE' in self.supported_types:
        # now we have the meshes, restore the rest arm position
        for i, arm in enumerate(bpy.data.armatures):
            arm.pose_position = self.ob_arms_orig_rest[i]

        if self.ob_arms_orig_rest:
            for ob_base in bpy.data.objects:
                if ob_base.type == 'ARMATURE':
                    ob_base.update_tag()
            # This causes the makeDisplayList command to effect the mesh
            
            scene = bpy.context.scene
            scene.frame_set(scene.frame_current)'''

    del self.file

##--------------------------------------------------------------------------------

##EXPORTING SCENE ----------------------------------------------------------------

  def ExportScene( self, scene ):
    if ( self.Verbose ):
      self.report({ 'INFO' }, 'Exporting scene ' + scene.name )
    el = self.file.push( 'scene' )
    el.attrib[ 'name' ] = scene.name
    if ( self.ExportSceneCamera ):
      if ( not ( scene.camera is None )):
        el.attrib[ 'camera' ] = scene.camera.name
    self.report({ 'INFO' }, ', '.join( self.supported_types ))
    for obj in scene.objects:
      if obj.type in self.supported_types:
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
      elif ( type == 'armature' ):
        self.file.armatures.add( obj ) #use object instead of data
    
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
    if ( totno == 0 ):
        return 0, 0
    return len( file.mesh.uv_layers ), int( totno / len( file.mesh.uv_layers ))
    
  def ExportLoops( self, file ):
    totno = 0
    for loop in file.mesh.loops:
      totno += 1  
      file.writeint( loop.vertex_index )
    return totno

  def ExportFaces( self, file ):
    cur_matidx = 0;
    materials = {}
    materials[ 0 ] = {}
    materials[ 0 ][ 'start' ] = 0
    
    for polygon in file.mesh.polygons: 
      if not ( polygon.material_index == cur_matidx ):
        materials[ cur_matidx ][ 'end' ] = polygon.index - 1
        cur_matidx = polygon.material_index
        materials[ cur_matidx ] = { 'start' : polygon.index }
      
      bin = struct.pack( 'i', polygon.loop_start )
      file.file.write( bin )

    materials[ cur_matidx ][ 'end' ] = polygon.index
    return len( file.mesh.polygons ), materials

  def ExportVertexGroups( self, file ):
    grps = []
    for vgroup in file.obj.vertex_groups:
        grps.append( vgroup.name )

    bin = b''
    for vertex in file.mesh.vertices: #make dictionary of all groups
        vgrps = {}
        for vgroup in vertex.groups:
            vgrps[ vgroup.group ] = vgroup.weight
        from operator import itemgetter
        srt = sorted( vgrps.items(), key=itemgetter( 1 ), reverse=True )

        vec = [ 0, 0, 0, 0 ] # make sure the length of the vecs is always 4
        idx = [ 0, 0, 0, 0 ] # fill with zero indices, weight will be zero if idx not used
        for i in range( 0, min( 4, len( vgrps ))):
            vec[ i ] = srt[ i ][ 1 ]
            idx[ i ] = srt[ i ][ 0 ]
        vec = Vector( vec ).normalized()
        #self.report({ 'INFO' }, str( idx ))
        #self.report({ 'INFO' }, str( vec ))
        file.writevec(( vec[ 0 ], vec[ 1 ], vec[ 2 ])) # we only need the first 3 weights as the last can be calculated as 1-other weights
        
        bin += struct.pack( '4i', *idx )
        #file.writeintvec( idx )
    file.file.write( bin )           
    return grps # no need to return the number of vertices again, instead we return the name of the groups
      
##--------------------------------------------------------------------------------

##EXPORTING MESH -----------------------------------------------------------------

  def ExportMesh( self, obj ):
    #armature = None
    if self.ApplyModifiers:
      #if self.ExportArmatures:
      #  armature = obj.find_armature()
      #  if ( not ( armature is None )):
      #    pose_position = armature.data.pose_position
      #    armature.data.pose_position = 'REST'    
      mesh = obj.to_mesh( bpy.context.scene, True, 'PREVIEW', True )
    else:
      mesh = obj.to_mesh( bpy.context.scene, False, 'PREVIEW', True )
    #if ( not ( armature is None )):  
    #  armature.data.pose_position = pose_position
    if ( self.Verbose ):
      self.report({ 'INFO' }, 'Exporting data ' + obj.data.name + ' for object ' + obj.name )

    el = self.file.push( 'mesh' )
    el.attrib[ 'name' ] = 'mesh_' + obj.data.name

    file = P3DBinaryFile( os.path.splitext( self.file.fname )[ 0 ] + '.mesh_' + obj.data.name + '.p3dmesh' ) #filename without extension as base
    file.mesh = mesh
    file.obj = obj

  
    el.attrib[ 'binary' ] = self.ExportPath( file.fname )
    
    el.attrib[ 'vertices' ] = self.ExportVertices( file )
    grps = self.ExportVertexGroups( file )
    for grp in grps:
        elgrp = self.file.push( 'weightgroup' )
        elgrp.attrib[ 'name' ] = grp
        self.file.pop()
        
    #el.attrib['vertexgroups'] = '\'' + '\', \''.join( grps ) + '\''
    
    el.attrib[ 'normals' ] = self.ExportNormals( file )
    el.attrib[ 'loops' ] = self.ExportLoops( file )

    nl, n = self.ExportUVs( file )
    if ( nl > 0 ):
        el.attrib[ 'texcoords' ] = str( n )
        el.attrib[ 'texlayers' ] = str( nl )
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

    if self.ExportArmatures:
        armature = obj.find_armature()
        if ( not ( armature is None )):
            el = self.file.push( 'modifier' )
            el.attrib[ 'name' ] = 'armature'
            el.attrib[ 'data' ] = 'armature_' + armature.name
            self.file.pop()

    self.file.pop()
    del mesh

##--------------------------------------------------------------------------------

##EXPORTING MAP --------------------------------------------------------------

  def ExportMap( self, map ):
    filepath = map.texture.image.filepath
    if not filepath:  # may be '' for generated images
      return
    if ( self.Verbose ):
      self.report({ 'INFO' }, 'Exporting data ' + map.name )

    el = self.file.push( 'map' )
    
    el.attrib[ 'name' ] = map.name

    # write relative image path
    #filepath = bpy_extras.io_utils.path_reference(filepath, , Config.FilePath,
    #                                  'AUTO', '', copy_set, face_img.library)        

    #el.attrib[ 'file' ] = self.ExportPath( filepath )
    el.attrib[ 'data' ] = 'tex_' + map.texture.name
    self.file.textures.add( map )

    if ( map.use_map_color_diffuse ):
      el.attrib[ 'diffuse' ] = str( map.diffuse_color_factor )

    if ( map.use_map_diffuse ):
      el.attrib[ 'diffuse_intensity' ] = str( map.diffuse_factor )

    if ( map.use_map_normal ):
      el.attrib[ 'normal' ] = str( map.normal_factor )

    if ( map.use_map_color_spec ):
      el.attrib[ 'specular' ] = str( map.specular_color_factor )

    if ( map.use_map_specular ):
      el.attrib[ 'specular_intensity' ] = str( map.specular_factor )

    if ( map.use_map_alpha ):
      el.attrib[ 'alpha' ] = str( map.alpha_factor )
      
    if ( not(( map.offset == Vector(( 0.0, 0.0, 0.0 )) and ( map.scale == Vector(( 1.0, 1.0, 1.0 )))))):
      transform = self.file.push( 'transform' )
      transform.attrib[ 'position' ] = '{:9f},{:9f},{:9f}'.format( *map.offset )
      transform.attrib[ 'quaternion' ] = '{:9f},{:9f},{:9f},{:9f}'.format( 0.0, 0.0, 0.0, 1.0 )
      transform.attrib[ 'scale' ] = '{:9f},{:9f},{:9f}'.format( *map.scale )
      self.file.pop()

    el.attrib[ 'mode' ] = str( map.blend_type.lower())
    
    if ( map.uv_layer != '' ):
      el.attrib[ 'layer' ] = 0 #self.file.mesh.uv_layers.find( map.uv_layer )

    self.file.pop()

##--------------------------------------------------------------------------------

##EXPORTING TEXTURE --------------------------------------------------------------

  def ExportTexture( self, map ):
    filepath = map.texture.image.filepath
    if not filepath:  # may be '' for generated images
      return
    if ( self.Verbose ):
      self.report({ 'INFO' }, 'Exporting data ' + map.name )

    el = self.file.push( 'texture' )
    
    el.attrib[ 'name' ] = 'tex_' + map.texture.name

    # write relative image path
    #filepath = bpy_extras.io_utils.path_reference(filepath, , Config.FilePath,
    #                                  'AUTO', '', copy_set, face_img.library)        

    el.attrib[ 'file' ] = self.ExportPath( filepath )

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
    texlist = (map for map in material.texture_slots if ( not ( map is None )) and map.use and ( map.texture.type == 'IMAGE' ) and ( not ( map.texture.image is None )))
    
    for map in texlist:
      self.ExportMap( map )

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

##EXPORTING JOINT ----------------------------------------------------------------

  def ExportJoint( self, bone ):
    if ( self.Verbose ):
      self.report({ 'INFO' }, 'Exporting data ' + bone.name )

    el = self.file.push( 'joint' )
    el.attrib[ 'name' ] = 'joint_' + bone.name 
    
    position = bone.head_local
    el.attrib['position'] = '{:9f},{:9f},{:9f}'.format( position[ 0 ], position[ 1 ], position[ 2 ])
    quat = bone.matrix_local.to_quaternion()
    el.attrib['quaternion'] = '{:9f},{:9f},{:9f},{:9f}'.format( quat[ 1 ], quat[ 2 ], quat[ 3 ], quat[ 0 ])

    bones = bone.children
    for bone in bones:
      self.ExportJoint( bone )

    self.file.pop()
##--------------------------------------------------------------------------------

  '''
    def get_pose_bone_matrix( self, arm, pose_bone ):
      #import mathutils
      #bone_space = mathutils.Matrix(((1,0,0,0),(0,0,1,0),(0,-1,0,0),(0,0,0,1)))
      #mat = arm.convert_space(pose_bone, pose_bone.matrix_basis, 'POSE', 'WORLD') * bone_space
      #return mat
      
      bone_matrix = pose_bone.matrix_channel
      if ( not ( pose_bone.parent is None )):
          parent_matrix = pose_bone.parent.matrix_channel
          bone_matrix = parent_matrix.inverted() * bone_matrix
      return bone_matrix
  ##EXPORTING ARMATURE -------------------------------------------------------------

  ##EXPORTING JOINT ----------------------------------------------------------------

    def ExportPoseJoint( self, armature, pose_bone ):
      if ( self.Verbose ):
        self.report({ 'INFO' }, 'Exporting data ' + pose_bone.name )

      el = self.file.push( 'joint' )
      el.attrib[ 'name' ] = 'joint_' + pose_bone.name 
      
      matrix = armature.matrix_local * self.get_pose_bone_matrix( armature, pose_bone )
      position, quat, scale = matrix.decompose()
      el.attrib['position'] = '{:9f},{:9f},{:9f}'.format( position[ 0 ], position[ 1 ], position[ 2 ])
      el.attrib['quaternion'] = '{:9f},{:9f},{:9f},{:9f}'.format( quat[ 1 ], quat[ 2 ], quat[ 3 ], quat[ 0 ])

      self.file.pop()
  ##--------------------------------------------------------------------------------
  
  
  OR
  
  def get_pose_bone_matrix( self, arm, pose_bone ):
    #import mathutils
    #bone_space = mathutils.Matrix(((1,0,0,0),(0,0,1,0),(0,-1,0,0),(0,0,0,1)))
    #mat = arm.convert_space(pose_bone, pose_bone.matrix_basis, 'POSE', 'WORLD') * bone_space
    #return mat
    
    bone_matrix = pose_bone.matrix_basis
    if ( not ( pose_bone.parent is None )):
        parent_matrix = pose_bone.parent.matrix_channel
        bone_matrix = parent_matrix.inverted() * bone_matrix
    return bone_matrix
  ##EXPORTING ARMATURE -------------------------------------------------------------

  ##EXPORTING JOINT ----------------------------------------------------------------

    def ExportPoseJoint( self, armature, pose_bone ):
      if ( self.Verbose ):
        self.report({ 'INFO' }, 'Exporting data ' + pose_bone.name )

      el = self.file.push( 'joint' )
      el.attrib[ 'name' ] = 'joint_' + pose_bone.name 
      
      matrix = armature.matrix_local * self.get_pose_bone_matrix( armature, pose_bone )
      position, quat, scale = matrix.decompose()
      el.attrib['position'] = '{:9f},{:9f},{:9f}'.format( position[ 0 ], position[ 1 ], position[ 2 ])
      el.attrib['quaternion'] = '{:9f},{:9f},{:9f},{:9f}'.format( quat[ 1 ], quat[ 2 ], quat[ 3 ], quat[ 0 ])

      self.file.pop()
  ##--------------------------------------------------------------------------------  
  '''
  


  def get_pose_bone_matrix( self, arm, pose_bone ):
    #import mathutils
    #bone_space = mathutils.Matrix(((1,0,0,0),(0,0,1,0),(0,-1,0,0),(0,0,0,1)))
    #mat = arm.convert_space(pose_bone, pose_bone.matrix_basis, 'POSE', 'WORLD') * bone_space
    #return mat
    
    bone_matrix = pose_bone.matrix_basis
    if ( not ( pose_bone.parent is None )):
        parent_matrix = pose_bone.parent.matrix_channel
        bone_matrix = parent_matrix.inverted() * bone_matrix
    return bone_matrix
##EXPORTING ARMATURE -------------------------------------------------------------

##EXPORTING JOINT ----------------------------------------------------------------

  def ExportPoseJoint( self, armature, pose_bone ):
    if ( self.Verbose ):
      self.report({ 'INFO' }, 'Exporting data ' + pose_bone.name )

    el = self.file.push( 'joint' )
    el.attrib[ 'name' ] = 'joint_' + pose_bone.name 
    
    matrix = self.get_pose_bone_matrix( armature, pose_bone ) * armature.matrix_local.inverted()
    position, quat, scale = matrix.decompose()
    el.attrib['position'] = '{:9f},{:9f},{:9f}'.format( position[ 0 ], position[ 1 ], position[ 2 ])
    el.attrib['quaternion'] = '{:9f},{:9f},{:9f},{:9f}'.format( quat[ 1 ], quat[ 2 ], quat[ 3 ], quat[ 0 ])

    self.file.pop()
##--------------------------------------------------------------------------------

  def ExportArmature( self, armature ):
    if ( self.Verbose ):
      self.report({ 'INFO' }, 'Exporting data ' + armature.data.name )

    el = self.file.push( 'armature' )
    el.attrib[ 'name' ] = 'armature_' + armature.data.name
    
    bones = armature.data.bones
    for bone in bones:
      if bone.parent is None:
        self.ExportJoint( bone )
        
    if ( not ( armature.animation_data is None ) and not ( armature.animation_data.action is None )):
        self.file.actions.add((armature.animation_data.action, armature ))
        el.attrib[ 'action' ] = 'action_' + armature.data.name + armature.animation_data.action.name

    self.file.pop()

##--------------------------------------------------------------------------------

##EXPORTING ACTION ---------------------------------------------------------------

  def ExportAction( self, action ):
    if ( self.Verbose ):
      self.report({ 'INFO' }, 'Exporting data ' + action[ 0 ].name )

    el = self.file.push( 'action' )
    el.attrib[ 'name' ] = 'action_' + action[ 1 ].name + action[ 0 ].name
    scene = bpy.context.scene
    idx = 0
    for frame in range( scene.frame_end + 1 ):
        elframe = self.file.push( 'frame' )
        scene.frame_set( frame )
        for pose_bone in action[ 1 ].pose.bones:
            #bone = action[ 1 ].data.bones[ pose_bone.name ]
            self.ExportPoseJoint( action[ 1 ], pose_bone )
        idx += 1
        self.file.pop()        

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
  
  
