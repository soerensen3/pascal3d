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

import bpy, os, struct
from mathutils import Vector, Matrix, Euler, Quaternion
import math

from bpy.props import StringProperty, EnumProperty, BoolProperty

from xml.etree import cElementTree as et

PathModes = (
    ( '0', 'Absolute Paths', '' ),
    ( '1', 'Relative Paths', '' ),
    ( '2', 'Filenames only', '' ),
    )

Version = '0.1'

legal_name = lambda name: "".join([ c if c.isalnum() else "_" for c in name ])

def legal_name_quote( s ):
    sp = s.split( "\"" )
    for i in range( 1, len( sp ), 2 ):
        sp[ i ] = legal_name( sp[ i ])
    return "\"".join( sp )



## P3DXMLFile ------------------------------------------------------------------

class P3DXMLFile:
    def __init__( self, fname ):
        self.fname = fname
        self.root = et.Element( 'p3dfile' )
        self.root.attrib[ 'version' ] = Version
        self.docstack = [ self.root ]
        self.meshes = set()
        self.lamps = set()
        self.cameras = set()
        self.materials = set()
        self.armatures = {}
        self.textures = set()
        self.actions = set()
        self.datapaths = {}
        self.elements = {}
        self.storednames = {}

    def push( self, name ):
        el = et.Element( name )
        self.docstack[ -1 ].append( el )
        self.docstack.append( el )
        return el

    def store( self, item, name ):
        path = self.todatapath()
        self.datapaths[ name ] = path
        self.elements[ name ] = self.docstack[ -1 ]
        if ( not ( item is None )):
            self.storednames[ item ] = name
        return [ name, path ]

    def pop( self ):
        self.docstack.pop()

    def tostring( self, element, level=0 ):
        indent = level * '  '
        result = indent + '<' + element.tag
        import html
        if ( len( element.attrib )):
            for attrib in element.attrib:
                result += '\n%s   %s = "%s"'%( indent, attrib, html.escape( str( element.attrib[ attrib ])))
        #since text is unused there is no need to check if a full closing tag is required
        if len( element ):
            result += '>' + '\n'
            for el in element:
              result += self.tostring( el, level + 1 )
            result += indent + '</' + element.tag + '>' + '\n'
        else:
            result += ' />' + '\n'
        return result

    def todatapath( self ):
        return '/' + '/'.join( self.toarray())

    def toarray( self ):
        result = []
        for element in self.docstack:
            try:
                name = element.attrib[ 'Name' ]
                result.append( name )
            except KeyError:
                pass
        return result

    def write( self ):
        file = open( self.fname, 'w')
        xml_text = self.tostring( self.root )

        file.write( '<?xml version="1.0" ?>\n' + xml_text )
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

    def getposition( self ):
        return self.file.tell()

    def getfname( self ):
        return self.file.name

    def getfileandpos( self ):
        return os.path.basename( self.file.name ) + ':' + str( self.file.tell())

class P3DExporter( bpy.types.Operator ):
    binfile = None
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

    SaveTextures = BoolProperty(
      name = 'Copy textures',
      description = 'Copies the textures to the target location.',
      default = True )

    ## -------------------------------------------------------------------------------

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

    ##EXPORTING TRANSFORM ------------------------------------------------------------
    def ExportTransform( self, obj ):
        transform = self.file.docstack[ - 1 ]
        loc, quat, scal = obj.matrix_local.decompose()
        transform.attrib[ 'Position' ] = '{:9f},{:9f},{:9f}'.format( *loc )
        #quat.negate() #change handedness zxw
        #quat = Vector(( -quat.y, quat.x, quat.z, quat.w ))
        transform.attrib[ 'Quaternion' ] = '{:9f},{:9f},{:9f},{:9f}'.format( quat[ 1 ], quat[ 2 ], quat[ 3 ], quat[ 0 ])
        transform.attrib[ 'Scale' ] = '{:9f},{:9f},{:9f}'.format( *scal )
        transform.attrib[ 'RotationOrder' ] = 'ro' + obj.rotation_mode # Does not matter for quaternion rotation but when rotating with euler angles later
    ##--------------------------------------------------------------------------------

    ##EXPORTING FILE -----------------------------------------------------------------

    def ExportFile( self ):
        if ( self.Verbose ):
            self.report({ 'INFO' }, 'Exporting to ' + self.filepath )

        self.file = P3DXMLFile( self.filepath )

        self.supported_types = []
        if self.ExportMeshes:
            self.supported_types.append( 'MESH' )
        if self.ExportLamps:
            self.supported_types.append( 'LAMP' )
        if self.ExportCameras:
            self.supported_types.append( 'CAMERA' )
        if self.ExportAnimations:
            self.supported_types.append( 'ACTION' )

        for scene in bpy.data.scenes:
            self.ExportScene( scene )

        if self.ExportArmatures:
            self.supported_types.append( 'ARMATURE' ) # add armatures later as we do not care about the armature objects but only the datablock

        for data in self.file.meshes: # will cause duplicates for now because objects instead of meshes are
                                      # put into self.file.meshes. This is necessary because of the modifiers
                                      # which need to be applied. There is no solution for detecting same modifer
                                      # configuration across multiple objects yet
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
            data.data.pose_position = 'REST'
            data.update_tag()
            scene = bpy.context.scene
            scene.frame_set(scene.frame_current)

            self.ExportArmature( data )

            data.data.pose_position = 'POSE'
            data.update_tag()
            scene = bpy.context.scene
            scene.frame_set(scene.frame_current)


        for data in self.file.actions:
            self.ExportAction( data )

        self.file.write()
        if not ( self.binfile is None ):
            self.binfile.close()
            self.binfile = None

        if ( self.Verbose ):
            self.report({ 'INFO' }, 'Written file' )

        del self.file

  ##--------------------------------------------------------------------------------

  ##EXPORTING SCENE ----------------------------------------------------------------
    def ExportSceneScreenshot( self, scene ):
        self.report({ 'INFO' }, 'Exporting thumb for scene ' + scene.name )
        scene.render.resolution_x = 128
        scene.render.resolution_y = 128
        scene.render.resolution_percentage = 100
        #render
        bpy.context.window.screen.scene = scene
        bpy.ops.render.opengl()
        #save image
        img_name = os.path.splitext( self.file.fname )[ 0 ] + '.thumb.' + scene.name + '.png' #filename without extension as base
        bpy.data.images[ 'Render Result' ].save_render( img_name )
        #bpy.ops.image.open( filepath = file_dir+img_name )
        #bpy.data.images[img_name].pack()

    def ExportScene( self, scene ):
        el = self.file.push( 'scene' )
        el.attrib[ 'Name' ] = legal_name( scene.name )
        path = self.file.store( scene, legal_name( scene.name ))[1]
        if ( self.Verbose ):
            self.report({ 'INFO' }, 'Exporting scene ' + scene.name + ' to ' + path )

        if ( self.ExportSceneCamera ):
          if ( not ( scene.camera is None )):
            el.attrib[ 'Cam' ] = legal_name( scene.camera.name )
        self.report({ 'INFO' }, ', '.join( self.supported_types ))
        self.ExportSceneScreenshot( scene )
        for obj in scene.objects:
            if obj.parent is None and obj.is_visible( scene ):
                self.ExportObject( obj )
        self.file.pop()

     ##--------------------------------------------------------------------------------

    ##EXPORTING OBJECT ---------------------------------------------------------------
    def ExportJointObject( self, joint, armature ):
        #bone_space = Matrix(((1,0,0,0),(0,0,1,0),(0,-1,0,0),(0,0,0,1)))
        def get_bone_relative_position( bone ):
            position = bone.matrix_local.inverted() * Vector(( 0, 0, 0 ))
            if bone.parent is None:
                position = position + bone.head
            else:
                parent = bone.parent
                parent_matrix = parent.matrix_local.inverted()
                diff = parent.tail_local - parent.head_local
                position = position + (parent_matrix * bone.head) + diff
            return position

        bone_space=Quaternion((0.7071068286895752, -0.7071068286895752, 0.0, 0.0))

        el = self.file.push( 'object' )
        el.attrib[ 'Name' ] = legal_name( joint.name )
        path = self.file.store( None, legal_name( joint.name ))[1]
        #loc, quat, scal = ( bone_space * joint.matrix ).decompose()
        #loc, quat = joint.bone.matrix_local.translation + joint.location, bone_space * joint.rotation_quaternion * joint.bone.matrix_local.to_quaternion()
        #loc = Vector(( _loc[ 0 ], _loc[ -2 ], _loc[ 1 ]))
        #loc, quat, scal = joint.matrix_basis.decompose()
        #loc, quat = get_bone_relative_position( joint.bone ), joint.matrix_basis.to_quaternion()
        #quat = bone_space * quat
        joint_matrix = joint.matrix_local
        if joint.parent:
            joint_matrix = joint.matrix_local.inverted() * joint.matrix_local
        loc, quat, scal = joint.matrix_basis.decompose()
        el.attrib[ 'Position' ] = '{:9f},{:9f},{:9f}'.format( *loc )
        #el.attrib[ 'Position' ] = '{:9f},{:9f},{:9f}'.format( loc[ 0 ], -loc[ 2 ], loc[ 1 ])
        el.attrib[ 'Quaternion' ] = '{:9f},{:9f},{:9f},{:9f}'.format( quat[ 1 ], quat[ 2 ], quat[ 3 ], quat[ 0 ])
        el.attrib[ 'RotationOrder' ] = 'ro' + joint.rotation_mode # Does not matter for quaternion rotation but when rotating with euler angles later
        el.attrib[ 'Data' ] = '/armature_' + legal_name( armature.data.name ) + '/joint_' + legal_name( joint.name )
        for child in joint.children:
            self.ExportJointObject( child, armature )
        self.file.pop()



    def ExportObject( self, obj ):
        el = self.file.push( 'object' )
        el.attrib[ 'Name' ] = legal_name( obj.name )

        path = self.file.store( obj, legal_name( obj.name ))[1]
        if ( self.Verbose ):
          self.report({ 'INFO' }, 'Exporting object ' + obj.name + ' to ' + path )

        if (( obj.game.use_collision_bounds ) and ( not obj.game.use_ghost )):
          el.attrib[ 'Collision' ] = 'ac' + obj.game.collision_bounds_type.capitalize()
        else:
          el.attrib[ 'Collision' ] = 'acNone'
        if ( obj.data and ( obj.type in self.supported_types )):
            type = str( obj.type ).lower()
            el.attrib[ 'Data' ] = '/' + type + '_' + legal_name( obj.data.name )
            if ( type == 'mesh' ):
                self.file.meshes.add( obj ) #use object instead of data
            elif ( type == 'lamp' ):
                self.file.lamps.add( obj.data )
            elif ( type == 'camera' ):
                self.file.cameras.add( obj.data )
        self.ExportTransform( obj )

        if self.ExportArmatures:
            armature = obj.find_armature()
            if ( armature ):
                for bone in armature.pose.bones:
                    if ( bone.parent is None ):
                        self.ExportJointObject( bone, armature )

        if ( obj.animation_data and obj.animation_data.action ):
            self.file.actions.add(( obj.animation_data.action, obj ))

        for child in obj.children:
            if child.type in self.supported_types: #and child.is_visible( scene ):
                self.ExportObject( child )

        self.file.pop()

  ##--------------------------------------------------------------------------------

  ##EXPORTING MESH HELPERS ---------------------------------------------------------
    def ExportVertices( self ):
        totno = 0
        self.binfile.writeint( len( self.binfile.mesh.vertices ))
        for vertex in self.binfile.mesh.vertices:
            totno += 1
            self.binfile.writevec( vertex.co )
        if ( self.Verbose ):
            self.report({ 'INFO' }, str( totno ) + ' positions written' )
        return totno

    def ExportNormals( self ):
        self.binfile.mesh.calc_normals_split()
        totno = 0
        self.binfile.writeint( len( self.binfile.mesh.loops ))
        for l in self.binfile.mesh.loops:
            totno += 1
            self.binfile.writevec( l.normal )
        if ( self.Verbose ):
            self.report({ 'INFO' }, str( totno ) + ' normals written' )
        return totno

    def ExportTangents( self ):
        self.binfile.mesh.calc_tangents()
        totno = 0
        self.binfile.LoopVertex = {}
        self.binfile.writeint( len( self.binfile.mesh.loops ))
        for l in self.binfile.mesh.loops:
            totno += 1
            self.binfile.writevec( l.tangent )
        if ( self.Verbose ):
            self.report({ 'INFO' }, str( totno ) + ' tangents written' )

        return totno

    def ExportCotangents( self ):
        totno = 0
        self.binfile.writeint( len( self.binfile.mesh.loops ))
        for l in self.binfile.mesh.loops:
          totno += 1
          self.binfile.writevec( l.bitangent )
        if ( self.Verbose ):
            self.report({ 'INFO' }, str( totno ) + ' cotangents written' )

        return totno

    def ExportUVs( self ):
        totno = 0
        el = self.file.docstack[ -1 ]
        idx = 0
        for uv in self.binfile.mesh.uv_layers:
          el.attrib[ 'TexCoords' + str( idx )] = self.binfile.getfileandpos()
          self.binfile.writeint( len( uv.data ))
          for uvloop in uv.data:
            totno += 1
            self.binfile.writevec([ uvloop.uv[ 0 ], 1 - uvloop.uv[ 1 ]])
          idx += 1

        if ( self.Verbose ):
            self.report({ 'INFO' }, str( totno ) + ' texcoords written' )

        if ( totno == 0 ):
            return 0, 0
        return len( self.binfile.mesh.uv_layers ), int( totno / len( self.binfile.mesh.uv_layers ))

    def ExportLoops( self ):
        totno = 0
        self.binfile.writeint( len( self.binfile.mesh.loops ))

        for loop in self.binfile.mesh.loops:
          totno += 1
          self.binfile.writeint( loop.vertex_index )
        if ( self.Verbose ):
            self.report({ 'INFO' }, str( totno ) + ' loops written' )

        return totno

    def ExportEdges( self ):
        totno = 0
        self.binfile.writeint( len( self.binfile.mesh.loops ) * 2 )

        for loop in self.binfile.mesh.loops:
          totno += 1
          self.binfile.writeint( self.binfile.mesh.edges[ loop.edge_index ].vertices[ 0 ])
          self.binfile.writeint( self.binfile.mesh.edges[ loop.edge_index ].vertices[ 1 ])
        if ( self.Verbose ):
            self.report({ 'INFO' }, str( totno ) + ' edges written' )

        return totno

    def ExportFaces( self ):
        cur_matidx = 0
        materials = {}
        materials[ 0 ] = {}
        materials[ 0 ][ 'start' ] = 0

        totno = 0
        mesh = self.binfile.mesh
        self.binfile.writeint( len( mesh.polygons ))
        for polygon in mesh.polygons:
            if not ( polygon.material_index == cur_matidx ):
                materials[ cur_matidx ][ 'end' ] = polygon.index - 1
                cur_matidx = polygon.material_index
                materials[ cur_matidx ] = { 'start' : polygon.index }

            bin = struct.pack( 'i', polygon.loop_start )
            self.binfile.file.write( bin )
            totno += 1

        if ( self.Verbose ):
            self.report({ 'INFO' }, str( totno ) + ' faces written' )

        materials[ cur_matidx ][ 'end' ] = polygon.index
        return materials

    def ExportIndices( self ):
        totno = 0
        self.binfile.writeint( len( self.binfile.mesh.polygons ))
        for polygon in self.binfile.mesh.polygons:
            self.binfile.writeint( polygon.loop_start )
            self.binfile.writeint( polygon.loop_total )
            totno += 1

        if ( self.Verbose ):
            self.report({ 'INFO' }, str( totno ) + ' faces written' )

        return totno

    def ExportVertexGroups( self ):
        armature = self.binfile.obj.find_armature()
        if ( armature is None ):
            baseindex = 0
            arm_bones = None
        else:
            arm_bones = self.file.armatures[ armature ]['bones']
            baseindex = len( armature.data.bones )

        grps = []
        for vgroup in self.binfile.obj.vertex_groups:
            n = baseindex + vgroup.index
            if ( not ( arm_bones is None )):
                try:
                    n = arm_bones.index( vgroup.name )
                except ValueError:
                    pass
            grps.append({ 'index': n, 'name': legal_name( vgroup.name )})

        totno = 0 #for debugging only
        totno_idx = 0

        if ( len( grps ) > 0 ):
            el = self.file.docstack[ -1 ]
            el.attrib[ 'VertexWeights' ] = self.binfile.getfileandpos()
            bin = b''
            self.binfile.writeint( len( self.binfile.mesh.vertices ))

            for vertex in self.binfile.mesh.vertices: #make dictionary of all groups
                vgrps = {}
                for vgroup in vertex.groups:
                    vgrps[ vgroup.group ] = vgroup.weight
                from operator import itemgetter
                srt = sorted( vgrps.items(), key=itemgetter( 1 ), reverse=True )

                vec = Vector(( 0, 0, 0, 0 )) # make sure the length of the vecs is always 4
                idx = [ 0, 0, 0, 0 ] # fill with zero indices, weight will be zero if idx not used
                for i in range( 0, min( 4, len( vgrps ))):
                    vec[ i ] = srt[ i ][ 1 ]
                    idx[ i ] = grps[ srt[ i ][ 0 ]][ 'index' ]
                lenManh = vec[ 0 ] + vec[ 1 ] + vec[ 2 ] + vec[ 3 ]
                if ( lenManh ):
                  	vec /= lenManh #scale vector by 1/manhattan distance

                if ( len( vgrps ) > 4 ):
                    self.report({ 'INFO' }, 'vertexweights: [{:2f},{:2f},{:2f},{:2f}]'.format( *vec )+ ' indices: [{:d},{:d},{:d},{:d}]'.format( *idx ))
                self.binfile.writevec(( vec[ 0 ], vec[ 1 ], vec[ 2 ])) # we only need the first 3 weights as the last can be calculated as 1-other weights
                totno += 1

                bin += struct.pack( '4i', *idx ) #write indices separately from weights
                totno_idx += 1
                #file.writeintvec( idx )
            el.attrib[ 'VertexWeightIndices' ] = self.binfile.getfileandpos()
            self.binfile.writeint( totno )
            self.binfile.file.write( bin )
        if ( self.Verbose ):
            self.report({ 'INFO' }, str( totno ) + ' vertex weights written' )
            self.report({ 'INFO' }, str( totno_idx ) + ' vertex weight indices written' )

        return grps # no need to return the number of vertices again, instead we return the name of the groups

  ##--------------------------------------------------------------------------------

  ##EXPORTING MESH -----------------------------------------------------------------

    def ExportMesh( self, obj ):
        #armature = None
        if self.ApplyModifiers:
          #if self.ExportArmatures:
          #  armature = obj.find_armature()
          #  if ( not ( armature is None )):
          #    armature.data.pose_position = 'REST'
          #    armature.update_tag()
          #    scene = bpy.context.scene
          #    scene.frame_set(scene.frame_current)
          mesh = obj.to_mesh( bpy.context.scene, True, 'PREVIEW', True )
        else:
          mesh = obj.to_mesh( bpy.context.scene, False, 'PREVIEW', True )
        #if ( not ( armature is None )):
        #  armature.data.pose_position = pose_position
        if ( self.Verbose ):
            self.report({ 'INFO' }, 'Exporting data ' + obj.data.name + ' for object ' + obj.name )

        el = self.file.push( 'mesh' )
        el.attrib[ 'Name' ] = 'mesh_' + legal_name( obj.data.name )

        path = self.file.store( mesh, 'mesh_' + legal_name( obj.data.name ))[1]

        if ( self.binfile is None ):
          self.binfile = P3DBinaryFile( os.path.splitext( self.file.fname )[ 0 ] + '.p3dbin' )
          #file = P3DBinaryFile( os.path.splitext( self.file.fname )[ 0 ] + '.mesh_' + obj.data.name + '.p3dmesh' ) #filename without extension as base
        self.binfile.mesh = mesh
        self.binfile.obj = obj

        if self.ExportArmatures:
            armature = obj.find_armature()
            if ( not ( armature is None )):
                mod_el = self.file.push( 'modifier' )
                mod_el.attrib[ 'Type' ] = 'TP3DMeshModifierArmature'
                mod_el.attrib[ 'Armature' ] = '/armature_' + legal_name( armature.data.name )
                self.file.armatures[ armature ] = { 'bones' : [ legal_name( bone.name ) for bone in armature.data.bones ]} #use armature object instead of data
                self.file.pop()

        el.attrib[ 'Positions' ] = self.binfile.getfileandpos()
        self.ExportVertices()

        grps = self.ExportVertexGroups() #xml elements will be added inside the function
        for grp in grps:
            elgrp = self.file.push( 'weightgroup' )
            elgrp.attrib[ 'Name' ] = grp['name']
            elgrp.attrib[ 'Index' ] = grp['index']
            self.file.pop()

        #el.attrib['vertexgroups'] = '\'' + '\', \''.join( grps ) + '\''

        el.attrib[ 'Normals' ] = self.binfile.getfileandpos()
        self.ExportNormals()
        el.attrib[ 'Loops' ] = self.binfile.getfileandpos()
        self.ExportLoops()

        nl, n = self.ExportUVs()
        if ( nl > 0 ):
            el.attrib[ 'Tangents' ] = self.binfile.getfileandpos()
            self.ExportTangents()
            el.attrib[ 'Cotangents' ] = self.binfile.getfileandpos()
            self.ExportCotangents()

        el.attrib[ 'Faces' ]= self.binfile.getfileandpos()
        materials = self.ExportFaces()

        for matidx in materials:
            mat = mesh.materials[ matidx ]
            el = self.file.push( 'materialgroup' )
            mat_offset = materials[ matidx ]
            el.attrib[ 'Material' ] = '/material_' + mat.name
            el.attrib[ 'PolyStart' ] = str( mat_offset[ 'start' ])
            el.attrib[ 'PolyEnd' ] = str( mat_offset[ 'end' ])

            self.file.materials.add( mat )
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

        #el.attrib[ 'Name' ] = map.name

        path = self.file.store( map, legal_name( map.name ))[1]

        # write relative image path
        #filepath = bpy_extras.io_utils.path_reference(filepath, , Config.FilePath,
        #                                  'AUTO', '', copy_set, face_img.library)

        #el.attrib[ 'file' ] = self.ExportPath( filepath )
        el.attrib[ 'Data' ] = '/tex_' + legal_name( map.texture.name )
        self.file.textures.add( map )

        if ( map.use_map_color_diffuse ):
            el.attrib[ 'DiffuseFactor' ] = str( map.diffuse_color_factor )
        else:
            el.attrib[ 'DiffuseFactor' ] = '0.0'

        if ( map.use_map_diffuse ):
            el.attrib[ 'DiffuseIntensity' ] = str( map.diffuse_factor ) # Not actually used at the moment

        if ( map.use_map_normal ):
            el.attrib[ 'NormalFactor' ] = str( map.normal_factor )

        if ( map.use_map_color_spec ):
            el.attrib[ 'SpecularFactor' ] = str( map.specular_color_factor )

        if ( map.use_map_specular ):
            el.attrib[ 'SpecularIntensity' ] = str( map.specular_factor )

        if ( map.use_map_alpha ):
            el.attrib[ 'AlphaFactor' ] = str( map.alpha_factor )

        if ( not(( map.offset == Vector(( 0.0, 0.0, 0.0 )) and ( map.scale == Vector(( 1.0, 1.0, 1.0 )))))):
            transform = self.file.push( 'transform' )
            transform.attrib[ 'Position' ] = '{:9f},{:9f},{:9f}'.format( *map.offset )
            transform.attrib[ 'Quaternion' ] = '{:9f},{:9f},{:9f},{:9f}'.format( 0.0, 0.0, 0.0, 1.0 )
            transform.attrib[ 'Scale' ] = '{:9f},{:9f},{:9f}'.format( *map.scale )
            self.file.pop()

        el.attrib[ 'Mode' ] = 'map' + str( map.blend_type ).capitalize()

        if ( map.uv_layer != '' ):
            el.attrib[ 'TexChannel' ] = 0 #self.file.mesh.uv_layers.find( map.uv_layer )

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

        el.attrib[ 'Name' ] = 'tex_' + legal_name( map.texture.name )

        path = self.file.store( map.texture, 'tex_' + legal_name( map.texture.name ))[1]

        if ( map.texture.use_interpolation ):
          el.attrib[ 'Filtering' ] = 'tfLinear'
          el.attrib[ 'FilteringMipMap' ] = 'tfLinear'
        else:
          el.attrib[ 'Filtering' ] = 'tfNearest'
          el.attrib[ 'FilteringMipMap' ] = 'tfNearest'
        if ( map.texture.use_mipmap ):
          el.attrib[ 'MipMap' ] = '1'


        if ( self.SaveTextures ):
            f = os.path.splitext( os.path.split( filepath )[ 1 ])[ 0 ] + '.png'

            map.texture.image.save_render( os.path.split( self.file.fname )[ 0 ] + '/' + f )
        else:
            f = self.ExportPath( filepath )
        el.attrib[ 'File' ] = f

        self.file.pop()

  ##--------------------------------------------------------------------------------


  ##EXPORTING MATERIAL -------------------------------------------------------------

    def ExportMaterial( self, material ):
        if ( self.Verbose ):
            self.report({ 'INFO' }, 'Exporting data ' + material.name )

        el = self.file.push( 'materialbase' )
        el.attrib[ 'Name' ] = 'material_' + material.name

        path = self.file.store( material, 'material_' + material.name )[1]

        el.attrib['Diff'] = '{:6f}, {:6f}, {:6f}'.format(*( material.diffuse_color * material.diffuse_intensity ))
        el.attrib['Spec'] = '{:6f}, {:6f}, {:6f}'.format( *( material.specular_color * material.specular_intensity ))
        el.attrib['Spec_Hardness'] = '{:6f}'.format( material.specular_hardness )  # Specular

        if ( material.use_shadeless ):
            el.attrib[ 'Unlit' ] = '1'
        else:
            el.attrib[ 'Unlit' ] = '0'
        el.attrib[ 'Instancing' ] = '1'

        if ( material.use_transparency ):
          if ( material.transparency_method == 'Mask' ):
            el.attrib[ 'AlphaThreshhold' ] = "0.9"
            el.attrib[ 'TransparencyMode' ] = "p3dmtStep"
          else:
            el.attrib[ 'TransparencyMode' ] = "p3dmMultiply"
        else:
          el.attrib[ 'TransparencyMode' ] = "p3dmtNone"

        texlist = (map for map in material.texture_slots if ( not ( map is None )) and map.use and ( map.texture.type == 'IMAGE' ) and ( not ( map.texture.image is None )))

        for map in texlist:
            self.ExportMap( map )

        if ( material.animation_data and material.animation_data.action ):
            self.file.actions.add(( material.animation_data.action, material ))

        self.file.pop()

  ##--------------------------------------------------------------------------------

  ##EXPORTING LAMP ---------------------------------------------------------------

    def ExportLamp( self, lamp ):
        if ( self.Verbose ):
            self.report({ 'INFO' }, 'Exporting data ' + lamp.name )

        el = self.file.push( 'light' )
        el.attrib[ 'Name' ] = 'lamp_' + legal_name( lamp.name )
        path = self.file.store( lamp, 'lamp_' + legal_name( lamp.name ))[1]

        el.attrib[ 'LightType' ] = 'lt' + lamp.type.capitalize()
        el.attrib[ 'Color' ] = str( '{:6f}, {:6f}, {:6f}'.format( *lamp.color ))
        el.attrib[ 'Energy' ] = str( lamp.energy )

        if ( lamp.animation_data and lamp.animation_data.action ):
            self.file.actions.add(( lamp.animation_data.action, lamp ))

        self.file.pop()

  ##--------------------------------------------------------------------------------

  ##EXPORTING CAMERA ---------------------------------------------------------------

    def ExportCamera( self, cam ):
        if ( self.Verbose ):
            self.report({ 'INFO' }, 'Exporting data ' + cam.name )

        el = self.file.push( 'camera' )
        el.attrib[ 'Name' ] = 'camera_' + legal_name( cam.name )
        path = self.file.store( cam, 'camera_' + legal_name( cam.name ))[1]

        el.attrib[ 'Near' ] = str( cam.clip_start )
        el.attrib[ 'Far' ] = str( cam.clip_end )
        el.attrib[ 'Fov' ] = str( cam.angle )

        if ( cam.animation_data and cam.animation_data.action ):
            self.file.actions.add(( cam.animation_data.action, cam ))

        self.file.pop()
  ##--------------------------------------------------------------------------------

  ##EXPORTING JOINT ----------------------------------------------------------------

    def ExportJoint( self, bone, arm, children = False ):
        if ( self.Verbose ):
            self.report({ 'INFO' }, 'Exporting data ' + bone.name )

        el = self.file.push( 'joint' )
        el.attrib[ 'Name' ] = 'joint_' + legal_name( bone.name )
        path = self.file.store( bone, 'joint_' + legal_name( bone.name ))[1]

        bone_space = Matrix(((1,0,0,0),(0,0,1,0),(0,-1,0,0),(0,0,0,1)))
        #bone_matrix = bone.matrix_local
        #if ( bone.parent ):
        #    bone_matrix = bone.parent.matrix_local.inverted() * bone_matrix
        #else:
        #    bone_matrix = bone_space * bone_matrix
        #bone_matrix = arm.matrix_world.inverted() * bone_matrix

        #self.BoneMatrix *= BlenderObject.matrix_world
        #if bone.parent is None:
        #  position = bone.head_local
        #else:
        #  position = bone.head_local - bone.parent.head_local

        #bone_matrix *= bone_space
        #position, quat, scale = bone_matrix.decompose()
        bone_matrix = arm.convert_space(bone, bone.matrix_basis, 'POSE', 'WORLD')
        tail = bone.bone.tail_local
        #quat = Vector(( 1.0, 0.0, 0.0, 0.0 ))
        #quat = bone_matrix.to_quaternion()
        position, quat, scale = bone_matrix.decompose()
        #quat = bone.rotation_quaternion
        position = bone.bone.head_local
        self.report({ 'INFO' }, 'quaternion = ' + str( quat ))
        el.attrib['Position'] = '{:9f},{:9f},{:9f}'.format( position[ 0 ], position[ 1 ], position[ 2 ])
        el.attrib['Tail'] = '{:9f},{:9f},{:9f}'.format( tail[ 0 ], tail[ 1 ], tail[ 2 ])
        length = ( bone.bone.head_local - bone.bone.tail_local ).length
        el.attrib['Length'] = '{:9f}'.format( length )
        #quat = ( bone_matrix ).to_quaternion()
        el.attrib['Quaternion'] = '{:9f},{:9f},{:9f},{:9f}'.format( quat[ 1 ], quat[ 2 ], quat[ 3 ], quat[ 0 ])

        if children:
            bones = bone.children
            for bone in bones:
                self.ExportJoint( bone, arm )

        self.file.pop()
        return el

    def get_pose_bone_matrix( self, arm, pose_bone ):

        #import mathutils
        #bone_space = mathutils.Matrix(((1,0,0,0),(0,0,1,0),(0,-1,0,0),(0,0,0,1)))
        #mat = arm.convert_space(pose_bone, pose_bone.matrix_basis, 'POSE', 'WORLD') * bone_space
        #return mat
        armature_bone = pose_bone.bone
        if pose_bone.parent is None:
          pose_bone_matrix = arm.matrix_world * armature_bone.matrix_local
        else:
          parent_bone = armature_bone.parent
          parent_matrix = arm.matrix_world * parent_bone.matrix_local
          pose_bone_matrix = arm.matrix_world * armature_bone.matrix_local
          pose_bone_matrix = parent_matrix.inverted() * pose_bone_matrix

        #pose_bone_matrix = pose_bone.matrix
        #if ( pose_bone.parent ):
        #    parent_matrix = pose_bone.parent.matrix
        #    pose_bone_matrix = parent_matrix.inverted() * pose_bone_matrix
        return pose_bone_matrix

  ##EXPORTING ARMATURE -------------------------------------------------------------

  ##EXPORTING JOINT ----------------------------------------------------------------

    def get_pose_matrix_in_other_space( self, mat, pose_bone ):
        """ Returns the transform matrix relative to pose_bone's current
            transform space.  In other words, presuming that mat is in
            armature space, slapping the returned matrix onto pose_bone
            should give it the armature-space transforms of mat.
            TODO: try to handle cases with axis-scaled parents better.
        """
        rest = pose_bone.bone.matrix_local.copy()
        rest_inv = rest.inverted()
        if pose_bone.parent:
            par_mat = pose_bone.parent.matrix.copy()
            par_inv = par_mat.inverted()
            par_rest = pose_bone.parent.bone.matrix_local.copy()
        else:
            par_mat = Matrix()
            par_inv = Matrix()
            par_rest = Matrix()

        # Get matrix in bone's current transform space
        smat = rest_inv * (par_rest * (par_inv * mat))

        # Compensate for non-local location
        #if not pose_bone.bone.use_local_location:
        #    loc = smat.to_translation() * (par_rest.inverted() * rest).to_quaternion()
        #    smat.translation = loc

        return smat


    def get_local_pose_matrix( self, pose_bone ):
        """ Returns the local transform matrix of the given pose bone.
        """
        return self.get_pose_matrix_in_other_space( pose_bone.matrix, pose_bone )

    def ExportPoseJoint( self, armature, pose_bone, bonedict ):
        if ( self.Verbose ):
            self.report({ 'INFO' }, 'Exporting data ' + pose_bone.name )

        el = self.file.push( 'pose' )
        el.attrib[ 'Name' ] = 'joint_' + legal_name( pose_bone.name )

        #bone_space = Matrix(((1,0,0,0),(0,0,1,0),(0,-1,0,0),(0,0,0,1)))
        #matrix = self.get_pose_bone_matrix( armature, pose_bone ) * bone_space
        #position, quat, scale = matrix.decompose()
        #matrix = self.get_local_pose_matrix( pose_bone )
        #matrix = pose_bone.matrix_basis
        position = pose_bone.head
        #quat = matrix.to_quaternion()
        #rot3 = rotation.to_3d()
        #rotation.xyz = rot3 * bone.matrix_local.inverted()
        #rotation.xyz = armature_matrix * rotation.xyzdef bn_mat( bn ):
        #if ( pose_bone.parent ):
        #  m = pose_bone.parent.matrix.inverted()
        #else:
        #  m = Matrix()
        #m = m*pose_bone.matrix

        #eul = Euler(mat_euler( m )[::-1]) #reverse order
        #quat = eul.to_quaternion()
        #quat = blbn_2_p3dbn( pose_bone )

        parent = pose_bone.parent
        rot = bonedict[ pose_bone ]
        while ( parent ):
          rot = bonedict[ parent ] * rot
          parent = parent.parent
        quat = rot

        #quat = pose_bone.rotation_quaternion #quat.normalized()
        #if ( pose_bone.parent ):
        #  quat = pose_bone.parent.rotation_quaternion * quat
        el.attrib['Position'] = '{:9f},{:9f},{:9f}'.format( position[ 0 ], position[ 1 ], position[ 2 ])
#        el.attrib['Quaternion'] = '{:9f},{:9f},{:9f},{:9f}'.format( quat[ 1 ], quat[ 2 ], quat[ 3 ], quat[ 0 ])
        el.attrib['Quaternion'] = '{:9f},{:9f},{:9f},{:9f}'.format( quat.x, quat.y, quat.z, quat.w )

        self.file.pop()
  ##--------------------------------------------------------------------------------

    def ExportArmature( self, armature ):
        if ( self.Verbose ):
            self.report({ 'INFO' }, 'Exporting data ' + armature.data.name )

        el = self.file.push( 'armature' )
        el.attrib[ 'Name' ] = 'armature_' + legal_name( armature.data.name )
        path = self.file.store( armature, 'armature_' + legal_name( armature.data.name ))[1]

        bones = armature.pose.bones
        for bone in bones:
            #if bone.parent is None:
            self.ExportJoint( bone, armature, False )

        self.file.pop()

  ##--------------------------------------------------------------------------------

  ##EXPORTING ACTION ---------------------------------------------------------------

    def ExportAction( self, action ):
        def fcurve_propname( data_path ):
            return data_path.rsplit( '.', 1 )

        convertprop = { 'diffuse_color': 'Diff',
                        'specular_color': 'Spec',
                        'specular_hardness': 'Specular_Hardness',
                        'location': 'Position',
                        'rotation_quaternion': 'Quaternion',
                        'rotation_euler': 'Rotation',
                        'scale': 'Scale',
                        'color': 'Color',
                        'energy': 'Energy',
                        'offset': 'Position',
                        'texture_slots': 'Maps',
                        'pose.bones': 'Owner.Objects' }

        convertquatnormal  = { 0: 3,
                               1: 0,
                               2: 1,
                               3: 2 }
        convertquatjoint   = { 0: 3,
                               1: 0,
                               2: 2,
                               3: 1 }
        converteulernormal = { 0: 0,
                               1: 1,
                               2: 2 }
        converteulerjoint  = { 0: 0,
                               1: 2,
                               2: 1 }

        import re
        if ( self.Verbose ):
            self.report({ 'INFO' }, 'Exporting data ' + action[ 0 ].name )

        #mat_2_euler = lambda mat : [ x for x in mat.to_euler('XYZ')]
        #blbn_2_p3dbn = lambda bn : Euler( mat_2_euler( bn.matrix_basis )[::-1]).to_quaternion()

        name = self.file.storednames.get( action[ 0 ])
        path = self.file.datapaths.get( name )
        deg2rad = 0.017453292;
        self.report({ 'INFO' }, 'Action path = ' + str( path ))
        if ( path is None ):
            el = self.file.push( 'action' )
            el.attrib[ 'Name' ] = 'action_' + legal_name( action[ 0 ].name )
            path = self.file.store( action[ 0 ], 'action_' + legal_name( action[ 0 ].name ))[ 1 ]

            for curve in action[ 0 ].fcurves:
                convertquat = convertquatnormal
                converteuler = converteulernormal
                elchannel = self.file.push( 'channel' )
                prop = curve.data_path
                pattern = re.compile(r'\b(' + '|'.join(convertprop.keys()) + r')\b')

                if ( 'pose.bones' in prop ):
                    convertquat = convertquatjoint
                    converteuler = converteulerjoint

                prop = pattern.sub(lambda x: convertprop[x.group()], prop)
                '''prop[ -1 ] = convertprop[ prop[ -1 ]]
                if ( len( prop ) > 1 ):
                    if ( "\"" in prop[ 0 ]):
                        prop[ 0 ] = prop[ 0 ].split( '"' )[ 1 ]
                        subel = self.file.elements[ prop[ 0 ]]
                        subel.attrib[ 'Action' ] = path
                        convertquat = convertquatjoint

                elchannel.attrib[ 'PropStr' ] = prop[ -1 ]
                if ( prop[ -1 ] == 'Quaternion' ):
                    elchannel.attrib[ 'ArrayIndex' ] = convertquat[ curve.array_index ]
                else:
                    elchannel.attrib[ 'ArrayIndex' ] = curve.array_index'''

                if ( 'Maps' in prop ):
                    if (( 'Quaternion' in prop ) or ( 'Position' in prop )):
                      mapidx = int(( prop.partition('[')[-1]).partition(']')[0])
                      self.report({ 'INFO' }, 'MapIndex = ' + str( mapidx ) + ' textureslotsname=' + action[ 1 ].texture_slots[ mapidx ].name )
                      subel = self.file.elements[ action[ 1 ].texture_slots[ mapidx ].name ]
                      subel.attrib[ 'TransformDynamic' ] = "1"

                if ( 'Quaternion' in prop ):
                    arridx = convertquat[ curve.array_index ]
                elif ( 'Rotation' in prop ):
                    arridx = converteuler[ curve.array_index ]
                else:
                    arridx = curve.array_index

                elchannel.attrib[ 'PropStr' ] = legal_name_quote( prop ) + '[' + str( arridx ) + ']'
                if ( 'Quaternion' in prop ):
                    elchannel.attrib[ 'ArrayIndex' ] = convertquat[ curve.array_index ]
                else:
                    elchannel.attrib[ 'ArrayIndex' ] = converteuler[ curve.array_index ]
                elchannel.attrib[ 'InterpolationMode' ] = "imExtrapolate"
                elchannel.attrib[ 'TimeMode' ] = "tmWrapAround"

                for key in curve.keyframe_points:
                    elkey = self.file.push( 'key' )
                    elkey.attrib[ 'Time' ] = key.co[0]
                    if ( 'Rotation' in prop ):
                        #if ( arridx == 1 ):
                        #  elkey.attrib[ 'Value' ] = -key.co[1] / deg2rad
                        #else:
                        elkey.attrib[ 'Value' ] = key.co[1] / deg2rad
                    else:
                        elkey.attrib[ 'Value' ] = key.co[1]
                    self.file.pop()
                self.file.pop()
            '''
            scene = bpy.context.scene
            idx = 0
            for frame in range( scene.frame_end + 1 ):
                elframe = self.file.push( 'frame' )
                scene.frame_set( frame )
                self.report({ 'INFO' }, 'Exporting frame ' + str( frame ))
                bonedict = {}
                for pose_bone in action[ 1 ].pose.bones:
                  bonedict[ pose_bone ] = blbn_2_p3dbn( pose_bone )

                for pose_bone in action[ 1 ].pose.bones:
                    #bone = action[ 1 ].data.bones[ pose_bone.name ]
                    self.ExportPoseJoint( action[ 1 ], pose_bone, bonedict )
                idx += 1
                self.file.pop()
            '''
            self.file.pop()
        el = self.file.elements[ self.file.storednames.get( action[ 1 ])]
        el.attrib[ 'Action' ] = path

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
