from . import p3ddatablock, p3dexporthelper, p3darmature
import bpy, re
from mathutils import Quaternion, Euler, Vector

prop_blender_to_p3d = { 'diffuse_color': 'Diff',
                        'specular_color': 'Spec',
                        'specular_hardness': 'Specular_Hardness',
                        'location': 'Transform.Position',
                        'rotation_quaternion': 'Transform.Quaternion',
                        'rotation_euler': 'Transform.Rotation',
                        'scale': 'Transform.Scale',
                        'color': 'Color',
                        'energy': 'Energy',
                        'offset': 'Position',
                        'texture_slots': 'Maps',
                        'pose.bones': 'Owner.Objects' }

quat_blender_to_p3d =         { 0: 3,
                                1: 0,
                                2: 1,
                                3: 2 }

quat_joint_blender_to_p3d =   { 0: 3,
                                1: 0,
                                2: 2,
                                3: 1 }

quat_joint_blender_to_quat =  { 0: 0,
                                1: 2,
                                2: 1,
                                3: 3 }

euler_joint_blender_to_p3d =  { 0: 0,
                                1: 2,
                                2: 1 }

class P3DAction( p3ddatablock.P3DDataBlock ):
    '''def ExportKey( self, key, is_euler, add ):
        return { "Time":    key.co[ 0 ],
                 "Value":   add + key.co[ 1 ] / [ 1, 0.017453292 ][ is_euler ]} #rad to deg conversion if euler angle'''
    def ExportKey( self, time, value ):
        return { "Time":    time,
                 "Value":   value }

    def ExportFCurve( self, root, prop, curve, obj ):
        #arr_idx = curve.array_index
        is_quaternion = "rotation_quaternion" in prop
        is_euler = "rotation_euler" in prop
        is_joint = "pose.bones" in prop
        is_location = "location" in prop
        joint_name = ""

        #add = 0.0

        if ( is_joint ):
            joint_name = prop[prop.find("pose.bones[\"")+len( "pose.bones[\"" ):prop.rfind( "\"" )] # not good but good enough
            '''
            if ( is_quaternion ):
                trans_base = p3darmature.armjointcache[ joint_name ][ "rotation_quaternion" ]
            elif ( is_euler ):
                trans_base = p3darmature.armjointcache[ joint_name ][ "rotation_euler" ]
            elif ( is_location ):
                trans_base = p3darmature.armjointcache[ joint_name ][ "location" ]
            '''


        pattern = re.compile( r'\b(' + '|'.join( prop_blender_to_p3d.keys()) + r')\b' )
        prop = pattern.sub(lambda x: prop_blender_to_p3d[x.group()], prop)

        keys = [[],[],[],[]]
        for time, values in curve.items():
            if ( is_joint ):
                '''if ( is_quaternion ):
                    q = Quaternion(( values[ 0 ], values[ 2 ], values[ 1 ], values[ 3 ]))
                    #values = list( trans_base )
                    values = list( trans_base * q )
                elif ( is_euler ):
                    q = Euler(( values[ 0 ], values[ 2 ], values[ 1 ])).to_quaternion()
                    #values = list( trans_base )
                    values = list(( trans_base.to_quaternion() * q ).to_euler())
                elif ( is_location ):
                    #values = list( trans_base )
                    values = list( trans_base + p3darmature.armjointcache[ joint_name ][ Vector( values[ 0:3 ]))'''
                root.ActiveScene.frame_set( time )
                bone = obj.pose.bones[ joint_name ]
                q,t = p3dexporthelper.get_bone_local_transform( bone )
                if ( is_quaternion ):
                    values = list( q )
                elif ( is_euler ):
                    values = list( q.to_euler())
                elif ( is_location ):
                    values = list( t )

            if ( is_quaternion ):
                values = [ values[ 1 ], values[ 2 ], values[ 3 ], values[ 0 ]]
            elif ( is_euler ):
                values = list( map( lambda x: x / 0.017453292 if ( isinstance( x, ( int, float ))) else x, values ))
            #curve[ time ] = values
            if ( is_joint ):
                print( values )
            for i, value in enumerate( values ):
                if ( not ( value == 'x' )):
                    keys[ i ].append( self.ExportKey( time, value ))

        result = []
        for arr_idx, curve_key in enumerate( keys ):
            if ( len( curve_key )):
                result.append(
                    { "PropStr" :                    prop + '[' + str( arr_idx ) + ']',
                      "InterpolationMode":           "imExtrapolate",
                      "TimeMode" :                   "tmWrapAround",
                      "Keys" :                       curve_key })
        return result

    def ExportRawData( self, block ):
        channels = {}
        for curve in block.fcurves:
            prop = curve.data_path
            arr_idx = curve.array_index
            if ( prop in channels ):
                keys = channels[ prop ]
            else:
                keys = {}
            for key in curve.keyframe_points:
                if ( key.co[ 0 ] in keys ):
                    vec = keys[ key.co[ 0 ]]
                else:
                    vec = [ "x", "x", "x", "x" ]
                vec[ arr_idx ] = key.co[ 1 ]
                keys[ key.co[ 0 ]] = vec
            channels[ prop ] = keys
        return channels

    def __init__( self, block, root = None, path='', obj = None ):
        self.Name = block.name
        super().__init__( block, root, p3dexporthelper.indexedprop.format( 'Actions', self.Name ))
        self.ClassName = 'TP3DAction'
        self.Channels = []
        channels = self.ExportRawData( block )

        if hasattr( obj.data, 'pose_position' ):
            pose = obj.data.pose_position
            obj.data.pose_position = 'POSE'
            root.ActiveScene.update()

        for prop, curve in channels.items():
            self.Channels = self.Channels + self.ExportFCurve( root, prop, curve, obj )
        if hasattr( obj.data, 'pose_position' ):
            obj.data.pose_position = pose
            root.ActiveScene.update()



    @staticmethod
    def find_storage( root ):
        return root.Actions

p3dexporthelper.dict_export_class[ bpy.types.Action ] = P3DAction
