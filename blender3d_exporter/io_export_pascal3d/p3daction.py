from . import p3ddatablock, p3dexporthelper
import bpy

prop_blender_to_p3d = { 'diffuse_color': 'Diff',
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

def convert_quat_blender_to_p3d( quat_blend ):
    convertquatnormal  = { 0: 3,
                           1: 0,
                           2: 1,
                           3: 2 }



class P3DAction( p3ddatablock.P3DDataBlock ):

    '''def ExportAction( self ):
        def fcurve_propname( data_path ):
            return data_path.rsplit( '.', 1 )


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
                self.file.pop()'''

    def __init__( self, block, root = None, path='', obj = None ):
        super().__init__( block, root, p3dexporthelper.indexedprop.format( 'Actions', block.name ))

    @staticmethod
    def find_storage( root ):
        return root.Actions

p3dexporthelper.dict_export_class[ bpy.types.Action ] = P3DAction
