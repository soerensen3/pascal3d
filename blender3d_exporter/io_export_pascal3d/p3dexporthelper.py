from . import p3ddata, p3ddatablock
from mathutils import Matrix, Vector, Quaternion

def swap_quat( quat ):
    return [ quat[ 1 ], quat[ 2 ], quat[ 3 ], quat[ 0 ]]
    #return [ quat[ 1 ], quat[ 2 ], quat[ 3 ], quat[ 0 ]]

def swap_quat_bone( quat ):
    return [ quat[ 0 ], quat[ 1 ], quat[ 2 ], quat[ 3 ]]
    #return Quaternion(( quat[ 0 ], quat[ 1 ], -quat[ 3 ], quat[ 2 ]))

# Generating a 3x3 "lookat" matrix pointing at the direction of the bone
# It would be possible to apply the roll of the bone as well but I don't
# know of any method on how to get the amount of roll.
def bone_quat( bone ):
    '''updir = Vector(( 0.0, 0.0, 1.0 ))
    forw = ( bone.tail - bone.head ).normalized()
    left = updir.cross( forw )
    up = forw.cross( left )
    m = Matrix().to_3x3()
    m.col[ 0 ] = left
    m.col[ 1 ] = up
    m.col[ 2 ] = forw
    return m.to_quaternion()'''
    return vec_roll_to_quat( bone.tail - bone.head, 0 )

def vec_roll_to_quat( vec, roll ):
    vec.normalize()
    target = Vector(( 0, 1, 0 ))
    axis = target.cross( vec )
    if ( axis.dot( axis ) > 1.0e-9 ):
        axis.normalize
        theta = target.angle( vec )
        q = Quaternion( axis, theta )
    else:
        if ( dot( axis, vec ) > 0 ):
            q = Quaternion()
        else:
            q = Quaternion(( 0, 0, 0, 1 ))
    q_roll = Quaternion( vec, roll )
    return q_roll * q

indexedprop = ".{}[\"{}\"]" #name["index"]
dict_export_class = {}

def find_export_class( obj ):
    try:
        result = dict_export_class[ type( obj )]
    except KeyError:
        result = p3ddatablock.P3DDataBlock
    return result

def export_data_root( block, root, obj = None ):
    if ( block is None ):
        return None
    data_class = find_export_class( block )
    storage = data_class.find_storage( root )
    if ( storage is None ):
        root.Exporter.report({ 'WARNING' }, 'No storage found for datablock ' + repr( type( block )))
        return
    if ( block in storage ):
        return storage[ block ]
    if ( data_class ):
        data = data_class( block, root, '', obj )
        storage[ block ] = data
        return data
    else:
        return None

def export_data( block ):
    root = p3ddata.P3DData()
    export_data_root( block, root )
    return root

def export_data_path( block, root, obj = None ):
    data = export_data_root( block, root, obj )
    if ( data ):
        return data.DataPath
    else:
        return None

def get_bone_local_transform( bone ):
    if bone.parent:
        location = bone.parent.matrix.inverted() * bone.head
    else:
        location = bone.head
    #if ( bone.bone.parent ):
    #    location = bone.bone.parent.matrix_local.inverted() * bone.bone.head_local
    #    #self.location = bone.bone.head_local - bone.bone.parent.head_local
    #    #location = bone_quat( bone.parent ).inverted() * ( loc )
    #else:
    #    location = bone.bone.head_local

    #if bone.parent: # We want to get the rotation relative to the parent bone or to the armature in case there is no parent
    #    quat = bone.rotation_quaternion #bone.parent.matrix.quaternion.inverted() * bone.quaternion#( bone_quat( bone.parent ).inverted() * bone_quat( bone ))
    #else:
    #    quat = bone.matrix.to_quaternion()#bone_quat( bone )
    quat = bone.rotation_quaternion
    return quat, location
