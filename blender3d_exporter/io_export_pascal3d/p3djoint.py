from . import p3ddatablock, p3dexporthelper
import bpy

class P3DJoint( p3ddatablock.P3DDataBlock ):

    def __init__( self, block, root = None, path='', obj = None ):
        super().__init__( block, root, p3dexporthelper.indexedprop.format( 'Joints', block.name ))

        matrix = obj.convert_space(block, block.matrix_basis, 'POSE', 'WORLD')
        self.Tail = list( block.bone.tail_local )
        self.Quaternion = list( matrix.to_quaternion())
        self.Position = list( block.bone.head_local )
        self.Length = ( block.bone.head_local - block.bone.tail_local ).length
    @staticmethod
    def find_storage( root ):
        return root.Joints

p3dexporthelper.dict_export_class[ bpy.types.PoseBone ] = P3DJoint
