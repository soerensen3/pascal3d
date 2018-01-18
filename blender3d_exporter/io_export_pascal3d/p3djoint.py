from . import p3ddatablock, p3dexporthelper
import bpy

class P3DJoint( p3ddatablock.P3DDataBlock ):

    def __init__( self, block, root = None, path='', obj = None ):
        self.Name = block.name
        super().__init__( block, root, p3dexporthelper.indexedprop.format( 'Joints', self.Name ), obj )
        self.ClassName = 'TP3DJoint'
        '''
        bone_space_inv = Matrix(((1,0,0,0),(0,0,1,0),(0,-1,0,0),(0,0,0,1))).inverted()

        matrix = bone_space_inv * block.matrix_local #obj.convert_space(block, block.matrix_basis, 'POSE', 'LOCAL')
        self.Tail = list( block.bone.tail_local )
        self.Quaternion = p3dexporthelper.swap_quat( list( matrix.to_quaternion()))
        #if ( block.bone.parent ):
        #    self.Position = list( block.bone.head_local - block.bone.parent.head_local )
        #else:
        self.Position = list( block.bone.head_local )
        '''
        self.Quaternion = p3dexporthelper.swap_quat( p3dexporthelper.bone_quat( block ))
        self.Position = list( block.head )

        self.Tail = list( block.tail )
        #self.Length = ( block.bone.head_local - block.bone.tail_local ).length
    @staticmethod
    def find_storage( root ):
        return root.Joints

p3dexporthelper.dict_export_class[ bpy.types.PoseBone ] = P3DJoint
