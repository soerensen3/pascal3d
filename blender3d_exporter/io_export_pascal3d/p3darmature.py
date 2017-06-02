from . import p3ddatablock, p3dexporthelper
import bpy

class P3DArmature( p3ddatablock.P3DDataBlock ):

    def __init__( self, block, root = None, path='', obj = None ):
        super().__init__( block, root, p3dexporthelper.indexedprop.format( 'Armatures', block.name ))
        self.Joints = []
        if ( obj ):
            for bone in obj.pose.bones:
                self.Joints.append( p3dexporthelper.export_data_root( bone, root, obj ))

    @staticmethod
    def find_storage( root ):
        return root.Armatures

p3dexporthelper.dict_export_class[ bpy.types.Armature ] = P3DArmature
