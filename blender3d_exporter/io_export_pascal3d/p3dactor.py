from . import p3ddatablock, p3dexporthelper
import bpy

class P3DObject( p3ddatablock.P3DDataBlock ):
    def __init__( self, block, root = None, path='', obj = None ):
        self.Name = block.name
        super().__init__( block, root, p3dexporthelper.indexedprop.format( 'Objects', self.Name ))

        self.ClassName = 'TP3DObject'
        self.Data = p3dexporthelper.export_data_path( block.data, root, block )
        self.Position = list( block.location )
        self.Quaternion = p3dexporthelper.swap_quat( list( block.rotation_quaternion ))
        self.Scale = list( block.scale )
        self.RotationOrder = 'ro' + block.rotation_mode
        # self.Visible = block.is_visible( scene )
        self.Children = []
        for child in block.children:
            self.Children.append( p3dexporthelper.export_data_path( child, root ))

    @staticmethod
    def find_storage( root ):
        return root.Objects

p3dexporthelper.dict_export_class[ bpy.types.Object ] = P3DObject
