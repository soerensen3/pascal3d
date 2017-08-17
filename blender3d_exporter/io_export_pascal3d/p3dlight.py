from . import p3ddatablock, p3dexporthelper
import bpy

class P3DLight( p3ddatablock.P3DDataBlock ):
    def __init__( self, block, root = None, path='', obj = None ):
        self.Name = 'light.' + block.name
        super().__init__( block, root, p3dexporthelper.indexedprop.format( 'Lights', self.Name ), obj )
        self.ClassName = 'TP3DLight'
        self.LightType = 'lt' + block.type.capitalize()
        self.Color = list( block.color )
        self.Energy = block.energy

    @staticmethod
    def find_storage( root ):
        return root.Lights

p3dexporthelper.dict_export_class[ bpy.types.Lamp ] = P3DLight
p3dexporthelper.dict_export_class[ bpy.types.PointLamp ] = P3DLight
p3dexporthelper.dict_export_class[ bpy.types.SunLamp ] = P3DLight
p3dexporthelper.dict_export_class[ bpy.types.SpotLamp ] = P3DLight
