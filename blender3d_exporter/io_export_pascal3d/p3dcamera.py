from . import p3ddatablock, p3dexporthelper
import bpy

class P3DCamera( p3ddatablock.P3DDataBlock ):
    def __init__( self, block, root = None, path='', obj = None ):
        super().__init__( block, root, p3dexporthelper.indexedprop.format( 'Cameras', block.name ))

        self.Near = block.clip_start
        self.Far = block.clip_end
        self.Fov = block.angle

    @staticmethod
    def find_storage( root ):
        return root.Cameras

p3dexporthelper.dict_export_class[ bpy.types.Camera ] = P3DCamera
