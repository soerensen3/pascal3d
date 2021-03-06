from . import p3ddatablock, p3dexporthelper
import bpy

class P3DScene( p3ddatablock.P3DDataBlock ):
    def __init__( self, block, root = None, path='', obj = None ):
        root.ActiveScene = block
        root.ActiveSceneObj = self
        self.Name = block.name
        super().__init__( block, root, p3dexporthelper.indexedprop.format( 'Scenes', self.Name ))
        self.ClassName = 'TP3DScene'
        self.Cam = p3dexporthelper.export_data_path( block.camera, root, block )

        self.Objects = []
        for child in block.objects:
            if ( child.parent is None ):
                if ( not ( root.Exporter.ExportVisibleOnly and ( not child.is_visible( block )))):
                    self.Objects.append( p3dexporthelper.export_data_path( child, root, block ))

    @staticmethod
    def find_storage( root ):
        return root.Scenes

p3dexporthelper.dict_export_class[ bpy.types.Scene ] = P3DScene
