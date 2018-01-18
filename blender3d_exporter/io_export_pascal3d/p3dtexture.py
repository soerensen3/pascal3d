from . import p3ddatablock, p3dexporthelper
import bpy, os

class P3DTexture( p3ddatablock.P3DDataBlock ):

    def __init__( self, block, root = None, path='', obj = None ):
        self.Name = block.name
        super().__init__( block, root, p3dexporthelper.indexedprop.format( 'Textures', self.Name ))
        self.ClassName = 'TP3DTexture'
        filepath = block.image.filepath
        if not filepath:  # may be '' for generated images
            return
        if ( block.use_interpolation ):
            self.Filtering = 'tfLinear'
            self.FilteringMipMap = 'tfLinear'
        else:
           self.Filtering = 'tfNearest'
           self.FilteringMipMap = 'tfNearest'
        if ( block.use_mipmap ):
          self.MipMap = 1


        if ( root.Exporter.SaveTextures ):
            f = os.path.splitext( os.path.split( filepath )[ 1 ])[ 0 ] + '.png'
            block.image.save_render( os.path.split( root.Exporter.filepath )[ 0 ] + '/' + f )
        else:
            f = self.ExportPath( filepath )
        self.File = f

    @staticmethod
    def find_storage( root ):
        return root.Textures

p3dexporthelper.dict_export_class[ bpy.types.ImageTexture ] = P3DTexture
