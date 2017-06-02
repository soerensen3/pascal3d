from . import p3dexporthelper

class P3DDataBlock( object ):
    def __init__( self, block, root = None, path='', obj = None ):
        super().__init__()
        self.Name = block.name
        self.DataPath = path

        if hasattr( block, 'animation_data'):
            if ( block.animation_data and block.animation_data.action ):
                self.Action = p3dexporthelper.export_data_path( block.animation_data.action, root, block )

    @staticmethod
    def find_storage( root ):
        return None

    def toDict( self ):
        return self.__dict__

    def __str__( self ):
        return str( self.__dict__ )

    def __repr__( self ):
        return str( self.__dict__ )
