import json
import os

from . import p3dfiles

class P3DDataBlockList ( dict ):
    # def __str__( self ):
    #    return json.dumps( list( self.values()), default=lambda o: o.__dict__ )
    def __repr__( self ):
        return json.dumps( list( self.values()), default=lambda o: o.__dict__, indent=4 )

class P3DData( object ):
    def __init__( self, filename ):
        self.Armatures = P3DDataBlockList()
        self.Actions = P3DDataBlockList()
        self.Cameras = P3DDataBlockList()
        self.Joints = P3DDataBlockList()
        self.Lights = P3DDataBlockList()
        self.Materials = P3DDataBlockList()
        self.Meshes = P3DDataBlockList()
        self.Objects = P3DDataBlockList()
        self.Scenes = P3DDataBlockList()
        self.Textures = P3DDataBlockList()
        self.ExportDict = self.__dict__.copy()
        self.FileName = filename
        self.BinFile = None
        self.Exporter = None
        self.ActiveScene = None
        self.ActiveSceneObj = None
        self.ActiveObj = None
        self.ActiveObjP3D = None
        self.FirstActionObj = None

    def createBinFile( self ):
        if ( self.BinFile is None ):
            self.BinFile = p3dfiles.P3DBinaryFile( filename=os.path.splitext( self.FileName )[ 0 ] + '.p3dbin' )

    def __repr__( self ):
        repr( self.__dict__ )

    def toDict( self ):
        return {
            str( key ): list( self.ExportDict[ key ].values()) for key in self.ExportDict.keys()
          }

    def toJSONFile( self ):
        file = open( self.FileName, 'w' )
        json.dump( self.toDict(), file, default=lambda o: o.__dict__, indent=4 )
        if ( self.BinFile ):
            del self.BinFile
        file.close()
