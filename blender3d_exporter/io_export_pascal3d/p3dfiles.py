import struct, os

class P3DBinaryFile:
    LoopVertex = {}

    def __init__( self, filename ):
        self.file = open( filename, 'wb' )

    def close( self ):
        self.file.close()

    def writefloat( self, f ):
        bin = struct.pack( 'f', f )
        self.file.write( bin )

    def writevec( self, vec ):
        bin = struct.pack( 'f' * len( vec ), *vec)
        self.file.write( bin )

    def writeint( self, i ):
        bin = struct.pack( 'i', i )
        self.file.write( bin )

    def writeintvec( self, ivec ):
        bin = struct.pack( 'i' * len( ivec ), *ivec )
        self.file.write( bin )

    def getposition( self ):
        return self.file.tell()

    def getfname( self ):
        return self.file.name

    def getfileandpos( self ):
        return os.path.basename( self.file.name ) + ':' + str( self.file.tell())
