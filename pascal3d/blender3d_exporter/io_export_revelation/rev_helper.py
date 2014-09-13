# -*- coding: utf-8 -*-
"""
Created on Thu Sep 11 15:17:10 2014

@author: johannes
"""

import os

def LegalName(Name):

    def ReplaceSet(String, OldSet, NewChar):
        for OldChar in OldSet:
            String = String.replace(OldChar, NewChar)
        return String

    import string

    NewName = ReplaceSet(Name, string.punctuation + " ", "_")
    if NewName[0].isdigit() or NewName in ["BEGIN",
                                           "END",
                                           "MATRIX",
                                           "MATERIAL",
                                           "MATERIALS",
                                           "EXTERNAL"]:
        NewName = "_" + NewName
    return NewName

def ExportPath( Config, Path ):
  if ( Config.PathMode == 1 ):
    return os.path.relpath( Path, os.path.dirname( Config.FilePath ))
  elif ( Config.PathMode == 2 ):
    return os.path.basename( Path )
  else:
    return os.path.abspath( Path )

def WriteLocalMatrix( Config, Object ):
    
    LocalMatrix = Object.matrix_local
    WriteMatrix( Config, LocalMatrix )
    
def WriteMatrix( Config, Matrix ):

    Config.File.write("{}matrix\n".format("  " * Config.Whitespace))
    Config.Whitespace += 1
    Config.File.write("{}{:9f},{:9f},{:9f},{:9f},\n".format("  " * Config.Whitespace, Matrix[0][0], Matrix[1][0], Matrix[2][0], Matrix[3][0]))
    Config.File.write("{}{:9f},{:9f},{:9f},{:9f},\n".format("  " * Config.Whitespace, Matrix[0][1], Matrix[1][1], Matrix[2][1], Matrix[3][1]))
    Config.File.write("{}{:9f},{:9f},{:9f},{:9f},\n".format("  " * Config.Whitespace, Matrix[0][2], Matrix[1][2], Matrix[2][2], Matrix[3][2]))
    Config.File.write("{}{:9f},{:9f},{:9f},{:9f};\n".format("  " * Config.Whitespace, Matrix[0][3], Matrix[1][3], Matrix[2][3], Matrix[3][3]))
    Config.Whitespace -= 1
    Config.File.write("{}end;\n".format("  " * Config.Whitespace))   
    
## HELPER
globalNormals = {}
globalUVs = {}

def veckey3d(v):
    return round(v.x, 6), round(v.y, 6), round(v.z, 6)

def veckey2d(v):
    return round(v[0], 6), round(v[1], 6)

import struct

def WriteVecToFile( Config, Vec ):
    if Config.ExportBinaryData:
        bin = struct.pack( "f" * len( Vec ), *Vec)
        Config.objfile.write( bin )
    else:
      Config.File.write("  " * Config.Whitespace + (", {:9f}" * len( Vec )).format( *Vec )[2:] + ";\n") #[2:] Strips away the first comma

def WriteFaceToFile( Config, idx1, idx2, idx3 ):
    if not Config.ExportBinaryData:
        return "{}/{}/{}, ".format( idx1, idx2, idx3 )
##      Config.File.write("  " * Config.Whitespace + (", {:9f}" * len( Vec )).format( *Vec )[2:] + ";\n") 
    