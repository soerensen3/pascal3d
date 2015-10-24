# -*- coding: utf-8 -*-
"""
Created on Thu Sep 11 15:17:10 2014

@author: johannes
"""

import os

from xml.etree import cElementTree as et

def LegalName(Name):
    return Name

def ExportPath( Config, Path ):
  if ( Config.PathMode == 1 ):
    return os.path.relpath( Path, os.path.dirname( Config.FilePath ))
  elif ( Config.PathMode == 2 ):
    return os.path.basename( Path )
  else:
    return os.path.abspath( Path )

    
def WriteMatrix( Config, Matrix ):
    matrix = et.Element("matrix")
    Config.DocStack[ -1 ].append( matrix )
    
    matrix.attrib['c0'] = "{:9f},{:9f},{:9f},{:9f}".format(Matrix[0][0], Matrix[1][0], Matrix[2][0], Matrix[3][0])
    matrix.attrib['c1'] = "{:9f},{:9f},{:9f},{:9f}".format(Matrix[0][1], Matrix[1][1], Matrix[2][1], Matrix[3][1]) 
    matrix.attrib['c2'] = "{:9f},{:9f},{:9f},{:9f}".format(Matrix[0][2], Matrix[1][2], Matrix[2][2], Matrix[3][2])  
    matrix.attrib['c3'] = "{:9f},{:9f},{:9f},{:9f}".format(Matrix[0][3], Matrix[1][3], Matrix[2][3], Matrix[3][3])
    
## HELPER
globalNormals = {}
globalUVs = {}
globalUVLayerNames = {}
globalTangents = {}
globalCotangents = {}
globalMaterials = set([])
globalMeshes = set([])
globalLamps = set([])
globalCameras = set([])
globalLoopVertex = {}

def veckey3d(v):
    return round(v.x, 6), round(v.y, 6), round(v.z, 6)

def veckey2d(v):
    return round(v[0], 6), round(v[1], 6)

import struct

def WriteVecToFile( Config, Vec ):
    bin = struct.pack( "f" * len( Vec ), *Vec)
    Config.objfile.write( bin )
