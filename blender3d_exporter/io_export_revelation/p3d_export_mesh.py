# -*- coding: utf-8 -*-
"""
Created on Thu Sep 11 23:01:52 2014

@author: johannes
"""
import bpy
import bmesh
import os
from . p3d_helper import *

from xml.etree import cElementTree as et

def ExportMesh(Config, Object):
    print("Exporting Mesh: " + Object.name )
    

    if Config.ApplyModifiers:
        if Config.ExportArmatures:
            #Create a copy of the object and remove all armature modifiers so an unshaped
            #mesh can be created from it.
            Object2 = Object.copy()
            for Modifier in [Modifier for Modifier in Object2.modifiers if Modifier.type == "ARMATURE"]:
                Object2.modifiers.remove(Modifier)
            Mesh = Object2.to_mesh(bpy.context.scene, True, "PREVIEW", True )
        else:
            Mesh = Object.to_mesh(bpy.context.scene, True, "PREVIEW", True )
    else:
        Mesh = Object.to_mesh(bpy.context.scene, False, "PREVIEW", True )
        
    #Mesh.quadToTriangle( 0 )

    meshEl = Config.DocStack[ -1 ]
    
    WriteMatrix(Config,Object.matrix_local);
    
    #bmesh.ops.triangulate(Mesh, faces=Mesh.polygons)


    ## BINARY EXPORT


    fn = os.path.splitext( Config.FilePath )[ 0 ] + "." + Object.name + ".p3dmesh" #filename without extension as base
   
    meshEl.attrib['binary'] = ExportPath( Config, fn )
    Config.objfile = open(fn, "wb")
    n = WriteMeshVertices(Config, Mesh)
    meshEl.attrib['vertices'] = str( n )

    n = WriteMeshNormals(Config, Mesh)
    meshEl.attrib['normals'] = str( n )
    
    n = WriteMeshTangents(Config, Mesh)
    meshEl.attrib['tangents'] = str( n )        
    
    n = WriteMeshCotangents(Config, Mesh)
    meshEl.attrib['cotangents'] = str( n )          

    n = WriteMeshUVs(Config, Mesh)
    meshEl.attrib['texcoords'] = str( n )

    n, Materials = WriteMeshFaces(Config, Mesh)
    meshEl.attrib['faces'] = str( n )

    Config.objfile.close()


    WriteMeshMaterials(Config, Mesh, Materials)

    

## VERTICES

def WriteMeshVertices(Config, Mesh):
    vcount = 0
    for Vertex in Mesh.vertices:
        Position = Vertex.co
        vcount += 1
        WriteVecToFile( Config, Position )        
    return vcount

## NORMALS

def WriteMeshNormals(Config, Mesh):
    global globalNormals
   
    Mesh.calc_normals()
    totno = 0
    globalNormals = {}
    for f in Mesh.polygons:
        if f.use_smooth:
            for v_idx in f.vertices:
                v = Mesh.vertices[ v_idx ]
                noKey = veckey3d(v.normal)
                if noKey not in globalNormals:
                    globalNormals[noKey] = totno
                    totno += 1
                    WriteVecToFile( Config, noKey )
#                    Config.File.write("  " * Config.Whitespace + '%9f, %.9f, %.9f;\n' % noKey)
        else:
            # Hard, 1 normal from the face.
            noKey = veckey3d(f.normal)
            if noKey not in globalNormals:
                globalNormals[noKey] = totno
                totno += 1
                WriteVecToFile( Config, noKey )
    return totno
    
## Tangents

def WriteMeshTangents(Config, Mesh):
    global globalTangents
    global globalLoopVertex
   
    Mesh.calc_tangents()
    tottan = 0
    globalTangents = {}
    for l in Mesh.loops:
        globalLoopVertex[l.vertex_index] = l.index
        noKey = veckey3d(l.tangent)
        if noKey not in globalTangents:
            globalTangents[noKey] = tottan
            tottan += 1
            WriteVecToFile( Config, noKey )
    return tottan

## Cotangents

def WriteMeshCotangents(Config, Mesh):
    global globalCotangents
    global globalLoopVertex
   
    tottan = 0
    globalCotangents = {}
    for l in Mesh.loops:
        noKey = veckey3d(l.bitangent)
        if noKey not in globalCotangents:
            globalCotangents[noKey] = tottan
            tottan += 1
            WriteVecToFile( Config, noKey )
    return tottan

## UV

def TransformUV( uv ):
	return [ uv[ 0 ], 1-uv[ 1 ]]

def WriteMeshUVs(Config, Mesh):
    global globalUVs
    globalUVs = {}
    totno = 0
    for uv in Mesh.uv_layers:
        for uvloop in uv.data:
            noKey = veckey2d( TransformUV( uvloop.uv ))
            if noKey not in globalUVs:
               globalUVs[noKey] = totno
               totno += 1  
               WriteVecToFile( Config, noKey )
    return totno

    
## FACES
    
## FACE
    #int: How many Vertices, 
    #int: How many uv_layers
    #for count Vertices
      #int idxvertex
      #int idxnormal
      #for count uv_layers
        #int idxuv

def WriteMeshFaces(Config, Mesh):
    global globalNormals;
    global globalUVs;
    global globalTangents;
    global globalCotangents;
    global globalMaterials;
    global globalLoopVertex;
    cur_matidx = 0;
    materials = {}
    materials[ 0 ] = {}
    materials[ 0 ]["start"] = 0
    
    for Polygon in Mesh.polygons:    
        v_idx = 0
        
        if not ( Polygon.material_index == cur_matidx ):
            materials[ cur_matidx ]["end"] = Polygon.index - 1
            cur_matidx = Polygon.material_index
            materials[ cur_matidx ] = { "start" : Polygon.index }
     

        bin = struct.pack( "2i", int( len( Polygon.vertices )), int( len( Mesh.uv_layers )))
        Config.objfile.write( bin )
       
    
        for Vertex in Polygon.vertices:
            if Polygon.use_smooth:
                normal = globalNormals[veckey3d(Mesh.vertices[Vertex].normal)]
            else:
                normal = globalNormals[veckey3d(Polygon.normal)]
                
            tangent = globalTangents[veckey3d( Mesh.loops[ globalLoopVertex[ Vertex ]].tangent )]
            cotangent = globalCotangents[veckey3d( Mesh.loops[ globalLoopVertex[ Vertex ]].bitangent )]
            

            bin = struct.pack( "i", Vertex )
            Config.objfile.write( bin )                                                  
            bin = struct.pack( "i", normal )
            Config.objfile.write( bin )
            bin = struct.pack( "i", tangent )
            Config.objfile.write( bin )  
            bin = struct.pack( "i", cotangent )
            Config.objfile.write( bin )         
            for uv in Mesh.uv_layers:          
                uv_idx = globalUVs[ veckey2d( TransformUV( uv.data[ v_idx + Polygon.loop_start ].uv ))]
                bin = struct.pack( "i", uv_idx )
                Config.objfile.write( bin )                    
#            for bin_v in bin_a:
#                if not ( len( bin_v) == len( Polygon.vertices )):
#                    print( "Warning: Inconsistent data! Mesh:" + Mesh.name + " Poly: " + str( Polygon.index ) + " Vertex: " + str( Vertex ))
#                    print( "length should be: " + str( len( Polygon.vertices )) + " length is " + str( len( bin_v )) + ' debug info: ' + str( bin_v ))
#                Config.objfile.write( bin_v )            

  
            v_idx += 1

    materials[ cur_matidx ]["end"] = Polygon.index
    return len( Mesh.polygons ), materials
    
def WriteMeshMaterials(Config, Mesh, Materials):    
    for matIdx in Materials:
        mat = Mesh.materials[ matIdx ]
        matOffset = Materials[ matIdx ]
        matEl= et.Element("material")
        Config.DocStack[ -1 ].append( matEl )
        matEl.attrib["name"] = mat.name  
        matEl.attrib["start"] = str( matOffset["start"])
        matEl.attrib["end"] = str( matOffset["end"])
        global globalMaterials
        globalMaterials.add( mat )
        texlist = (tex for tex in mat.texture_slots if ( not ( tex is None )) and tex.use and ( tex.texture.type == 'IMAGE' ) and ( not ( tex.texture.image is None )) and ( tex.uv_layer != '' ))
        # TEXTURES
        for tex in texlist:
            global globalUVLayerNames
            globalUVLayerNames[ mat.name, tex.uv_layer ] = Mesh.uv_layers.find( tex.uv_layer )
    
