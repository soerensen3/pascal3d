# -*- coding: utf-8 -*-
"""
Created on Thu Sep 11 23:01:52 2014

@author: johannes
"""
import bpy
from . rev_helper import *

def ExportMesh(Config, Object):
    print("Exporting Mesh")

    if Config.ApplyModifiers:
        if Config.ExportArmatures:
            #Create a copy of the object and remove all armature modifiers so an unshaped
            #mesh can be created from it.
            Object2 = Object.copy()
            for Modifier in [Modifier for Modifier in Object2.modifiers if Modifier.type == "ARMATURE"]:
                Object2.modifiers.remove(Modifier)
            Mesh = Object2.to_mesh(bpy.context.scene, True, "PREVIEW")
        else:
            Mesh = Object.to_mesh(bpy.context.scene, True, "PREVIEW")
    else:
        Mesh = Object.to_mesh(bpy.context.scene, False, "PREVIEW")

    WriteLocalMatrix(Config,Object);

    ## BINARY EXPORT
    
    if ( Config.ExportBinaryData ):
        fn = Config.FilePath + Object.name + ".revobj"
       
        Config.File.write("{}external \'{}\'\n".format("  " * Config.Whitespace, ExportPath( Config, fn )))
        Config.Whitespace += 1
        Config.objfile = open(fn, "wb")

        n = WriteMeshVertices(Config, Mesh)
        Config.File.write("{}vertices {}\n".format("  " * Config.Whitespace, n))

        n = WriteMeshNormals(Config, Mesh)
        Config.File.write("{}normals {}\n".format("  " * Config.Whitespace, n))

        n = WriteMeshUVs(Config, Mesh)
        Config.File.write("{}texcoords {}\n".format("  " * Config.Whitespace, n))


        n = WriteMeshFaces(Config, Mesh)
        Config.File.write("{}faces {}\n".format("  " * Config.Whitespace, n))

        Config.Whitespace -= 1
        Config.objfile.close()
        Config.File.write("{}end;\n".format("  " * Config.Whitespace))
    else: ## ASCII EXPORT
        Config.File.write("{}vertices\n".format("  " * Config.Whitespace))
        Config.Whitespace += 1

        WriteMeshVertices(Config, Mesh)

        Config.Whitespace -= 1
        Config.File.write("{}end;\n".format("  " * Config.Whitespace))
    
        Config.File.write("{}normals\n".format("  " * Config.Whitespace))
        Config.Whitespace += 1

        WriteMeshNormals(Config, Mesh)

        Config.Whitespace -= 1
        Config.File.write("{}end;\n".format("  " * Config.Whitespace))  


        Config.File.write("{}texcoords\n".format("  " * Config.Whitespace))
        Config.Whitespace += 1

        WriteMeshUVs(Config, Mesh)

        Config.Whitespace -= 1
        Config.File.write("{}end;\n".format("  " * Config.Whitespace))


        Config.File.write("{}faces\n".format("  " * Config.Whitespace))
        Config.Whitespace += 1

        WriteMeshFaces(Config, Mesh)

        Config.Whitespace -= 1
        Config.File.write("{}end;\n".format("  " * Config.Whitespace))
        
    Config.File.write("{}materials\n".format("  " * Config.Whitespace))
    Config.Whitespace += 1

    WriteMeshMaterials(Config, Mesh)

    Config.Whitespace -= 1
    Config.File.write("{}end;\n".format("  " * Config.Whitespace))


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

def WriteMeshFaces(Config, Mesh):
    #total_v_idx = 0
    for Polygon in Mesh.polygons:    
        s = "{}".format("  " * Config.Whitespace)
        v_idx = 0
                
        if Config.ExportBinaryData:
            bin = struct.pack( "2i", len( Polygon.vertices ), len( Mesh.uv_layers ))
            Config.objfile.write( bin )
       
    
        for Vertex in Polygon.vertices:
            uv_s = ""   
            bin_a = []                 
            for uv in Mesh.uv_layers:          
                uv_idx = globalUVs[ veckey2d( TransformUV( uv.data[ v_idx + Polygon.loop_start ].uv ))]
                if Config.ExportBinaryData:
                    bin_a.append( struct.pack( "i", uv_idx ))
                else:
                    uv_s += str( uv_idx ) + "|"

            if Polygon.use_smooth:
                normal = globalNormals[veckey3d(Mesh.vertices[Vertex].normal)]
            else:
                normal = globalNormals[veckey3d(Polygon.normal)]
            
            if Config.ExportBinaryData:
                bin = struct.pack( "i", Vertex )
                Config.objfile.write( bin )                                                  
                bin = struct.pack( "i", normal )
                Config.objfile.write( bin )
                for bin_v in bin_a:
                    Config.objfile.write( bin_v )            
            else:
                uv_s =  uv_s[:-1]
                s += WriteFaceToFile( Config, Vertex, normal, uv_s)   
  
            v_idx += 1
#        total_v_idx += v_idx
        if not Config.ExportBinaryData:
            Config.File.write(s[:-2] + "\n")
    return len( Mesh.polygons )
    
def WriteMeshMaterials(Config, Mesh):
    for mat in Mesh.materials:
        Config.File.write("{}material \'{}\'\n".format("  " * Config.Whitespace, mat.name ))    
    