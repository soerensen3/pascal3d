import bpy
import struct
import os
from math import radians
from mathutils import *


def LegalName(Name):

    def ReplaceSet(String, OldSet, NewChar):
        for OldChar in OldSet:
            String = String.replace(OldChar, NewChar)
        return String

    import string

    NewName = ReplaceSet(Name, string.punctuation + " ", "_")
    if NewName[0].isdigit() or NewName in ["ARRAY",
                                           "DWORD",
                                           "UCHAR",
                                           "BINARY",
                                           "FLOAT",
                                           "ULONGLONG",
                                           "BINARY_RESOURCE",
                                           "SDWORD",
                                           "UNICODE",
                                           "CHAR",
                                           "STRING",
                                           "WORD",
                                           "CSTRING",
                                           "SWORD",
                                           "DOUBLE",
                                           "TEMPLATE"]:
        NewName = "_" + NewName
    return NewName

export_type = {
    "MESH"
}

def ExportRevelation(Config):
    Config.Whitespace = 0
    print("----------\nExporting to {}".format(Config.FilePath))
    if Config.Verbose:
        print("Opening File...")
    Config.File = open(Config.FilePath, "w")
    if Config.Verbose:
        print("Done")

    if Config.Verbose:
        print("Generating Object list for export... (Root parents only)")

    if Config.Verbose:
        for Object in Config.context.scene.objects:
            print(Object)
            print(Object.type)
            print(Object.parent)
    if Config.ExportMode == 1:
        Config.ExportList = [Object for Object in Config.context.scene.objects
                             if Object.type in export_type
                             and Object.parent is None
                             and not Object.hide]
    
    Config.File.write("{}world\n".format("  " * Config.Whitespace))
    Config.Whitespace += 1

    WriteWorld(Config)

    Config.Whitespace -= 1
    Config.File.write("{}end;\n".format("  " * Config.Whitespace))
    
    
    ExportObjects(Config, Config.ExportList)
    ExportMaterials(Config)
    CloseFile(Config)
    print("Finished")

def GetObjectChildren(Parent):
    return [Object for Object in Parent.children
            if Object.type in export_type]


def ExportObjects(Config, ObjectList):
    for Object in ObjectList:
        Config.File.write("{}object {}\n".format("  " * Config.Whitespace, LegalName(Object.name)))
        Config.Whitespace += 1

        if Object.type == 'MESH':
            ExportMesh(Config, Object)

        if len( GetObjectChildren(Object)):
            Config.File.write("{}children\n".format("  " * Config.Whitespace))
            Config.Whitespace += 1

            ExportObjects(Config,GetObjectChildren(Object))

            Config.Whitespace -= 1
            Config.File.write("{}end;\n".format("  " * Config.Whitespace))

        Config.Whitespace -= 1
        Config.File.write("{}end;\n".format("  " * Config.Whitespace))

def WriteLocalMatrix(Config, Object):
    LocalMatrix = Object.matrix_local

    Config.File.write("{}matrix\n".format("  " * Config.Whitespace))
    Config.Whitespace += 1
    Config.File.write("{}{:9f},{:9f},{:9f},{:9f},\n".format("  " * Config.Whitespace, LocalMatrix[0][0], LocalMatrix[1][0], LocalMatrix[2][0], LocalMatrix[3][0]))
    Config.File.write("{}{:9f},{:9f},{:9f},{:9f},\n".format("  " * Config.Whitespace, LocalMatrix[0][1], LocalMatrix[1][1], LocalMatrix[2][1], LocalMatrix[3][1]))
    Config.File.write("{}{:9f},{:9f},{:9f},{:9f},\n".format("  " * Config.Whitespace, LocalMatrix[0][2], LocalMatrix[1][2], LocalMatrix[2][2], LocalMatrix[3][2]))
    Config.File.write("{}{:9f},{:9f},{:9f},{:9f};\n".format("  " * Config.Whitespace, LocalMatrix[0][3], LocalMatrix[1][3], LocalMatrix[2][3], LocalMatrix[3][3]))
    Config.Whitespace -= 1
    Config.File.write("{}end;\n".format("  " * Config.Whitespace))        

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
       
        Config.File.write("{}external \'{}\'\n".format("  " * Config.Whitespace, fn))
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

## HELPER
globalNormals = {}
globalUVs = {}

def veckey3d(v):
    return round(v.x, 6), round(v.y, 6), round(v.z, 6)

def veckey2d(v):
    return round(v[0], 6), round(v[1], 6)

def WriteVecToFile( Config, Vec ):
    if Config.ExportBinaryData:
        bin = struct.pack( "f" * len( Vec ), *Vec)
        Config.objfile.write( bin )
    else:
      Config.File.write("  " * Config.Whitespace + (", {:9f}" * len( Vec )).format( *Vec )[2:] + ";\n") 

def WriteFaceToFile( Config, idx1, idx2, idx3 ):
    if not Config.ExportBinaryData:
        return "{}/{}/{}, ".format( idx1, idx2, idx3 )
##      Config.File.write("  " * Config.Whitespace + (", {:9f}" * len( Vec )).format( *Vec )[2:] + ";\n") 

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

def WriteMeshUVs(Config, Mesh):
    global globalUVs
    globalUVs = {}
    totno = 0
    for uv in Mesh.uv_layers:
        for uvloop in uv.data:
            noKey = veckey2d(uvloop.uv)
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
                uv_idx = globalUVs[ veckey2d( uv.data[ v_idx + Polygon.loop_start ].uv )]
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

## MATERIALS

def WriteWorld(Config):
    world = bpy.data.scenes[0].world
    if world:
        val=world.ambient_color
        Config.File.write("{}ambient {:6f}, {:6f}, {:6f}\n".format("  " * Config.Whitespace, *val))  # Ambient, uses mirror color,
        val=world.horizon_color
        Config.File.write("{}horizon {:6f}, {:6f}, {:6f}\n".format("  " * Config.Whitespace, *val))  # Horizon
        
def WriteMeshMaterials(Config, Mesh):
    for mat in Mesh.materials:
        Config.File.write("{}material \'{}\'\n".format("  " * Config.Whitespace, mat.name ))    

import bpy_extras
def ExportMaterials(Config):
    world = bpy.data.scenes[0].world
    if world:
        world_amb = world.ambient_color
    else:
        world_amb = Color((0.0, 0.0, 0.0))    
    for mat in bpy.data.materials:
        Config.File.write("{}material \'{}\'\n".format("  " * Config.Whitespace, mat.name))
        Config.Whitespace += 1
        
        # COLORS
        amb = mat.ambient
        Config.File.write("{}ambient {:6f}, {:6f}, {:6f}\n".format("  " * Config.Whitespace, *amb))  # Ambient, uses mirror color,
        Config.File.write("{}diffuse {:6f}, {:6f}, {:6f}\n".format("  " * Config.Whitespace, *(mat.diffuse_intensity * mat.diffuse_color))) # Diffuse        
        Config.File.write("{}specular {:6f}, {:6f}, {:6f}\n".format("  " * Config.Whitespace, *(mat.specular_intensity * mat.specular_color)))  # Specular        
        
        # TEXTURES
        for tex in mat.texture_slots:
            if ( not ( tex is None )):
                if ( tex.texture.type == 'IMAGE' ):
                    if ( not ( tex.texture.image is None )):                  
                        filepath = tex.texture.image.filepath
                        if filepath:  # may be '' for generated images
                        # write relative image path
                            #filepath = bpy_extras.io_utils.path_reference(filepath, , Config.FilePath,
                            #                                  'AUTO', "", copy_set, face_img.library)                        
                            Config.File.write("{}texture \'{}\'\n".format("  " * Config.Whitespace, filepath ))
        
        Config.Whitespace -= 1
        Config.File.write("{}end;\n".format("  " * Config.Whitespace))

## END

def CloseFile(Config):
    if Config.Verbose:
        print("Closing File...")
    Config.File.close()
    if Config.Verbose:
        print("Done")
