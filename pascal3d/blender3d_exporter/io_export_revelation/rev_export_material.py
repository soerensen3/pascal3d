import bpy
from . rev_helper import *

#import bpy_extras
def ExportMaterials(Config):
#    world = bpy.data.scenes[0].world
#    if world:
#        world_amb = world.ambient_color
#    else:
#        world_amb = Color((0.0, 0.0, 0.0))    
    for mat in bpy.data.materials:
        Config.File.write("{}material \'{}\'\n".format("  " * Config.Whitespace, mat.name))
        Config.Whitespace += 1
        
        # COLORS
#        amb = mat.ambient
#        Config.File.write("{}ambient {:6f}, {:6f}, {:6f}\n".format("  " * Config.Whitespace, *amb))  # Ambient, uses mirror color,
        Config.File.write("{}diffuse {:6f}, {:6f}, {:6f}\n".format("  " * Config.Whitespace, *( mat.diffuse_color * mat.diffuse_intensity ))) # Diffuse        
        Config.File.write("{}specular {:6f}, {:6f}, {:6f}, {:6f}\n".format("  " * Config.Whitespace, mat.specular_hardness, *( mat.specular_color * mat.specular_intensity )))  # Specular        
        
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
                            Config.File.write("{}texture \'{}\'\n".format("  " * Config.Whitespace, ExportPath( Config, filepath )))
                            Config.Whitespace += 1
                            
                            if ( tex.use_map_color_diffuse ):
                                Config.File.write("{}diffuse {}\n".format("  " * Config.Whitespace, tex.diffuse_color_factor ))

                            if ( tex.use_map_normal ):
                                Config.File.write("{}normal {}\n".format("  " * Config.Whitespace, tex.normal_factor ))
                            
                            Config.Whitespace -= 1
                            Config.File.write("{}end;\n".format("  " * Config.Whitespace ))
        
        Config.Whitespace -= 1
        Config.File.write("{}end;\n".format("  " * Config.Whitespace))