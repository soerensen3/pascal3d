import bpy
from . rev_helper import *

#import bpy_extras
def ExportMaterials(Config):   
    materials = et.Element("materials")
    Config.DocStack[ -1 ].append( materials )
    Config.DocStack.append( materials )        
    for mat in bpy.data.materials:    
        matEl= et.Element("material")
        Config.DocStack[ -1 ].append( matEl )
        matEl.attrib['name'] = mat.name   
        
        # COLORS
#        amb = mat.ambient
#        Config.File.write("{}ambient {:6f}, {:6f}, {:6f}\n".format("  " * Config.Whitespace, *amb))  # Ambient, uses mirror color,
        matEl.attrib['diffuse'] = "{:6f}, {:6f}, {:6f}".format(*( mat.diffuse_color * mat.diffuse_intensity )) # Diffuse        
        matEl.attrib['specular'] = "{:6f}, {:6f}, {:6f}, {:6f}".format( mat.specular_hardness, *( mat.specular_color * mat.specular_intensity ))  # Specular        
        
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
                            texEl= et.Element("texture")
                            Config.DocStack[ -1 ].append( texEl )
                            texEl.attrib['file'] = ExportPath( Config, filepath )  
                            
                            if ( tex.use_map_color_diffuse ):
                                texEl.attrib['diffuse'] = str( tex.diffuse_color_factor )

                            if ( tex.use_map_normal ):
                                texEl.attrib['normal'] = str( tex.normal_factor )
                            
        
    Config.DocStack.pop()