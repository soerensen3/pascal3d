#import bpy
from . p3d_helper import *

#import bpy_extras
def ExportMaterials(Config):        
    global globalMaterials
    for mat in globalMaterials: #bpy.data.materials:    
        matEl= et.Element("material")
        Config.DocStack[ -1 ].append( matEl )
        matEl.attrib['name'] = 'material_' + LegalName( mat.name )
        
        # COLORS
#        amb = mat.ambient
#        Config.File.write("{}ambient {:6f}, {:6f}, {:6f}\n".format("  " * Config.Whitespace, *amb))  # Ambient, uses mirror color,
        matEl.attrib['diffuse'] = "{:6f}, {:6f}, {:6f}".format(*( mat.diffuse_color * mat.diffuse_intensity )) # Diffuse        
        matEl.attrib['specular'] = "{:6f}, {:6f}, {:6f}, {:6f}".format( mat.specular_hardness, *( mat.specular_color * mat.specular_intensity ))  # Specular        
        
        texlist = (tex for tex in mat.texture_slots if ( not ( tex is None )) and tex.use and ( tex.texture.type == 'IMAGE' ) and ( not ( tex.texture.image is None )))
        # TEXTURES
        for tex in texlist:
            filepath = tex.texture.image.filepath
            if filepath:  # may be '' for generated images
            # write relative image path
                #filepath = bpy_extras.io_utils.path_reference(filepath, , Config.FilePath,
                #                                  'AUTO', "", copy_set, face_img.library)        
                texEl= et.Element("texture")
                matEl.append( texEl )
                texEl.attrib['file'] = ExportPath( Config, filepath )  
                
                if ( tex.use_map_color_diffuse ):
                    texEl.attrib['diffuse'] = str( tex.diffuse_color_factor )
                    
                if ( tex.use_map_diffuse ):
                    texEl.attrib['diffuse_intensity'] = str( tex.diffuse_factor )
                    
                if ( tex.use_map_normal ):
                    texEl.attrib['normal'] = str( tex.normal_factor )      
                    
                if ( tex.use_map_color_spec ):
                    texEl.attrib['specular'] = str( tex.specular_color_factor )
                    
                if ( tex.use_map_specular ):
                    texEl.attrib['specular_intensity'] = str( tex.specular_factor )   
                    
                if ( tex.use_map_alpha ):
                    texEl.attrib['alpha'] = str( tex.alpha_factor )      
                    
                texEl.attrib['mode'] = str( tex.blend_type.lower() )
                
                global globalUVLayerNames
                if ( tex.uv_layer != '' ):
                    texEl.attrib['layer'] = str( globalUVLayerNames[ mat.name, tex.uv_layer ])   
