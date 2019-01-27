from . import p3ddatablock, p3dexporthelper
import bpy

'''class P3DMaterial( p3ddatablock.P3DDataBlock ):
    def ExportMap( self, tex_slot, root ):
        filepath = tex_slot.texture.image.filepath
        if not filepath:  # may be '' for generated images
            return

        class P3DMap( object ): pass
        mapobj = P3DMap()

        mapobj.Map = p3dexporthelper.export_data_path( tex_slot.texture, root )

        if ( tex_slot.use_map_color_diffuse ):
            mapobj.DiffuseFactor = tex_slot.diffuse_color_factor
        else:
            mapobj.DiffuseFactor = 0.0

        if ( tex_slot.use_map_diffuse ):
            mapobj.DiffuseIntensity = tex_slot.diffuse_factor # Not actually used at the moment

        if ( tex_slot.use_map_normal ):
            mapobj.NormalFactor = tex_slot.normal_factor

        #if ( tex_slot.use_map_color_spec ):
        #    mapobj.SpecularFactor = tex_slot.specular_color_factor

        if ( tex_slot.use_map_specular ):
            mapobj.SpecularFactor = tex_slot.specular_factor
        #if ( tex_slot.use_map_specular ):
        #    mapobj.SpecularIntensity = tex_slot.specular_factor

        if ( tex_slot.use_map_alpha ):
            mapobj.AlphaFactor = tex_slot.alpha_factor

        if ( not(( list( tex_slot.offset ) == [ 0.0, 0.0, 0.0 ]) and ( list( tex_slot.scale ) == [ 1.0, 1.0, 1.0 ]))):
            Position = list( tex_slot.offset )
            Quaternion = [ 0.0, 0.0, 0.0, 1.0 ]
            Scale = list( tex_slot.scale )
            mapobj.Transform = { "Position": Position, "Quaternion": Quaternion, "Scale": Scale }

        mapobj.Mode = 'map' + str( tex_slot.blend_type ).capitalize()
        mapobj.ClassName = 'TP3DMaterialMapBase'
        if ( tex_slot.uv_layer != '' ):
            mapobj.TexChannel = 0 #self.file.mesh.uv_layers.find( tex_slot.uv_layer )

        return mapobj

    def __init__( self, block, root = None, path='', obj = None ):
        self.Name = block.name
        super().__init__( block, root, p3dexporthelper.indexedprop.format( 'Materials', self.Name ))
        self.ClassName = 'TP3DMaterialBase'
        self.Diff = list( block.diffuse_color * block.diffuse_intensity )
        self.Spec = list( block.specular_color * block.specular_intensity )
        self.Spec_Hardness = block.specular_hardness

        self.Unlit = int( block.use_shadeless )
        self.Instancing = 1

        if ( block.use_transparency ):
          if ( block.transparency_method == 'Mask' ):
            self.AlphaThreshhold = 0.9
            self.TransparencyMode = "tmStep"
          else:
            self.TransparencyMode = "tmMultiply"
        else:
          self.TransparencyMode = "tmNone"

        texlist = (map for map in block.texture_slots if ( not ( map is None )) and map.use and ( map.texture.type == 'IMAGE' ) and ( not ( map.texture.image is None )))

        self.Maps = []
        for map in texlist:
            root.Exporter.report({ 'INFO' }, map.name )
            self.Maps.append( self.ExportMap( map, root ))

    @staticmethod
    def find_storage( root ):
        return root.Materials'''

class P3DMaterial( p3ddatablock.P3DDataBlock ):
    def ExportMap( self, tex_slot, root ):
        filepath = tex_slot.texture.image.filepath
        if not filepath:  # may be '' for generated images
            return

        class P3DMap( object ): pass
        mapobj = P3DMap()

        mapobj.Map = p3dexporthelper.export_data_path( tex_slot.texture, root )


        '''if ( tex_slot.use_map_color_diffuse ):
            mapobj.DiffuseFactor = tex_slot.diffuse_color_factor
        else:
            mapobj.DiffuseFactor = 0.0

        if ( tex_slot.use_map_diffuse ):
            mapobj.DiffuseIntensity = tex_slot.diffuse_factor # Not actually used at the moment

        if ( tex_slot.use_map_normal ):
            mapobj.NormalFactor = tex_slot.normal_factor

        #if ( tex_slot.use_map_color_spec ):
        #    mapobj.SpecularFactor = tex_slot.specular_color_factor

        if ( tex_slot.use_map_specular ):
            mapobj.SpecularFactor = tex_slot.specular_factor
        #if ( tex_slot.use_map_specular ):
        #    mapobj.SpecularIntensity = tex_slot.specular_factor

        if ( tex_slot.use_map_alpha ):
            mapobj.AlphaFactor = tex_slot.alpha_factor

        if ( not(( list( tex_slot.offset ) == [ 0.0, 0.0, 0.0 ]) and ( list( tex_slot.scale ) == [ 1.0, 1.0, 1.0 ]))):
            Position = list( tex_slot.offset )
            Quaternion = [ 0.0, 0.0, 0.0, 1.0 ]
            Scale = list( tex_slot.scale )
            mapobj.Transform = { "Position": Position, "Quaternion": Quaternion, "Scale": Scale }

        mapobj.Mode = 'map' + str( tex_slot.blend_type ).capitalize()
        '''
        mapobj.ClassName = 'TP3DMaterialMap'
        if ( tex_slot.uv_layer != '' ):
            mapobj.TexChannel = 0 #self.file.mesh.uv_layers.find( tex_slot.uv_layer )

        return mapobj

    def __init__( self, block, root = None, path='', obj = None ):
        self.Name = block.name
        super().__init__( block, root, p3dexporthelper.indexedprop.format( 'Materials', self.Name ))
        self.ClassName = 'TP3DMaterialPBR'
        #self.Diff = list( block.diffuse_color * block.diffuse_intensity )
        #self.Spec = list( block.specular_color * block.specular_intensity )
        #self.Spec_Hardness = block.specular_hardness

        #self.Unlit = int( block.use_shadeless )
        #self.Instancing = 1

        '''if ( block.use_transparency ):
          if ( block.transparency_method == 'Mask' ):
            self.AlphaThreshhold = 0.9
            self.TransparencyMode = "tmStep"
          else:
            self.TransparencyMode = "tmMultiply"
        else:
          self.TransparencyMode = "tmNone"
        '''
        texlist = (map for map in block.texture_slots if ( not ( map is None )) and map.use and ( map.texture.type == 'IMAGE' ) and ( not ( map.texture.image is None )))

        self.Maps = []
        for map in texlist:
            root.Exporter.report({ 'INFO' }, map.name )
            self.Maps.append( self.ExportMap( map, root ))

        self.Albedo = list( block.diffuse_color * block.diffuse_intensity )
        self.Albedo.append( 1.0 )
        self.Roughness = 1-block.specular_hardness / 512

        '''
        self.NodeTree = [{ "CloneOf" : "P3DShaderNodeLib.Nodes[ \"material_pbr\" ]",
            "Inputs" : [{
              "Name" : "WorldNormal",
              "Connected" : ".Materials[ \"" + self.Name + "\" ].GeometryNode.Outputs[6]",
              "ClassName" : "TP3DShaderNodeSocketClone"
            }],
            "ClassName" : "TP3DNodeClone" }]

        self.RootNode = {
            "CloneOf" : "P3DShaderNodeLib.Nodes[ \"output\" ]",
            "Inputs" : [{
              "Name" : "WorldNormal",
              "Connected" : ".Materials[ \"" + self.Name + "\" ].NodeTree[0].Outputs[0]",
              "ClassName" : "TP3DShaderNodeSocketClone"
            }],
            "ClassName" : "TP3DNodeClone" }
        self.GeometryNode = {
            "CloneOf" : "P3DShaderNodeLib.Nodes[ \"geometry\" ]",
            "ClassName" : "TP3DNodeClone" }
        '''

    @staticmethod
    def find_storage( root ):
        return root.Materials


p3dexporthelper.dict_export_class[ bpy.types.Material ] = P3DMaterial
