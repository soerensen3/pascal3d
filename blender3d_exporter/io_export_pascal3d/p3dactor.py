from . import p3ddatablock, p3dexporthelper
import bpy

class P3DObject( p3ddatablock.P3DDataBlock ):
    def ExportModifiers( self, root, obj ):
        self.Modifiers = []
        root.Exporter.report({ 'INFO' }, str( obj ))
        if root.Exporter.ExportArmatures and obj:
            armature = obj.find_armature()
            if ( armature ):
                self.Modifiers = [{ "ClassName" : "TP3DObjectModifierArmature", "Armature": "Armatures[\"" + armature.data.name + "\"]" }]


    def __init__( self, block, root = None, path='', obj = None ): # obj = scene
        self.Name = block.name
        root.ActiveObj = block
        root.ActiveObjP3D = self
        print( "root.ActiveObj", type( root.ActiveObj ))
        print( "root.ActiveObjP3D", type( root.ActiveObjP3D ))

        self.ClassName = 'TP3DObject'
        self.Transform = {
            "Position" : list( block.location ),
            "Quaternion": p3dexporthelper.swap_quat( list( block.matrix_local.to_quaternion())),
            "Scale": list( block.scale ),
            "RotationOrder": 'ro' + block.rotation_mode }
        self.Visible = int( block.is_visible( obj ))
        self.Children = []
        self.Data = p3dexporthelper.export_data_path( block.data, root, block )
        self.ExportModifiers( root, block )

        super().__init__( block, root, p3dexporthelper.indexedprop.format( 'Objects', self.Name ))

        for child in block.children:
            if ( not ( root.Exporter.ExportVisibleOnly and ( not child.is_visible( obj )))):
                self.Children.append( p3dexporthelper.export_data_path( child, root, obj ))

    @staticmethod
    def find_storage( root ):
        return root.Objects

p3dexporthelper.dict_export_class[ bpy.types.Object ] = P3DObject
