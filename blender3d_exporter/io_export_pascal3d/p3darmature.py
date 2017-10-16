from . import p3ddatablock, p3dexporthelper, p3dactor
from mathutils import Matrix
import bpy

class P3DFakeBone:
    def is_visible( self, obj ):
        return True

    def __init__( self, bone ):

        self.name = bone.name
        self.data = bone

        if ( bone.bone.parent ):
            #self.location = bone.bone.head_local - bone.bone.parent.head_local
            self.location = p3dexporthelper.bone_quat( bone.parent ).inverted() * ( bone.head - bone.parent.head )
        else:
            self.location = bone.head
        #self.matrix_local = ( p3dexporthelper.bone_quat( bone ) * bone.rotation_quaternion ).to_matrix()

        if bone.parent:
            self.matrix_local = ( p3dexporthelper.bone_quat( bone.parent ).inverted() * p3dexporthelper.bone_quat( bone )).to_matrix()
        else:
            self.matrix_local = p3dexporthelper.bone_quat( bone ).to_matrix()

        '''p3dexporthelper.swap_quat_bone( bone.rotation_quaternion ).to_matrix()'''
        self.scale = [ 1, 1, 1 ]
        self.rotation_mode = 'XYZ'
        self.children = []
        self.DataPath = 'Objects[\"' + self.name + "\"]"

class P3DArmature( p3ddatablock.P3DDataBlock ):
    def export_bone( self, bone, root, armobj ):
        fakebone = P3DFakeBone( bone )
        for child in bone.children:
            fakebone.children.append( self.export_bone( child, root, fakebone ))
        p3dexporthelper.export_data_path( fakebone, root )
        if ( armobj and ( bone.parent is None )):
            armobj.Children.append( fakebone.DataPath )
        #if ( root.ActiveSceneObj and ( bone.parent is None )):
        #    root.ActiveSceneObj.Objects.append( fakebone.DataPath )
        return fakebone



    def __init__( self, block, root = None, path='', obj = None ):
        self.Name = block.name
        super().__init__( block, root, p3dexporthelper.indexedprop.format( 'Armatures', self.Name ), obj )
        block.pose_position = 'REST'
        self.ClassName = 'TP3DArmature'
        self.Joints = []
        rootbones = []
        if ( obj ):
            for bone in obj.pose.bones:
                self.Joints.append( p3dexporthelper.export_data_path( bone, root, obj ))
                if bone.parent is None:
                    rootbones.append( bone )
        for bone in rootbones:
            self.export_bone( bone, root, root.ActiveObj )


    @staticmethod
    def find_storage( root ):
        return root.Armatures

p3dexporthelper.dict_export_class[ bpy.types.Armature ] = P3DArmature
p3dexporthelper.dict_export_class[ P3DFakeBone ] = p3dactor.P3DObject
