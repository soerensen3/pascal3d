from . import p3ddatablock, p3dexporthelper, p3dactor
from mathutils import Matrix
import bpy

armjointcache = {}

class P3DFakeBone:
    def is_visible( self, obj ):
        return True

    def find_armature( self ):
        return None

    def __init__( self, bone, armobj ):

        self.name = bone.name
        self.data = bone

        if ( bone.bone.parent ):
            #self.location = bone.bone.head_local - bone.bone.parent.head_local
            self.location = p3dexporthelper.bone_quat( bone.parent ).inverted() * ( bone.head - bone.parent.head )
        else:
            self.location = bone.head
        #self.matrix_local = ( p3dexporthelper.bone_quat( bone ) * bone.rotation_quaternion ).to_matrix()

        if bone.parent: # We want to get the rotation relative to the parent bone or to the armature in case there is no parent
            self.matrix_local = ( p3dexporthelper.bone_quat( bone.parent ).inverted() * p3dexporthelper.bone_quat( bone )).to_matrix()
        else:
            self.matrix_local = p3dexporthelper.bone_quat( bone ).to_matrix()
        global armjointcache
        armjointcache[ self.name ] = {
            "rotation_quaternion": self.matrix_local.to_quaternion(),
            "rotation_euler": self.matrix_local.to_euler(),
            "location": self.location }

        '''p3dexporthelper.swap_quat_bone( bone.rotation_quaternion ).to_matrix()'''
        self.scale = [ 1, 1, 1 ]
        self.rotation_mode = 'XYZ'
        self.children = []
        self.DataPath = 'Objects[\"' + self.name + "\"]"

class P3DArmature( p3ddatablock.P3DDataBlock ):
    def export_pose_bone( self, bone, root, armobj ):
        fakebone = P3DFakeBone( bone, armobj )
        for child in bone.children:
            fakebone.children.append( self.export_pose_bone( child, root, fakebone ))
        p3dexporthelper.export_data_path( fakebone, root )
        if ( armobj and ( bone.parent is None )):
            armobj.Children.append( fakebone.DataPath )
        #if ( root.ActiveSceneObj and ( bone.parent is None )):
        #    root.ActiveSceneObj.Objects.append( fakebone.DataPath )
        return fakebone

    def export_bone( self, bone, root ):
        rot_local, pos_local = p3dexporthelper.get_bone_local_transform( bone )
        path = self.DataPath + '.Joints[ "%s" ]'
        root.Exporter.report({ 'INFO' }, path )
        return { 'Name' : bone.name,
                 'Transform' : {
                     'Quaternion': p3dexporthelper.swap_quat( p3dexporthelper.bone_quat( bone )),
                     'Position' : list( bone.head ), },
                 'TransformLocal' : {
                     'Quaternion': p3dexporthelper.swap_quat( rot_local ),
                     'Position' : list( pos_local ), },
                 'Parent' : path % bone.parent.name if ( bone.parent ) else None,
                 'Children': [ path % child.name for child in bone.children ],
                 'Tail' : list( bone.tail - bone.head ),
                 'ClassName': 'TP3DRestJoint' }


    def __init__( self, block, root = None, path='', obj = None ):
        self.Name = block.name
        super().__init__( block, root, p3dexporthelper.indexedprop.format( 'Armatures', self.Name ), obj )
        global armjointcache
        armjointcache = {}
        pose = block.pose_position
        block.pose_position = 'REST'
        root.ActiveScene.update()
        self.ClassName = 'TP3DArmature'
        self.Joints = []
        rootbones = []
        armobj = root.ActiveObjP3D
        print( 'armobj: ', type( root.ActiveObjP3D ))
        if ( obj ):
            for bone in obj.pose.bones:
                #self.Joints.append( p3dexporthelper.export_data_path( bone, root, obj ))
                self.Joints.append( self.export_bone( bone, root ))
                if not ( bone.parent ):
                    rootbones.append( bone )
        #for bone in rootbones:
        #    self.export_pose_bone( bone, root, armobj )
        block.pose_position = pose
        root.ActiveScene.update()
        #import json
        #root.Exporter.report({ 'INFO' }, "armjointcache: " + json.dumps( armjointcache, indent=4 ))

    @staticmethod
    def find_storage( root ):
        return root.Armatures

p3dexporthelper.dict_export_class[ bpy.types.Armature ] = P3DArmature
p3dexporthelper.dict_export_class[ P3DFakeBone ] = p3dactor.P3DObject
