# -*- coding: utf-8 -*-
"""
Created on Thu Sep 11 23:44:04 2014

@author: johannes
"""

import bpy
from . p3d_helper import *
from mathutils import *
from . p3d_export_anim import *

def ExportBonesActions( Config, Bones ):
        for Bone in Bones:
            BoneMatrix = Matrix()

            Config.File.write("{}bone \'{}\'\n".format("  " * Config.Whitespace, Bone.name ))
            Config.Whitespace += 1        
            
            #if Config.ExportRestBone:
            #Rotation = ArmatureObject.data.bones[Bone.name] \
            #    .matrix.to_quaternion() * \
             #   Bone.rotation_quaternion
            
            PoseMatrix = Matrix()
            if Bone.parent:
                PoseMatrix = Bone.parent.matrix.inverted()
            PoseMatrix *= Bone.matrix
                      
            WriteMatrix( Config, PoseMatrix )

            ExportBonesActions( Config, Bone.children )     
             
            Config.Whitespace -= 1        
            Config.File.write("{}end;\n".format("  " * Config.Whitespace ))

def ExportActions(Config, Armature):
    if ( Armature.animation_data == None ) or ( Armature.animation_data.action == None ):
        return
    
    AnimationData = Armature.animation_data.action
    scene = bpy.context.scene
    
    Config.File.write("{}action \'{}\'\n".format("  " * Config.Whitespace, AnimationData.name ))
    Config.Whitespace += 1   

    Bones = Armature.data.bones
    BlenderCurrentFrame = scene.frame_current
    
    for frame in range(scene.frame_start, scene.frame_end + 1):
        scene.frame_set( frame )

        Config.File.write("{}frame {}\n".format("  " * Config.Whitespace, frame ))
        Config.Whitespace += 1        
        RootBones = [Bone for Bone in Bones if Bone.parent is None]     
        ExportBonesActions( Config, RootBones )
        Config.Whitespace -= 1        
        Config.File.write("{}end;\n".format("  " * Config.Whitespace ))        
        
    scene.frame_set( BlenderCurrentFrame )    
    
    Config.Whitespace -= 1        
    Config.File.write("{}end;\n".format("  " * Config.Whitespace ))    

def ExportArmature(Config, Object):
        print("Exporting Armature")
        
        Armature = Object.data
        RootBones = [Bone for Bone in Armature.bones if Bone.parent is None]
        Config.File.write("{}bones\n".format("  " * Config.Whitespace ))
        Config.Whitespace += 1        
        ExportBones( Config, RootBones )
        Config.Whitespace -= 1        
        Config.File.write("{}end;\n".format("  " * Config.Whitespace ))
        if Config.ExportAnimation:
                Config.File.write("{}actions\n".format("  " * Config.Whitespace ))
                Config.Whitespace += 1
                ExportActions( Config, Object )
                Config.Whitespace -= 1        
                Config.File.write("{}end;\n".format("  " * Config.Whitespace ))
              
        
def ExportBones( Config, Bones ):
        for Bone in Bones:
            BoneMatrix = Matrix()

            Config.File.write("{}bone \'{}\'\n".format("  " * Config.Whitespace, Bone.name ))
            Config.Whitespace += 1        
            
            #if Config.ExportRestBone:
            if Bone.parent:
                BoneMatrix = Bone.parent.matrix_local.inverted()
            BoneMatrix *= Bone.matrix_local
            WriteMatrix( Config, BoneMatrix )

            ExportBones( Config, Bone.children )     
             
            Config.Whitespace -= 1        
            Config.File.write("{}end;\n".format("  " * Config.Whitespace ))
                
'''            else:
                PoseBone = self.BlenderObject.pose.bones[Bone.name]
                if Bone.parent:
                    BoneMatrix = PoseBone.parent.matrix.inverted()
                BoneMatrix *= PoseBone.matrix
'''
            
              
            
#            BoneSafeName = self.SafeName + "_" + \
#                Util.SafeName(Bone.name)
#            self.__OpenBoneFrame(BoneSafeName, BoneMatrix)
            
#            self.__WriteBoneChildren(Bone)
            
#            self.__CloseBoneFrame(BoneSafeName)    