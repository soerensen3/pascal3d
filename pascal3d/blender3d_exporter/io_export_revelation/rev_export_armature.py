# -*- coding: utf-8 -*-
"""
Created on Thu Sep 11 23:44:04 2014

@author: johannes
"""

import bpy
from . rev_helper import *
from mathutils import *

def ExportArmature(Config, Object):
        print("Exporting Armature")
        
        Armature = Object.data
        RootBones = [Bone for Bone in Armature.bones if Bone.parent is None]
        ExportBones( Config, RootBones )
              
        
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