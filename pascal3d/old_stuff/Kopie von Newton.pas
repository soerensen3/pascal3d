{
********************************************************************************
*                                                                              *
* Newton Game Dynamics Header Translation (Delphi/FreePascal)                  *
*  Newton Version 3.03 (2012/12/15)                                            *
*  Header Version 1.02                                                         *
*                                                                              *
*  Translation started by Stuart "Stucuk" Carey on 2012/08/09                  *
*                                                                              *
*  Contributors:                                                               *
*                                                                              *
*                                                                              *
********************************************************************************
*                                                                              *
* Copyright (c) <2003-2012> <Julio Jerez, Newton Game Dynamics>                *
*                                                                              *
* This software is provided 'as-is', without any express or implied            *
* warranty. In no event will the authors be held liable for any damages        *
* arising from the use of this software.                                       *
*                                                                              *
* Permission is granted to anyone to use this software for any purpose,        *
* including commercial applications, and to alter it and redistribute it       *
* freely, subject to the following restrictions:                               *
*                                                                              *
* 1. The origin of this software must not be misrepresented; you must not      *
* claim that you wrote the original software. If you use this software         *
* in a product, an acknowledgment in the product documentation would be        *
* appreciated but is not required.                                             *
*                                                                              *
* 2. Altered source versions must be plainly marked as such, and must not be   *
* misrepresented as being the original software.                               *
*                                                                              *
* 3. This notice may not be removed or altered from any source distribution.   *
*                                                                              *
********************************************************************************
}

unit Newton;

{== Local Defines ==
  __NONDELPHI__            - Enables compatiblity with compilers other than Delphi.
                             Note that pascaldefines.inc and newtonpascal.inc
                             are protected under the Mozilla Public License.

 == Conditionals ==
 Declare the Following in Projects->Options->Conditionals to enable them:
  __USE_DOUBLE_PRECISION__ - Toggles Double Precision. DLL used must match this setting.

 == Local Defines ==}
 //{$DEFINE __NONDELPHI__}


{$linklib stdc++}
{$linklib Newton}

{$IFDEF __NONDELPHI__}
 {$I pascaldefines.inc}
{$ENDIF}

interface

{$IFDEF __NONDELPHI__}
 {$I newtonpascal.inc}
{$ELSE}
 const
 {$IFDEF LINUX}
  NEWTON_API = 'libNewton.so';
 {$ELSE IF WINDOWS}
   {$IFDEF __USE_DOUBLE_PRECISION__}
    NEWTON_API = 'newtond.dll';
   {$ELSE}
    NEWTON_API = 'newton.dll';
   {$ENDIF}
 {$ENDIF}
{$ENDIF}

 NEWTON_MAJOR_VERSION                            = 3;
 NEWTON_MINOR_VERSION                            = 03;

 NEWTON_PROFILER_WORLD_UPDATE                    = 0;

 NEWTON_PROFILER_COLLISION_UPDATE                = 1;
 NEWTON_PROFILER_COLLISION_UPDATE_BROAD_PHASE    = 2;
 NEWTON_PROFILER_COLLISION_UPDATE_NARROW_PHASE   = 3;

 NEWTON_PROFILER_DYNAMICS_UPDATE                 = 4;
 NEWTON_PROFILER_DYNAMICS_CONSTRAINT_GRAPH       = 5;
 NEWTON_PROFILER_DYNAMICS_SOLVE_CONSTRAINT_GRAPH = 6;

 NEWTON_PROFILER_FORCE_CALLBACK_UPDATE           = 7;
 NEWTON_PRE_LISTERNER_CALLBACK_UPDATE            = 8;
 NEWTON_POST_LISTERNER_CALLBACK_UPDATE           = 9;

 NEWTON_DYNAMIC_BODY                             = 0;
 NEWTON_KINEMATIC_BODY                           = 1;
 NEWTON_DEFORMABLE_BODY                          = 2;

 SERIALIZE_ID_SPHERE                             = 0;
 SERIALIZE_ID_CAPSULE                            = 1;
 SERIALIZE_ID_CHAMFERCYLINDER                    = 2;
 SERIALIZE_ID_TAPEREDCAPSULE                     = 3;
 SERIALIZE_ID_CYLINDER                           = 4;
 SERIALIZE_ID_TAPEREDCYLINDER                    = 5;
 SERIALIZE_ID_BOX                                = 6;
 SERIALIZE_ID_CONE                               = 7;
 SERIALIZE_ID_CONVEXHULL                         = 8;
 SERIALIZE_ID_NULL                               = 9;
 SERIALIZE_ID_COMPOUND                           = 10;
 SERIALIZE_ID_TREE                               = 11;
 SERIALIZE_ID_HEIGHTFIELD                        = 12;
 SERIALIZE_ID_DEFORMABLEMESH                     = 13;
 SERIALIZE_ID_USERMESH                           = 14;
 SERIALIZE_ID_SCENE                              = 15;
 SERIALIZE_ID_COMPOUND_BREAKABLE                 = 16;

type
{$IFDEF __USE_DOUBLE_PRECISION__}
 dFloat                       = Double;
{$ELSE}
 dFloat                       = Single;
{$ENDIF}
 dFloat64                     = Double;

 PdFloat                      = ^dFloat;
 PdFloat64                    = ^dFloat64;

 NewtonMesh                   = ^Pointer;
 NewtonBody                   = ^Pointer;
 NewtonWorld                  = ^Pointer;
 NewtonJoint                  = ^Pointer;
 NewtonMaterial               = ^Pointer;
 NewtonCollision              = ^Pointer;
// NewtonSceneProxy           = ^Pointer;
 NewtonDeformableMeshSegment  = ^Pointer;
 NewtonBreakableComponentMesh = ^Pointer;
 NewtonSerializeHandle        = ^Pointer;
 NewtonMeshHandle             = ^Pointer;
 NewtonMeshVertex             = ^Pointer;
 NewtonMeshPoint              = ^Pointer;
 NewtonMeshEdge               = ^Pointer;
 NewtonMeshFace               = ^Pointer;
 NewtonListener               = ^Pointer;

 TNewtonBoxParam = Packed Record
  m_x,
  m_y,
  m_z                    : dFloat;
 end;

 TNewtonSphereParam = Packed Record
  m_radio                : dFloat;
 end;

 TNewtonCylinderParam = Packed Record
  m_radio,
  m_height               : dFloat;
 end;

 TNewtonCapsuleParam = Packed Record
  m_radio,
  m_height               : dFloat;
 end;

 TNewtonConeParam = Packed Record
  m_radio,
  m_height               : dFloat;
 end;

 TNewtonTaperedCapsuleParam = Packed Record
  m_radio0,
  m_radio1,
  m_height               : dFloat;
 end;


 TNewtonTaperedCylinderParam = Packed Record
  m_radio0,
  m_radio1,
  m_height               : dFloat;
 end;

 TNewtonChamferCylinderParam = Packed Record
  m_radio,
  m_height               : dFloat;
 end;

 TNewtonConvexHullParam = Packed Record
  m_vertexCount,
  m_vertexStrideInBytes,
  m_faceCount            : Integer;
  m_vertex               : PdFloat;
 end;


 TNewtonCompoundCollisionParam = Packed Record
  m_chidrenCount         : Integer;
 end;


 TNewtonCollisionTreeParam = Packed Record
  m_vertexCount,
  m_indexCount           : Integer;
 end;

 TNewtonDeformableMeshParam = Packed Record
  m_vertexCount,
  m_triangleCount,
  m_vertexStrideInBytes  : Integer;
  m_indexList            : PWord;
  m_vertexList           : PdFloat;
 end;


 TNewtonHeightFieldCollisionParam = Packed Record
  m_width,
  m_height,
  m_gridsDiagonals,
  m_elevationDataType    : Integer;
  m_horizonalScale,
  m_verticalScale        : dFloat;
  m_elevation            : PdFloat;
  m_atributes            : PShortInt;
 end;

 TNewtonSceneCollisionParam = Packed Record
  m_childrenProxyCount   : Integer;
 end;

 TNewtonCollisionNullParam = packed record
 end;

 PNewtonCollisionInfoRecord = ^TNewtonCollisionInfoRecord;
 TNewtonCollisionInfoRecord = Packed Record
  m_offsetMatrix         : Array [0..3,0..3] of dFloat;
  m_collisionType,    // tag id to identify the collision primitive
  m_collisionUserID      : Integer;

  case Integer of
       SERIALIZE_ID_SPHERE          : (sdSphere             : TNewtonSphereParam);
       SERIALIZE_ID_CAPSULE         : (sdCapsule            : TNewtonCapsuleParam);
       SERIALIZE_ID_CHAMFERCYLINDER : (sdChamferCylinder    : TNewtonChamferCylinderParam);
       SERIALIZE_ID_TAPEREDCAPSULE  : (sdTaperedCapsule     : TNewtonTaperedCapsuleParam);
       SERIALIZE_ID_CYLINDER        : (sdCylinder           : TNewtonCylinderParam);
       SERIALIZE_ID_TAPEREDCYLINDER : (sdTaperedCylinder    : TNewtonTaperedCylinderParam);
       SERIALIZE_ID_BOX             : (sdBox                : TNewtonBoxParam);
       SERIALIZE_ID_CONE            : (sdCone               : TNewtonConeParam);
       SERIALIZE_ID_CONVEXHULL      : (sdConvexHull         : TNewtonConvexHullParam);
       SERIALIZE_ID_NULL            : (sdNull               : TNewtonCollisionNullParam);
       SERIALIZE_ID_COMPOUND        : (sdCompound           : TNewtonCompoundCollisionParam);
       SERIALIZE_ID_TREE            : (sdTree               : TNewtonCollisionTreeParam);
       SERIALIZE_ID_HEIGHTFIELD     : (sdHeightField        : TNewtonHeightFieldCollisionParam);
       SERIALIZE_ID_DEFORMABLEMESH  : (sdDeformableMesh     : TNewtonDeformableMeshParam);
       SERIALIZE_ID_USERMESH        : (sdUserMesh           : Array[0..63] of dFloat);
       SERIALIZE_ID_SCENE           : (sdSceneCollision     : TNewtonSceneCollisionParam);

  end;

 PNewtonJointRecord = ^TNewtonJointRecord;
 TNewtonJointRecord = Packed Record
  m_attachmenMatrix_0    : Array [0..3,0..3] of dFloat;
  m_attachmenMatrix_1    : Array [0..3,0..3] of dFloat;
  m_minLinearDof         : Array [0..2] of dFloat;
  m_maxLinearDof         : Array [0..2] of dFloat;
  m_minAngularDof        : Array [0..2] of dFloat;
  m_maxAngularDof        : Array [0..2] of dFloat;
  m_attachBody_0         : NewtonBody;
  m_attachBody_1         : NewtonBody;
  m_extraParameters      : Array [0..15] of dFloat;
  m_bodiesCollisionOn    : Integer;
  m_descriptionType      : Array [0..31] of ShortInt;
 end;

 PNewtonUserMeshCollisionCollideDesc = ^TNewtonUserMeshCollisionCollideDesc;
 TNewtonUserMeshCollisionCollideDesc = Packed Record
  m_boxP0,                                              // lower bounding box of intersection query in local space
  m_boxP1,                                              // upper bounding box of intersection query in local space
  m_m_boxDistanceTravel  : Array [0..3] of dFloat;      // max distance that box bpxP0 and boxP1 can travel on this timestep, used this for continue collision mode.
                                                        // used this for continue collision mode
  m_threadNumber,                                       // current thread executing this query
  m_faceCount,                                          // the application should set here how many polygons intersect the query box
  m_vertexStrideInBytes  : Integer;                     // the application should set here the size of each vertex
  m_skinThickness        : dFloat;                      // this is the minimum skin separation specified by the material between these two colliding shapes
  m_userData             : Pointer;                     // user data passed to the collision geometry at creation time

  m_objBody,                                            // pointer to the colliding body
  m_polySoupBody         : NewtonBody;                  // pointer to the rigid body owner of this collision tree
  m_objCollision,                                       // collision shape of the colliding body, (no necessarily the collision of m_objBody)
  m_polySoupCollision    : NewtonCollision;             // collision shape of teh collsion tree, (no necessarily the collision of m_polySoupBody)

  m_vertex               : PdFloat;                     // the application should set here the pointer to the global vertex of the mesh.
  m_faceIndexCount,                                     // the application should set here the pointer to the vertex count of each face.
  m_faceVertexIndex      : PInteger;                    // the application should set here the pointer index array for each vertex on a face.
                                                        // the format of a face is I0, I1, I2, I3, ..., M, N, E0, E1, E2, ..., A
                                                        // I0, I1, I2, .. are the indices to the vertex, relative to m_vertex pointer
                                                        // M is the index to the material sub shape id
                                                        // N in the index to the vertex normal relative to m_vertex pointer
                                                        // E0, E1, E2, ... are the indices of the the face normal that is shared to that face edge, when the edge does not share a face normal then the edge index is set to index N, which the index to the face normal
                                                        // A is and estimate of the largest diagonal of the face, this used internally as a hint to improve floating point accuracy and algorithm performance.
 end;

 PNewtonWorldConvexCastReturnInfo = ^TNewtonWorldConvexCastReturnInfo;
 TNewtonWorldConvexCastReturnInfo = Packed Record
  m_point,                                              // collision point in global space
  m_normal,                                             // surface normal at collision point in global space
  m_normalOnHitPoint     : Array [0..3] of dFloat;      // surface normal at the surface of the hit body,
                                                        // is the same as the normal calculated by a ray cast hitting the body at the hit point
  m_penetration          : dFloat;                      // contact penetration at collision point
  m_contactID            : Integer;                     // collision ID at contact point
  m_hitBody              : NewtonBody;                  // body hit at contact point
 end;

 PNewtonUserMeshCollisionRayHitDesc = ^TNewtonUserMeshCollisionRayHitDesc;
 TNewtonUserMeshCollisionRayHitDesc = Packed Record
  m_p0,                                                 // ray origin in collision local space
  m_p1,                                                 // ray destination in collision local space
  m_normalOut            : Array [0..3] of dFloat;      // copy here the normal at the ray intersection
  m_userIdOut            : Integer;                     // copy here a user defined id for further feedback
  m_userData             : Pointer;                     // user data passed to the collision geometry at creation time
 end;

 PNewtonHingeSliderUpdateDesc = ^TNewtonHingeSliderUpdateDesc;
 TNewtonHingeSliderUpdateDesc = Packed Record
  m_accel,
  m_minFriction,
  m_maxFriction,
  m_timestep             : dFloat;
 end;

 // Newton callback functions
 PNewtonAllocMemory                          = ^TNewtonAllocMemory;
 TNewtonAllocMemory                          = function (sizeInBytes : Integer) : Pointer;
 PNewtonFreeMemory                           = ^TNewtonFreeMemory;
 TNewtonFreeMemory                           = procedure (ptr : Pointer; sizeInBytes : Integer);

 PNewtonWorldDestructorCallback              = ^TNewtonWorldDestructorCallback;
 TNewtonWorldDestructorCallback              = procedure (const World : NewtonWorld);
 
 PNewtonWorldUpdateListenerCallback          = ^TNewtonWorldUpdateListenerCallback;
 TNewtonWorldUpdateListenerCallback          = procedure (const World : NewtonWorld; listenerUserData : Pointer; timestep : dFloat);
 PNewtonWorldDestroyListenerCallback         = ^TNewtonWorldDestroyListenerCallback;
 TNewtonWorldDestroyListenerCallback         = procedure (const World : NewtonWorld; listenerUserData : Pointer);

 PNewtonGetTicksCountCallback                = ^TNewtonGetTicksCountCallback;
 TNewtonGetTicksCountCallback                = function () : Cardinal;

 PNewtonSerializeCallback                    = ^TNewtonSerializeCallback;
 TNewtonSerializeCallback                    = procedure (Handle : NewtonSerializeHandle; const buffer : Pointer; size : Integer);
 PNewtonDeserializeCallback                  = ^TNewtonDeserializeCallback;
 TNewtonDeserializeCallback                  = procedure (Handle : NewtonSerializeHandle; buffer : Pointer; size : Integer);

 PNewtonOnBodySerializationCallback          = ^TNewtonOnBodySerializationCallback;
 TNewtonOnBodySerializationCallback          = procedure (body : NewtonBody; Callback : PNewtonSerializeCallback; Handle : NewtonSerializeHandle);
 PNewtonOnBodyDeserializationCallback        = ^TNewtonOnBodyDeserializationCallback;
 TNewtonOnBodyDeserializationCallback        = procedure (body : NewtonBody; Callback : PNewtonDeserializeCallback; Handle : NewtonSerializeHandle);

 PNewtonOnUserCollisionSerializationCallback = ^TNewtonOnUserCollisionSerializationCallback;
 TNewtonOnUserCollisionSerializationCallback = procedure (userData : Pointer; Callback : PNewtonSerializeCallback; Handle : NewtonSerializeHandle);


 // user collision callbacks
 PNewtonUserMeshCollisionDestroyCallback     = ^TNewtonUserMeshCollisionDestroyCallback;
 TNewtonUserMeshCollisionDestroyCallback     = procedure (userData : Pointer);
 PNewtonUserMeshCollisionCollideCallback     = ^TNewtonUserMeshCollisionCollideCallback;
 TNewtonUserMeshCollisionCollideCallback     = procedure (collideDescData : PNewtonUserMeshCollisionCollideDesc);
 PNewtonUserMeshCollisionRayHitCallback      = ^TNewtonUserMeshCollisionRayHitCallback;
 TNewtonUserMeshCollisionRayHitCallback      = function (lineDescData : PNewtonUserMeshCollisionRayHitDesc) : dFloat;
 PNewtonUserMeshCollisionGetCollisionInfo    = ^TNewtonUserMeshCollisionGetCollisionInfo;
 TNewtonUserMeshCollisionGetCollisionInfo    = procedure (userData : Pointer; infoRecord : PNewtonCollisionInfoRecord);
 PNewtonUserMeshCollisionAABBTest            = ^TNewtonUserMeshCollisionAABBTest;
 TNewtonUserMeshCollisionAABBTest            = function (userData : Pointer; const boxP0,boxP1 : PdFloat) : Integer;
 PNewtonUserMeshCollisionGetFacesInAABB      = ^TNewtonUserMeshCollisionGetFacesInAABB;
 TNewtonUserMeshCollisionGetFacesInAABB      = function (userData : Pointer; const p0 : PdFloat; const p1 : PdFloat;
                                                        const vertexArray : PdFloat; vertexCount : PInteger;
                                                        vertexStrideInBytes : PInteger; const indexList : PInteger;
                                                        maxIndexCount : Integer; const userDataList : PInteger) : Integer;

 PNewtonCollisionTreeRayCastCallback         = ^TNewtonCollisionTreeRayCastCallback;
 TNewtonCollisionTreeRayCastCallback         = function (const body : NewtonBody; const treeCollision : NewtonCollision;
                                                        interception : dFloat; normal : PdFloat; faceID : Integer;
                                                        userData : Pointer) : dFloat;
 PNewtonHeightFieldRayCastCallback           = ^TNewtonHeightFieldRayCastCallback;
 TNewtonHeightFieldRayCastCallback           = function (const body : NewtonBody; const heightFieldCollision : NewtonCollision;
                                                        interception : dFloat; row, col : Integer; normal : PdFloat;
                                                        faceID : Integer; userData : Pointer) : dFloat;

 // collision tree call back (obsoleted no recommended)
 PNewtonTreeCollisionCallback                = ^TNewtonTreeCollisionCallback;
 TNewtonTreeCollisionCallback                = procedure (const bodyWithTreeCollision : NewtonBody; const body : NewtonBody;
                                                         faceID : Integer; vertexCount : Integer; const vertexArray : PdFloat;
                                                         vertexStrideInBytes : Integer);

 PNewtonBodyDestructor                       = ^TNewtonBodyDestructor;
 TNewtonBodyDestructor                       = procedure (const body : NewtonBody);
 PNewtonApplyForceAndTorque                  = ^TNewtonApplyForceAndTorque;
 TNewtonApplyForceAndTorque                  = procedure (const body : NewtonBody; timestep : dFloat; threadIndex : Integer);
 PNewtonSetTransform                         = ^TNewtonSetTransform;
 TNewtonSetTransform                         = procedure (const body : NewtonBody; const matrix : PdFloat; threadIndex : Integer);

 PNewtonIslandUpdate                         = ^TNewtonIslandUpdate;
 TNewtonIslandUpdate                         = function (const world : NewtonWorld; islandHandle : Pointer; bodyCount : Integer) : Integer;
 PNewtonBodyLeaveWorld                       = ^TNewtonBodyLeaveWorld;
 TNewtonBodyLeaveWorld                       = procedure (const body : NewtonBody; threadIndex : Integer);
 PNewtonDestroyBodyByExeciveForce            = ^TNewtonDestroyBodyByExeciveForce;
 TNewtonDestroyBodyByExeciveForce            = procedure (const body : NewtonBody; const contact : NewtonJoint);

 TNewtonCollisionCompoundBreakableCallback   = function (mesh : NewtonMesh; userData : Pointer; planeMatrixOut : PdFloat) : Integer;

 PNewtonGetBuoyancyPlane                     = ^TNewtonGetBuoyancyPlane;
 TNewtonGetBuoyancyPlane                     = function (const collisionID : Integer; context : Pointer; const globalSpaceMatrix : PdFloat;
                                                        globalSpacePlane : PdFloat) : Integer;
 PNewtonWorldRayPrefilterCallback            = ^TNewtonWorldRayPrefilterCallback;
 TNewtonWorldRayPrefilterCallback            = function (const body : NewtonBody; collision : NewtonCollision; userData : Pointer) : Cardinal;
 PNewtonWorldRayFilterCallback               = ^TNewtonWorldRayFilterCallback;
 TNewtonWorldRayFilterCallback               = function (const body : NewtonBody; const hitNormal : PdFloat; collisionID : Integer;
                                                        userData : Pointer; intersectParam : dFloat) : dFloat;

 PNewtonOnAABBOverlap                        = ^TNewtonOnAABBOverlap;
 TNewtonOnAABBOverlap                        = function (const material : NewtonMaterial; const body0 : NewtonBody; const body1 : NewtonBody;
                                                        threadIndex : Integer) : Integer;
 PNewtonContactsProcess                      = ^TNewtonContactsProcess;
 TNewtonContactsProcess                      = procedure (const contact : NewtonJoint; timestep : dFloat; threadIndex : Integer);

 PNewtonBodyIterator                         = ^TNewtonBodyIterator;
 TNewtonBodyIterator                         = procedure (const body : NewtonBody; userData : Pointer);
 PNewtonJointIterator                        = ^TNewtonJointIterator;
 TNewtonJointIterator                        = procedure (const joint : NewtonJoint; userData : Pointer);
 PNewtonCollisionIterator                    = ^TNewtonCollisionIterator;
 TNewtonCollisionIterator                    = procedure (userData : Pointer; vertexCount : Integer; const faceArray : PdFloat; faceId : Integer);

 PNewtonBallCallback                         = ^TNewtonBallCallback;
 TNewtonBallCallback                         = procedure (const ball : NewtonJoint; timestep : dFloat);
 PNewtonHingeCallback                        = ^TNewtonHingeCallback;
 TNewtonHingeCallback                        = function (const hinge : NewtonJoint; desc : PNewtonHingeSliderUpdateDesc) : Cardinal;
 PNewtonSliderCallback                       = ^TNewtonSliderCallback;
 TNewtonSliderCallback                       = function (const slider : NewtonJoint; desc : PNewtonHingeSliderUpdateDesc) : Cardinal;
 PNewtonUniversalCallback                    = ^TNewtonUniversalCallback;
 TNewtonUniversalCallback                    = function (const universal : NewtonJoint; desc : PNewtonHingeSliderUpdateDesc) : Cardinal;
 PNewtonCorkscrewCallback                    = ^TNewtonCorkscrewCallback;
 TNewtonCorkscrewCallback                    = function (const corkscrew : NewtonJoint; desc : PNewtonHingeSliderUpdateDesc) : Cardinal;

 PNewtonUserBilateralCallback                = ^TNewtonUserBilateralCallback;
 TNewtonUserBilateralCallback                = procedure (const userJoint : NewtonJoint; timestep : dFloat; threadIndex : Integer);
 PNewtonUserBilateralGetInfoCallback         = ^TNewtonUserBilateralGetInfoCallback;
 TNewtonUserBilateralGetInfoCallback         = procedure (const userJoint : NewtonJoint; info : PNewtonJointRecord);

 PNewtonConstraintDestructor                 = ^TNewtonConstraintDestructor;
 TNewtonConstraintDestructor                 = procedure (const me : NewtonJoint);

 PNewtonJobTask                              = ^TNewtonJobTask;
 TNewtonJobTask                              = procedure (userData : Pointer; threadIndex : Integer);

 PNewtonReportProgress                       = ^TNewtonReportProgress;
 TNewtonReportProgress                       = procedure (progressNormalzedPercent : dFloat);

// typedef void (*NewtonSetRagDollTransform) (const NewtonRagDollBone* const bone);
// typedef void (*NewtonBodyActivationState) (const body : NewtonBody; unsigned state);
// typedef void (*NewtonVehicleTireUpdate) (const NewtonJoint* const vehicle, dFloat timestep);


 // **********************************************************************************************
 //
 // world control functions
 //
 // **********************************************************************************************
 function NewtonWorldGetVersion () : Integer; cdecl; external NEWTON_API;
 function NewtonWorldFloatSize () : Integer; cdecl; external NEWTON_API;

 function NewtonGetMemoryUsed () : Integer; cdecl; external NEWTON_API;
 procedure NewtonSetMemorySystem (malloc : PNewtonAllocMemory; free : PNewtonFreeMemory); cdecl; external NEWTON_API;

 function NewtonCreate () : NewtonWorld; cdecl; external NEWTON_API;
 procedure NewtonDestroy (const world : NewtonWorld); cdecl; external NEWTON_API;
 procedure NewtonDestroyAllBodies (const world : NewtonWorld); cdecl; external NEWTON_API;

 // procedure NewtonSetPlatformArchitecture (const world : NewtonWorld; int mode);
 // NEWTON_API int NewtonGetPlatformArchitecture(const world : NewtonWorld; char* description);

 function NewtonEnumrateDevices (const world : NewtonWorld) : Integer; cdecl; external NEWTON_API;
 function NewtonGetCurrentDevice (const world : NewtonWorld) : Integer; cdecl; external NEWTON_API;
 procedure NewtonSetCurrentDevice (const world : NewtonWorld; deviceIndex : Integer); cdecl; external NEWTON_API;
 procedure NewtonGetDeviceString (const world : NewtonWorld; deviceIndex : Integer; vendorString : PChar; maxSize : Integer); cdecl; external NEWTON_API;

 procedure NewtonInvalidateCache (const world : NewtonWorld); cdecl; external NEWTON_API;

 procedure NewtonSetSolverModel (const world : NewtonWorld; model : Integer); cdecl; external NEWTON_API;

 procedure NewtonSetMultiThreadSolverOnSingleIsland (const world : NewtonWorld; mode : Integer); cdecl; external NEWTON_API;
 function NewtonGetMultiThreadSolverOnSingleIsland (const world : NewtonWorld) : Integer; cdecl; external NEWTON_API;

 procedure NewtonSetPerformanceClock (const world : NewtonWorld; callback : PNewtonGetTicksCountCallback); cdecl; external NEWTON_API;
 function NewtonReadPerformanceTicks (const world : NewtonWorld; performanceEntry : Cardinal) : Cardinal; cdecl; external NEWTON_API;

 function NewtonGetBroadphaseAlgorithm (const world : NewtonWorld) : Integer; cdecl; external NEWTON_API;
 procedure NewtonSelectBroadphaseAlgorithm (const world : NewtonWorld; algorithmType : Integer); cdecl; external NEWTON_API;
 
 procedure NewtonUpdate (const world : NewtonWorld; timestep : dFloat); cdecl; external NEWTON_API;
 procedure NewtonUpdateAsync (const world : NewtonWorld; timestep : dFloat); cdecl; external NEWTON_API;
 procedure NewtonWaitForUpdateToFinish (const world : NewtonWorld); cdecl; external NEWTON_API;

 procedure NewtonSerializeToFile (const world : NewtonWorld; const filename : PChar); cdecl; external NEWTON_API;
 procedure NewtonSerializeBodyArray (const world : NewtonWorld; bodyArray : NewtonBody; bodyCount : Integer;
                                     serializeBody : PNewtonOnBodySerializationCallback; serializeFunction : PNewtonSerializeCallback;
                                     Handle : NewtonSerializeHandle); cdecl; external NEWTON_API;
 procedure NewtonDeserializeBodyArray (const world : NewtonWorld; deserializeBody : PNewtonOnBodyDeserializationCallback;
                                       serializeFunction : PNewtonDeserializeCallback; Handle : NewtonSerializeHandle); cdecl; external NEWTON_API;

 function NewtonReadThreadPerformanceTicks (const world : NewtonWorld; threadIndex : Cardinal) : Cardinal; cdecl; external NEWTON_API;
 
 // multi threading interface
 procedure NewtonWorldCriticalSectionLock (const world : NewtonWorld; threadIndex : Integer); cdecl; external NEWTON_API;
 procedure NewtonWorldCriticalSectionUnlock (const world : NewtonWorld); cdecl; external NEWTON_API;
 procedure NewtonSetThreadsCount (const world : NewtonWorld; threads : Integer); cdecl; external NEWTON_API;
 function NewtonGetThreadsCount(const world : NewtonWorld) : Integer; cdecl; external NEWTON_API;
 function NewtonGetMaxThreadsCount(const world : NewtonWorld) : Integer; cdecl; external NEWTON_API;
 procedure NewtonDispachThreadJob(const world : NewtonWorld; task : PNewtonJobTask; userData : Pointer); cdecl; external NEWTON_API;
 procedure NewtonSyncThreadJobs(const world : NewtonWorld); cdecl; external NEWTON_API;

 // atomic operations
 function NewtonAtomicAdd(ptr : PInteger; Value : Integer) : Integer; cdecl; external NEWTON_API;
 function NewtonAtomicSwap(ptr : PInteger; Value : Integer) : Integer; cdecl; external NEWTON_API;
 procedure NewtonYield(); cdecl; external NEWTON_API;

 procedure NewtonSetFrictionModel (const world : NewtonWorld; model : Integer); cdecl; external NEWTON_API;
 procedure NewtonSetMinimumFrameRate (const world : NewtonWorld; frameRate : dFloat); cdecl; external NEWTON_API;
 procedure NewtonSetBodyLeaveWorldEvent (const world : NewtonWorld; callback : PNewtonBodyLeaveWorld); cdecl; external NEWTON_API;
 procedure NewtonSetIslandUpdateEvent (const world : NewtonWorld; islandUpdate : PNewtonIslandUpdate); cdecl; external NEWTON_API;
 procedure NewtonSetDestroyBodyByExeciveForce (const world : NewtonWorld; callback : PNewtonDestroyBodyByExeciveForce); cdecl; external NEWTON_API;

// procedure NewtonWorldForEachBodyDo (const world : NewtonWorld; NewtonBodyIterator callback);
 procedure NewtonWorldForEachJointDo (const world : NewtonWorld; callback : PNewtonJointIterator; userData : Pointer); cdecl; external NEWTON_API;
 procedure NewtonWorldForEachBodyInAABBDo (const world : NewtonWorld; const p0 : PdFloat; const p1 : PdFloat; callback : PNewtonBodyIterator; userData : Pointer); cdecl; external NEWTON_API;

 
 procedure NewtonWorldSetUserData (const world : NewtonWorld; userData : Pointer); cdecl; external NEWTON_API;
 function NewtonWorldGetUserData (const world : NewtonWorld) : Pointer; cdecl; external NEWTON_API;

 function NewtonWorldGetListenerUserData (const world : NewtonWorld; Listener : NewtonListener) : Pointer; cdecl; external NEWTON_API;

 function NewtonWorldGetPreListener (const world : NewtonWorld; nameID : PChar) : NewtonListener; cdecl; external NEWTON_API;
 function NewtonWorldAddPreListener (const world : NewtonWorld; nameID : PChar; listenerUserData : Pointer; Update : PNewtonWorldUpdateListenerCallback; Destroy : PNewtonWorldDestroyListenerCallback) : NewtonListener; cdecl; external NEWTON_API;

 function NewtonWorldGetPostListener (const world : NewtonWorld; nameID : PChar) : NewtonListener; cdecl; external NEWTON_API;
 function NewtonWorldAddPostListener (const world : NewtonWorld; nameID : PChar; listenerUserData : Pointer; Update : PNewtonWorldUpdateListenerCallback; Destroy : PNewtonWorldDestroyListenerCallback) : NewtonListener; cdecl; external NEWTON_API;

 procedure NewtonWorldSetDestructorCallback (const world : NewtonWorld; callback : PNewtonWorldDestructorCallback); cdecl; external NEWTON_API;
 function NewtonWorldGetDestructorCallback (const world : NewtonWorld) : PNewtonWorldDestructorCallback; cdecl; external NEWTON_API;

 procedure NewtonWorldRayCast (const world : NewtonWorld; const p0 : PdFloat; const p1 : PdFloat; filter : PNewtonWorldRayFilterCallback; userData : Pointer;
                               prefilter : PNewtonWorldRayPrefilterCallback); cdecl; external NEWTON_API;

 function NewtonWorldCollide (const world : NewtonWorld; const Matrix : PdFloat; const shape : NewtonCollision; UserData : Pointer;
                              prefilter : PNewtonWorldRayPrefilterCallback; Info : PNewtonWorldConvexCastReturnInfo; maxContactsCount : Integer; threadIndex : Integer) : Integer; cdecl; external NEWTON_API;

 function NewtonWorldConvexCast (const world : NewtonWorld; const matrix : PdFloat; const target : PdFloat; const shape : NewtonCollision; hitParam : PdFloat; userData : Pointer;
                                 prefilter : PNewtonWorldRayPrefilterCallback; info : PNewtonWorldConvexCastReturnInfo; maxContactsCount : Integer; threadIndex : Integer) : Integer; cdecl; external NEWTON_API;


 // world utility functions
 function NewtonWorldGetBodyCount(const world : NewtonWorld) : Integer; cdecl; external NEWTON_API;
 function NewtonWorldGetConstraintCount(const world : NewtonWorld) : Integer; cdecl; external NEWTON_API;

 // NEWTON_API int NewtonGetActiveBodiesCount();
 // NEWTON_API int NewtonGetActiveConstraintsCount();
 // NEWTON_API dFloat NewtonGetGlobalScale (const world : NewtonWorld);



 // **********************************************************************************************
 //
 // Simulation islands 
 //
 // **********************************************************************************************
 function NewtonIslandGetBody (const island : Pointer; bodyIndex : Integer) : NewtonBody; cdecl; external NEWTON_API;
 procedure NewtonIslandGetBodyAABB (const island : Pointer; bodyIndex : Integer; p0 : PdFloat; p1 : PdFloat); cdecl; external NEWTON_API;

 // **********************************************************************************************
 //
 // Physics Material Section
 //
 // **********************************************************************************************
 function NewtonMaterialCreateGroupID(const world : NewtonWorld) : Integer; cdecl; external NEWTON_API;
 function NewtonMaterialGetDefaultGroupID(const world : NewtonWorld) : Integer; cdecl; external NEWTON_API;
 procedure NewtonMaterialDestroyAllGroupID(const world : NewtonWorld); cdecl; external NEWTON_API;

 // material definitions that can not be overwritten in function callback
 function NewtonMaterialGetUserData (const world : NewtonWorld; id0 : Integer; id1 : Integer) : Pointer; cdecl; external NEWTON_API;
 procedure NewtonMaterialSetSurfaceThickness (const world : NewtonWorld; id0 : Integer; id1 : Integer; thickness : dFloat); cdecl; external NEWTON_API;

// deprecated, not longer continue collision is set on the material   
// procedure NewtonMaterialSetContinuousCollisionMode (const world : NewtonWorld; id0 : Integer; id1 : Integer; state : Integer);
 procedure NewtonMaterialSetCollisionCallback (const world : NewtonWorld; id0 : Integer; id1 : Integer; userData : Pointer;
                                               aabbOverlap : PNewtonOnAABBOverlap; process : PNewtonContactsProcess); cdecl; external NEWTON_API;

 procedure NewtonMaterialSetDefaultSoftness (const world : NewtonWorld; id0 : Integer; id1 : Integer; value : dFloat); cdecl; external NEWTON_API;
 procedure NewtonMaterialSetDefaultElasticity (const world : NewtonWorld; id0 : Integer; id1 : Integer; elasticCoef : dFloat); cdecl; external NEWTON_API;
 procedure NewtonMaterialSetDefaultCollidable (const world : NewtonWorld; id0 : Integer; id1 : Integer; state : Integer); cdecl; external NEWTON_API;
 procedure NewtonMaterialSetDefaultFriction (const world : NewtonWorld; id0 : Integer; id1 : Integer; staticFriction : dFloat; kineticFriction : dFloat); cdecl; external NEWTON_API;

 function NewtonWorldGetFirstMaterial (const world : NewtonWorld) : NewtonMaterial; cdecl; external NEWTON_API;
 function NewtonWorldGetNextMaterial (const world : NewtonWorld; const material : NewtonMaterial) : NewtonMaterial; cdecl; external NEWTON_API;

 function NewtonWorldGetFirstBody (const world : NewtonWorld) : NewtonBody; cdecl; external NEWTON_API;
 function NewtonWorldGetNextBody (const world : NewtonWorld; const curBody : NewtonBody) : NewtonBody; cdecl; external NEWTON_API;


 // **********************************************************************************************
 //
 // Physics Contact control functions
 //
 // **********************************************************************************************
 function NewtonMaterialGetMaterialPairUserData (const material : NewtonMaterial) : Pointer; cdecl; external NEWTON_API;
 function NewtonMaterialGetContactFaceAttribute (const material : NewtonMaterial) : Cardinal; cdecl; external NEWTON_API;
 function NewtonMaterialGetBodyCollidingShape (const material : NewtonMaterial; const body : NewtonBody) : NewtonCollision; cdecl; external NEWTON_API;
 //NEWTON_API unsigned NewtonMaterialGetBodyCollisionID (const material : NewtonMaterial; const body : NewtonBody);
 function NewtonMaterialGetContactNormalSpeed (const material : NewtonMaterial) : dFloat; cdecl; external NEWTON_API;
 procedure NewtonMaterialGetContactForce (const material : NewtonMaterial; const body : NewtonBody; force : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonMaterialGetContactPositionAndNormal (const material : NewtonMaterial; const body : NewtonBody; posit : PdFloat; normal : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonMaterialGetContactTangentDirections (const material : NewtonMaterial; const body : NewtonBody; dir0 : PdFloat; dir1 : PdFloat); cdecl; external NEWTON_API;
 function NewtonMaterialGetContactTangentSpeed (const material : NewtonMaterial; index : Integer) : dFloat; cdecl; external NEWTON_API;
 
 procedure NewtonMaterialSetContactSoftness (const material : NewtonMaterial; softness : dFloat); cdecl; external NEWTON_API;
 procedure NewtonMaterialSetContactElasticity (const material : NewtonMaterial; restitution : dFloat); cdecl; external NEWTON_API;
 procedure NewtonMaterialSetContactFrictionState (const material : NewtonMaterial; state : Integer; index : Integer); cdecl; external NEWTON_API;
 procedure NewtonMaterialSetContactFrictionCoef (const material : NewtonMaterial; staticFrictionCoef : dFloat; kineticFrictionCoef : dFloat; index : Integer); cdecl; external NEWTON_API;
 
 procedure NewtonMaterialSetContactNormalAcceleration (const material : NewtonMaterial; accel : dFloat); cdecl; external NEWTON_API;
 procedure NewtonMaterialSetContactNormalDirection (const material : NewtonMaterial; const directionVector : PdFloat); cdecl; external NEWTON_API;

 procedure NewtonMaterialSetContactTangentAcceleration (const material : NewtonMaterial; accel : dFloat; index : Integer); cdecl; external NEWTON_API;
 procedure NewtonMaterialContactRotateTangentDirections (const material : NewtonMaterial; const directionVector : PdFloat); cdecl; external NEWTON_API;

 

 // **********************************************************************************************
 //
 // convex collision primitives creation functions
 //
 // **********************************************************************************************
 function NewtonCreateNull (const world : NewtonWorld) : NewtonCollision; cdecl; external NEWTON_API;
 function NewtonCreateSphere (const world : NewtonWorld; radius : dFloat; shapeID : Integer; const offsetMatrix : PdFloat) : NewtonCollision; cdecl; external NEWTON_API;
 function NewtonCreateBox (const world : NewtonWorld; dx : dFloat; dy : dFloat; dz : dFloat; shapeID : Integer; const offsetMatrix : PdFloat) : NewtonCollision; cdecl; external NEWTON_API;
 function NewtonCreateCone (const world : NewtonWorld; radius : dFloat; height : dFloat; shapeID : Integer; const offsetMatrix : PdFloat) : NewtonCollision; cdecl; external NEWTON_API;
 function NewtonCreateCapsule (const world : NewtonWorld; radius : dFloat; height : dFloat; shapeID : Integer; const offsetMatrix : PdFloat) : NewtonCollision; cdecl; external NEWTON_API;
 function NewtonCreateCylinder (const world : NewtonWorld; radius : dFloat; height : dFloat; shapeID : Integer; const offsetMatrix : PdFloat) : NewtonCollision; cdecl; external NEWTON_API;
 function NewtonCreateTaperedCapsule (const world : NewtonWorld; radio0 : dFloat; radio1 : dFloat; height : dFloat; shapeID : Integer; const offsetMatrix : PdFloat) : NewtonCollision; cdecl; external NEWTON_API;
 function NewtonCreateTaperedCylinder (const world : NewtonWorld; radio0 : dFloat; radio1 : dFloat; height : dFloat; shapeID : Integer; const offsetMatrix : PdFloat) : NewtonCollision; cdecl; external NEWTON_API;
 function NewtonCreateChamferCylinder (const world : NewtonWorld; radius : dFloat; height : dFloat; shapeID : Integer; const offsetMatrix : PdFloat) : NewtonCollision; cdecl; external NEWTON_API;
 function NewtonCreateConvexHull (const world : NewtonWorld; count : Integer; const vertexCloud : PdFloat; strideInBytes : Integer; tolerance : dFloat; shapeID : Integer; const offsetMatrix : PdFloat) : NewtonCollision; cdecl; external NEWTON_API;
 function NewtonCreateConvexHullFromMesh (const world : NewtonWorld; const mesh : NewtonMesh; tolerance : dFloat; shapeID : Integer) : NewtonCollision; cdecl; external NEWTON_API;

 function NewtonCollisionGetMode(const convexCollision : NewtonCollision) : Integer; cdecl; external NEWTON_API;
 procedure NewtonCollisionSetCollisonMode(const convexCollision : NewtonCollision; mode : Integer); cdecl; external NEWTON_API;

 //function NewtonCollisionIsTriggerVolume(const convexCollision : NewtonCollision) : Integer; cdecl; external NEWTON_API;
 //procedure NewtonCollisionSetAsTriggerVolume(const convexCollision : NewtonCollision; trigger : Integer); cdecl; external NEWTON_API;

// procedure NewtonCollisionSetMaxBreakImpactImpulse(const convexHullCollision : NewtonCollision; dFloat maxImpactImpulse);
// NEWTON_API dFloat NewtonCollisionGetMaxBreakImpactImpulse(const NewtonCollision* const convexHullCollision);


 function NewtonConvexHullGetFaceIndices (const convexHullCollision : NewtonCollision; face : Integer; faceIndices : PInteger) : Integer; cdecl; external NEWTON_API;
 function NewtonConvexCollisionCalculateVolume (const convexCollision : NewtonCollision) : dFloat; cdecl; external NEWTON_API;
 procedure NewtonConvexCollisionCalculateInertialMatrix (const convexCollision : NewtonCollision; inertia : PdFloat; origin : PdFloat); cdecl; external NEWTON_API;


 // **********************************************************************************************
 //
 // compound collision primitives creation functions
 //
 // **********************************************************************************************
 function NewtonCreateCompoundCollision (const world : NewtonWorld; shapeID : Integer) : NewtonCollision; cdecl; external NEWTON_API;
 function NewtonCreateCompoundCollisionFromMesh (const world : NewtonWorld; const mesh : NewtonMesh; hullTolerance : dFloat; shapeID : Integer; subShapeID : Integer) : NewtonCollision; cdecl; external NEWTON_API;

 procedure NewtonCompoundCollisionBeginAddRemove (compoundCollision : NewtonCollision); cdecl; external NEWTON_API;
 function NewtonCompoundCollisionAddSubCollision (const compoundCollision : NewtonCollision; convexCollision : NewtonCollision) : Pointer; cdecl; external NEWTON_API;
 procedure NewtonCompoundCollisionRemoveSubCollision (const compoundCollision : NewtonCollision; collisionNode : Pointer); cdecl; external NEWTON_API;
 procedure NewtonCompoundCollisionRemoveSubCollisionByIndex (const compoundCollision : NewtonCollision; nodeIndex : Integer); cdecl; external NEWTON_API;
 procedure NewtonCompoundCollisionSetSubCollisionMatrix (const compoundCollision : NewtonCollision; node : Pointer; matrix : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonCompoundCollisionEndAddRemove (compoundCollision : NewtonCollision); cdecl; external NEWTON_API;

 function NewtonCompoundCollisionGetFirstNode (compoundCollision : NewtonCollision) : Pointer; cdecl; external NEWTON_API;
 function NewtonCompoundCollisionGetNextNode (const compoundCollision : NewtonCollision; node : Pointer) : Pointer; cdecl; external NEWTON_API;

 function NewtonCompoundCollisionGetNodeByIndex (const compoundCollision : NewtonCollision; index : Integer) : Pointer; cdecl; external NEWTON_API;
 function NewtonCompoundCollisionGetNodeIndex (const compoundCollision : NewtonCollision; node : Pointer) : Integer; cdecl; external NEWTON_API;
 function NewtonCompoundCollisionGetCollisionFromNode (const compoundCollision : NewtonCollision; node : Pointer) : NewtonCollision; cdecl; external NEWTON_API;


 // **********************************************************************************************
 //
 // scene collision are static compound collision that can take polygonal static collisions
 //
 // **********************************************************************************************
 function NewtonCreateSceneCollision (const world : NewtonWorld; shapeID : Integer) : NewtonCollision; cdecl; external NEWTON_API;

 procedure NewtonSceneCollisionBeginAddRemove (sceneCollision : NewtonCollision); cdecl; external NEWTON_API;
 function NewtonSceneCollisionAddSubCollision (sceneCollision : NewtonCollision; collision : NewtonCollision) : Pointer; cdecl; external NEWTON_API;
 procedure NewtonSceneCollisionSetSubCollisionMatrix (sceneCollision : NewtonCollision; node : Pointer; matrix : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonSceneCollisionEndAddRemove (sceneCollision : NewtonCollision); cdecl; external NEWTON_API;


 function NewtonSceneCollisionGetFirstNode (sceneCollision : NewtonCollision) : Pointer; cdecl; external NEWTON_API;
 function NewtonSceneCollisionGetNextNode (sceneCollision : NewtonCollision; node : Pointer) : Pointer; cdecl; external NEWTON_API;

 function NewtonSceneCollisionGetCollisionFromNode (sceneCollision : NewtonCollision; node : Pointer) : NewtonCollision; cdecl; external NEWTON_API;
 // NEWTON_API NewtonSceneProxy* NewtonSceneCollisionCreateProxy (NewtonCollision* const scene, collision : NewtonCollision; const matrix : PdFloat);
 // procedure NewtonSceneCollisionDestroyProxy (NewtonCollision* const scene, NewtonSceneProxy* Proxy);
 // procedure NewtonSceneProxySetMatrix (NewtonSceneProxy* const proxy, const dFloat* matrix);
 // procedure NewtonSceneProxyGetMatrix (NewtonSceneProxy* const proxy, dFloat* matrix);
 // procedure NewtonSceneSetProxyUserData (NewtonSceneProxy* const proxy, void* userData);
 // NEWTON_API void* NewtonSceneGetProxyUserData (NewtonSceneProxy* const proxy);
 // NEWTON_API NewtonSceneProxy* NewtonSceneGetFirstProxy (NewtonCollision* const scene);
 // NEWTON_API NewtonSceneProxy* NewtonSceneGetNextProxy (NewtonCollision* const scene, NewtonSceneProxy* const proxy);
 // procedure NewtonSceneCollisionOptimize (NewtonCollision* scene);




 // **********************************************************************************************
 //
 // complex breakable collision primitives interface
 //
 // **********************************************************************************************
(*
 NEWTON_API NewtonCollision* NewtonCreateCompoundBreakable (const world : NewtonWorld; int meshCount, 
                  const NewtonMesh** const solids, const int* const shapeIDArray, 
                  const dFloat* const densities, const int* const internalFaceMaterial, 
                  shapeID : Integer; int debriID, dFloat debriSeparationGap); cdecl; external NEWTON_API;


 procedure NewtonCompoundBreakableResetAnchoredPieces (const NewtonCollision* const compoundBreakable); cdecl; external NEWTON_API;
 procedure NewtonCompoundBreakableSetAnchoredPieces (const NewtonCollision* const compoundBreakable, int fixShapesCount, matrix : PdFloatPallete, NewtonCollision** const fixedShapesArray); cdecl; external NEWTON_API;

 NEWTON_API int NewtonCompoundBreakableGetVertexCount (const NewtonCollision* const compoundBreakable); cdecl; external NEWTON_API;
 procedure NewtonCompoundBreakableGetVertexStreams (const NewtonCollision* const compoundBreakable, int vertexStrideInByte, dFloat* const vertex,
               int normalStrideInByte, dFloat* const normal, int uvStrideInByte, dFloat* const uv); cdecl; external NEWTON_API;

 
 NEWTON_API NewtonBreakableComponentMesh* NewtonBreakableGetMainMesh (const NewtonCollision* const compoundBreakable); cdecl; external NEWTON_API;
 NEWTON_API NewtonBreakableComponentMesh* NewtonBreakableGetFirstComponent (const NewtonCollision* const compoundBreakable); cdecl; external NEWTON_API;
 NEWTON_API NewtonBreakableComponentMesh* NewtonBreakableGetNextComponent (const NewtonBreakableComponentMesh* const component); cdecl; external NEWTON_API;

 procedure NewtonBreakableBeginDelete (const NewtonCollision* const compoundBreakable); cdecl; external NEWTON_API;
 NEWTON_API NewtonBody* NewtonBreakableCreateDebrieBody (const NewtonCollision* const compoundBreakable, const NewtonBreakableComponentMesh* const component); cdecl; external NEWTON_API;
 procedure NewtonBreakableDeleteComponent (const NewtonCollision* const compoundBreakable, const NewtonBreakableComponentMesh* const component); cdecl; external NEWTON_API;
 procedure NewtonBreakableEndDelete (const NewtonCollision* const compoundBreakable); cdecl; external NEWTON_API;


 NEWTON_API int NewtonBreakableGetComponentsInRadius (const NewtonCollision* const compoundBreakable, const dFloat* position, radius : dFloat; NewtonBreakableComponentMesh** const segments, int maxCount); cdecl; external NEWTON_API;

 NEWTON_API void* NewtonBreakableGetFirstSegment (const NewtonBreakableComponentMesh* const breakableComponent); cdecl; external NEWTON_API;
 NEWTON_API void* NewtonBreakableGetNextSegment (const void* const segment); cdecl; external NEWTON_API;

 NEWTON_API int NewtonBreakableSegmentGetMaterial (const void* const segment); cdecl; external NEWTON_API;
 NEWTON_API int NewtonBreakableSegmentGetIndexCount (const void* const segment); cdecl; external NEWTON_API;
 NEWTON_API int NewtonBreakableSegmentGetIndexStream (const NewtonCollision* const compoundBreakable, const NewtonBreakableComponentMesh* const meshOwner, const void* const segment, int* const index); cdecl; external NEWTON_API;
 NEWTON_API int NewtonBreakableSegmentGetIndexStreamShort (const NewtonCollision* const compoundBreakable, const NewtonBreakableComponentMesh* const meshOwner, const void* const segment, short int* const index); cdecl; external NEWTON_API;
*)

 function NewtonCreateUserMeshCollision (const world : NewtonWorld; const minBox : PdFloat; const maxBox : PdFloat; userData : Pointer;
                                         collideCallback : PNewtonUserMeshCollisionCollideCallback; rayHitCallback : PNewtonUserMeshCollisionRayHitCallback;
                                         destroyCallback : PNewtonUserMeshCollisionDestroyCallback; getInfoCallback : PNewtonUserMeshCollisionGetCollisionInfo;
                                         getLocalAABBCallback : PNewtonUserMeshCollisionAABBTest; facesInAABBCallback : PNewtonUserMeshCollisionGetFacesInAABB; serializeCallback : PNewtonOnUserCollisionSerializationCallback;
                                         shapeID : Integer) : NewtonCollision; cdecl; external NEWTON_API;

 //  ***********************************************************************************************************
 //
 // Collision serialization functions
 //
 // ***********************************************************************************************************
 function NewtonCreateCollisionFromSerialization (const world : NewtonWorld; deserializeFunction : PNewtonDeserializeCallback; Handle : NewtonSerializeHandle) : NewtonCollision; cdecl; external NEWTON_API;
 procedure NewtonCollisionSerialize (const world : NewtonWorld; const collision : NewtonCollision; serializeFunction : PNewtonSerializeCallback; Handle : NewtonSerializeHandle); cdecl; external NEWTON_API;
 procedure NewtonCollisionGetInfo (const collision : NewtonCollision; collisionInfo : PNewtonCollisionInfoRecord); cdecl; external NEWTON_API;

 // **********************************************************************************************
 //
 // Static collision shapes functions
 //
 // **********************************************************************************************
 function NewtonCreateHeightFieldCollision (const world : NewtonWorld; width : Integer; height : Integer; gridsDiagonals : Integer;
                                            elevationDataType: Integer; const elevationMap : PdFloat;
                                            const attributeMap : PShortInt; verticalScale: dFloat; horizontalScale : dFloat; shapeID : Integer) : NewtonCollision; cdecl; external NEWTON_API; //changend 22.08.14
 procedure NewtonHeightFieldSetUserRayCastCallback (const heightfieldCollision : NewtonCollision; rayHitCallback : PNewtonHeightFieldRayCastCallback); cdecl; external NEWTON_API;

 
 function NewtonCreateTreeCollision (const world : NewtonWorld; shapeID : Integer) : NewtonCollision; cdecl; external NEWTON_API;
 function NewtonCreateTreeCollisionFromMesh (const world : NewtonWorld; const mesh : NewtonMesh; shapeID : Integer) : NewtonCollision; cdecl; external NEWTON_API;

 procedure NewtonTreeCollisionSetUserRayCastCallback (const treeCollision : NewtonCollision; rayHitCallback : PNewtonCollisionTreeRayCastCallback); cdecl; external NEWTON_API;

 procedure NewtonTreeCollisionBeginBuild (const treeCollision : NewtonCollision); cdecl; external NEWTON_API;
 procedure NewtonTreeCollisionAddFace (const treeCollision : NewtonCollision; vertexCount : Integer; const vertexPtr : PdFloat; strideInBytes : Integer; faceAttribute : Integer); cdecl; external NEWTON_API;
 procedure NewtonTreeCollisionEndBuild (const treeCollision : NewtonCollision; optimize : Integer); cdecl; external NEWTON_API;

 function NewtonTreeCollisionGetFaceAtribute (const treeCollision : NewtonCollision; const faceIndexArray : PInteger; indexCount : Integer) : Integer; cdecl; external NEWTON_API;
 procedure NewtonTreeCollisionSetFaceAtribute (const treeCollision : NewtonCollision; const faceIndexArray : PInteger; indexCount : Integer; attribute : Integer); cdecl; external NEWTON_API;
 function NewtonTreeCollisionGetVertexListIndexListInAABB (const treeCollision : NewtonCollision; const p0 : PdFloat; const p1 : PdFloat; const vertexArray : PdFloat; vertexCount : PInteger;
                                                                 vertexStrideInBytes : PInteger; const indexList : PInteger; maxIndexCount : Integer; const faceAttribute : PInteger) : Integer; cdecl; external NEWTON_API;


 procedure NewtonStaticCollisionSetDebugCallback (const staticCollision : NewtonCollision; userCallback : PNewtonTreeCollisionCallback); cdecl; external NEWTON_API;

 // **********************************************************************************************
 //
 // General purpose collision library functions
 //
 // **********************************************************************************************

 function NewtonCollisionCreateInstance (const collision : NewtonCollision) : NewtonCollision; cdecl; external NEWTON_API;
 function NewtonCollisionGetType (const collision : NewtonCollision) : Integer; cdecl; external NEWTON_API;

 procedure NewtonCollisionSetUserData (const collision : NewtonCollision; userData : Pointer); cdecl; external NEWTON_API;
 function NewtonCollisionGetUserData (const collision : NewtonCollision) : Pointer; cdecl; external NEWTON_API;

 procedure NewtonCollisionSetUserID (const collision : NewtonCollision; id : Cardinal); cdecl; external NEWTON_API;
 function NewtonCollisionGetUserID (const collision : NewtonCollision) : Cardinal; cdecl; external NEWTON_API;

 procedure NewtonCollisionSetMatrix (const collision : NewtonCollision; const matrix : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonCollisionGetMatrix (const collision : NewtonCollision; matrix : PdFloat); cdecl; external NEWTON_API;

 procedure NewtonCollisionSetScale (const collision : NewtonCollision; scaleX : dFloat; scaleY : dFloat; scaleZ : dFloat); cdecl; external NEWTON_API;
 procedure NewtonCollisionGetScale (const collision : NewtonCollision; scaleX : dFloat; scaleY : dFloat; scaleZ : dFloat); cdecl; external NEWTON_API;
 procedure NewtonDestroyCollision (const collision : NewtonCollision); cdecl; external NEWTON_API;



 function NewtonCollisionPointDistance (const world : NewtonWorld; const point : PdFloat; const collision : NewtonCollision; const matrix : PdFloat;
                                        contact : PdFloat; normal : PdFloat; threadIndex : Integer) : Integer; cdecl; external NEWTON_API;

 function NewtonCollisionClosestPoint (const world : NewtonWorld;
                                       const collisionA : NewtonCollision; const matrixA : PdFloat; const collisionB : NewtonCollision; const matrixB : PdFloat;
                                       contactA : PdFloat; contactB : PdFloat; normalAB : PdFloat; threadIndex : Integer) : Integer; cdecl; external NEWTON_API;
 
 function NewtonCollisionCollide (const world : NewtonWorld; maxSize : Integer; const collisionA : NewtonCollision; const matrixA : PdFloat;
                                  const collisionB : NewtonCollision; const matrixB : PdFloat; contacts : PdFloat; normals : PdFloat;
                                  penetration : PdFloat; attribute : PInteger; threadIndex : Integer) : Integer; cdecl; external NEWTON_API;

 function NewtonCollisionCollideContinue (const world : NewtonWorld; maxSize : Integer; const timestep : dFloat; const collisionA : NewtonCollision;
                                          const matrixA : PdFloat; const velocA : PdFloat; const omegaA : PdFloat; const collisionB : NewtonCollision;
                                          const matrixB : PdFloat; const velocB : PdFloat; const omegaB : PdFloat; timeOfImpact : PdFloat; contacts : PdFloat;
                                          normals : PdFloat; penetration : PdFloat; threadIndex : Integer) : Integer; cdecl; external NEWTON_API;

 procedure NewtonCollisionSupportVertex (const collision : NewtonCollision; const dir : PdFloat; vertex : PdFloat); cdecl; external NEWTON_API;
 function NewtonCollisionRayCast (const collision : NewtonCollision; const p0 : PdFloat; const p1 : PdFloat; normal : PdFloat; attribute : PInteger) : dFloat; cdecl; external NEWTON_API;
 procedure NewtonCollisionCalculateAABB (const collision : NewtonCollision; const matrix : PdFloat; p0 : PdFloat; p1 : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonCollisionForEachPolygonDo (const collision : NewtonCollision; const matrix : PdFloat; callback : PNewtonCollisionIterator; userData : Pointer); cdecl; external NEWTON_API;

 
 
 // **********************************************************************************************
 //
 // transforms utility functions
 //
 // **********************************************************************************************
 procedure NewtonGetEulerAngle (const matrix : PdFloat; eulersAngles : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonSetEulerAngle (const eulersAngles : PdFloat; matrix : PdFloat); cdecl; external NEWTON_API;
 function NewtonCalculateSpringDamperAcceleration (dt : dFloat; ks : dFloat; x : dFloat; kd : dFloat; s : dFloat) : dFloat; cdecl; external NEWTON_API;

 // **********************************************************************************************
 //
 // body manipulation functions
 //
 // **********************************************************************************************
 function  NewtonCreateDynamicBody (const world : NewtonWorld; const collision : NewtonCollision; const matrix : PdFloat) : NewtonBody; cdecl; external NEWTON_API;
 function  NewtonCreateKinematicBody (const world : NewtonWorld; const collision : NewtonCollision; const matrix : PdFloat) : NewtonBody; cdecl; external NEWTON_API;

 procedure NewtonDestroyBody (const body : NewtonBody); cdecl; external NEWTON_API; //CHANGED 22.08.14

 function  NewtonBodyGetType (const body : NewtonBody) : Integer; cdecl; external NEWTON_API;

 procedure NewtonBodyAddForce (const body : NewtonBody; const force : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonBodyAddTorque (const body : NewtonBody; const torque : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonBodyCalculateInverseDynamicsForce (const body : NewtonBody; timestep : dFloat; const desiredVeloc : PdFloat; forceOut : PdFloat); cdecl; external NEWTON_API;

 procedure NewtonBodySetCentreOfMass (const body : NewtonBody; const com : PdFloat); cdecl; external NEWTON_API;
// procedure NewtonBodySetMassMatrix___ (const body : NewtonBody; mass : dFloat; Ixx : dFloat; Iyy : dFloat; Izz : dFloat); cdecl; external NEWTON_API;
 procedure NewtonBodySetMassMatrix (const body : NewtonBody; mass : dFloat; Ixx : dFloat; Iyy : dFloat; Izz : dFloat); cdecl; external NEWTON_API;

 procedure NewtonBodySetMassProperties (const body : NewtonBody; mass : dFloat; const collision : NewtonCollision); cdecl; external NEWTON_API;
 procedure NewtonBodySetMatrix (const body : NewtonBody; const matrix : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonBodySetMatrixRecursive (const body : NewtonBody; const matrix : PdFloat); cdecl; external NEWTON_API;

 procedure NewtonBodySetMaterialGroupID (const body : NewtonBody; id : Integer); cdecl; external NEWTON_API;
 procedure NewtonBodySetContinuousCollisionMode (const body : NewtonBody; state : Cardinal); cdecl; external NEWTON_API;
 procedure NewtonBodySetJointRecursiveCollision (const body : NewtonBody; state : Cardinal); cdecl; external NEWTON_API;
 procedure NewtonBodySetOmega (const body : NewtonBody; const omega : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonBodySetVelocity (const body : NewtonBody; const velocity : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonBodySetForce (const body : NewtonBody; const force : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonBodySetTorque (const body : NewtonBody; const torque : PdFloat); cdecl; external NEWTON_API;

 procedure NewtonBodySetLinearDamping (const body : NewtonBody; linearDamp : dFloat); cdecl; external NEWTON_API;
 procedure NewtonBodySetAngularDamping (const body : NewtonBody; const angularDamp : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonBodySetCollision (const body : NewtonBody; const collision : NewtonCollision); cdecl; external NEWTON_API;
 procedure NewtonBodySetCollisionScale (const body : NewtonBody; scaleX : dFloat; scaleY : dFloat; scaleZ : dFloat); cdecl; external NEWTON_API;

 function  NewtonBodyGetSleepState (const body : NewtonBody) : Integer; cdecl; external NEWTON_API;
 procedure NewtonBodySetSleepState (const body : NewtonBody; state : Integer); cdecl; external NEWTON_API;

 function  NewtonBodyGetAutoSleep (const body : NewtonBody) : Integer; cdecl; external NEWTON_API;
 procedure NewtonBodySetAutoSleep (const body : NewtonBody; state : Integer); cdecl; external NEWTON_API;

 function  NewtonBodyGetFreezeState(const body : NewtonBody) : Integer; cdecl; external NEWTON_API;
 procedure NewtonBodySetFreezeState (const body : NewtonBody; state : Integer); cdecl; external NEWTON_API;


// procedure NewtonBodySetAutoFreeze(const body : NewtonBody; state : Integer);
// procedure NewtonBodyCoriolisForcesMode (const body : NewtonBody; int mode);
// procedure NewtonBodySetGyroscopicForcesMode (const body : NewtonBody; int mode);
// NEWTON_API int  NewtonBodyGetGyroscopicForcesMode (const body : NewtonBody);
// NEWTON_API int  NewtonBodyGetFreezeState (const body : NewtonBody);
// procedure NewtonBodySetFreezeState  (const body : NewtonBody; state : Integer);
// procedure NewtonBodyGetFreezeTreshold (const body : NewtonBody; dFloat* freezeSpeed2, dFloat* freezeOmega2);
// procedure NewtonBodySetFreezeTreshold (const body : NewtonBody; dFloat freezeSpeed2, dFloat freezeOmega2, int framesCount);
// procedure NewtonBodySetAutoactiveCallback (const body : NewtonBody; NewtonBodyActivationState callback);
 

 procedure NewtonBodySetDestructorCallback (const body : NewtonBody; callback : PNewtonBodyDestructor); cdecl; external NEWTON_API;
 function NewtonBodyGetDestructorCallback (const body : NewtonBody) : PNewtonBodyDestructor; cdecl; external NEWTON_API;

 procedure NewtonBodySetTransformCallback (const body : NewtonBody; callback : PNewtonSetTransform); cdecl; external NEWTON_API;
 function NewtonBodyGetTransformCallback (const body : NewtonBody) : PNewtonSetTransform; cdecl; external NEWTON_API;

 procedure NewtonBodySetForceAndTorqueCallback (const body : NewtonBody; callback : PNewtonApplyForceAndTorque); cdecl; external NEWTON_API;
 function NewtonBodyGetForceAndTorqueCallback (const body : NewtonBody) : PNewtonApplyForceAndTorque; cdecl; external NEWTON_API;

 function NewtonBodyGetID (const body : NewtonBody) : Integer; cdecl; external NEWTON_API;

 procedure NewtonBodySetUserData (const body : NewtonBody; userData : Pointer); cdecl; external NEWTON_API;
 function NewtonBodyGetUserData (const body : NewtonBody) : Pointer; cdecl; external NEWTON_API;

 function NewtonBodyGetWorld (const body : NewtonBody) : NewtonWorld; cdecl; external NEWTON_API;
 function NewtonBodyGetCollision (const body : NewtonBody) : NewtonCollision; cdecl; external NEWTON_API;
 function NewtonBodyGetMaterialGroupID (const body : NewtonBody) : Integer; cdecl; external NEWTON_API;

 function NewtonBodyGetContinuousCollisionMode (const body : NewtonBody) : Integer; cdecl; external NEWTON_API;
 function NewtonBodyGetJointRecursiveCollision (const body : NewtonBody) : Integer; cdecl; external NEWTON_API;

 procedure  NewtonBodyGetMatrix(const body : NewtonBody; matrix : PdFloat); cdecl; external NEWTON_API;
 procedure  NewtonBodyGetRotation(const body : NewtonBody; rotation : PdFloat); cdecl; external NEWTON_API;
 procedure  NewtonBodyGetMassMatrix (const body : NewtonBody; mass : PdFloat; Ixx : PdFloat; Iyy : PdFloat; Izz : PdFloat); cdecl; external NEWTON_API;
 procedure  NewtonBodyGetInvMass(const body : NewtonBody; invMass : PdFloat; invIxx : PdFloat; invIyy : PdFloat; invIzz : PdFloat); cdecl; external NEWTON_API;
 procedure  NewtonBodyGetInertiaMatrix(const body : NewtonBody; inertiaMatrix : PdFloat); cdecl; external NEWTON_API;
 procedure  NewtonBodyGetInvInertiaMatrix(const body : NewtonBody; invInertiaMatrix : PdFloat); cdecl; external NEWTON_API;
 procedure  NewtonBodyGetOmega(const body : NewtonBody; vector : PdFloat); cdecl; external NEWTON_API;
 procedure  NewtonBodyGetVelocity(const body : NewtonBody; vector : PdFloat); cdecl; external NEWTON_API;
 procedure  NewtonBodyGetForce(const body : NewtonBody; vector : PdFloat); cdecl; external NEWTON_API;
 procedure  NewtonBodyGetTorque(const body : NewtonBody; vector : PdFloat); cdecl; external NEWTON_API;
 procedure  NewtonBodyGetForceAcc(const body : NewtonBody; vector : PdFloat); cdecl; external NEWTON_API;
 procedure  NewtonBodyGetTorqueAcc(const body : NewtonBody; vector : PdFloat); cdecl; external NEWTON_API;
 procedure  NewtonBodyGetCentreOfMass (const body : NewtonBody; com : PdFloat); cdecl; external NEWTON_API;

 procedure NewtonBodyGetPointVelocity (const body : NewtonBody; const point : PdFloat; velocOut : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonBodyAddImpulse (const body : NewtonBody; const pointDeltaVeloc : PdFloat; const pointPosit : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonBodyApplyImpulseArray (const body : NewtonBody; impuleCount : Integer; strideinBytes : Integer; const impulseArray : PdFloat; const pointArray : PdFloat); cdecl; external NEWTON_API;

 procedure NewtonBodyApplyImpulsePair (const body : NewtonBody; linearImpulse : PdFloat; angularImpulse : PdFloat); cdecl; external NEWTON_API;

 procedure NewtonBodyIntegrateVelocity (const body : NewtonBody; timestep : dFloat); cdecl; external NEWTON_API;

 function NewtonBodyGetLinearDamping (const body : NewtonBody) : dFloat; cdecl; external NEWTON_API;
 procedure  NewtonBodyGetAngularDamping (const body : NewtonBody; vector : PdFloat); cdecl; external NEWTON_API;
 procedure  NewtonBodyGetAABB (const body : NewtonBody; p0 : PdFloat; p1 : PdFloat); cdecl; external NEWTON_API;

 function NewtonBodyGetFirstJoint (const body : NewtonBody) : NewtonJoint; cdecl; external NEWTON_API;
 function NewtonBodyGetNextJoint (const body : NewtonBody; const joint : NewtonJoint) : NewtonJoint; cdecl; external NEWTON_API;
 function NewtonBodyGetFirstContactJoint (const body : NewtonBody) : NewtonJoint; cdecl; external NEWTON_API;
 function NewtonBodyGetNextContactJoint (const body : NewtonBody; const contactJoint : NewtonJoint) : NewtonJoint; cdecl; external NEWTON_API;

 procedure  NewtonBodyAddBuoyancyForce (const body : NewtonBody; fluidDensity : dFloat; fluidLinearViscosity : dFloat; fluidAngularViscosity : dFloat; const gravityVector : PdFloat; buoyancyPlane : PNewtonGetBuoyancyPlane; context : Pointer); cdecl; external NEWTON_API;


 // **********************************************************************************************
 //
 // contact joints interface
 //
 // **********************************************************************************************

 function NewtonContactJointGetFirstContact (const contactJoint : NewtonJoint) : Pointer; cdecl; external NEWTON_API;
 function NewtonContactJointGetNextContact (const contactJoint : NewtonJoint; contact : Pointer) : Pointer; cdecl; external NEWTON_API;

 function NewtonContactJointGetContactCount(const contactJoint : NewtonJoint) : Integer; cdecl; external NEWTON_API;
 procedure NewtonContactJointRemoveContact(const contactJoint : NewtonJoint; contact : Pointer); cdecl; external NEWTON_API;

 function NewtonContactGetMaterial (const contact : Pointer) : NewtonMaterial; cdecl; external NEWTON_API;


 // **********************************************************************************************
 //
 // Common joint functions
 //
 // **********************************************************************************************
 function NewtonJointGetUserData (const joint : NewtonJoint) : Pointer; cdecl; external NEWTON_API;
 procedure NewtonJointSetUserData (const joint : NewtonJoint; userData : Pointer); cdecl; external NEWTON_API;

 function NewtonJointGetBody0 (const joint : NewtonJoint) : NewtonBody; cdecl; external NEWTON_API;
 function NewtonJointGetBody1 (const joint : NewtonJoint) : NewtonBody; cdecl; external NEWTON_API;

 procedure NewtonJointGetInfo (const joint : NewtonJoint; info : PNewtonJointRecord); cdecl; external NEWTON_API;
 function NewtonJointGetCollisionState (const joint : NewtonJoint) : Integer; cdecl; external NEWTON_API;
 procedure NewtonJointSetCollisionState (const joint : NewtonJoint; state : Integer); cdecl; external NEWTON_API;

 function NewtonJointGetStiffness (const joint : NewtonJoint) : dFloat; cdecl; external NEWTON_API;
 procedure NewtonJointSetStiffness (const joint : NewtonJoint; state : dFloat); cdecl; external NEWTON_API;

 procedure NewtonDestroyJoint(const world : NewtonWorld; const joint : NewtonJoint); cdecl; external NEWTON_API;
 procedure NewtonJointSetDestructor (const joint : NewtonJoint; callback : PNewtonConstraintDestructor); cdecl; external NEWTON_API;



 // **********************************************************************************************
 //
 // particle system interface (soft bodies, individual, pressure bodies and cloth)
 //
 // **********************************************************************************************
 function NewtonCreateDeformableMesh (const world : NewtonWorld; mesh : NewtonMesh; shapeID : Integer) : NewtonCollision; cdecl; external NEWTON_API;

 procedure NewtonDeformableMeshSetPlasticity (deformableMesh : NewtonCollision; plasticity : dFloat); cdecl; external NEWTON_API;
 procedure NewtonDeformableMeshSetStiffness (deformableMesh : NewtonCollision; stiffness : dFloat); cdecl; external NEWTON_API;
 procedure NewtonDeformableMeshSetSkinThickness (deformableMesh : NewtonCollision; skinThickness : dFloat); cdecl; external NEWTON_API;

 function NewtonCreateDeformableBody (const world : NewtonWorld; const deformableMesh : NewtonCollision; const matrix : PdFloat) : NewtonBody; cdecl; external NEWTON_API;


 procedure NewtonDeformableMeshUpdateRenderNormals (const deformableMesh : NewtonCollision); cdecl; external NEWTON_API;
 function NewtonDeformableMeshGetVertexCount (const deformableMesh : NewtonCollision) : Integer; cdecl; external NEWTON_API;
 procedure NewtonDeformableMeshGetVertexStreams (const deformableMesh : NewtonCollision;
                                                 vertexStrideInBytes : Integer; vertex : PdFloat;
                                                 normalStrideInBytes : Integer; normal : PdFloat;
                                                 uvStrideInBytes0 : Integer; uv0 : PdFloat;
                                                 uvStrideInBytes1 : Integer; uv1 : PdFloat); cdecl; external NEWTON_API;
 function NewtonDeformableMeshGetFirstSegment (const deformableMesh : NewtonCollision) : NewtonDeformableMeshSegment; cdecl; external NEWTON_API;
 function NewtonDeformableMeshGetNextSegment (const deformableMesh : NewtonCollision; const segment : NewtonDeformableMeshSegment) : NewtonDeformableMeshSegment; cdecl; external NEWTON_API;

 function NewtonDeformableMeshSegmentGetMaterialID (const deformableMesh : NewtonCollision; const segment : NewtonDeformableMeshSegment) : Integer; cdecl; external NEWTON_API;
 function NewtonDeformableMeshSegmentGetIndexCount (const deformableMesh : NewtonCollision; const segment : NewtonDeformableMeshSegment) : Integer; cdecl; external NEWTON_API;
 function NewtonDeformableMeshSegmentGetIndexList (const deformableMesh : NewtonCollision; const segment : NewtonDeformableMeshSegment) : PSmallInt; cdecl; external NEWTON_API;




 // **********************************************************************************************
 //
 // Ball and Socket joint functions
 //
 // **********************************************************************************************
 function NewtonConstraintCreateBall (const world : NewtonWorld; const pivotPoint : PdFloat;
                                      const childBody : NewtonBody; const parentBody : NewtonBody) : NewtonJoint; cdecl; external NEWTON_API;
 procedure NewtonBallSetUserCallback (const ball : NewtonJoint; callback : PNewtonBallCallback); cdecl; external NEWTON_API;
 procedure NewtonBallGetJointAngle (const ball : NewtonJoint; angle : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonBallGetJointOmega (const ball : NewtonJoint; omega : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonBallGetJointForce (const ball : NewtonJoint; force : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonBallSetConeLimits (const ball : NewtonJoint; const pin : PdFloat; maxConeAngle : dFloat; maxTwistAngle : dFloat); cdecl; external NEWTON_API;

 // **********************************************************************************************
 //
 // Hinge joint functions
 //
 // **********************************************************************************************
 function NewtonConstraintCreateHinge (const world : NewtonWorld; const pivotPoint : PdFloat; const pinDir : PdFloat;
                                       const childBody : NewtonBody; const parentBody : NewtonBody) : NewtonJoint; cdecl; external NEWTON_API;

 procedure NewtonHingeSetUserCallback (const hinge : NewtonJoint; callback : PNewtonHingeCallback); cdecl; external NEWTON_API;
 function NewtonHingeGetJointAngle (const hinge : NewtonJoint) : dFloat; cdecl; external NEWTON_API;
 function NewtonHingeGetJointOmega (const hinge : NewtonJoint) : dFloat; cdecl; external NEWTON_API;
 procedure NewtonHingeGetJointForce (const hinge : NewtonJoint; force : PdFloat); cdecl; external NEWTON_API;
 function NewtonHingeCalculateStopAlpha (const hinge : NewtonJoint; const desc : PNewtonHingeSliderUpdateDesc; angle : dFloat) : dFloat; cdecl; external NEWTON_API;

 // **********************************************************************************************
 //
 // Slider joint functions
 //
 // **********************************************************************************************
 function NewtonConstraintCreateSlider (const world : NewtonWorld; const pivotPoint : PdFloat; const pinDir : PdFloat;
                                        const childBody : NewtonBody; const parentBody : NewtonBody) : NewtonJoint; cdecl; external NEWTON_API;
 procedure NewtonSliderSetUserCallback (const slider : NewtonJoint; callback : PNewtonSliderCallback); cdecl; external NEWTON_API;
 function NewtonSliderGetJointPosit (const slider : NewtonJoint) : dFloat; cdecl; external NEWTON_API;
 function NewtonSliderGetJointVeloc (const slider : NewtonJoint) : dFloat; cdecl; external NEWTON_API;
 procedure NewtonSliderGetJointForce (const slider : NewtonJoint; force : PdFloat); cdecl; external NEWTON_API;
 function NewtonSliderCalculateStopAccel (const slider : NewtonJoint; const desc : PNewtonHingeSliderUpdateDesc; position : dFloat) : dFloat; cdecl; external NEWTON_API;


 // **********************************************************************************************
 //
 // Corkscrew joint functions
 //
 // **********************************************************************************************
 function NewtonConstraintCreateCorkscrew (const world : NewtonWorld; const pivotPoint : PdFloat; const pinDir : PdFloat;
                                           const childBody : NewtonBody; const parentBody : NewtonBody) : NewtonJoint; cdecl; external NEWTON_API;
 procedure NewtonCorkscrewSetUserCallback (const corkscrew : NewtonJoint; callback : PNewtonCorkscrewCallback); cdecl; external NEWTON_API;
 function NewtonCorkscrewGetJointPosit (const corkscrew : NewtonJoint) : dFloat; cdecl; external NEWTON_API;
 function NewtonCorkscrewGetJointAngle (const corkscrew : NewtonJoint) : dFloat; cdecl; external NEWTON_API;
 function NewtonCorkscrewGetJointVeloc (const corkscrew : NewtonJoint) : dFloat; cdecl; external NEWTON_API;
 function NewtonCorkscrewGetJointOmega (const corkscrew : NewtonJoint) : dFloat; cdecl; external NEWTON_API;
 procedure NewtonCorkscrewGetJointForce (const corkscrew : NewtonJoint; force: PdFloat); cdecl; external NEWTON_API;
 function NewtonCorkscrewCalculateStopAlpha (const corkscrew : NewtonJoint; const desc : PNewtonHingeSliderUpdateDesc; angle: dFloat) : dFloat; cdecl; external NEWTON_API;
 function NewtonCorkscrewCalculateStopAccel (const corkscrew : NewtonJoint; const desc : PNewtonHingeSliderUpdateDesc; position: dFloat) : dFloat; cdecl; external NEWTON_API;


 // **********************************************************************************************
 //
 // Universal joint functions
 //
 // **********************************************************************************************
 function NewtonConstraintCreateUniversal (const world : NewtonWorld; const pivotPoint : PdFloat; const pinDir0 : PdFloat; const pinDir1 : PdFloat;
                                           const childBody : NewtonBody; const parentBody : NewtonBody) : NewtonJoint; cdecl; external NEWTON_API;
 procedure NewtonUniversalSetUserCallback (const universal : NewtonJoint; callback : PNewtonUniversalCallback); cdecl; external NEWTON_API;
 function NewtonUniversalGetJointAngle0 (const universal : NewtonJoint): dFloat; cdecl; external NEWTON_API;
 function NewtonUniversalGetJointAngle1 (const universal : NewtonJoint): dFloat; cdecl; external NEWTON_API;
 function NewtonUniversalGetJointOmega0 (const universal : NewtonJoint): dFloat; cdecl; external NEWTON_API;
 function NewtonUniversalGetJointOmega1 (const universal : NewtonJoint): dFloat; cdecl; external NEWTON_API;
 procedure NewtonUniversalGetJointForce (const universal : NewtonJoint; force : PdFloat); cdecl; external NEWTON_API;
 function NewtonUniversalCalculateStopAlpha0 (const universal : NewtonJoint; const desc : PNewtonHingeSliderUpdateDesc; angle : dFloat) : dFloat; cdecl; external NEWTON_API;
 function NewtonUniversalCalculateStopAlpha1 (const universal : NewtonJoint; const desc : PNewtonHingeSliderUpdateDesc; angle : dFloat) : dFloat; cdecl; external NEWTON_API;


 // **********************************************************************************************
 //
 // Up vector joint functions
 //
 // **********************************************************************************************
 function NewtonConstraintCreateUpVector (const world : NewtonWorld; const pinDir : PdFloat; const body : NewtonBody) : NewtonJoint; cdecl; external NEWTON_API;
 procedure NewtonUpVectorGetPin (const upVector : NewtonJoint; pin : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonUpVectorSetPin (const upVector : NewtonJoint; const pin : PdFloat); cdecl; external NEWTON_API;


 // **********************************************************************************************
 //
 // User defined bilateral Joint
 //
 // **********************************************************************************************
 function NewtonConstraintCreateUserJoint (const world : NewtonWorld; maxDOF : Integer; callback : PNewtonUserBilateralCallback;
                                           getInfo : PNewtonUserBilateralGetInfoCallback; const childBody : NewtonBody;
                                           const parentBody : NewtonBody) : NewtonJoint; cdecl; external NEWTON_API;
 procedure NewtonUserJointSetFeedbackCollectorCallback (const joint : NewtonJoint; getFeedback : PNewtonUserBilateralCallback); cdecl; external NEWTON_API;
 procedure NewtonUserJointAddLinearRow (const joint : NewtonJoint; const pivot0 : PdFloat; const pivot1 : PdFloat; const dir : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonUserJointAddAngularRow (const joint : NewtonJoint; relativeAngle : dFloat; const dir : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonUserJointAddGeneralRow (const joint : NewtonJoint; const jacobian0 : PdFloat; const jacobian1 : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonUserJointSetRowMinimumFriction (const joint : NewtonJoint; friction : dFloat); cdecl; external NEWTON_API;
 procedure NewtonUserJointSetRowMaximumFriction (const joint : NewtonJoint; friction : dFloat); cdecl; external NEWTON_API;
 procedure NewtonUserJointSetRowAcceleration (const joint : NewtonJoint; acceleration : dFloat); cdecl; external NEWTON_API;
 procedure NewtonUserJointSetRowSpringDamperAcceleration (const joint : NewtonJoint; springK : dFloat; springD : dFloat); cdecl; external NEWTON_API;
 procedure NewtonUserJointSetRowStiffness (const joint : NewtonJoint; stiffness : dFloat); cdecl; external NEWTON_API;
 function NewtonUserJointGetRowForce (const joint : NewtonJoint; row : Integer) : dFloat; cdecl; external NEWTON_API;

 procedure NewtonUserJointSetSolver (const joint : NewtonJoint; solver : Integer; maxContactJoints : Integer); cdecl; external NEWTON_API;


 // **********************************************************************************************
 //
 // Mesh joint functions
 //
 // **********************************************************************************************
 function NewtonMeshCreate(const world : NewtonWorld) : NewtonMesh; cdecl; external NEWTON_API;
 function NewtonMeshCreateFromMesh(const mesh : NewtonMesh) : NewtonMesh; cdecl; external NEWTON_API;
 function NewtonMeshCreateFromCollision(const collision : NewtonCollision) : NewtonMesh; cdecl; external NEWTON_API;
 function NewtonMeshCreateConvexHull (const world : NewtonWorld; pointCount : Integer; const vertexCloud : PdFloat; strideInBytes : Integer; tolerance : dFloat) : NewtonMesh; cdecl; external NEWTON_API;
 function NewtonMeshCreateDelaunayTetrahedralization (const world : NewtonWorld; pointCount : Integer; const vertexCloud : PdFloat; strideInBytes : Integer; materialID : Integer; const textureMatrix : PdFloat) : NewtonMesh; cdecl; external NEWTON_API;
 function NewtonMeshCreateVoronoiConvexDecomposition (const world : NewtonWorld; pointCount : Integer; const vertexCloud : PdFloat; strideInBytes : Integer; materialID : Integer; const textureMatrix : PdFloat; boderConvexSize : dFloat) : NewtonMesh; cdecl; external NEWTON_API;
 
 procedure NewtonMeshDestroy(const mesh : NewtonMesh); cdecl; external NEWTON_API;

 procedure NewtonMeshSaveOFF(const mesh : NewtonMesh; const filename : PChar); cdecl; external NEWTON_API;
 function NewtonMeshLoadOFF(const world : NewtonWorld; const filename : PChar) : NewtonMesh; cdecl; external NEWTON_API;

 procedure NewtonMesApplyTransform (const mesh : NewtonMesh; const matrix : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonMeshCalculateOOBB(const mesh : NewtonMesh; matrix : PdFloat; x : PdFloat; y : PdFloat; z : PdFloat); cdecl; external NEWTON_API;

 procedure NewtonMeshCalculateVertexNormals(const mesh : NewtonMesh; angleInRadians : dFloat); cdecl; external NEWTON_API;
 procedure NewtonMeshApplySphericalMapping(const mesh : NewtonMesh; material : Integer); cdecl; external NEWTON_API;
 procedure NewtonMeshApplyBoxMapping(const mesh : NewtonMesh; front : Integer; side : Integer; top : Integer); cdecl; external NEWTON_API;
 procedure NewtonMeshApplyCylindricalMapping(const mesh : NewtonMesh; cylinderMaterial : Integer; capMaterial : Integer); cdecl; external NEWTON_API;

 function NewtonMeshIsOpenMesh (const mesh : NewtonMesh) : Integer; cdecl; external NEWTON_API;
 procedure NewtonMeshFixTJoints (const mesh : NewtonMesh); cdecl; external NEWTON_API;

 procedure NewtonMeshPolygonize (const mesh : NewtonMesh); cdecl; external NEWTON_API;
 procedure NewtonMeshTriangulate (const mesh : NewtonMesh); cdecl; external NEWTON_API;
 function NewtonMeshUnion (const mesh : NewtonMesh; const clipper : NewtonMesh; const clipperMatrix : PdFloat) : NewtonMesh; cdecl; external NEWTON_API;
 function NewtonMeshDifference (const mesh : NewtonMesh; const clipper : NewtonMesh; const clipperMatrix : PdFloat) : NewtonMesh; cdecl; external NEWTON_API;
 function NewtonMeshIntersection (const mesh : NewtonMesh; const clipper : NewtonMesh; const clipperMatrix : PdFloat) : NewtonMesh; cdecl; external NEWTON_API;
 procedure NewtonMeshClip (const mesh : NewtonMesh; const clipper : NewtonMesh; const clipperMatrix : PdFloat; topMesh : NewtonMesh; bottomMesh : NewtonMesh); cdecl; external NEWTON_API;

 function NewtonMeshSimplify (const mesh : NewtonMesh; maxVertexCount : Integer; reportPrograssCallback : PNewtonReportProgress) : NewtonMesh; cdecl; external NEWTON_API;
 function NewtonMeshApproximateConvexDecomposition (const mesh : NewtonMesh; maxConcavity : dFloat; backFaceDistanceFactor : dFloat; maxCount : Integer; maxVertexPerHull : Integer; reportPrograssCallback : PNewtonReportProgress) : NewtonMesh; cdecl; external NEWTON_API;

 procedure NewtonRemoveUnusedVertices(const mesh : NewtonMesh; vertexRemapTable : PInteger); cdecl; external NEWTON_API;

 procedure NewtonMeshBeginFace(const mesh : NewtonMesh); cdecl; external NEWTON_API;
 procedure NewtonMeshAddFace(const mesh : NewtonMesh; vertexCount : Integer; const vertex : PdFloat; strideInBytes : Integer; materialIndex : Integer); cdecl; external NEWTON_API;
 procedure NewtonMeshEndFace(const mesh : NewtonMesh); cdecl; external NEWTON_API;

 procedure NewtonMeshBuildFromVertexListIndexList(const mesh : NewtonMesh; faceCount : Integer; const faceIndexCount : PInteger; const faceMaterialIndex : PInteger;
  const vertex : PdFloat; vertexStrideInBytes : Integer; const vertexIndex: PInteger; const normal : PdFloat; normalStrideInBytes : Integer; const normalIndex: PInteger;
  const uv0 : PdFloat; uv0StrideInBytes : Integer; const uv0Index : PInteger; const uv1 : PdFloat; uv1StrideInBytes : Integer; const uv1Index : PInteger); cdecl; external NEWTON_API;



 procedure NewtonMeshGetVertexStreams (const mesh : NewtonMesh; vertexStrideInByte : Integer; vertex : PdFloat;
                                       normalStrideInByte : Integer; normal : PdFloat; uvStrideInByte0 : Integer;
                                       uv0 : PdFloat; uvStrideInByte1 : Integer; uv1 : PdFloat); cdecl; external NEWTON_API;
 procedure NewtonMeshGetIndirectVertexStreams(const mesh : NewtonMesh; vertexStrideInByte : Integer;
                                              vertex : PdFloat; vertexIndices : PInteger; vertexCount : PInteger;
                                              normalStrideInByte : Integer; normal : PdFloat; normalIndices : PInteger;
                                              normalCount : PInteger; uvStrideInByte0 : Integer; uv0 : PdFloat;
                                              uvIndices0 : PInteger; uvCount0 : PInteger; uvStrideInByte1 : Integer;
                                              uv1 : PdFloat; uvIndices1 : PInteger; uvCount1 : PInteger); cdecl; external NEWTON_API;
 function NewtonMeshBeginHandle (const mesh : NewtonMesh) : NewtonMeshHandle; cdecl; external NEWTON_API;
 procedure NewtonMeshEndHandle (const mesh : NewtonMesh; handle : NewtonMeshHandle); cdecl; external NEWTON_API;
 function NewtonMeshFirstMaterial (const mesh : NewtonMesh; handle : NewtonMeshHandle) : Integer; cdecl; external NEWTON_API;
 function NewtonMeshNextMaterial (const mesh : NewtonMesh; handle : NewtonMeshHandle; materialId : Integer) : Integer; cdecl; external NEWTON_API;
 function NewtonMeshMaterialGetMaterial (const mesh : NewtonMesh; handle : NewtonMeshHandle; materialId : Integer) : Integer; cdecl; external NEWTON_API;
 function NewtonMeshMaterialGetIndexCount (const mesh : NewtonMesh; handle : NewtonMeshHandle; materialId : Integer) : Integer; cdecl; external NEWTON_API;
 procedure NewtonMeshMaterialGetIndexStream (const mesh : NewtonMesh; handle : NewtonMeshHandle; materialId : Integer; index : PInteger); cdecl; external NEWTON_API;
 procedure NewtonMeshMaterialGetIndexStreamShort (const mesh : NewtonMesh; handle : NewtonMeshHandle; materialId : Integer; index : PSmallInt); cdecl; external NEWTON_API;

 function NewtonMeshCreateFirstSingleSegment (const mesh : NewtonMesh) : NewtonMesh; cdecl; external NEWTON_API;
 function NewtonMeshCreateNextSingleSegment (const mesh : NewtonMesh; const segment : NewtonMesh) : NewtonMesh; cdecl; external NEWTON_API;

 function NewtonMeshCreateFirstLayer (const mesh : NewtonMesh) : NewtonMesh; cdecl; external NEWTON_API;
 function NewtonMeshCreateNextLayer (const mesh : NewtonMesh; const segment : NewtonMesh) : NewtonMesh; cdecl; external NEWTON_API;


 function NewtonMeshGetTotalFaceCount (const mesh : NewtonMesh) : Integer; cdecl; external NEWTON_API;
 function NewtonMeshGetTotalIndexCount (const mesh : NewtonMesh) : Integer; cdecl; external NEWTON_API;
 procedure NewtonMeshGetFaces (const mesh : NewtonMesh; faceIndexCount : PInteger; faceMaterial : PInteger; faceIndices : Pointer); cdecl; external NEWTON_API;


 function NewtonMeshGetPointCount (const mesh : NewtonMesh) : Integer; cdecl; external NEWTON_API;
 function NewtonMeshGetPointStrideInByte (const mesh : NewtonMesh) : Integer; cdecl; external NEWTON_API;
 function NewtonMeshGetPointArray (const mesh : NewtonMesh) : PdFloat64; cdecl; external NEWTON_API;
 function NewtonMeshGetNormalArray (const mesh : NewtonMesh) : PdFloat64; cdecl; external NEWTON_API;
 function NewtonMeshGetUV0Array (const mesh : NewtonMesh) : PdFloat64; cdecl; external NEWTON_API;
 function NewtonMeshGetUV1Array (const mesh : NewtonMesh) : PdFloat64; cdecl; external NEWTON_API;

 function NewtonMeshGetVertexCount (const mesh : NewtonMesh) : Integer; cdecl; external NEWTON_API;
 function NewtonMeshGetVertexStrideInByte (const mesh : NewtonMesh) : Integer; cdecl; external NEWTON_API;
 function NewtonMeshGetVertexArray (const mesh : NewtonMesh) : PdFloat64; cdecl; external NEWTON_API;


 function NewtonMeshGetFirstVertex (const mesh : NewtonMesh) : NewtonMeshVertex; cdecl; external NEWTON_API;
 function NewtonMeshGetNextVertex (const mesh : NewtonMesh; const vertex : NewtonMeshVertex) : NewtonMeshVertex; cdecl; external NEWTON_API;
 function NewtonMeshGetVertexIndex (const mesh : NewtonMesh; const vertex : NewtonMeshVertex) : Integer; cdecl; external NEWTON_API;

 function NewtonMeshGetFirstPoint (const mesh : NewtonMesh) : NewtonMeshPoint; cdecl; external NEWTON_API;
 function NewtonMeshGetNextPoint (const mesh : NewtonMesh; const point : NewtonMeshPoint) : NewtonMeshPoint; cdecl; external NEWTON_API;
 function NewtonMeshGetPointIndex (const mesh : NewtonMesh; const point : NewtonMeshPoint) : Integer; cdecl; external NEWTON_API;
 function NewtonMeshGetVertexIndexFromPoint (const mesh : NewtonMesh; const point : NewtonMeshPoint) : Integer; cdecl; external NEWTON_API;


 function NewtonMeshGetFirstEdge (const mesh : NewtonMesh) : NewtonMeshEdge; cdecl; external NEWTON_API;
 function NewtonMeshGetNextEdge (const mesh : NewtonMesh; const edge : NewtonMeshEdge) : NewtonMeshEdge; cdecl; external NEWTON_API;
 procedure NewtonMeshGetEdgeIndices (const mesh : NewtonMesh; const edge : NewtonMeshEdge; v0 : PInteger; v1 : PInteger); cdecl; external NEWTON_API;
 //procedure NewtonMeshGetEdgePointIndices (const mesh : NewtonMesh; const void* const edge, int* const v0, int* const v1);

 function NewtonMeshGetFirstFace (const mesh : NewtonMesh) : NewtonMeshFace; cdecl; external NEWTON_API;
 function NewtonMeshGetNextFace (const mesh : NewtonMesh; const face : NewtonMeshFace) : NewtonMeshFace; cdecl; external NEWTON_API;
 function NewtonMeshIsFaceOpen (const mesh : NewtonMesh; const face : NewtonMeshFace) : Integer; cdecl; external NEWTON_API;
 function NewtonMeshGetFaceMaterial (const mesh : NewtonMesh; const face : NewtonMeshFace) : Integer; cdecl; external NEWTON_API;
 function NewtonMeshGetFaceIndexCount (const mesh : NewtonMesh; const face : NewtonMeshFace) : Integer; cdecl; external NEWTON_API;
 procedure NewtonMeshGetFaceIndices (const mesh : NewtonMesh; const face : NewtonMeshFace; indices : PInteger); cdecl; external NEWTON_API;
 procedure NewtonMeshGetFacePointIndices (const mesh : NewtonMesh; const face : NewtonMeshFace; indices : PInteger); cdecl; external NEWTON_API;
 procedure NewtonMeshCalculateFaceNormal (const mesh : NewtonMesh; const face : NewtonMeshFace; normal : PdFloat64); cdecl; external NEWTON_API;

 procedure NewtonMeshSetFaceMaterial (const mesh : NewtonMesh; const face : NewtonMeshFace; matId : Integer); cdecl; external NEWTON_API;


implementation

end.
