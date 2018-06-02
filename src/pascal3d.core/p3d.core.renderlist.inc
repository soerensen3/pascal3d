{ include.inc }

{$IFDEF INTERFACE}

{ TP3DRenderObject }

TP3DRenderObject = class ( TObject )
  private
    FActiveSubset: String;
    FObjects: TP3DObjectPointerList;
    FData: TP3DStreamablePointer;
    FInstanceCount: Integer;
    FMaterial: TP3DStreamablePointer;
    FMatrices: TMat4List;
    FPositions: TP3DVec3BufferGL;
    FRotations: TP3DVec4BufferGL;
    FScalings: TP3DVec3BufferGL;
    function GetData: TP3DDataBlock;
    function GetMaterial: TP3DMaterial;
    procedure SetData(AValue: TP3DDataBlock);
    procedure SetMaterial(AValue: TP3DMaterial);

  public
    Modifiers: array of TP3DRenderListModifier;

    constructor Create;
    destructor Destroy; override;

    function CollideWithRenderObject( ARenderObject: TP3DRenderObject; out Intersect: TVec3; out N: TVec3 ): Boolean;
    function CollideWithSphere( ASphere: TP3DSphere; out Intersect: TVec3; out N: TVec3 ): Boolean;
    function CollideWithAABB( AAABB: TP3DAABB; out Intersect: TVec3; out N: TVec3 ): Boolean;

    property Data: TP3DDataBlock read GetData write SetData;
    property Positions: TP3DVec3BufferGL read FPositions write FPositions;
    property Rotations: TP3DVec4BufferGL read FRotations write FRotations;
    property Scalings: TP3DVec3BufferGL read FScalings write FScalings;
    property Matrices: TMat4List read FMatrices write FMatrices;
    property Material: TP3DMaterial read GetMaterial write SetMaterial;
    property InstanceCount: Integer read FInstanceCount write FInstanceCount;
    property ActiveSubset: String read FActiveSubset write FActiveSubset;
end;

TP3DRenderListScoreFunc = function ( Item: TP3DRenderObject ): Integer;
TP3DRenderListModifierList = specialize gP3DCustomObjectList < TP3DRenderListModifier >;

{ TP3DCustomRenderList }

TP3DCustomRenderList = class( specialize gP3DCustomObjectList < TP3DRenderObject >)
  private
    FModifierStack: TP3DRenderListModifierList;

  protected
    function PushRenderObject( AData: TP3DDataBlock; AMatrix: TMat4; AMaterial: TP3DMaterial ): Integer; virtual;

    property ModifierStack: TP3DRenderListModifierList read FModifierStack write FModifierStack;

  public
    constructor Create;
    destructor Destroy; override;

    function Find( AData: TP3DDataBlock; Material: TP3DMaterial ): Integer;
end;

{ TP3DLightRenderList }

TP3DLightRenderList = class( TP3DCustomRenderList )
  public
    function PushRenderObject( AData: TP3DDataBlock; AMatrix: TMat4; AMaterial: TP3DMaterial ): Integer; override;
    procedure PassToActiveShader( matView: TMat4; Params: TP3DLightInformationSet );
end;

{ TP3DRenderListModifier }

TP3DRenderListModifier = class ( TPersistent )
  public
    procedure PassToShader( Obj: TP3DRenderObject ); virtual; abstract;
    procedure AddObject( var Obj: TP3DRenderObject ); virtual; abstract;
end;


type
TP3DRenderListAddCallBack = procedure ( AData: TP3DDataBlock; AWorld: TMat4; AScene: TP3DScene; AFlags: TP3DRenderListFlags; var Accept: Boolean ) of object;

TP3DRenderList = class ( TP3DCustomRenderList )
  private
    FActiveSubset: String;
    FLights: TP3DLightRenderList;
    FOnAdd: TP3DRenderListAddCallBack;
    FTransparencyStart: Integer;
    FIBLFactor: Float;
    FIBLDiffuse: TP3DStreamablePointer;
    FIBLSpecular: TP3DStreamablePointer;
    FIBLIntegrateBRDF: TP3DStreamablePointer;
    function GetIBLDiffuse: TP3DTexture;
    function GetIBLIntegrateBRDF: TP3DTexture;
    function GetIBLSpecular: TP3DTexture;
    procedure SetIBLDiffuse(AValue: TP3DTexture);
    procedure SetIBLIntegrateBRDF(AValue: TP3DTexture);
    procedure SetIBLSpecular(AValue: TP3DTexture);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add( AData: TP3DDataBlock; AWorld: TMat4; AScene: TP3DScene; const AFlags: TP3DRenderListFlags = P3DRenderListFlagsDefault ); reintroduce;
    function PickFromCamera( X, Y: Integer; Camera: TP3DObject ): TP3DObject;
    procedure Render( Camera: TP3DObject; const AFlags: TP3DRenderListFlags = P3DRenderListFlagsDefault );
    procedure SetupMaterial( Material: TP3DMaterial; Camera: TP3DObject; const AFlags: TP3DRenderListFlags = P3DRenderListFlagsDefault );
    procedure RenderMesh( Idx: Integer; Camera: TP3DObject; const AFlags: TP3DRenderListFlags = P3DRenderListFlagsDefault );
    procedure Sort( Func: TP3DRenderListScoreFunc; lo, hi: Integer );
    procedure Optimize;
    procedure Clear; reintroduce;
    function PushRenderObject(AData: TP3DDataBlock; AMatrix: TMat4; AMaterial: TP3DMaterial): Integer; override;
    function CheckCollision( RenderList: TP3DRenderList; out A, B: TP3DRenderObject; out Intersect: TVec3; out N: TVec3 ): Boolean;
    function CheckCollision( ASphere: TP3DSphere; out B: TP3DRenderObject; out Intersect: TVec3; out N: TVec3 ): Boolean;
    function CheckCollision( AAABB: TP3DAABB; out B: TP3DRenderObject; out Intersect: TVec3; out N: TVec3 ): Boolean;

    property Lights: TP3DLightRenderList read FLights write FLights;
    property OnAdd: TP3DRenderListAddCallBack read FOnAdd write FOnAdd;
    property ActiveSubset: String read FActiveSubset write FActiveSubset;
    property IBLDiffuse: TP3DTexture read GetIBLDiffuse write SetIBLDiffuse;
    property IBLIntegrateBRDF: TP3DTexture read GetIBLIntegrateBRDF write SetIBLIntegrateBRDF;
    property IBLSpecular: TP3DTexture read GetIBLSpecular write SetIBLSpecular;
    property IBLFactor: Float read FIBLFactor write FIBLFactor;
end;


{$ENDIF}

{$IFDEF IMPLEMENTATION}

function P3DRenderListScoreTransp( Item: TP3DRenderObject ): Integer;
begin
  if ( Assigned( Item.Material )) then
    Result:= Integer( Item.Material.HasTransparency )
  else
    Result:= 99999;
end;

{ TP3DLightRenderList }

function TP3DLightRenderList.PushRenderObject(AData: TP3DDataBlock; AMatrix: TMat4; AMaterial: TP3DMaterial): Integer;
begin
  if ( AData is TP3DLight ) then
    Result:= inherited PushRenderObject(AData, AMatrix, AMaterial)
  else
    Result:= -1;
end;

procedure TP3DLightRenderList.PassToActiveShader( matView: TMat4; Params: TP3DLightInformationSet );
var
  numLights: Integer;
  i, j: Integer;
  l: String;
  pos: TVec4;
  obj: TP3DDataBlock;

begin
  if ( not Assigned( P3DShaderActive )) then
    exit;

  numLights:= 0;
  for i:= 0 to Count - 1 do
    for j:= 0 to Items[ i ].Positions.Count - 1 do
      with ( Items[ i ].Data as TP3DLight ) do
        begin
          l:= 'LightSource[' + IntToStr( numLights ) + ']';

          if ( liLightParams in Params ) then
            begin
              //Color
              glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.color' ), Color.R, Color.G, Color.B, Energy ); cle( Self );

              //linear Attenuation
              glUniform1f( P3DShaderActive.Uniforms.AddrByName( l + '.linearAttenuation' ), LinearAttenuation ); cle( Self );

              //quadratic Attenuation
              glUniform1f( P3DShaderActive.Uniforms.AddrByName( l + '.quadraticAttenuation' ), QuadraticAttenuation ); cle( Self );

              //range
              glUniform1f( P3DShaderActive.Uniforms.AddrByName( l + '.range' ), GetLightRange ); cle( Self );

              //type
              glUniform1i( P3DShaderActive.Uniforms.AddrByName( l + '.type' ), Ord( LightType )); cle( Self );
            end;

          if ( liPosition in Params ) then
            begin
              //position in viewspace
              pos:= matView * vec4( Items[ i ].Positions[ j ], 1 );
              glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.position' ), pos.X, pos.Y, pos.Z, pos.W ); cle( Self );
              pos:= vec4( Items[ i ].Positions[ j ], 1 );
              glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.worldposition' ), pos.X, pos.Y, pos.Z, pos.W ); cle( Self );
              pos:= vec4( normalize( mat3( matView ) * Items[ i ].Matrices[ j ].Row[ 2 ].XYZ ), 0 );
              glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.direction' ), pos.X, pos.Y, pos.Z, pos.W ); cle( Self );
              pos:= vec4( normalize( Items[ i ].Matrices[ j ].Row[ 2 ].XYZ ), 0 );
              glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.worlddirection' ), pos.X, pos.Y, pos.Z, pos.W ); cle( Self );
              pos:= normalize( -pos + vec4( 0, 0, 1, 0 ));
              glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.halfvector' ), pos.X, pos.Y, pos.Z, pos.W ); cle( Self );
              //light_vect = light_position - face_center_position
              //cam_vect = cam_position - face_center_position
              //halfangle_vect = (light_vect.normal() + cam_vect.normal()).normal()
            end;
          Inc( numLights );
          //WriteLn( P3DShaderActive.DumpUniforms );
        end;
  glUniform1i( P3DShaderActive.Uniforms.AddrByName( 'numLightSource'), numLights ); cle( Self );
end;


{ TP3DCustomRenderList }

function TP3DCustomRenderList.PushRenderObject(AData: TP3DDataBlock; AMatrix: TMat4; AMaterial: TP3DMaterial): Integer;
var
  Obj: TP3DRenderObject;
  q: TQuat;
  i: Integer;
  loc, scale: TVec3;
  rot: TMat3;
begin
  Result:= Find( AData, AMaterial );
  {if ( Result > -1 ) then
    begin
      Obj:= Items[ Result ];
      mat4decompose( AMatrix, loc, rot, scale );
      q:= quat( rot );
      Obj.Rotations.Add( q );
      Obj.Positions.Add( loc );
      Obj.Scalings.Add( scale );
      Obj.Matrices.Add( AMatrix );
      Obj.InstanceCount:= Obj.InstanceCount + 1;
    end
  else}
    begin
      Obj:= TP3DRenderObject.Create;
      Obj.Data:= AData;

      {if ( Assigned( Obj.Data )) then
        WriteLn( 'Creating RenderObject with data ' + Obj.Data.Name )
      else
        WriteLn( 'Creating RenderObject without data.' );}

      mat4decompose( AMatrix, loc, rot, scale );
      q:= quat( rot );
      Obj.Rotations.Add( vec4( q ));
      Obj.Positions.Add( loc );
      Obj.Scalings.Add( scale );
      Obj.Matrices.Add( AMatrix );
      Obj.Material:= AMaterial;
      Obj.InstanceCount:= 1;
      SetLength( Obj.Modifiers, ModifierStack.Count );
      for i:= 0 to ModifierStack.Count - 1 do
        if ( Assigned( ModifierStack[ i ])) then
          begin
            Obj.Modifiers[ i ]:= ModifierStack[ i ];
            Obj.Modifiers[ i ].AddObject( Obj )
          end;
      Result:= inherited Add( Obj );
    end;
end;

constructor TP3DCustomRenderList.Create;
begin
  inherited Create;
  ModifierStack:= TP3DRenderListModifierList.Create;
end;

destructor TP3DCustomRenderList.Destroy;
begin
  FreeAndNil( FModifierStack );
  inherited Destroy;
end;


{ TP3DRenderList }

function TP3DRenderList.GetIBLDiffuse: TP3DTexture;
begin
  Result:= TP3DTexture( FIBLDiffuse.AsValue );
end;

function TP3DRenderList.GetIBLIntegrateBRDF: TP3DTexture;
begin
  Result:= TP3DTexture( FIBLIntegrateBRDF.AsValue );
end;

function TP3DRenderList.GetIBLSpecular: TP3DTexture;
begin
  Result:= TP3DTexture( FIBLSpecular.AsValue );
end;

procedure TP3DRenderList.SetIBLDiffuse(AValue: TP3DTexture);
begin
  FIBLDiffuse.AsValue:= AValue;
end;

procedure TP3DRenderList.SetIBLIntegrateBRDF(AValue: TP3DTexture);
begin
  FIBLIntegrateBRDF.AsValue:= AValue;
end;

procedure TP3DRenderList.SetIBLSpecular(AValue: TP3DTexture);
begin
  FIBLSpecular.AsValue:= AValue;
end;

constructor TP3DRenderList.Create;
begin
  inherited Create;
  Lights:= TP3DLightRenderList.Create;
  ActiveSubset:= '';
  FIBLSpecular:= TP3DStreamablePointer.Create( 'IBLSpecular', TP3DTexture );
  FIBLDiffuse:= TP3DStreamablePointer.Create( 'IBLDiffuse', TP3DTexture );
  FIBLIntegrateBRDF:= TP3DStreamablePointer.Create( 'IBLIntegrateBRDF', TP3DTexture );
end;

destructor TP3DRenderList.Destroy;
begin
  inherited Clear;
  FIBLSpecular.Free;
  FIBLDiffuse.Free;
  FIBLIntegrateBRDF.Free;
  Lights.Clear;
  FreeAndNil( FLights );
  inherited Destroy;
end;

procedure TP3DRenderList.Add(AData: TP3DDataBlock; AWorld: TMat4; AScene: TP3DScene; const AFlags: TP3DRenderListFlags);
var
  Accept: Boolean;
begin
  if ( not ( AData is IP3DRenderable )) then
    exit;
  if ( Assigned( FOnAdd )) then
    FOnAdd( AData, AWorld, AScene, AFlags, Accept )
  else
    Accept:= True;
  if ( Accept ) then
    ( AData as IP3DRenderable ).AddToRenderList( AScene, AWorld, Self, AFlags );
end;

function TP3DRenderList.PickFromCamera(X, Y: Integer; Camera: TP3DObject ): TP3DObject;
begin
  {glScissor( X, Y, 1, 1 );
  glEnable( GL_SCISSOR_TEST );
  if ( not Assigned( PickMaterial )) then
    begin
      PickMaterial.Create();
      PickMaterial.Unlit:= True;
    end;
  PickMaterial.Diff:= vec3( 0 );
  PickMaterial.PassToShader();

  glDisable( GL_SCISSOR_TEST );}
end;

function TP3DCustomRenderList.Find(AData: TP3DDataBlock; Material: TP3DMaterial): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if (( Self[ i ].Data = AData ) and ( Self[ i ].Material = Material )) then
      begin
        Result:= i;
        break;
      end;
end;

procedure TP3DRenderList.Render(Camera: TP3DObject; const AFlags: TP3DRenderListFlags);

  function Mat4ListToTexture( m: TMat4List ): TP3DTexture;
    function nextPowerOfTwo( Value: Integer ): Integer;
    begin
      Dec( Value );
      Value:= Value or Value shr 1;
      Value:= Value or Value shr 2;
      Value:= Value or Value shr 4;
      Value:= Value or Value shr 8;
      Value:= Value or Value shr 16;
      Inc( Value );
      Result:= Value;
    end;

  var
    size, i, oldc: Integer;
    m2: TMat4List;
  begin
    size:= Max( 4, nextPowerOfTwo( m.Count * 4 )); //Max( 4, ceil( sqrt( power( 2, ceil( log2( m.Count * 4 ))))));

    oldc:= m.Count;
    m.Count:= ( size * size ) div 4;
    //m2:= TMat4List.Create;
    //m.AssignTo( m2 );
    //for i:= m2.Count to ( size*size ) div 4 do
    //  m2.Add( mat4( 0 ));
    Result:= TP3DTexture.CreateFromBuffer( size, size, 4, dtFloat, m.Ptr );
    m.Count:= oldc;
    Result.MipMap:= False;
    Result.Filtering:= tfNearest;
    Result.FilteringMipMap:= tfNearest;
    //m2.Free;
  end;

var
  cam: TP3DCamera;
  mat: TP3DMaterial;
  i, j: Integer;
  idx: LongInt;
  decl: TP3DShaderDecl;
  //iv: array [0..3] of byte absolute i;
  Diff: TVec3;
begin
  if (( not Assigned( Camera )) or ( not ( Camera.Data is TP3DCamera ))) then
    exit;
  cam:= TP3DCamera( Camera.Data );
  if ( cam.View <> Camera.Transform.MatrixInv ) then
    cam.UpdateMatrices( Camera );
  mat:= P3DMaterialActive;
  if ( Assigned( mat )) then
    SetupMaterial( mat, Camera, AFlags );

  if ( rlfMaterials in AFlags ) then
    Optimize
  else
    FTransparencyStart:= Count;

  if ( rlfDepthTest in AFlags ) then
    begin
      glEnable( GL_DEPTH_TEST );
      glDepthMask( True );
    end
  else
    begin
      glDisable( GL_DEPTH_TEST );
      glDepthMask( False );
    end;
  if ( rlfWireFrame in AFlags ) then
    glPolygonMode( GL_FRONT_AND_BACK, GL_LINE );

  for i:= 0 to Count - 1 do
    if ( Items[ i ].Data is TP3DMesh ) then
      begin
        RenderMesh( i, Camera, AFlags );
      end;
  if ( Assigned( P3DShaderActive ) and P3DShaderActive.Attributes.FindByName( 'instanceID', decl )) then
    begin
      glVertexAttrib1f( decl.Addr, 0.0 );
      glVertexAttribDivisorARB( decl.Addr, 0 ); cle( Self );
    end;

  if ( rlfWireFrame in AFlags ) then
    glPolygonMode( GL_FRONT_AND_BACK, GL_FILL );

  glDepthMask( True );
  glEnable( GL_DEPTH_TEST );
end;

procedure TP3DRenderList.SetupMaterial(Material: TP3DMaterial; Camera: TP3DObject; const AFlags: TP3DRenderListFlags);
var
  decl: TP3DShaderDecl;
begin
  Material.PassToShader( ActiveSubset );
  if ( not Assigned( P3DShaderActive )) then
    exit;
  TP3DCamera( Camera.Data ).PassToShader();
  if ( rlfLighting in AFlags ) then
    Lights.PassToActiveShader( TP3DCamera( Camera.Data ).View, [ liLightParams, liPosition ]);
  if ( Assigned( IBLDiffuse ) and ( P3DShaderActive.Uniforms.FindByName( 'IBLDiffuse', decl ))) then
    begin
      //IBLDiffuse.Bind( Material.Maps.Count );
      glActiveTexture( GL_TEXTURE0 + Material.Maps.Count );
      glBindTexture( {GL_TEXTURE_CUBE_MAP} GL_TEXTURE_2D, IBLDiffuse.GLTexture );
      decl.AsInt:= Material.Maps.Count;
    end;
  if ( Assigned( IBLSpecular ) and ( P3DShaderActive.Uniforms.FindByName( 'IBLSpecular', decl ))) then
    begin
      //IBLSpecular.Bind( Material.Maps.Count + 1 );
      glActiveTexture( GL_TEXTURE0 + Material.Maps.Count + 1 );
      glBindTexture( {GL_TEXTURE_CUBE_MAP} GL_TEXTURE_2D, IBLSpecular.GLTexture );
      decl.AsInt:= Material.Maps.Count + 1;
    end;
  if ( P3DShaderActive.Uniforms.FindByName( 'IBLFactor', decl )) then
    decl.AsFloat:= IBLFactor;
  if ( Assigned( IBLIntegrateBRDF ) and ( P3DShaderActive.Uniforms.FindByName( 'IBLIntegrateBRDF', decl ))) then
    begin
      //IBLSpecular.Bind( Material.Maps.Count + 1 );
      glActiveTexture( GL_TEXTURE0 + Material.Maps.Count + 2 );
      glBindTexture( GL_TEXTURE_2D, IBLIntegrateBRDF.GLTexture );
      decl.AsInt:= Material.Maps.Count + 2;
    end;
end;

procedure TP3DRenderList.RenderMesh(Idx: Integer; Camera: TP3DObject; const AFlags: TP3DRenderListFlags);
var
  j, n: Integer;
  decl: TP3DShaderDecl;
  ObjProp: IP3DPropAccessStreamable;
  P: TP3DPropertyAccess;
begin
  if ( Items[ Idx ].Data is TP3DMesh ) then
    begin
      if ( Idx = FTransparencyStart ) then
        glDepthMask( False );

      if (( rlfMaterials in AFlags ) and (( P3DMaterialActive <> Items[ Idx ].Material ) or ( ActiveSubset <> Items[ Idx ].ActiveSubset ))) then
        begin
          ActiveSubset:= Items[ Idx ].ActiveSubset;
          if ( not Assigned( Items[ Idx ].Material )) then
            exit;
          SetupMaterial( Items[ Idx ].Material, Camera, AFlags );
        end;
      if ( not Assigned( P3DMaterialActive )) then
        raise Exception.Create( 'Error rendering mesh! No material is assigned! If you rendered without rlfMaterials it means you have to enable an material before. If you rendered with rlfMaterials it means that a material in the mesh is nil.' );
      if ( rlfIndexColors in AFlags ) then
        begin
          //Diff:= vec3( iv[ 0 ], iv[ 1 ], iv[ 2 ]) / 255;
//          glVertexAttrib4f( P3DAttribColor,
//            Diff.R, Diff.G, Diff.B, 1.0 ); cle( nil );
        end;

      with( TP3DMesh( Items[ Idx ].Data )) do
        begin
          if ( ActiveSubset > '' ) then
            begin
              P:= OwnerLib.Properties.GetPropByPath( ActiveSubset );
              if ( Assigned( P ) and P.GetInterface( IP3DPropAccessStreamable, ObjProp )) then
                if ( Assigned( ObjProp.GetValueAsStreamable )) then
                  for n:= 0 to TP3DObject( ObjProp.GetValueAsStreamable ).Modifiers.Count - 1 do
                    TP3DObject( ObjProp.GetValueAsStreamable ).Modifiers[ n ].PassToShader( P3DMaterialActive, Mat4Identity );
            end;
          if ( not Assigned( P3DShaderActive )) then
            exit;
          {if ( rlfMaterials in AFlags ) then
            n:= MaterialGroups.Find( P3DMaterialActive )
          else
            n:= -1;}

          if ( P3DShaderActive.Uniforms.FindByName( 'instanceGridWidth', decl )) then
            decl.AsInt:= 1;

          if ( P3DShaderActive.Uniforms.FindByName( 'instanceGridWorldSize', decl )) then
            decl.AsFloat:= 1.0;

          if ( P3DShaderActive.Uniforms.FindByName( 'world', decl )) then
            decl.AsMat4:= Items[ Idx ].Matrices[ 0 ];

          if ( P3DShaderActive.Attributes.FindByName( 'instanceID', decl )) then
            begin
//                P3DMaterialActive.Shader.DumpAttributes;
              glVertexAttrib1f( decl.Addr, 0.0 );
              glVertexAttribDivisorARB( decl.Addr, Items[ Idx ].InstanceCount div Items[ Idx ].Positions.Count ); cle( Self );
//                glDisableVertexAttribArray( decl.Addr ); //Disable by default
//                Items[ Idx ].Material.Shader.DumpAttributes;
            end;

          if ( P3DShaderActive.Attributes.FindByName( 'instance_position', decl )) then
            begin
              Items[ Idx ].Positions.PushData();
              Items[ Idx ].Positions.Divisor:= Items[ Idx ].InstanceCount div Items[ Idx ].Positions.Count;
              Items[ Idx ].Positions.SetAttribArray( decl.Addr );
              //WriteLn( 'Positions divisor:', Items[ Idx ].Positions.Divisor );
              //glVertexAttribDivisorARB( decl.Addr, Items[ Idx ].InstanceCount div Items[ Idx ].Positions.Count ); cle( Self );
            end;

          if ( P3DShaderActive.Attributes.FindByName( 'instance_rotation', decl )) then
            begin
              Items[ Idx ].Rotations.PushData();
              Items[ Idx ].Rotations.Divisor:= Items[ Idx ].InstanceCount div Items[ Idx ].Rotations.Count;
              Items[ Idx ].Rotations.SetAttribArray( decl.Addr );
              //WriteLn( 'Rotations divisor:', Items[ Idx ].Rotations.Divisor );
              //glVertexAttribDivisorARB( decl.Addr, Items[ Idx ].InstanceCount div Items[ Idx ].Rotations.Count ); cle( Self );
            end;

          if ( P3DShaderActive.Attributes.FindByName( 'instance_scaling', decl )) then
            begin
              Items[ Idx ].Scalings.PushData();
              Items[ Idx ].Scalings.Divisor:= Items[ Idx ].InstanceCount div Items[ Idx ].Scalings.Count;
              Items[ Idx ].Scalings.SetAttribArray( decl.Addr );
              //WriteLn( 'Scalings divisor:', Items[ Idx ].Scalings.Divisor );
              //glVertexAttribDivisorARB( decl.Addr, Items[ Idx ].InstanceCount div Items[ Idx ].Scalings.Count ); cle( Self );
            end;

          for j:= 0 to high( Items[ Idx ].Modifiers ) do
            Items[ Idx ].Modifiers[ j ].PassToShader( Items[ Idx ]);

          if ( rlfRenderEdges in AFlags ) then
            RenderEdgesInstanced( Items[ Idx].InstanceCount )
          else
            RenderInstanced( Items[ Idx ].InstanceCount );

          if ( P3DShaderActive.Attributes.FindByName( 'instanceID', decl )) then
            begin
//                P3DMaterialActive.Shader.DumpAttributes;
              glVertexAttrib1f( decl.Addr, 0.0 );
              glVertexAttribDivisorARB( decl.Addr, 0 ); cle( Self );
//                glDisableVertexAttribArray( decl.Addr ); //Disable by default
//                Items[ Idx ].Material.Shader.DumpAttributes;
            end;

          //WriteLn( P3DMaterialActive.Shader.DumpAttributes );
        end;
    end;
end;

procedure TP3DRenderList.Sort(Func: TP3DRenderListScoreFunc; lo, hi: Integer);
  procedure swap( a, b: Integer );
  var
    Element: TP3DRenderObject;
  begin
    Element:= Self[ a ];
    FItems[ a ]:= Self[ b ];
    FItems[ b ]:= Element;
  end;

  function partition( lo, hi: Integer ): Integer;
  var
    pivot, i, j: Integer;
  begin
    pivot:= Func( Self[ hi ]);
    i:= lo;        // place for swapping
    for j := lo to hi - 1 do
      if ( Func( Self[ j ]) <= pivot ) then
        begin
          swap( i, j );
          Inc( i );
        end;
    swap( i, hi );
    Result:= i;
  end;

  procedure quicksort( lo, hi: Integer );
  var
    p: Integer;
  begin
    if lo < hi then
      begin
        p:= partition( lo, hi );
        quicksort( lo, p - 1 );
        quicksort( p + 1, hi );
      end;
  end;

begin
  FItems.OwnsObjects:= False;
  quicksort( lo, hi );
  FItems.OwnsObjects:= True;
end;

procedure TP3DRenderList.Optimize;
var
  i: Integer;
begin
  Sort( @P3DRenderListScoreTransp, 0, Count - 1 );

  FTransparencyStart:= Count;
  for i:= 0 to Count - 1 do
    if ( Assigned( Items[ i ].Material ) and
      ( Items[ i ].Material.HasTransparency )) then
      begin
        FTransparencyStart:= i;
        break;
      end;
end;

procedure TP3DRenderList.Clear;
begin
  inherited Clear;
  Lights.Clear;
  ModifierStack.Clear;
end;

function TP3DRenderList.PushRenderObject(AData: TP3DDataBlock; AMatrix: TMat4; AMaterial: TP3DMaterial): Integer;
begin
  Result:= inherited PushRenderObject(AData, AMatrix, AMaterial);
  Items[ Result ].ActiveSubset:= ActiveSubset;
end;

function TP3DRenderList.CheckCollision(RenderList: TP3DRenderList; out A, B: TP3DRenderObject; out Intersect: TVec3; out N: TVec3): Boolean;
var
  i, j: Integer;
begin
  Result:= False;
  A:= nil;
  B:= nil;
  for i:= 0 to Count - 1 do
    for j:= 0 to RenderList.Count - 1 do
      if ( Self[ i ].CollideWithRenderObject( RenderList[ j ], Intersect, N )) then
        begin
          Result:= True;
          A:= Self[ i ];
          B:= RenderList[ j ];
        end;
end;

function TP3DRenderList.CheckCollision(ASphere: TP3DSphere; out B: TP3DRenderObject; out Intersect: TVec3; out N: TVec3): Boolean;
var
  i: Integer;
begin
  Result:= False;
  B:= nil;
  for i:= 0 to Count - 1 do
    if ( Self[ i ].CollideWithSphere( ASphere, Intersect, N )) then
      begin
        Result:= True;
        B:= Self[ i ];
        break;
      end;
end;

function TP3DRenderList.CheckCollision(AAABB: TP3DAABB; out B: TP3DRenderObject; out Intersect: TVec3; out N: TVec3): Boolean;
var
  i: Integer;
begin
  Result:= False;
  B:= nil;
  for i:= 0 to Count - 1 do
    if ( Self[ i ].CollideWithAABB( AAABB, Intersect, N )) then
      begin
        Result:= True;
        B:= Self[ i ];
        break;
      end;
end;


{ TP3DRenderObject }

function TP3DRenderObject.GetData: TP3DDataBlock;
begin
  Result:= TP3DDataBlock( FData.AsValue );
end;

function TP3DRenderObject.GetMaterial: TP3DMaterial;
begin
  Result:= TP3DMaterial( FMaterial.AsValue );
end;

procedure TP3DRenderObject.SetData(AValue: TP3DDataBlock);
begin
  FData.AsValue:= AValue;
end;

procedure TP3DRenderObject.SetMaterial(AValue: TP3DMaterial);
begin
  FMaterial.AsValue:= AValue;
end;

constructor TP3DRenderObject.Create;
begin
  inherited Create;
  FPositions:= TP3DVec3BufferGL.Create;
  FRotations:= TP3DVec4BufferGL.Create;
  FScalings:= TP3DVec3BufferGL.Create;
  FObjects:= TP3DObjectPointerList.Create( 'Objects' );
  FMatrices:= TMat4List.Create;
  FData:= TP3DStreamablePointer.Create( 'Data', TP3DDataBlock );
  FMaterial:= TP3DStreamablePointer.Create( 'Material', TP3DMaterial );
  Material:= nil;
  Data:= nil;
  InstanceCount:= 0;
end;

destructor TP3DRenderObject.Destroy;
begin
  FPositions.Free;
  FRotations.Free;
  FScalings.Free;
  FMatrices.Free;
  FObjects.Free;
  SetLength( Modifiers, 0 );
  FData.Free;
  FMaterial.Free;
  {if ( Assigned( Data )) then
    WriteLn( 'Destroying RenderObject with data ' + Data.Name )
  else
    WriteLn( 'Destroying RenderObject without data.' );}
  inherited Destroy;
end;

function TP3DRenderObject.CollideWithRenderObject(ARenderObject: TP3DRenderObject; out Intersect: TVec3; out N: TVec3): Boolean;
var
  i, j: Integer;
begin
  Result:= False;
  if (( ARenderObject.Data is TP3DObject ) and ( Self.Data is TP3DObject ) and
      ( TP3DObject( ARenderObject.Data ).Collision <> acNone ) and ( TP3DObject( Self.Data ).Collision <> acNone )) then
    for i:= 0 to Self.Matrices.Count - 1 do
      for j:= 0 to ARenderObject.Matrices.Count - 1 do
        if ( TP3DObject( Data ).CollideObject( Self.Matrices[ i ], ARenderObject.Matrices[ j ], TP3DObject( ARenderObject.Data ), Intersect, N )) then
          begin
            Result:= True;
            break;
          end;
end;

function TP3DRenderObject.CollideWithSphere(ASphere: TP3DSphere; out Intersect: TVec3; out N: TVec3): Boolean;
var
  i: Integer;
begin
  Result:= False;
  if (( Self.Data is TP3DObject ) and ( TP3DObject( Self.Data ).Collision <> acNone )) then
    for i:= 0 to Self.Matrices.Count - 1 do
      if ( TP3DObject( Data ).CollideSphere( Self.Matrices[ i ], ASphere, Intersect, N )) then
        begin
          Result:= True;
          break;
        end;
end;

function TP3DRenderObject.CollideWithAABB(AAABB: TP3DAABB; out Intersect: TVec3; out N: TVec3): Boolean;
var
  i: Integer;
begin
  Result:= False;
  if (( Self.Data is TP3DObject ) and ( TP3DObject( Self.Data ).Collision <> acNone )) then
    for i:= 0 to Self.Matrices.Count - 1 do
      if ( TP3DObject( Data ).CollideAABB( Self.Matrices[ i ], AAABB, Intersect, N )) then
        begin
          Result:= True;
          break;
        end;
end;
{$ENDIF}

