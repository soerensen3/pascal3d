//p3dscene.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}
type
  { TP3DRenderQueryObject }

  TP3DRenderQueryObject = class
    private
      FObj: TP3DObject;
      FMatrix: TMat4;
      FScene: TP3DScene;

    public
      property Obj: TP3DObject read FObj write FObj;
      property Matrix: TMat4 read FMatrix write FMatrix;
      property Scene: TP3DScene read FScene write FScene;
  end;

  { TP3DRenderObject }

  TP3DRenderObject = class ( TP3DInterfacedPersistent )
    private
      FActiveSubset: String;
      FObjects: TP3DObjectList;
      FData: TP3DDataBlock;
      FInstanceCount: Integer;
      FMaterial: TP3DMaterial;
      FMatrices: TMat4List;
      FPositions: TP3DVec3BufferGL;
      FRotations: TP3DVec4BufferGL;
      FScalings: TP3DVec3BufferGL;

    public
      Modifiers: array of TP3DRenderListModifier;

      constructor Create;
      destructor Destroy; override;

      function CollideWithRenderObject( ARenderObject: TP3DRenderObject; out Intersect: TVec3; out N: TVec3 ): Boolean;
      function CollideWithSphere( ASphere: TP3DSphere; out Intersect: TVec3; out N: TVec3 ): Boolean;
      function CollideWithAABB( AAABB: TP3DAABB; out Intersect: TVec3; out N: TVec3 ): Boolean;

      property Data: TP3DDataBlock read FData write FData;
      property Positions: TP3DVec3BufferGL read FPositions write FPositions;
      property Rotations: TP3DVec4BufferGL read FRotations write FRotations;
      property Scalings: TP3DVec3BufferGL read FScalings write FScalings;
      property Matrices: TMat4List read FMatrices write FMatrices;
      property Material: TP3DMaterial read FMaterial write FMaterial;
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
      property LastUpdate: Integer read FLastUpdate;
      property OnAdd: TP3DRenderListAddCallBack read FOnAdd write FOnAdd;
      property ActiveSubset: String read FActiveSubset write FActiveSubset;
  end;

  { TP3DScene }

  TP3DSceneRenderCallback = procedure ( world: TMat4; AObject: TP3DObject ) of object;
  TP3DScene = class( TP3DDataBlock, IP3DRenderable )
    private
      FCam: TP3DObject;
      FIsDesignTime: Boolean;
      FInheritedDesignTime: Boolean;
      FLights: TP3DLightList;
      FObjects: TP3DObjectList;
      FActiveCam: TP3DObject;
      FOnRenderCallback: TP3DSceneRenderCallback;
      FRootScene: TP3DScene;

      procedure SetCam( AValue: TP3DObject );
      procedure RenderCallback( world: TMat4; AObject: TP3DObject ); virtual;
      procedure OnObjChange( Sender: TP3DNamedInterfacedPersistentList; Item: TP3DObject; AAction: TP3DCustomListOnChangeAction );
      function GetDataPath: String; override;

    protected
      property ActiveCam: TP3DObject read FActiveCam write FActiveCam;
      property RootScene: TP3DScene read FRootScene write FRootScene;
      property Lights: TP3DLightList read FLights write FLights;

    public
      class function GetContainer( ALib: TP3DLibrary): TP3DNamedInterfacedPersistentList; override;
      constructor Create( const AData: TP3DLibrary = nil ); override;
      constructor Create( AFileName: String; const AWatchFileForChanges: Boolean = False; const AData: TP3DLibrary = nil );
      procedure FastDestroy; override;

      procedure ClearFields; override;
      procedure UpdateLights;

      procedure Clear;

//      function Debug: String;

      procedure Render(world: TMat4; Scene: TP3DScene; RenderObject: TP3DObject);
      procedure Render(const ARootScene: TP3DScene=nil);
      procedure MakeRenderList( ARootScene: TP3DScene; world: TMat4; var ARenderList: TP3DRenderList; const AFlags: TP3DRenderListFlags = P3DRenderListFlagsDefault );
      procedure AddToRenderList( ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags );
      procedure RenderFromCamera( ACam: TP3DObject );
      procedure RenderFromCamera( ACam: TP3DObject; world: TMat4 );
      procedure AppendFile( AFileName: String );
      procedure AppendFromData(AData: TP3DLibrary);
      procedure PassToShader; virtual;
      procedure RemoveFromFields( Block: TP3DDataBlock ); override;
      //function Pick( X,Y: Integer; const ARootScene: TP3DScene=nil ): TP3DObject;

      property Objects: TP3DObjectList read FObjects;
      property Cam: TP3DObject read FCam write SetCam;
      property IsDesignTime: Boolean read FIsDesignTime write FIsDesignTime;
      property OnRenderCallback: TP3DSceneRenderCallback read FOnRenderCallback write FOnRenderCallback;
  end;
  TP3DSceneList = specialize gP3DNamedInterfacedPersistentList < TP3DScene >;
  TP3DPropertyAccessScene = specialize gP3DPropertyAccessDataBlock < TP3DScene >;
  TP3DPropertyAccessSceneList = specialize gP3DPropertyAccessDataBlockList < TP3DSceneList, TP3DScene, TP3DPropertyAccessScene >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

function P3DRenderListScoreTransp( Item: TP3DRenderObject ): Integer;
begin
  Result:= Integer( Item.Material.HasTransparency );
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
              pos:= vec4( normalize( mat3( matView ) * Items[ i ].Matrices[ j ].Row[ 2 ].XYZ ), 0 );
              glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.direction' ), pos.X, pos.Y, pos.Z, pos.W ); cle( Self );
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
  q: TQuaternion;
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
      Obj.Rotations.Add( q );
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

constructor TP3DRenderList.Create;
begin
  inherited Create;
  Lights:= TP3DLightRenderList.Create;
  ActiveSubset:= '';
end;

destructor TP3DRenderList.Destroy;
begin
  inherited Clear( True );
  Lights.Clear( True );
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
  if ( not ( Camera.Data is TP3DCamera )) then
    exit;
  cam:= TP3DCamera( Camera.Data );
  mat:= P3DMaterialActive;
  if ( Assigned( mat )) then
    SetupMaterial( mat, Camera, AFlags );

  Optimize;
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
begin
  Material.PassToShader( ActiveSubset );
  TP3DCamera( Camera.Data ).PassToShader();
  if ( rlfLighting in AFlags ) then
    Lights.PassToActiveShader( TP3DCamera( Camera.Data ).View, [ liLightParams, liPosition ]);
end;

procedure TP3DRenderList.RenderMesh(Idx: Integer; Camera: TP3DObject; const AFlags: TP3DRenderListFlags);
var
  n, j: Integer;
  decl: TP3DShaderDecl;
  Obj: TP3DPropertyAccess;
begin
  if ( Items[ Idx ].Data is TP3DMesh ) then
    begin
      if ( Idx = FTransparencyStart ) then
        glDepthMask( False );
      if (( rlfMaterials in AFlags ) and (( P3DMaterialActive <> Items[ Idx ].Material ) or ( ActiveSubset <> Items[ Idx ].ActiveSubset ))) then
        begin
          ActiveSubset:= Items[ Idx ].ActiveSubset;
          SetupMaterial( Items[ Idx ].Material, Camera, AFlags );
        end;
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
              Obj:= Owner.Properties.GetPropByPath( ActiveSubset );
              if (( Obj is TP3DPropertyAccessObject ) and ( Assigned( TP3DPropertyAccessObject( Obj ).Value ))) then
                for n:= 0 to TP3DPropertyAccessObject( Obj ).Value.Modifiers.Count - 1 do
                  TP3DPropertyAccessObject( Obj ).Value.Modifiers[ n ].PassToShader( P3DMaterialActive, Mat4Identity );
            end;
          if ( rlfMaterials in AFlags ) then
            n:= MaterialGroups.Find( P3DMaterialActive )
          else
            n:= -1;

          if ( P3DShaderActive.Uniforms.FindByName( 'instanceGridWidth', decl )) then
            decl.AsInt:= 1;

          if ( P3DShaderActive.Uniforms.FindByName( 'instanceGridWorldSize', decl )) then
            decl.AsFloat:= 1.0;

          if ( P3DShaderActive.Uniforms.FindByName( 'world', decl )) then
            decl.AsMat4:= Mat4Identity;

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
            RenderMaterialGroupInstanced( Items[ Idx ].InstanceCount, n );

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
  quicksort( lo, hi );
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
  inherited Clear( True );
  Lights.Clear( True );
  ModifierStack.Clear( False );
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

{ TP3DScene }

procedure TP3DScene.SetCam(AValue: TP3DObject);
begin
  if FCam=AValue then Exit;

  if ( Assigned( FCam )) then
    FCam.RemoveUser( Self );

  FCam:= AValue;

  if ( Assigned( FCam )) then
    FCam.AddUser( Self );
end;

procedure TP3DScene.RenderCallback(world: TMat4; AObject: TP3DObject);
begin
  if ( Assigned( OnRenderCallback )) then
    OnRenderCallback( world, AObject );
end;

procedure TP3DScene.OnObjChange(Sender: TP3DNamedInterfacedPersistentList; Item: TP3DObject; AAction: TP3DCustomListOnChangeAction);
begin
  if ( AAction = actAdd ) then
    begin
      Item.AddUser( Self );
      Item.FScene:= Self;
      Item.FParent:= Self;
    end
  else
    begin
      Item.RemoveUser( Self );
      Item.FScene:= nil;
      Item.FParent:= nil;
    end;
end;

function TP3DScene.GetDataPath: String;
begin
  Result:= 'Scenes[ "' + Name + '" ]';
end;

class function TP3DScene.GetContainer( ALib: TP3DLibrary): TP3DNamedInterfacedPersistentList;
begin
  Result:= ALib.Scenes;
end;

constructor TP3DScene.Create( const AData: TP3DLibrary );
begin
  inherited Create( AData );
  FObjects:= TP3DObjectList.Create( Self );
  FObjects.OnChange:= @OnObjChange;
  Lights:= TP3DLightList.Create( Self );
  //Owner.Scenes.Add( Self );
  Properties.Add( TP3DPropertyAccessObject.CreateFieldSet( 'Cam', @FCam, @SetCam, smAttribute ));
  Properties.Add( TP3DPropertyAccessObjectList.CreateFieldSet( 'Objects', @FObjects, nil, smAttribute ));
end;

constructor TP3DScene.Create(AFileName: String; const AWatchFileForChanges: Boolean; const AData: TP3DLibrary);
begin
  if ( not FileExists( AFileName )) then
    raise Exception.Create( ClassName + ': Cannot load scene from file "' + AFileName + '". The specified file does not exist!' );
  Create( AData );
  AppendFile( AFileName );
end;

procedure TP3DScene.FastDestroy;
begin
  Objects.Free;
  Lights.Free;
  //Owner.Scenes.Remove( Self );
  inherited FastDestroy;
end;

procedure TP3DScene.ClearFields;
begin
  Cam:= nil;
  Lights.Clear();
  Objects.Clear();
  ActiveCam:= nil;
  RootScene:= nil;
  inherited ClearFields;
end;

procedure TP3DScene.UpdateLights;
var
  Obj: TP3DObject;
  light: TP3DLight;
begin
  Lights.Clear();
  for Obj in Objects do
    if ( Obj.Data is TP3DLight ) then
      Lights.Add( TP3DLight( Obj.Data ))
    else if ( Obj.Data is TP3DScene ) then
      begin
        TP3DScene( Obj.Data ).UpdateLights;
        for light in TP3DScene( Obj.Data ).Lights do
          Lights.Add( light );
      end;
  {WriteLn( Name + ' light list:' );
  for light in Lights do
    WriteLn( ExtractFileName( light.Parent.FileWatch.FileName ) + '.' + light.Name );
  WriteLn( '-------------------' );}
end;


procedure TP3DScene.Clear;
begin
  Objects.Clear( False );
end;

procedure TP3DScene.Render(world: TMat4; Scene: TP3DScene;
  RenderObject: TP3DObject);
begin
  if ( Assigned( Scene )) then
    if ( Assigned( Scene.RootScene )) then
      RootScene:= Scene.RootScene
    else
      begin
        RootScene:= Scene;
        RootScene.UpdateLights;
      end;
  if ( Scene.FInheritedDesignTime or IsDesignTime ) then
    FInheritedDesignTime:= True;
  if ( Assigned( Cam )) then
    begin
      ActiveCam:= Cam;
      if ( Assigned( Cam.Data )) then
        TP3DCamera( Cam.Data ).UpdateMatrices( Cam );
      Objects.Render( world, Self );
    end
  else
    RenderFromCamera( Scene.ActiveCam, world );
end;

procedure TP3DScene.Render( const ARootScene: TP3DScene = nil );
begin
  ActiveCam:= Cam;
  RootScene:= ARootScene;
  if ( Assigned( Cam.Data )) then
    TP3DCamera( Cam.Data ).UpdateMatrices( Cam );
  UpdateLights;
  Objects.Render( Mat4Identity, Self );
end;

procedure TP3DScene.MakeRenderList(ARootScene: TP3DScene; world: TMat4; var ARenderList: TP3DRenderList; const AFlags: TP3DRenderListFlags);
begin
  if ( not Assigned( ARenderList )) then
    ARenderList:= TP3DRenderList.Create;
  if ( not Assigned( ARootScene )) then
    ARootScene:= Self;
  Objects.MakeRenderList( ARootScene, world, ARenderList, AFlags );
end;

procedure TP3DScene.AddToRenderList(ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags);
begin
  MakeRenderList( ARootScene, AWorld, AList, AFlags );
end;

procedure TP3DScene.RenderFromCamera( ACam: TP3DObject );
begin
  RenderFromCamera( ACam, Mat4Identity );
end;

procedure TP3DScene.RenderFromCamera( ACam: TP3DObject; world: TMat4 );
begin
  ActiveCam:= ACam;
  if ( Assigned( ACam.Data )) then
    TP3DCamera( ACam.Data ).UpdateMatrices( ACam );
  Objects.Render( world, Self );
end;

procedure TP3DScene.AppendFile( AFileName: String );
var
  n: Integer;
begin
  n:= P3DData.OpenLibrary( AFileName );
  AppendFromData( P3DData.Libraries[ n ]);
end;

procedure TP3DScene.AppendFromData( AData: TP3DLibrary );
var
  Scene: TP3DScene;
  Obj: TP3DObject;
begin
  for Scene in AData.Scenes do
    begin
      Obj:= TP3DObject.Create( Owner );
      Obj.Data:= Scene;
      Objects.Add( Obj );
    end;
end;

procedure TP3DScene.PassToShader;
var
  LightList: TP3DLightList;
begin
  if ( Assigned( ActiveCam )) then
    begin
      if ( Assigned( RootScene )) then
        LightList:= RootScene.Lights
      else
        LightList:= Lights;

      if ( ActiveCam.Data is TP3DCamera ) then
        begin
          //TP3DCamera( ActiveCam.Data ).UpdateMatrices( ActiveCam );
          TP3DCamera( ActiveCam.Data ).PassToShader;
          LightList.PassToActiveShader( TP3DCamera( ActiveCam.Data ).View, [ liPosition, liLightParams ]);
        end;
    end;
end;

procedure TP3DScene.RemoveFromFields( Block: TP3DDataBlock );
begin
  if ( Block = Cam ) then
    Cam:= nil;
  if ( Block is TP3DObject ) then
    Objects.Remove( TP3DObject( Block ));
  inherited RemoveFromFields( Block );
end;

{
function TP3DScene.Pick(X, Y: Integer; const ARootScene: TP3DScene): TP3DObject;
begin
  if ( not Assigned( ARootScene )) then
    begin
      glScissor( X, Y, 1, 1 );
      glEnable( GL_SCISSOR_TEST );
      if ( not Assigned( PickMaterial )) then
        PickMaterial.Create();
      PickMaterial.Diff:= vec3( 0 );
      PickMaterial.PassToShader();
    end;

  RenderCallbackOnly();

  if ( not Assigned( ARootScene )) then
    begin
      glDisable( GL_SCISSOR_TEST );
    end;
end;
}


{ TP3DRenderObject }


constructor TP3DRenderObject.Create;
begin
  inherited Create;
  FPositions:= TP3DVec3BufferGL.Create( True );
  FRotations:= TP3DVec4BufferGL.Create( True );
  FScalings:= TP3DVec3BufferGL.Create( True );
  FObjects:= TP3DObjectList.Create( Self );
  FMatrices:= TMat4List.Create;
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
