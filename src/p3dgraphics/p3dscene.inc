//p3dscene.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}
type

  { TP3DRenderQueryObject }

  TP3DRenderQueryObject = class
    private
      FActor: TP3DActor;
      FMatrix: TMat4;
      FScene: TP3DScene;

    public
      property Actor: TP3DActor read FActor write FActor;
      property Matrix: TMat4 read FMatrix write FMatrix;
      property Scene: TP3DScene read FScene write FScene;
  end;

  { TP3DRenderObject }

  TP3DRenderObject = class
    private
      FData: TP3DDataBlock;
      FInstanceCount: Integer;
      FMaterial: TP3DMaterial;
      FPositions: TP3DVec3BufferGL;
      FRotations: TP3DVec4BufferGL;
      FScalings: TP3DVec3BufferGL;

    public
      Modifiers: array of TP3DRenderListModifier;
      constructor Create;
      destructor Destroy; override;

      property Data: TP3DDataBlock read FData write FData;
      property Positions: TP3DVec3BufferGL read FPositions write FPositions;
      property Rotations: TP3DVec4BufferGL read FRotations write FRotations;
      property Scalings: TP3DVec3BufferGL read FScalings write FScalings;
      property Material: TP3DMaterial read FMaterial write FMaterial;
      property InstanceCount: Integer read FInstanceCount write FInstanceCount;
  end;

  TP3DCustomRenderList = specialize gP3DCustomObjectList < TP3DRenderObject >;

  { TP3DRenderList }

  TP3DRenderListScoreFunc = function ( Item: TP3DRenderObject ): Integer;

  { TP3DRenderListModifier }

  TP3DRenderListModifier = class ( TPersistent )
    public
      procedure PassToShader( Obj: TP3DRenderObject ); virtual; abstract;
      procedure AddObject( var Obj: TP3DRenderObject ); virtual; abstract;
  end;

  TP3DRenderListModifierList = specialize gP3DCustomObjectList < TP3DRenderListModifier >;

  TP3DRenderList = class ( TP3DCustomRenderList )
    private
      FLastUpdate: Integer;
      FLights: TP3DLightList;
      FModifierStack: TP3DRenderListModifierList;
      FTransparencyStart: Integer;

    protected
      property ModifierStack: TP3DRenderListModifierList read FModifierStack write FModifierStack;

    public
      constructor Create;
      destructor Destroy; override;

      function Add( AData: TP3DDataBlock; world: TMat4; AScene: TP3DScene; const AFlags: TP3DRenderListFlags = P3DRenderListFlagsDefault ): Integer; reintroduce;
      function PickFromCamera( X, Y: Integer; Camera: TP3DActor ): TP3DActor;
      function Find( AData: TP3DDataBlock; Material: TP3DMaterial ): Integer;
      procedure Render( Camera: TP3DActor; const AFlags: TP3DRenderListFlags = P3DRenderListFlagsDefault );
      procedure SetupMaterial( Material: TP3DMaterial; Camera: TP3DActor; const Fast: Boolean = False; const AFlags: TP3DRenderListFlags = P3DRenderListFlagsDefault );
      procedure RenderMesh( Idx: Integer; Camera: TP3DActor; const AFlags: TP3DRenderListFlags = P3DRenderListFlagsDefault );
      procedure Sort( Func: TP3DRenderListScoreFunc; lo, hi: Integer );
      procedure Optimize;

      property Lights: TP3DLightList read FLights write FLights;
      property LastUpdate: Integer read FLastUpdate;
  end;

  { TP3DScene }

  TP3DSceneRenderCallback = procedure ( world: TMat4; AActor: TP3DActor ) of object;
  TP3DScene = class( TP3DDataBlock )
    private
      FCam: TP3DActor;
      FIsDesignTime: Boolean;
      FInheritedDesignTime: Boolean;
      FLights: TP3DLightList;
      FObjects: TP3DActorList;
      FActiveCam: TP3DActor;
      FOnRenderCallback: TP3DSceneRenderCallback;
      FRootScene: TP3DScene;

      procedure SetCam( AValue: TP3DActor );
      procedure RenderCallback( world: TMat4; AActor: TP3DActor ); virtual;
      procedure OnObjChange( Sender: TPersistent; Item: TP3DActor; Action: TP3DCustomListOnChangeAction );

    protected
      property ActiveCam: TP3DActor read FActiveCam write FActiveCam;
      property RootScene: TP3DScene read FRootScene write FRootScene;
      property Lights: TP3DLightList read FLights write FLights;

    public
      constructor Create( const AData: TP3DLibrary = nil ); override;
      constructor Create( AFileName: String; const AWatchFileForChanges: Boolean = False; const AData: TP3DLibrary = nil );
      procedure LoadFromDOM(ADOMNode: TDOMElement); override;
      procedure LoadFromDOMNew(ADOMNode: TDOMElement); override;
      function SaveToDOM( AParent: TDOMElement ): TDOMElement; override;
      class function DOMNodeName: String; override;
      procedure FastDestroy; override;

      procedure ClearFields; override;
      procedure UpdateLights;

      procedure Clear;

//      function Debug: String;

      procedure Render(world: TMat4; Scene: TP3DScene; RenderObject: TP3DActor); override;
      procedure Render(const ARootScene: TP3DScene=nil); reintroduce;
      procedure MakeRenderList( ARootScene: TP3DScene; world: TMat4; var ARenderList: TP3DRenderList; const AFlags: TP3DRenderListFlags = P3DRenderListFlagsDefault );
      procedure RenderFromCamera( ACam: TP3DActor );
      procedure RenderFromCamera( ACam: TP3DActor; world: TMat4 );
      procedure AppendFile( AFileName: String );
      procedure AppendFromData(AData: TP3DLibrary);
      procedure PassToShader; virtual;
      procedure RemoveFromFields( Block: TP3DDataBlock ); override;
      //function Pick( X,Y: Integer; const ARootScene: TP3DScene=nil ): TP3DActor;

      property Objects: TP3DActorList read FObjects;
      property Cam: TP3DActor read FCam write SetCam;
      property IsDesignTime: Boolean read FIsDesignTime write FIsDesignTime;
      property OnRenderCallback: TP3DSceneRenderCallback read FOnRenderCallback write FOnRenderCallback;
  end;
  TP3DSceneList = specialize gP3DCustomDatablockList < TP3DScene >;
  TP3DPropertyAccessScene = specialize gP3DPropertyAccessDataBlock < TP3DScene >;
  TP3DPropertyAccessSceneList = specialize gP3DListPropertyAccess < TP3DSceneList, TP3DScene >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

function P3DRenderListScoreTransp( Item: TP3DRenderObject ): Integer;
begin
  Result:= Integer( Item.Material.HasTransparency );
end;

{ TP3DRenderList }

constructor TP3DRenderList.Create;
begin
  inherited Create;
  Lights:= TP3DLightList.Create;
  ModifierStack:= TP3DRenderListModifierList.Create;
end;

destructor TP3DRenderList.Destroy;
begin
  Clear( True );
  Lights.Clear( False );
  FreeAndNil( FLights );
  FreeAndNil( FModifierStack );
  inherited Destroy;
end;

function TP3DRenderList.Add(AData: TP3DDataBlock; world: TMat4; AScene: TP3DScene; const AFlags: TP3DRenderListFlags): Integer;
  function MakeRenderObject( AData: TP3DDataBlock; AMatrix: TMat4; AMaterial: TP3DMaterial ): Integer;
  var
    Obj: TP3DRenderObject;
    q: TQuaternion;
    i: Integer;
    loc, scale: TVec3;
    rot: TMat3;
  begin
    Result:= Find( AData, AMaterial );
    if ( Result > -1 ) then
      begin
        Obj:= Items[ Result ];
        mat4decompose( AMatrix, loc, rot, scale );
        q:= quat( rot );
        Obj.Rotations.Add( q );
        Obj.Positions.Add( loc );
        Obj.Scalings.Add( scale );
        Obj.InstanceCount:= Obj.InstanceCount + 1;
      end
    else
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

var
  i: Integer;
  mat: TP3DMaterial;
  Modifier: TP3DRenderListModifier;
begin
  if ( Assigned( AData )) then
    begin
      if ( AData is TP3DScene ) then
        TP3DScene( AData ).MakeRenderList( AScene, world, Self, AFlags )
      else if ( AData is TP3DMesh ) then
        begin
          if ( rlfMaterials in AFlags ) then
            for i:= 0 to TP3DMesh( AData ).MaterialGroups.Count - 1 do
              begin
                mat:= TP3DMesh( AData ).MaterialGroups[ i ].Material;
                Result:= MakeRenderObject( AData, world, mat );
              end
          else
            Result:= MakeRenderObject( AData, world, P3DMaterialActive );
        end
      else if ( AData is TP3DTileGrid ) then
        with ( TP3DTileGrid( AData )) do
          for i:= 0 to Scenes.Count - 1 do
            begin
              //Mods:= AModifiers;
              //SetLength( Mods, Length( Mods ) + 1 );
              //Mods[ high( Mods )]:= Modifiers[ i ];
              if ( Assigned( Scenes[ i ].Scene) and Assigned( Scenes[ i ].Modifier )) then
                begin
                  Modifier:= Scenes[ i ].Modifier;

                  ModifierStack.Add( Modifier );
                  Scenes[ i ].Scene.MakeRenderList( AScene.RootScene, world, Self, AFlags );
                  ModifierStack.Remove( Modifier );
                end;
            end
      else if ( AData is TP3DLight ) then
        Lights.Add( AData as TP3DLight );
    end
  else
    Result:= -1;
end;

function TP3DRenderList.PickFromCamera(X, Y: Integer; Camera: TP3DActor ): TP3DActor;
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

function TP3DRenderList.Find(AData: TP3DDataBlock; Material: TP3DMaterial): Integer;
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

procedure TP3DRenderList.Render(Camera: TP3DActor; const AFlags: TP3DRenderListFlags);

  function Mat4ListToTexture( m: TMat4List ): TP3DTexture;
    function nextPowerOfTwo( Value: Integer ): Integer;
    begin
      Dec( Value );
      value:= value or value shr 1;
      value:= value or value shr 2;
      value:= value or value shr 4;
      value:= value or value shr 8;
      value:= value or value shr 16;
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
  SetupMaterial( mat, Camera, False, AFlags );

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
      RenderMesh( i, Camera, AFlags );
  if ( P3DMaterialActive.Shader.Attributes.FindByName( 'instanceID', decl )) then
    begin
      glVertexAttrib1f( decl.Addr, 0.0 );
      glVertexAttribDivisorARB( decl.Addr, 0 ); cle( Self );
    end;

  if ( rlfWireFrame in AFlags ) then
    glPolygonMode( GL_FRONT_AND_BACK, GL_FILL );

  glDepthMask( True );
  glEnable( GL_DEPTH_TEST );
end;

procedure TP3DRenderList.SetupMaterial( Material: TP3DMaterial; Camera: TP3DActor; const Fast: Boolean; const AFlags: TP3DRenderListFlags );
begin
  Material.PassToShader();
  TP3DCamera( Camera.Data ).PassToShader();
  if ( rlfLighting in AFlags ) then
    Lights.PassToActiveShader( TP3DCamera( Camera.Data ).View, [ liLightParams, liPosition ]);
end;

procedure TP3DRenderList.RenderMesh(Idx: Integer; Camera: TP3DActor; const AFlags: TP3DRenderListFlags);
var
  n, j: Integer;
  decl: TP3DShaderDecl;
begin
  if ( Items[ Idx ].Data is TP3DMesh ) then
    begin
      if ( Idx = FTransparencyStart ) then
        glDepthMask( False );
      if (( rlfMaterials in AFlags ) and ( P3DMaterialActive <> Items[ Idx ].Material )) then
        SetupMaterial( Items[ Idx ].Material, Camera, False, AFlags );
      if ( rlfIndexColors in AFlags ) then
        begin
          //Diff:= vec3( iv[ 0 ], iv[ 1 ], iv[ 2 ]) / 255;
//          glVertexAttrib4f( P3DAttribColor,
//            Diff.R, Diff.G, Diff.B, 1.0 ); cle( nil );
        end;


      with( TP3DMesh( Items[ Idx ].Data )) do
        begin
          if ( rlfMaterials in AFlags ) then
            n:= MaterialGroups.Find( P3DMaterialActive )
          else
            n:= -1;

          if ( P3DMaterialActive.Shader.Uniforms.FindByName( 'instanceGridWidth', decl )) then
            decl.AsInt:= 1;

          if ( P3DMaterialActive.Shader.Uniforms.FindByName( 'instanceGridWorldSize', decl )) then
            decl.AsFloat:= 1.0;

          for j:= 0 to high( Items[ Idx ].Modifiers ) do
            Items[ Idx ].Modifiers[ j ].PassToShader( Items[ Idx ]);

          if ( P3DMaterialActive.Shader.Uniforms.FindByName( 'world', decl )) then
            decl.AsMat4:= Mat4Identity;

          if ( P3DMaterialActive.Shader.Attributes.FindByName( 'instanceID', decl )) then
            begin
//                P3DMaterialActive.Shader.DumpAttributes;
              glVertexAttrib1f( decl.Addr, 0.0 );
              glVertexAttribDivisorARB( decl.Addr, Items[ Idx ].InstanceCount div Items[ Idx ].Positions.Count ); cle( Self );
//                glDisableVertexAttribArray( decl.Addr ); //Disable by default
//                Items[ Idx ].Material.Shader.DumpAttributes;
            end;

          if ( P3DMaterialActive.Shader.Attributes.FindByName( 'instance_position', decl )) then
            begin
              Items[ Idx ].Positions.PushData();
              Items[ Idx ].Positions.Divisor:= Items[ Idx ].InstanceCount div Items[ Idx ].Positions.Count;
              Items[ Idx ].Positions.SetAttribArray( decl.Addr );
              //WriteLn( 'Positions divisor:', Items[ Idx ].Positions.Divisor );
              //glVertexAttribDivisorARB( decl.Addr, Items[ Idx ].InstanceCount div Items[ Idx ].Positions.Count ); cle( Self );
            end;

          if ( P3DMaterialActive.Shader.Attributes.FindByName( 'instance_rotation', decl )) then
            begin
              Items[ Idx ].Rotations.PushData();
              Items[ Idx ].Rotations.Divisor:= Items[ Idx ].InstanceCount div Items[ Idx ].Rotations.Count;
              Items[ Idx ].Rotations.SetAttribArray( decl.Addr );
              //WriteLn( 'Rotations divisor:', Items[ Idx ].Rotations.Divisor );
              //glVertexAttribDivisorARB( decl.Addr, Items[ Idx ].InstanceCount div Items[ Idx ].Rotations.Count ); cle( Self );
            end;

          if ( P3DMaterialActive.Shader.Attributes.FindByName( 'instance_scaling', decl )) then
            begin
              Items[ Idx ].Scalings.PushData();
              Items[ Idx ].Scalings.Divisor:= Items[ Idx ].InstanceCount div Items[ Idx ].Scalings.Count;
              Items[ Idx ].Scalings.SetAttribArray( decl.Addr );
              //WriteLn( 'Scalings divisor:', Items[ Idx ].Scalings.Divisor );
              //glVertexAttribDivisorARB( decl.Addr, Items[ Idx ].InstanceCount div Items[ Idx ].Scalings.Count ); cle( Self );
            end;
          if ( rlfRenderEdges in AFlags ) then
            RenderEdgesInstanced( Items[ Idx].InstanceCount )
          else
            RenderMaterialGroupInstanced( Items[ Idx ].InstanceCount, n );
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

{ TP3DScene }

procedure TP3DScene.SetCam(AValue: TP3DActor);
begin
  if FCam=AValue then Exit;

  if ( Assigned( FCam )) then
    FCam.RemoveUser( Self );

  FCam:= AValue;

  if ( Assigned( FCam )) then
    FCam.AddUser( Self );
end;

procedure TP3DScene.RenderCallback(world: TMat4; AActor: TP3DActor);
begin
  if ( Assigned( OnRenderCallback )) then
    OnRenderCallback( world, AActor );
end;

procedure TP3DScene.OnObjChange( Sender: TPersistent; Item: TP3DActor; Action: TP3DCustomListOnChangeAction );
begin
  if ( Action = actAdd ) then
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

constructor TP3DScene.Create( const AData: TP3DLibrary );
begin
  inherited Create( AData );
  FObjects:= TP3DActorList.Create( Self );
  FObjects.OnChange:= @OnObjChange;
  Lights:= TP3DLightList.Create;
  Owner.Scenes.Add( Self );
  Properties.Add( TP3DPropertyAccessActor.CreateFieldSet( 'Cam', @FCam, @SetCam, smAttribute ));
end;

constructor TP3DScene.Create(AFileName: String; const AWatchFileForChanges: Boolean; const AData: TP3DLibrary);
begin
  if ( not FileExists( AFileName )) then
    raise Exception.Create( ClassName + ': Cannot load scene from file "' + AFileName + '". The specified file does not exist!' );
  Create( AData );
  AppendFile( AFileName );
end;

procedure TP3DScene.LoadFromDOM(ADOMNode: TDOMElement);
var
  Element: TDOMElement;
  Child: TP3DActor;
  CamName: DOMString;
  n: Integer;
begin
  inherited LoadFromDOM( ADOMNode );
  Name:= ADOMNode.GetAttribute( 'name' );

  Element:= TDOMElement( ADOMNode.FirstChild );
  while ( Assigned( Element )) do
    begin
      case Element.NodeName of
        'object': begin Child:= TP3DActor.CreateFromDOM( Element, Owner ); Objects.Add( Child ); end;
      else
        raise Exception.Create( 'Unknown tag inside scene element: '+ Element.NodeName );
      end;
      Element:= TDOMElement( Element.NextSibling );
    end;
  CamName:= ADOMNode.GetAttribute( 'camera' );
  if ( CamName > '' ) then
    begin
      n:= Owner.AppendFromPath( CamName, TP3DActor );
      Cam:= TP3DActor( Owner.DataBlocks[ n ]);
    end;
end;

procedure TP3DScene.LoadFromDOMNew(ADOMNode: TDOMElement);
var
  Element: TDOMElement;
  Child: TP3DActor;
begin
  Element:= TDOMElement( ADOMNode.FirstChild );
  while ( Assigned( Element )) do
    begin
      case Element.NodeName of
        'object': begin Child:= TP3DActor.CreateFromDOM( Element, Owner ); Objects.Add( Child ); end;
      else
        raise Exception.Create( 'Unknown tag inside scene element: '+ Element.NodeName );
      end;
      Element:= TDOMElement( Element.NextSibling );
    end;

  inherited LoadFromDOMNew(ADOMNode);
end;

function TP3DScene.SaveToDOM(AParent: TDOMElement): TDOMElement;
begin
  Result:= inherited SaveToDOM( AParent );
  Objects.SaveToDOM( Result );
end;

class function TP3DScene.DOMNodeName: String;
begin
  Result:= 'scene';
end;

{
constructor TP3DScene.CreateFromDOM(ADOMNode: TDOMElement; const AData: TP3DData );
begin
  if ( not Assigned( ADOMNode )) then
    raise Exception.Create( ClassName + ': Cannot create from DOM Node. Not Assigned!' );

  if ( not ( ADOMNode.NodeName = 'p3dscene' )) then
    raise Exception.Create( ClassName + ': Cannot create from DOM Node. Scenes must begin with p3dscene!' );

  Create( AData );

  AppendFromDom( ADOMNode );
end;}

procedure TP3DScene.FastDestroy;
begin
  Objects.Free;
  Lights.Free;
  Owner.Scenes.Remove( Self );
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
  Obj: TP3DActor;
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
  RenderObject: TP3DActor);
begin
  inherited Render(world, Scene, RenderObject);
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

procedure TP3DScene.RenderFromCamera( ACam: TP3DActor );
begin
  RenderFromCamera( ACam, Mat4Identity );
end;

procedure TP3DScene.RenderFromCamera( ACam: TP3DActor; world: TMat4 );
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
  Obj: TP3DActor;
begin
  for Scene in AData.Scenes do
    begin
      Obj:= TP3DActor.Create( AData );
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
  if ( Block is TP3DActor ) then
    Objects.Remove( TP3DActor( Block ));
  inherited RemoveFromFields( Block );
end;

{
function TP3DScene.Pick(X, Y: Integer; const ARootScene: TP3DScene): TP3DActor;
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
  Material:= nil;
  Data:= nil;
  InstanceCount:= 0;
end;

destructor TP3DRenderObject.Destroy;
begin
  FPositions.Free;
  FRotations.Free;
  FScalings.Free;
  SetLength( Modifiers, 0 );
  {if ( Assigned( Data )) then
    WriteLn( 'Destroying RenderObject with data ' + Data.Name )
  else
    WriteLn( 'Destroying RenderObject without data.' );
  inherited Destroy;}
end;


{$ENDIF}
