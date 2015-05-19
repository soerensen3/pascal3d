unit p3dmodel;

{$mode objfpc}{$H+}

{$DEFINE VERBOSE}
{$DEFINE BUFFERS}

interface
uses
  Classes, SysUtils, dglOpenGL, Math, p3dMath, strutils, p3dshaders, p3dtexture,
  p3dgeometry, LCLIntf, p3dfilewatch, p3dobjects, p3dbuffers;

type

  TRenderFlag = ( rfShadowMap, rfWireFrame, rfDebugShowLocation, rfDebugShowBoundingBox, rfDebugShowArmature );
  TRenderFlags = set of TRenderFlag;

  { TMaterial }
  TP3DMaterialMap = record
    Map: TP3DTexture;
    DiffuseFactor: Single;
    NormalFactor: Single;
  end;

  TP3DMaterial = class
    Maps: array [ 0..7 ] of TP3DMaterialMap;
    NumMaps: Integer;
    Diff: TVec3;
    Spec: TVec3;
    Spec_Hardness: Single;
    alpha: Real;
    Name: String;
    Shader: TShader;
    ShaderShadow: TShader;

    constructor Create;
  end;

  TP3DFaceVertex = record
    v, n, t, b: Integer;
    texc: array of Integer;
  end;

  TP3DFace = record
    verts: array of TP3DFaceVertex;
    mat: TP3DMaterial;
  end;

  TP3DFaceArray = array of TP3DFace;
//  TMaterialArray = array of TMaterial;
  TP3DMesh = class;

  { TBone }

  {$MACRO ON}
{  {$DEFINE TCustomList:= TCustomModelList}
  {$DEFINE TCustomListEnumerator:= TModelEnumerator}
  {$DEFINE TCustomItem:= TModel}
  {$DEFINE INTERFACE}
  {$INCLUDE custom_list.inc}}

  {$DEFINE TCustomList:= TP3DCustomMaterialList}
  {$DEFINE TCustomListEnumerator:= TP3DMaterialEnumerator}
  {$DEFINE TCustomItem:= TP3DMaterial}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}


  TBone = class;

  {$DEFINE TCustomList:= TCustomBoneList}
  {$DEFINE TCustomListEnumerator:= TBoneEnumerator}
  {$DEFINE TCustomItem:= TBone}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}

  TFrame = class;

  {$DEFINE TCustomList:= TCustomFrameList}
  {$DEFINE TCustomListEnumerator:= TFrameEnumerator}
  {$DEFINE TCustomItem:= TFrame}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}

  TArmatureAction = class;

  {$DEFINE TCustomList:= TCustomActionList}
  {$DEFINE TCustomListEnumerator:= TActionEnumerator}
  {$DEFINE TCustomItem:= TArmatureAction}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}

  { TBoneList }

  TBoneList = class( TCustomBoneList )
    public
      function FindByName( Name: String ): Integer;
      procedure Clear; override;
  end;

  { TFrameList }

  TFrameList = class( TCustomFrameList )
    public
      procedure Clear; override;
  end;

  { TActionList }

  TActionList = class( TCustomActionList )
    public
      procedure Clear; override;
  end;

  { TMaterialList }

  TP3DMaterialList = class( TP3DCustomMaterialList )
    public
      function FindByName( Name: String ): Integer;
      procedure Clear; override;
  end;

  { TRenderableObjectList }

  TRenderableObjectList = class( TObjectList )
    public
      procedure Render( world: TMat4; const RenderFlag: TRenderFlags = []);
  end;

  TBoundingBox = record
    Min, Max, Center: TVec3;
  end;

  { TRenderableObject }

  TRenderableObject = class ( TBaseObject )
    private
      FChildren: TRenderableObjectList;
      FVisible: Boolean;

    public
      constructor Create( AParentList: TObjectList );
      destructor Destroy; override;
      procedure Render( world: TMat4; const RenderFlag: TRenderFlags = []); virtual;

    published
      property Children: TRenderableObjectList read FChildren;
      property Visible: Boolean read FVisible write FVisible;
  end;

  TFrame = class
    private
      FBones: TBoneList;

    public
      constructor Create;
      destructor Destroy; override;

    published
      property Bones: TBoneList read FBones write FBones;
  end;

  { TArmatureAction }

  TArmatureAction = class
    private
      FFrames: TFrameList;
      FName: String;

    public
      constructor Create;
      destructor Destroy; override;

    published
      property Frames: TFrameList read FFrames write FFrames;
      property Name: String read FName write FName;
  end;

  { TArmature }

  TArmature = class ( TRenderableObject )
    private
      FActions: TActionList;
      FCurrentAction: Integer;
      FCurrentFrame: Integer;
      FStillFrame: TFrame;
      procedure SetCurrentAction(AValue: Integer);
      procedure SetCurrentFrame(AValue: Integer);

    public
      constructor Create( AParentList: TObjectList );
      destructor Destroy; override;

      procedure Render( world: TMat4; const RenderFlag: TRenderFlags ); override;

    published
      property Actions: TActionList read FActions;
      property CurrentAction: Integer read FCurrentAction write SetCurrentAction;
      property CurrentFrame: Integer read FCurrentFrame write SetCurrentFrame;
      property StillFrame: TFrame read FStillFrame;
  end;

  TBone = class( TPersistent )
    private
      FBones: TBoneList;
      FMatrix: TMat4;
      FName: String;

    public
      constructor Create;
      destructor Destroy; override;

      property Matrix: TMat4 read FMatrix write FMatrix;
      property Name: String read FName write FName;
      property Bones: TBoneList read FBones;
  end;

  { TModel }

  { TP3DMesh }

  TP3DMesh = class( TRenderableObject )
    private
      FBoundingBox: TBoundingBox;

    public
      Positions: TP3DVec3BufferGL;
      Normals: TP3DVec3BufferGL;
      Tangents: TP3DVec3BufferGL;
      Binormals: TP3DVec3BufferGL;
      Faces: TP3DFaceArray;
      TexCoords: TP3DVec2BufferGL;
      Indices: TP3DIntBufferGL;
      Matrix: TMat4;
      Material: TP3DMaterial;
      testTex: Integer;
      VBArray: TP3DVertexBufferArray;
      {glVBuffer: GLuint;
      glIBuffer: GLuint;
      glPositionArray: GLuint;
      glNormalArray: GLuint;
      glTangentArray: GLuint;
      glBinormalArray: GLuint;
      glTCArray: GLuint;
      glIndexArray: GLuint;}

//      constructor Create( FName: String );
      constructor Create(AParentList: TObjectList);
      destructor Destroy; override;

      procedure Render( world: TMat4; const RenderFlag: TRenderFlags = []); override;
      procedure UnpackBuffers;
      procedure ClearVBO;
      procedure CreateVBO;
      procedure Calc_Tangent_Binormal;
      procedure CalcBoundingBox;
      procedure ClearChildren;

      property BoundingBox: TBoundingBox read FBoundingBox write FBoundingBox;
  end;


  { TModelFile }

  TModelFile = class( TPersistent )
    private
      FFileWatch: TFileWatch;
      FModelList: TRenderableObjectList;
      FMaterials: TP3DMaterialList;

    public
      constructor Create( const AFileName: String = ''; const AWatchFileForChanges: Boolean = False );
      destructor Destroy; override;

      procedure Clear;

      function Debug: String;

      procedure Render( world: TMat4; const RenderFlag: TRenderFlags = []);

      property Children: TRenderableObjectList read FModelList;
      property Materials: TP3DMaterialList read FMaterials;
      property FileWatch: TFileWatch read FFileWatch write FFileWatch;
  end;

implementation

{ TFrame }

constructor TFrame.Create;
begin
  inherited;
  FBones:= TBoneList.Create;
end;

destructor TFrame.Destroy;
begin
  Bones.Free;
  inherited Destroy;
end;

{ TActionList }

procedure TActionList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].Free;
  inherited Clear;
end;

{ TFrameList }

procedure TFrameList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].Free;
  inherited Clear;
end;

{ TArmatureAction }

constructor TArmatureAction.Create;
begin
  inherited;
  FFrames:= TFrameList.Create;
end;

destructor TArmatureAction.Destroy;
begin
  FFrames.Free;
  inherited Destroy;
end;

{ TArmature }

procedure TArmature.SetCurrentAction(AValue: Integer);
begin
  if ( FCurrentAction = AValue ) then
    Exit;

  FCurrentAction:= Min( Actions.Count - 1, Max( -1, AValue ));
end;

procedure TArmature.SetCurrentFrame(AValue: Integer);
begin
  if ( FCurrentFrame = AValue ) then
    Exit;

  if ( InRange( FCurrentAction, 0, Actions.Count - 1 )) then
    begin
      FCurrentFrame:= AValue mod FActions[ FCurrentAction ].Frames.Count;
      if ( FCurrentFrame < 0 ) then
        FCurrentFrame += FActions[ FCurrentAction ].Frames.Count;
    end
  else
    FCurrentFrame:= 0;
end;

constructor TArmature.Create(AParentList: TObjectList);
begin
  inherited;
  FActions:= TActionList.Create;
  FStillFrame:= TFrame.Create;
  FVisible:= True;
  FCurrentFrame:= 0;
  FCurrentAction:= -1;
end;

destructor TArmature.Destroy;
begin
  FStillFrame.Free;
  FActions.Free;
  inherited Destroy;
end;

procedure TArmature.Render(world: TMat4; const RenderFlag: TRenderFlags);
var
  Bone: TBone;
  view, proj: TMat4;
  BackupShader: TShader;

  procedure RenderBone( ABone: TBone; Mat: TMat4; const RootBone: Boolean = False );
  var
    matNew: TMat4;
    p1: TVec3;
    p2: TVec3;
  begin
    matNew:= ABone.Matrix * Mat;
    if ( not RootBone ) then
      begin
        p1:= vec3( vec4( 0 ) * Mat );
        p2:= vec3( vec4( 0 ) * matNew );
        RenderLines3D([ p1, p2 ], vec4( 1, 0, 0, 1 ));
      end;
    for Bone in ABone.Bones do
      RenderBone( Bone, matNew );
  end;

begin
  inherited Render(world, RenderFlag);
  if ( rfDebugShowArmature in RenderFlag ) then
    begin
      glDisable( GL_DEPTH_TEST );
      glGetUniformfv( ActShad.ShaderObj, ActShad.Uniforms.AddrByName( 'view' ), @view );
      glGetUniformfv( ActShad.ShaderObj, ActShad.Uniforms.AddrByName( 'proj' ), @proj );

      BackupShader:= ActShad;

      Setup3D( view * proj );
      if ( CurrentAction = -1 ) then
        for Bone in StillFrame.Bones do
          RenderBone( Bone, world, True )
        else
          if ( InRange( FCurrentAction, 0, Actions.Count - 1 )) then
            if ( InRange( FCurrentFrame, 0, Actions[ FCurrentAction ].Frames.Count - 1 )) then
              for Bone in Actions[ FCurrentAction ].Frames[ FCurrentFrame ].Bones do
                RenderBone( Bone, world, True );

      glEnable( GL_DEPTH_TEST );

      BackupShader.Enable;
    end;
end;

{ TBone }

constructor TBone.Create;
begin
  inherited;
  FBones:= TBoneList.Create;
end;

destructor TBone.Destroy;
begin
  FBones.Free;
  inherited Destroy;
end;

{ TArmature }

constructor TRenderableObject.Create( AParentList: TObjectList );
begin
  inherited;
  FChildren:= TRenderableObjectList.Create;
end;

destructor TRenderableObject.Destroy;
begin
  FChildren.Clear;
  FChildren.Free;
  inherited Destroy;
end;

procedure TRenderableObject.Render(world: TMat4; const RenderFlag: TRenderFlags);
begin
  Children.Render( world, RenderFlag );
end;

{ TBoneList }

function TBoneList.FindByName(Name: String): Integer;
var
  i: Integer;
begin
  Result:= -1;

  for i:= 0 to Count - 1 do
    if ( Items[ i ].Name = Name ) then
      begin
        Result:= i;
        break;
      end;
end;

procedure TBoneList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].Free;
  inherited Clear;
end;

{ TMaterial }

constructor TP3DMaterial.Create;
begin
  inherited;
  NumMaps:= 0;
//  Diff_Map:= -1;
end;

{ TMaterialList }

function TP3DMaterialList.FindByName(Name: String): Integer;
var
  i: Integer;
begin
  Result:= -1;

  for i:= 0 to Count - 1 do
    if ( Items[ i ].Name = Name ) then
      begin
        Result:= i;
        break;
      end;
end;

procedure TP3DMaterialList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].Free;
  inherited Clear;
end;

{ TRenderableObjectList }

procedure TRenderableObjectList.Render(world: TMat4; const RenderFlag: TRenderFlags = []);
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    if ( Items[ i ] is TRenderableObject ) then
      if ( TRenderableObject( Items[ i ]).Visible ) then
        TRenderableObject( Items[ i ]).Render( world, RenderFlag );
end;

{ TModelFile }

constructor TModelFile.Create(const AFileName: String;
  const AWatchFileForChanges: Boolean);
begin
  inherited Create;

  FModelList:= TRenderableObjectList.Create;
  FMaterials:= TP3DMaterialList.Create;
end;

destructor TModelFile.Destroy;
begin
  Clear;

  FModelList.Free;
  FMaterials.Free;
  inherited Destroy;
end;

procedure TModelFile.Clear;
begin
  FModelList.Clear;
end;

function TModelFile.Debug: String;
var
  Indent: Integer;

  function DebugList( Items: TRenderableObjectList ): String;
    function WriteVertices( Mdl: TP3DMesh ): String;
    var
      i: Integer;
    begin
      Result:= StringOfChar( ' ', Indent * 2 ) + 'vertices' + #13#10;

      Inc( Indent );

      for i:= 0 to Mdl.Positions.Count - 1 do
        Result += StringOfChar( ' ', Indent * 2 ) +
          Format( '%9.4f, %9.4f, %9.4f;', [ Mdl.Positions[ i ].X, Mdl.Positions[ i ].Y, Mdl.Positions[ i ].Z ])
          + #13#10;

      Dec( Indent );

      Result += StringOfChar( ' ', Indent * 2 ) + 'end;' + #13#10;
    end;

    function WriteNormals( Mdl: TP3DMesh ): String;
    var
      i: Integer;
    begin
      Result:= StringOfChar( ' ', Indent * 2 ) + 'normals' + #13#10;

      Inc( Indent );

      for i:= 0 to Mdl.Normals.Count - 1 do
        Result += StringOfChar( ' ', Indent * 2 ) +
          Format( '%9.4f, %9.4f, %9.4f;', [ Mdl.Normals[ i ].X, Mdl.Normals[ i ].Y, Mdl.Normals[ i ].Z ])
          + #13#10;

      Dec( Indent );

      Result += StringOfChar( ' ', Indent * 2 ) + 'end;' + #13#10;
    end;

    function WriteFaces( Mdl: TP3DMesh ): String;
    var
      i: Integer;
      s: String;
      j: Integer;
      k: Integer;
    begin
      Result:= StringOfChar( ' ', Indent * 2 ) + 'faces' + #13#10;

      Inc( Indent );

      for i:= 0 to high( Mdl.Faces ) do
        begin
          s:= '';

          for j:= 0 to high( Mdl.Faces[ i ].verts ) do
            begin
              s += ', ' + IntToStr( Mdl.Faces[ i ].verts[ j ].v ) + '/' + IntToStr( Mdl.Faces[ i ].verts[ j ].n ) + '/';
              for k:= 0 to High( Mdl.Faces[ i ].verts[ j ].texc ) do
                begin
                  if ( k > 0 ) then
                    s += '|';
                  s += IntToStr( Mdl.Faces[ i ].verts[ j ].texc[ k ]);
                end;
            end;


          Delete( s, 1, 2 );
          Result += StringOfChar( ' ', Indent * 2 ) + s + #13#10;
        end;

      Dec( Indent );

      Result += StringOfChar( ' ', Indent * 2 ) + 'end;' + #13#10;
    end;

  var
    i: Integer;
  begin
    Result:= '';
    for i:= 0 to Items.Count - 1 do
      begin
        Result += StringOfChar( ' ', Indent * 2 ) + 'object ' + Items[ i ].Name +  #13#10;
        Inc( Indent );
        Result += WriteVertices( Items[ i ] as TP3DMesh );
        Result += WriteNormals( Items[ i ] as TP3DMesh );
        Result += WriteFaces( Items[ i ] as TP3DMesh );
        Result += StringOfChar( ' ', Indent * 2 ) + 'children' + #13#10;
        Inc( Indent );
        Result += DebugList( ( Items[ i ] as TP3DMesh ).Children );
        Dec( Indent );
        Result += StringOfChar( ' ', Indent * 2 ) + 'end;' + #13#10;
        Dec( Indent );
        Result += StringOfChar( ' ', Indent * 2 ) + 'end;' + #13#10;
      end;
  end;

begin
  Indent:= 0;
  Result:= DebugList( Children );
end;

procedure TModelFile.Render(world: TMat4; const RenderFlag: TRenderFlags);
begin
  Children.Render( world, RenderFlag );
end;

{ TP3DMesh }

procedure TP3DMesh.ClearChildren;
begin
  FChildren.Clear;
end;

constructor TP3DMesh.Create( AParentList: TObjectList );
var
  vertexLoc: GLuint;
begin
  inherited;

  Positions:= TP3DVec3BufferGL.Create( True );
  Normals:= TP3DVec3BufferGL.Create( True );
  Binormals:= TP3DVec3BufferGL.Create( True );
  Tangents:= TP3DVec3BufferGL.Create( True );
  Indices:= TP3DIntBufferGL.Create( True );
  TexCoords:= TP3DVec2BufferGL.Create( True );
  VBArray:= TP3DVertexBufferArray.Create;

  FVisible:= True;
end;

destructor TP3DMesh.Destroy;
begin
  Positions.Free;
  Normals.Free;
  Binormals.Free;
  Tangents.Free;
  Indices.Free;
  TexCoords.Free;
  VBArray.Free;

  inherited Destroy;
end;

procedure TP3DMesh.Render( world: TMat4; const RenderFlag: TRenderFlags = []);
var
  i: Integer;
  j: Integer;
  matId: Integer;
  vNorm, Vec2: TVec3;
  _world, view, proj: TMat4;
  k: Integer;
  normal, vertex, color: GLint;
  n: Integer;
  _worldview: TMat4;
  p: TVec3;
  BackupShader: TShader;
  //Res: Boolean;

  function SetShader( Shader: TShader ): Boolean;
  begin
    Result:= False;
    if ( Shader = nil ) then
      exit;
    if ( Shader <> ActShad ) then
      Shader.Enable;
    Result:= True;
  end;

begin
  if ( not Visible ) then
    exit;
  if ( Assigned( Material )) then
    begin
      glVertexAttrib4f( P3DAttribColor,
        Material.Diff.R, Material.Diff.G, Material.Diff.B, 1.0 );
      glUniform4f( ActShad.Uniforms.AddrByName( 'mat_diffuse' ), Material.Diff.r, Material.Diff.g, Material.Diff.b, 1 );
      glUniform4f( ActShad.Uniforms.AddrByName( 'mat_specular' ), Material.Spec.r, Material.Spec.g, Material.Spec.b, 1 );
      glUniform1f( ActShad.Uniforms.AddrByName( 'mat_hardness' ), Material.Spec_Hardness );

      glUniform1i( ActShad.Uniforms.AddrByName( 'numMaps' ), Material.NumMaps );
      if ( Material.NumMaps > 0 ) then
        begin
          for j:= 0 to Material.NumMaps - 1 do
            begin
              glActiveTexture( GL_TEXTURE0 + j );
              glEnable( GL_TEXTURE_2D );
              glBindTexture( GL_TEXTURE_2D, Material.Maps[ j ].Map.fGLTexture );
              glUniform1i( ActShad.Uniforms.AddrByName( 'tex' + IntToStr( j )), j );


              glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
              glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
              glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
              glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
            end;

        end
      else
        begin
{          glColor3f( 1.0, 1.0, 1.0 );
          glActiveTexture( GL_TEXTURE0 );
          glEnable( GL_TEXTURE_2D );
          glBindTexture( GL_TEXTURE_2D, testTex );
          glActiveTexture( GL_TEXTURE1 );
          glEnable( GL_TEXTURE_2D );
          glBindTexture( GL_TEXTURE_2D, testTex );
          {$IFDEF DEFAULT_TEX}
          glEnable( GL_TEXTURE_2D );
          glBindTexture( GL_TEXTURE_2D, DefaultTex^.ID );
          {$ENDIF}}
        end;
    end
  else
    begin
      {glColor3f( 1.0, 1.0, 1.0 );
      glActiveTexture( GL_TEXTURE0 );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, testTex );
      glActiveTexture( GL_TEXTURE1 );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, testTex );}
      glVertexAttrib4f( P3DAttribColor,
      1, 1, 1, 1.0 );
    end;
  _world:= Matrix * world;

  //VBArray.Bind;  //Workaround for VAO not really working
  if ( Positions.Count > 0 ) then
    Positions.SetAttribArray( P3DAttribPosition );
  if ( Normals.Count > 0 ) then
    Normals.SetAttribArray( P3DAttribNormal );
  if ( Tangents.Count > 0 ) then
    Tangents.SetAttribArray( P3DAttribTangent );
   if ( TexCoords.Count > 0 ) then
    TexCoords.SetAttribArray( P3DAttribTexCoord0 );

  Indices.Bind( GL_ELEMENT_ARRAY_BUFFER );

  glUniformMatrix4fv( ActShad.Uniforms.AddrByName( 'world' ), 1, False, @_world );

  {if ( rfWireFrame in RenderFlag ) then
    glPolygonMode( GL_FRONT_AND_BACK, GL_LINE );}
  glDrawElements( GL_TRIANGLES, Indices.Count, GL_UNSIGNED_INT, Pointer( 0 ));
  {
  if ( rfDebugShowLocation in RenderFlag ) then
    begin
      glGetUniformfv( ActShad.ShaderObj, ActShad.Uniforms.AddrByName( 'view' ), @view );
      glGetUniformfv( ActShad.ShaderObj, ActShad.Uniforms.AddrByName( 'proj' ), @proj );

      BackupShader:= ActShad;

      Setup3D( view * proj );
      p:= vec3( vec4( BoundingBox.Center, 1 ) * world );

      RenderLines3D([ p, vec3( 0 )], vec4( 1, 0, 0, 1 ));

      BackupShader.Enable;
    end;
  if ( rfDebugShowBoundingBox in RenderFlag ) then
    begin
      glGetUniformfv( ActShad.ShaderObj, ActShad.Uniforms.AddrByName( 'view' ), @view );
      glGetUniformfv( ActShad.ShaderObj, ActShad.Uniforms.AddrByName( 'proj' ), @proj );

      BackupShader:= ActShad;

      Setup3D( view * proj * world );
      with ( BoundingBox ) do
      RenderLines3D([
         Min, vec3( Min.x, Min.y, Max.Z ),
         vec3( Max.x, Min.y, Max.z ), Max
         ], vec4( 1, 0, 0, 1 ));

      BackupShader.Enable;
    end;
  }
  Children.Render( _world );
  glUniformMatrix4fv( ActShad.Uniforms.AddrByName( 'world'), 1, False, @_world );
  if ( rfWireFrame in RenderFlag ) then
    glPolygonMode( GL_FRONT_AND_BACK, GL_FILL );
end;

const
  IDX_POS: Integer = 0;
  IDX_NOR: Integer = 1;
  IDX_TAN: Integer = 2;
  IDX_BIN: Integer = 3;

procedure TP3DMesh.UnpackBuffers;
var
  i: Integer;
  j: Integer;
  ibase: Integer;
  vbase: Integer;
  N: TVec3;
  T: TVec3;
  B: TVec3;

  _Positions,
  _Tangents,
  _Binormals,
  _Normals: TP3DVec3BufferGL;
  _Texures: TP3DVec2BufferGL;

  numTex: Integer;
  k: Integer;
  baseTex: Integer;

begin
  ClearVBO;
  _Positions:= TP3DVec3BufferGL.Create( True );
  _Tangents:= TP3DVec3BufferGL.Create( True );
  _Binormals:= TP3DVec3BufferGL.Create( True );
  _Normals:= TP3DVec3BufferGL.Create( True );
  _Texures:= TP3DVec2BufferGL.Create( True );

  baseTex:= 0;

  if (( Length( Faces ) > 0 ) AND ( Length( Faces[ 0 ].verts ) > 0 )) then
    numTex:= Length( Faces[ 0 ].verts[ 0 ].texc )
  else
    numTex:= 0;

  for i:= 0 to high( Faces ) do
    begin
      vbase:= _Positions.Count;
      _Positions.Count:= vbase + Length( Faces[ i ].verts );
      ibase:= Indices.Count;
      Indices.Count:= ibase + ( Length( Faces[ i ].verts ) - 2 ) * 3;

      for j:= 0 to high( Faces[ i ].verts ) do
        begin
          _Positions[ vbase + j ]:= Positions[ Faces[ i ].verts[ j ].v ];
          Faces[ i ].verts[ j ].v:= vbase + j;
          if ( Faces[ i ].verts[ j ].n >= 0 ) then
            begin
              _Normals.Count:= _Positions.Count;
              _Normals [ vbase + j ]:= Normals[ Faces[ i ].verts[ j ].n ];
            end;

          if ( Faces[ i ].verts[ j ].t >= 0 ) then
            begin
              _Tangents.Count:= _Positions.Count;
              _Tangents[ vbase + j ]:= Tangents[ Faces[ i ].verts[ j ].t ];
            end;

          if ( Faces[ i ].verts[ j ].b >= 0 ) then
            begin
              _Binormals.Count:= _Positions.Count;
              _Binormals[ vbase + j ]:= Binormals[ Faces[ i ].verts[ j ].b ];
            end;

          if ( numTex > 0 ) then
            begin
              baseTex:= _Texures.Count;
              _Texures.Count:= _Texures.Count + numTex;
              for k:= 0 to numTex - 1 do
                _Texures[ baseTex + k ]:= TexCoords[ Faces[ i ].verts[ j ].texc[ k ]];
            end;

          if ( j >= 2 ) then
            begin
              Indices[ ibase + ( j - 2 ) * 3 + 0 ]:= vbase + 0;
              Indices[ ibase + ( j - 2 ) * 3 + 1 ]:= vbase + j - 1;
              Indices[ ibase + ( j - 2 ) * 3 + 2 ]:= vbase + j;
            end;
        end;
    end;

  Positions.Free;
  Positions:= _Positions;
  Normals.Free;
  Normals:= _Normals;
  Tangents.Free;
  Tangents:= _Tangents;
  Binormals.Free;
  Binormals:= _Binormals;
  TexCoords.Free;
  TexCoords:= _Texures;

  CreateVBO;
end;

procedure TP3DMesh.ClearVBO;
begin
  VBArray.Free;
  VBArray:= TP3DVertexBufferArray.Create;
end;

procedure TP3DMesh.CreateVBO;
begin
  VBArray.Bind;
  if ( Positions.Count > 0 ) then
    begin
      Positions.PushData;
      Positions.SetAttribArray( P3DAttribPosition );
    end;
  if ( Normals.Count > 0 ) then
    begin
      Normals.PushData;
      Normals.SetAttribArray( P3DAttribNormal );
    end;
  if ( Binormals.Count > 0 ) then
    begin
      Binormals.PushData;
      Binormals.SetAttribArray( P3DAttribBinormal );
    end;
  if ( Tangents.Count > 0 ) then
    begin
      Tangents.PushData;
      Tangents.SetAttribArray( P3DAttribTangent );
    end;
  if ( TexCoords.Count > 0 ) then
    begin
      TexCoords.PushData;
      TexCoords.SetAttribArray( P3DAttribTexCoord0 );
    end;

  Indices.Bind( GL_ELEMENT_ARRAY_BUFFER );
  Indices.PushData;
  VBArray.Unbind;
end;

procedure TP3DMesh.Calc_Tangent_Binormal;
  procedure CalcTriangleTB( f: Integer; v0, v1, v2: Integer );
  var
    deltaPos1: TVec3;  // Edges
    deltaPos2: TVec3;
    deltaUV1: TVec2;  // UV Directions
    deltaUV2: TVec2;
    r: Single;
    tang: TVec3;
    bin: TVec3;

    procedure CalcTriangle_TB( out tangent: TVec3; out binormal: TVec3 );
    begin
      tangent:=  ( deltaPos1 * deltaUV2.y - deltaPos2 * deltaUV1.y ) * r;
      binormal:= ( deltaPos2 * deltaUV1.x - deltaPos1 * deltaUV2.x ) * r;
    end;

  begin
    if ( Length( Faces[ f ].verts[ v1 ].texc ) <= 0 ) then //not failsafe
      begin
        Faces[ f ].verts[ v0 ].t:= -1;
        Faces[ f ].verts[ v0 ].b:= -1;
        Faces[ f ].verts[ v1 ].t:= -1;
        Faces[ f ].verts[ v1 ].b:= -1;
        Faces[ f ].verts[ v2 ].t:= -1;
        Faces[ f ].verts[ v2 ].b:= -1;
      end
    else
      begin
        // Edges of the triangle : postion delta
        deltaPos1:= Positions[ Faces[ f ].verts[ v1 ].v ] - Positions[ Faces[ f ].verts[ v0 ].v ];
        deltaPos2:= Positions[ Faces[ f ].verts[ v2 ].v ] - Positions[ Faces[ f ].verts[ v0 ].v ];

        // UV delta
        deltaUV1:= TexCoords[ Faces[ f ].verts[ v1 ].texc[ 0 ]] - TexCoords[ Faces[ f ].verts[ v0 ].texc[ 0 ]];
        deltaUV2:= TexCoords[ Faces[ f ].verts[ v2 ].texc[ 0 ]] - TexCoords[ Faces[ f ].verts[ v0 ].texc[ 0 ]];

        r:= 1.0 / Max(0.00001, ( deltaUV1.x * deltaUV2.y - deltaUV1.y * deltaUV2.x ));

        CalcTriangle_TB( tang, bin );
        Tangents.Add( tang );
        Binormals.Add( bin );

        Faces[ f ].verts[ v0 ].t:= Tangents.Count - 1;
        Faces[ f ].verts[ v0 ].b:= Binormals.Count - 1;
        Faces[ f ].verts[ v1 ].t:= Tangents.Count - 1;
        Faces[ f ].verts[ v1 ].b:= Binormals.Count - 1;
        Faces[ f ].verts[ v2 ].t:= Tangents.Count - 1;
        Faces[ f ].verts[ v2 ].b:= Binormals.Count - 1;
      end;
  end;

var
  i: Integer;
  j: Integer;
  flip: Boolean;
begin
  Tangents.Clear;
  Binormals.Clear;

  for i:= 0 to high( Faces ) do
    begin
      flip:= False; //For now only one tangent/binormal per face
      for j:= 0 to high( Faces[ i ].verts ) - 2 do
        begin
          if ( flip ) then
            CalcTriangleTB( i, j, j+2, j+1)
          else
            CalcTriangleTB( i, j, j+1, j+2);
          flip:= not flip;
        end;
    end;
end;

procedure TP3DMesh.CalcBoundingBox;
var
  p: TVec3;
begin
  FBoundingBox.Min:= vec3( 0 );
  FBoundingBox.Max:= vec3( 0 );
  FBoundingBox.Center:= vec3( 0 );
  for p in Positions do
    begin
      FBoundingBox.Min.X:= Min( p.X, BoundingBox.Min.X );
      FBoundingBox.Min.Y:= Min( p.Y, BoundingBox.Min.Y );
      FBoundingBox.Min.Z:= Min( p.Z, BoundingBox.Min.Z );
      FBoundingBox.Max.X:= Max( p.X, BoundingBox.Max.X );
      FBoundingBox.Max.Y:= Max( p.Y, BoundingBox.Max.Y );
      FBoundingBox.Max.Z:= Max( p.Z, BoundingBox.Max.Z );
    end;
  FBoundingBox.Center:= ( BoundingBox.Min + BoundingBox.Max ) / 2;
end;

{{$DEFINE TCustomList:= TCustomModelList}
{$DEFINE TCustomListEnumerator:= TModelEnumerator}
{$DEFINE TCustomItem:= TModel}
{$DEFINE IMPLEMENTATION}
{$INCLUDE custom_list.inc}}

{$DEFINE TCustomList:= TP3DCustomMaterialList}
{$DEFINE TCustomListEnumerator:= TP3DMaterialEnumerator}
{$DEFINE TCustomItem:= TP3DMaterial}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}

{$DEFINE TCustomList:= TCustomBoneList}
{$DEFINE TCustomListEnumerator:= TBoneEnumerator}
{$DEFINE TCustomItem:= TBone}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}

{$DEFINE TCustomList:= TCustomFrameList}
{$DEFINE TCustomListEnumerator:= TFrameEnumerator}
{$DEFINE TCustomItem:= TFrame}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}

{$DEFINE TCustomList:= TCustomActionList}
{$DEFINE TCustomListEnumerator:= TActionEnumerator}
{$DEFINE TCustomItem:= TArmatureAction}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}


end.

