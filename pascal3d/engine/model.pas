unit Model;

{$mode objfpc}{$H+}

{$DEFINE VERBOSE}
{$DEFINE BUFFERS}

interface
uses
  Classes, SysUtils, dglOpenGL, Math, Math3D, strutils, shaders, texture_sdl, geometry, LCLIntf, filewatch;

type

  TRenderFlag = ( rfShadowMap, rfWireFrame, rfDebugShowLocation, rfDebugShowBoundingBox );
  TRenderFlags = set of TRenderFlag;

  { TMaterial }
  TMap = record
    Map: TSDLSurface;
    DiffuseFactor: Single;
    NormalFactor: Single;
  end;

  TMaterial = class
    Maps: array [ 0..7 ] of TMap;
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

  TFaceVertex = record
    v, n, t, b: Integer;
    texc: array of Integer;
  end;

  TFace = record
    verts: array of TFaceVertex;
    mat: TMaterial;
  end;

  TFaceArray = array of TFace;
//  TMaterialArray = array of TMaterial;
  TModel = class;

  { TBone }

  TBone = class( TPersistent )
    private
      FMatrix: TMat4;
      FName: String;

    public
      property Matrix: TMat4 read FMatrix write FMatrix;
      property Name: String read FName write FName;
  end;

  {$MACRO ON}
  {$DEFINE TCustomList:= TCustomModelList}
  {$DEFINE TCustomListEnumerator:= TModelEnumerator}
  {$DEFINE TCustomItem:= TModel}
  {$DEFINE INTERFACE}
  {$INCLUDE custom_list.inc}

  {$DEFINE TCustomList:= TCustomMaterialList}
  {$DEFINE TCustomListEnumerator:= TMaterialEnumerator}
  {$DEFINE TCustomItem:= TMaterial}
  {$DEFINE INTERFACE}
  {$INCLUDE custom_list.inc}

  {$DEFINE TCustomList:= TCustomBoneList}
  {$DEFINE TCustomListEnumerator:= TBoneEnumerator}
  {$DEFINE TCustomItem:= TBone}
  {$DEFINE INTERFACE}
  {$INCLUDE custom_list.inc}

  { TBoneList }

  TBoneList = class( TCustomBoneList )
    public
      function FindByName( Name: String ): Integer;
      procedure Clear; override;
  end;

  { TMaterialList }

  TMaterialList = class( TCustomMaterialList )
    public
      function FindByName( Name: String ): Integer;
      procedure Clear; override;
  end;

  { TModelList }

  TModelList = class( TCustomModelList )
    public
      procedure Clear; override;
      procedure Render( world: TMat4; const RenderFlag: TRenderFlags = []);
  end;

  TBoundingBox = record
    Min, Max, Center: TVec3;
  end;

  { TArmature }

  TArmature = class
    private
      FBones: TBoneList;
      FChildren: TModelList;

    published
      property Children: TModelList read FChildren;
      property Bones: TBoneList read FBones;
  end;


  { TModel }
  TModel = class( TPersistent )
    private
      FBoundingBox: TBoundingBox;
      FChildren: TModelList;
      FVisible: Boolean;

    public
      Positions: TVec3List;
      Normals: TVec3List;
      Tangents: TVec3List;
      Binormals: TVec3List;
      Faces: TFaceArray;
      TexCoords: TVec2List;
      Indices: TIntList;
      Name: String;
      Matrix: TMat4;
      Material: TMaterial;
      testTex: Integer;
      glVBuffer: GLuint;
      glIBuffer: GLuint;
      glPositionArray: GLuint;
      glNormalArray: GLuint;
      glTangentArray: GLuint;
      glBinormalArray: GLuint;
      glTCArray: GLuint;
      glIndexArray: GLuint;

//      constructor Create( FName: String );
      constructor Create;
      destructor Destroy; override;

      procedure Render( world: TMat4; const RenderFlag: TRenderFlags = []);
      procedure UnpackBuffers;
      procedure ClearVBO;
      procedure CreateVBO;
      procedure Calc_Tangent_Binormal;
      procedure CalcBoundingBox;
      procedure ClearChildren;

      property Children: TModelList read FChildren;
      property Visible: Boolean read FVisible write FVisible;
      property BoundingBox: TBoundingBox read FBoundingBox write FBoundingBox;
  end;


  { TModelFile }

  TModelFile = class( TPersistent )
    private
      FFileWatch: TFileWatch;
      FModelList: TModelList;
      FMaterials: TMaterialList;

    public
      constructor Create( const AFileName: String = ''; const AWatchFileForChanges: Boolean = False );
      destructor Destroy; override;

      procedure Clear;

      function Debug: String;

      procedure Render( world: TMat4; const RenderFlag: TRenderFlags = []);

      property Children: TModelList read FModelList;
      property Materials: TMaterialList read FMaterials;
      property FileWatch: TFileWatch read FFileWatch write FFileWatch;
  end;

implementation

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

constructor TMaterial.Create;
begin
  inherited;
  NumMaps:= 0;
//  Diff_Map:= -1;
end;

{ TMaterialList }

function TMaterialList.FindByName(Name: String): Integer;
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

procedure TMaterialList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].Free;
  inherited Clear;
end;

{ TModelList }

procedure TModelList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].Free;
  inherited Clear;
end;

procedure TModelList.Render(world: TMat4; const RenderFlag: TRenderFlags = []);
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    if ( Items[ i ].Visible ) then
      Items[ i ].Render( world, RenderFlag );
end;

{ TModelFile }

constructor TModelFile.Create(const AFileName: String;
  const AWatchFileForChanges: Boolean);
begin
  inherited Create;

  FModelList:= TModelList.Create;
  FMaterials:= TMaterialList.Create;
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

  function DebugList( Items: TModelList ): String;
    function WriteVertices( Mdl: TModel ): String;
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

    function WriteNormals( Mdl: TModel ): String;
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

    function WriteFaces( Mdl: TModel ): String;
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
        Result += WriteVertices( Items[ i ]);
        Result += WriteNormals( Items[ i ]);
        Result += WriteFaces( Items[ i ]);
        Result += StringOfChar( ' ', Indent * 2 ) + 'children' + #13#10;
        Inc( Indent );
        Result += DebugList( Items[ i ].Children );
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

{ TModel }

procedure TModel.ClearChildren;
begin
  FChildren.Clear;
end;

constructor TModel.Create;
var
  vertexLoc: GLuint;
begin
  inherited;

  Positions:= TVec3List.Create;
  Normals:= TVec3List.Create;
  Binormals:= TVec3List.Create;
  Tangents:= TVec3List.Create;
  Indices:= TIntList.Create;
  TexCoords:= TVec2List.Create;

  FChildren:= TModelList.Create;
  FVisible:= True;
end;

destructor TModel.Destroy;
begin
  Positions.Free;
  Normals.Free;
  Binormals.Free;
  Tangents.Free;
  Indices.Free;
  TexCoords.Free;

  ClearChildren;
  FChildren.Free;
  inherited Destroy;
end;

procedure TModel.Render( world: TMat4; const RenderFlag: TRenderFlags = []);
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
      {if ( rfShadowMap in RenderFlag ) then
        Res:= SetShader( Material.ShaderShadow )
      else
        Res:= SetShader( Material.Shader );}

      glVertexAttrib4f( ActShad.Attributes.AddrByName( 'in_color' ),
        Material.Diff.R, Material.Diff.G, Material.Diff.B, 1.0 );
//      if ( not Res ) then
//        exit;
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
//              glUniform1i( ActShad.Uniforms.AddrByName( 'btex' + IntToStr( j )), 1 );
            end;

        end
      else
        begin
          glColor3f( 1.0, 1.0, 1.0 );
          glActiveTexture( GL_TEXTURE0 );
          glEnable( GL_TEXTURE_2D );
          glBindTexture( GL_TEXTURE_2D, testTex );
          glActiveTexture( GL_TEXTURE1 );
          glEnable( GL_TEXTURE_2D );
          glBindTexture( GL_TEXTURE_2D, testTex );
          {$IFDEF DEFAULT_TEX}
          glEnable( GL_TEXTURE_2D );
          glBindTexture( GL_TEXTURE_2D, DefaultTex^.ID );
          {$ENDIF}
//          glDisable( GL_TEXTURE_2D );
//          glBindTexture( GL_TEXTURE_2D, 0 );
        end;
    end
  else
    begin
      glColor3f( 1.0, 1.0, 1.0 );
      glActiveTexture( GL_TEXTURE0 );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, testTex );
      glActiveTexture( GL_TEXTURE1 );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, testTex );
//          glDisable( GL_TEXTURE_2D );
//          glBindTexture( GL_TEXTURE_2D, 0 );
    end;
  _world:= Matrix * world;

  glUniformMatrix4fv( ActShad.Uniforms.AddrByName( 'world' ), 1, False, @_world );

//  _worldview:= world * view;

//  glMatrixMode( GL_MODELVIEW );
//  glLoadMatrixf( @_worldview );

  {$IFDEF BUFFERS}
  glBindVertexArray( glVBuffer );
  glBindBuffer( GL_ELEMENT_ARRAY_BUFFER, glIndexArray );

  if ( rfWireFrame in RenderFlag ) then
    glPolygonMode( GL_FRONT_AND_BACK, GL_LINE );
  glDrawElements( GL_TRIANGLES, Indices.Count, GL_UNSIGNED_INT, Pointer( 0 ));

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

{  n:= 0;
  for i:= 0 to high( Faces ) do
    begin
      glDrawArrays( GL_TRIANGLE_FAN, n, ( Length( Faces[ i ].verts ) - 2 ) * 3 );
      n:= n + Length( Faces[ i ].verts );
    end;}
  {$ELSE}
  for i:= 0 to high( Faces ) do
    begin
      glBegin( GL_TRIANGLE_FAN );

{      tangent:=

      glVertexAttrib3f( ActShad.Attributes.AddrByName( 'in_tangent' ),//glGetAttribLocation( ActiveShader, 'in_normal' ),
                  normals[ faces[ i ].verts[ j ].n ].x,
                  normals[ faces[ i ].verts[ j ].n ].y,
                  normals[ faces[ i ].verts[ j ].n ].z );
      glVertexAttrib3f( ActShad.Attributes.AddrByName( 'in_binormal' ),//glGetAttribLocation( ActiveShader, 'in_normal' ),
                  normals[ faces[ i ].verts[ j ].n ].x,
                  normals[ faces[ i ].verts[ j ].n ].y,
                  normals[ faces[ i ].verts[ j ].n ].z );}


      for j:= 0 to high( Faces[ i ].verts ) do
        begin
          // bind buffer for positions and copy data into buffer
          //glBindBuffer( GL_ELEMENT_ARRAY_BUFFER, glIBuffer );
          //glBufferData( GL_ELEMENT_ARRAY_BUFFER, SizeOf( Faces[ i ].verts ), @Faces[ i ].verts[ 0 ], GL_STATIC_DRAW);
//          glDrawElements(GL_TRIANGLE_FAN, Length( Faces[ i ].verts ), GL_UNSIGNED_INT, @Faces[ i ].verts[ 0 ]);

//          if ( Length( Faces[ i ].verts[ j ].texc ) > 0 ) then
//            for k:= 0 to high( Faces[ i ].verts[ j ].texc ) do
//              glMultiTexCoord2f( GL_TEXTURE0 + k, TexCoords[ Faces[ i ].verts[ j ].texc[ k ]].S, TexCoords[ Faces[ i ].verts[ j ].texc[ k ]].T );
//          normal:= glGetAttribLocation( ActiveShader, 'in_normal' );
//          k:= 0;
          if ( Length( Faces[ i ].verts[ j ].texc ) > 0 ) then
            for k:= 0 to high( Faces[ i ].verts[ j ].texc ) do
              begin
                glVertexAttrib2f( ActShad.Attributes.AddrByName( 'in_texc' + IntToStr( k )),
                  TexCoords[ Faces[ i ].verts[ j ].texc[ k ]].S, TexCoords[ Faces[ i ].verts[ j ].texc[ k ]].T );
              end;
          glVertexAttrib3f( ActShad.Attributes.AddrByName( 'in_normal' ),
                      normals[ faces[ i ].verts[ j ].n ].x,
                      normals[ faces[ i ].verts[ j ].n ].y,
                      normals[ faces[ i ].verts[ j ].n ].z );
          if ( faces[ i ].verts[ j ].t >= 0 ) then
            glVertexAttrib3f( ActShad.Attributes.AddrByName( 'in_tangent' ),
                        Tangents[ faces[ i ].verts[ j ].t ].x,
                        Tangents[ faces[ i ].verts[ j ].t ].y,
                        Tangents[ faces[ i ].verts[ j ].t ].z );
          if ( faces[ i ].verts[ j ].b >= 0 ) then
            glVertexAttrib3f( ActShad.Attributes.AddrByName( 'in_binormal' ),
                        Binormals[ faces[ i ].verts[ j ].b ].x,
                        Binormals[ faces[ i ].verts[ j ].b ].y,
                        Binormals[ faces[ i ].verts[ j ].b ].z );
          glVertexAttrib4f( ActShad.Attributes.AddrByName( 'in_vertex' ),
                      Positions[ faces[ i ].verts[ j ].v ].x,
                      Positions[ faces[ i ].verts[ j ].v ].y,
                      Positions[ faces[ i ].verts[ j ].v ].z, 1 );
//          glNormal3f( normals[ faces[ i ].verts[ j ].n ].x,
//                      normals[ faces[ i ].verts[ j ].n ].y,
//                      normals[ faces[ i ].verts[ j ].n ].z );
{          glVertex3f( vertices[ faces[ i ].verts[ j ].v ].x,
                      vertices[ faces[ i ].verts[ j ].v ].y,
                      vertices[ faces[ i ].verts[ j ].v ].z );}
        end;
      glEnd();
    end;
  {$ENDIF}

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

procedure TModel.UnpackBuffers;
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
  _Normals: TVec3List;
  _Texures: TVec2List;

  numTex: Integer;
  k: Integer;
  baseTex: Integer;

begin
  ClearVBO;
  _Positions:= TVec3List.Create;
  _Tangents:= TVec3List.Create;
  _Binormals:= TVec3List.Create;
  _Normals:= TVec3List.Create;
  _Texures:= TVec2List.Create;
{  SetLength( _Positions, 0 );
  SetLength( _Binormals, 0 );
  SetLength( _Normals,  0 );
  SetLength( _Tangents, 0 );
  SetLength( Indices, 0 );}

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

procedure TModel.ClearVBO;
begin
  glBindBuffer( GL_ARRAY_BUFFER, glPositionArray );
  glDeleteBuffers( 1, @glPositionArray );
  glBindBuffer( GL_ARRAY_BUFFER, glNormalArray );
  glDeleteBuffers( 1, @glNormalArray );
  glBindBuffer( GL_ARRAY_BUFFER, glTangentArray );
  glDeleteBuffers( 1, @glTangentArray );
  glBindBuffer( GL_ARRAY_BUFFER, glBinormalArray );
  glDeleteBuffers( 1, @glBinormalArray );
  glBindBuffer( GL_ARRAY_BUFFER, glTCArray );
  glDeleteBuffers( 1, @glTCArray );
  glBindBuffer( GL_ELEMENT_ARRAY_BUFFER, glIndexArray );
  glDeleteBuffers( 1, @glIndexArray );
end;

procedure TModel.CreateVBO;
begin
  glGenBuffers( 1, @glPositionArray );
  glGenBuffers( 1, @glNormalArray );
  glGenBuffers( 1, @glBinormalArray );
  glGenBuffers( 1, @glTangentArray );
  glGenBuffers( 1, @glTCArray );
  glGenBuffers( 1, @glIndexArray );


  glGenVertexArrays( 1, @glVBuffer );
  glBindVertexArray( glVBuffer );

  glBindBuffer( GL_ARRAY_BUFFER, glPositionArray );
  glBufferData( GL_ARRAY_BUFFER, SizeOf( TVec3 ) * Positions.Count, Positions.PtrTo( 0 ), GL_STATIC_DRAW );
  glVertexAttribPointer( 0, 3, GL_FLOAT, False, 0, nil );
//  glVertexPointer( 3, GL_FLOAT, 0, nil );
  glEnableVertexAttribArray( 0 );

  if ( Normals.Count > 0 ) then
    begin
      glBindBuffer( GL_ARRAY_BUFFER, glNormalArray );
      glBufferData( GL_ARRAY_BUFFER, SizeOf( TVec3 ) * Normals.Count, Normals.Ptr, GL_STATIC_DRAW );
      glVertexAttribPointer( 1, 3, GL_FLOAT, False, 0, nil );
//      glNormalPointer( GL_FLOAT, 0, Pointer( 12 ));

      glEnableVertexAttribArray( 1 );
    end;

  if ( Tangents.Count > 0 ) then
    begin
      glBindBuffer( GL_ARRAY_BUFFER, glTangentArray );
      glBufferData( GL_ARRAY_BUFFER, SizeOf( TVec3 ) * Tangents.Count, Tangents.Ptr, GL_STATIC_DRAW );
      glVertexAttribPointer( 2, 3, GL_FLOAT, False, 0, nil );
      glEnableVertexAttribArray( 2 );
    end;

  if ( Binormals.Count > 0 ) then
    begin
      glBindBuffer( GL_ARRAY_BUFFER, glBinormalArray );
      glBufferData( GL_ARRAY_BUFFER, SizeOf( TVec3 ) * Binormals.Count, Binormals.Ptr, GL_STATIC_DRAW );
      glVertexAttribPointer( 3, 3, GL_FLOAT, False, 0, nil );
      glEnableVertexAttribArray( 3 );
    end;

  if ( TexCoords.Count > 0 ) then
    begin
      glBindBuffer( GL_ARRAY_BUFFER, glTCArray );
      glBufferData( GL_ARRAY_BUFFER, SizeOf( TVec2 ) * TexCoords.Count, TexCoords.Ptr, GL_STATIC_DRAW );
      glVertexAttribPointer( 4, 2, GL_FLOAT, False, 0, nil );
      glEnableVertexAttribArray( 4 );
    end;


  glBindBuffer( GL_ELEMENT_ARRAY_BUFFER, glIndexArray );
  glBufferData( GL_ELEMENT_ARRAY_BUFFER, sizeOf( Cardinal ) * Indices.Count, Indices.Ptr, GL_STATIC_DRAW );
end;

procedure TModel.Calc_Tangent_Binormal;
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

procedure TModel.CalcBoundingBox;
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

{$DEFINE TCustomList:= TCustomModelList}
{$DEFINE TCustomListEnumerator:= TModelEnumerator}
{$DEFINE TCustomItem:= TModel}
{$DEFINE IMPLEMENTATION}
{$INCLUDE custom_list.inc}

{$DEFINE TCustomList:= TCustomMaterialList}
{$DEFINE TCustomListEnumerator:= TMaterialEnumerator}
{$DEFINE TCustomItem:= TMaterial}
{$DEFINE IMPLEMENTATION}
{$INCLUDE custom_list.inc}

{$DEFINE TCustomList:= TCustomBoneList}
{$DEFINE TCustomListEnumerator:= TBoneEnumerator}
{$DEFINE TCustomItem:= TBone}
{$DEFINE IMPLEMENTATION}
{$INCLUDE custom_list.inc}



end.
