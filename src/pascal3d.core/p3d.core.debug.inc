{$IFDEF INTERFACE}
{$MACRO ON}
{$IfDef CHECKFORERRORS}{$Assertions ON}{$EndIf}

function P3DCheckLastError( Sender: TObject; const AddMsg: String = '' ): Boolean; inline;
procedure P3DDumpGraphicsInfo;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

procedure P3DDumpGraphicsInfo;
var
  debuginfo: String;
begin
  debuginfo:= '<span>GPU vendor</span>' + glGetString( GL_VENDOR );
  debuginfo+= '<br /><span>Renderer</span>' + glGetString( GL_RENDERER );
  debuginfo+= '<br /><span>GL version</span>' + glGetString( GL_VERSION );
  debuginfo+= '<br /><span>GLSL version</span>' + glGetString( GL_SHADING_LANGUAGE_VERSION );
  debuginfo+= '<br /><details><summary>Extensions</summary><div class="whitebox">' + glGetString( GL_EXTENSIONS ) + '</div></details>';
  log_info( 'Initialized OpenGL <p class="messageheader">' + debuginfo + '</p>' );
  //P3DLog.LogInfoXML( nil, 'Initialized OpenGL <p class="messageheader">' + debuginfo + '</p>');
end;

procedure cle( Sender: TObject; const AddMsg: String = ''  ); inline;
begin
  P3DCheckLastError( Sender, AddMsg );
end;


var
  P3DAssertSender: TObject = nil;

procedure P3DAssert(const M, F: ShortString; L: LongInt; E: Pointer);
begin
  //P3DLog.LogException( P3DAssertSender, Format( '%s(%d): %s', [F,L,M]));
  log_exception( message_header( P3DAssertSender ) + Format( '%s(%d): %s', [F,L,M]));
end;


function P3DCheckLastError(Sender: TObject; const AddMsg: String): Boolean;
var
  Code: Cardinal;
  S: TAssertErrorProc;
  Msg: String;
begin
  Code:= glGetError();
  Result:= Code = GL_NO_ERROR;
  if ( not Result ) then
    try
      S:= AssertErrorProc;
      AssertErrorProc:= @P3DAssert;
      P3DAssertSender:= Sender;
      if ( Assigned( gluErrorString )) then
        Msg:= 'OpenGL Error: ' + gluErrorString( Code )
      else
        Msg:= 'OpenGL Error: <Error message not shown because GLU was not found>';
      if ( AddMsg > '' ) then
        Msg+= ': ' + AddMsg;
      Assert( False, Msg );
    finally
      AssertErrorProc := S;
    end;
end;

{ TP3DPlotCommandExtensions }

constructor TP3DPlotCommandExtensions.Create(AExtensions: array of String);
var
  i: Integer;
begin
  inherited Create;
  FExtensions:= TStringList.Create;
  for i:= low( AExtensions ) to high( AExtensions ) do
    Extensions.Add( AExtensions[ i ]);
end;

destructor TP3DPlotCommandExtensions.Destroy;
begin
  FExtensions.Free;
  inherited Destroy;
end;

procedure TP3DPlotCommandExtensions.GetExtensions(Extension: TStringList);
begin
  Extension.AddStrings( Extensions );
end;

procedure TP3DPlotCommandExtensions.Execute(AContext: TP3DPlotCommandContext);
begin

end;

{ TP3DPlotCommandTexCoordVec4 }

function TP3DPlotCommandTexCoordVec4.GetShaderNode: TP3DNodeClone;
begin
  Result:= FShaderNode;
end;

procedure TP3DPlotCommandTexCoordVec4.MakeShaderModifications(ASender: TP3DPlotListSetting; ASenderIdx: Integer; ABuilder: TP3DShaderBuilder);
begin
  if ( Name > '' ) then begin
    FShaderNode:= ABuilder.Uniforms.AddCopy( 'attrib_vec4' );
    FShaderNode.FindInput( 'Name' ).ValueProp.AsString:= Name;
  end;
end;

{ TP3DPlotCommandTexCoordVec3 }

function TP3DPlotCommandTexCoordVec3.GetShaderNode: TP3DNodeClone;
begin
  Result:= FShaderNode;
end;

procedure TP3DPlotCommandTexCoordVec3.MakeShaderModifications(ASender: TP3DPlotListSetting; ASenderIdx: Integer; ABuilder: TP3DShaderBuilder);
begin
  if ( Name > '' ) then begin
    FShaderNode:= ABuilder.Uniforms.AddCopy( 'attrib_vec3' );
    FShaderNode.FindInput( 'Name' ).ValueProp.AsString:= Name;
  end;
end;

{ TP3DPlotCommandTexCoordVec2 }

function TP3DPlotCommandTexCoordVec2.GetShaderNode: TP3DNodeClone;
begin
  Result:= FShaderNode;
end;

procedure TP3DPlotCommandTexCoordVec2.MakeShaderModifications(ASender: TP3DPlotListSetting; ASenderIdx: Integer; ABuilder: TP3DShaderBuilder);
begin
  if ( Name > '' ) then begin
    FShaderNode:= ABuilder.Uniforms.AddCopy( 'attrib_vec2' );
    FShaderNode.FindInput( 'Name' ).ValueProp.AsString:= Name;
  end;
end;

{ TP3DPlotCommandTexCoord }

function TP3DPlotCommandTexCoord.GetShaderNode: TP3DNodeClone;
begin
  Result:= FShaderNode;
end;

{ TP3DPlotCommandMaterialPBR }

constructor TP3DPlotCommandMaterialPBR.Create();
begin
  inherited Create;
  FCategory:= 'PBR';
end;

procedure TP3DPlotCommandMaterialPBR.MakeShaderModifications(ASender: TP3DPlotListSetting; ASenderIdx: Integer; ABuilder: TP3DShaderBuilder);
var
  setting: TP3DPlotCommandSetting;
  Outp: TP3DNodeSocketClone;
  mat: TP3DNodeClone;
begin
  inherited MakeShaderModifications(ASender, ASenderIdx, ABuilder);
  mat:= ABuilder.Uniforms.AddCopy( 'material_pbr' );
  with ( mat ) do begin
    FindInput( 'Albedo' ).Connected:= ABuilder.RootNode.FindInput( 'Color' ).Connected;
    FindOutput( 'Color' ).Connected:= ABuilder.RootNode.FindInput( 'Color' );

    setting:= ASender.Items[ ASender.FindLast( ASenderIdx, 'pbr_roughness' )];
    if (( setting is IP3DPlotCommandSettingNode ) and
        ( Assigned(( setting as IP3DPlotCommandSettingNode ).ShaderNode ))) then begin
      Outp:= ( setting as IP3DPlotCommandSettingNode ).ShaderNode.FindOutputFirst([ 'Value', 'Color' ]);
      FindInput( 'Roughness' ).Connected:= outp;
    end;

    setting:= ASender.Items[ ASender.FindLast( ASenderIdx, 'pbr_metalness' )];
    if (( setting is IP3DPlotCommandSettingNode ) and
        ( Assigned(( setting as IP3DPlotCommandSettingNode ).ShaderNode ))) then begin
      FindInput( 'Metalness' ).Connected:= ( setting as IP3DPlotCommandSettingNode ).ShaderNode.FindOutputFirst([ 'Value', 'Color' ]);
    end;

    setting:= ASender.Items[ ASender.FindLast( ASenderIdx, 'pbr_occlusion' )];
    if (( setting is IP3DPlotCommandSettingNode ) and
        ( Assigned(( setting as IP3DPlotCommandSettingNode ).ShaderNode ))) then begin
      FindInput( 'Occlusion' ).Connected:= ( setting as IP3DPlotCommandSettingNode ).ShaderNode.FindOutputFirst([ 'Value', 'Color' ]);
    end;

    setting:= ASender.Items[ ASender.FindLast( ASenderIdx, 'normal_map' )];
    if (( setting is IP3DPlotCommandSettingNode ) and
        ( Assigned(( setting as IP3DPlotCommandSettingNode ).ShaderNode ))) then begin
      Outp:= ( setting as IP3DPlotCommandSettingNode ).ShaderNode.FindOutput( 'Normal' );
      if ( Assigned( Outp )) then
        //mat.FindInput( 'WorldNormal' ).Connected:= Outp;
        with ( ABuilder.Uniforms.AddCopy( 'perturb_normal_world' )) do begin
          FindInput( 'Normal' ).Connected:= Outp;
          mat.FindInput( 'WorldNormal' ).Connected:= FindOutput( 'Normal' );
        end;
    end;

  end;
end;

procedure TP3DPlotCommandMaterialPBR.Execute(AContext: TP3DPlotCommandContext);
begin

end;

{ TP3DPlotCommandSDF }

constructor TP3DPlotCommandSDF.Create(const Color: TP3DPlotCommandSetting; const UV: TP3DPlotCommandSetting; const sdf_bias: Float;
  const sdf_smoothing: Float; const sdf_basealpha: Float);
begin
  inherited Create;
  Bias:= sdf_bias;
  Smoothing:= sdf_smoothing;
  BaseAlpha:= sdf_basealpha;
end;

procedure TP3DPlotCommandSDF.MakeShaderModifications(ASender: TP3DPlotListSetting; ASenderIdx: Integer; ABuilder: TP3DShaderBuilder);
begin
  with ( ABuilder.Uniforms.AddCopy( 'signed_distance_field' )) do begin
    {FindInput( 'uv' ).ValueProp.AsString:= Name;
    texture2D:= ABuilder.Uniforms.AddCopy( 'texture2D' );
    texture2D.FindInput( 'Map' ).Connected:= FindOutput( 'Value' );

    with ( ABuilder.Uniforms.AddCopy( 'texcoord' )) do begin
      texture2D.FindInput( 'UV' ).Connected:= FindOutput( 'UV' );
    end;

    texture2D.FindOutput( 'Color' ).Connected:= ABuilder.RootNode.FindInput( 'Color' );}
  end;
end;

{ TP3DShaderBuilder }


procedure TP3DShaderBuilder.SetupRootNode;
var
  p: TP3DNodeSocketClone;
begin
  if ( Assigned( RootNode )) then
    begin
      RootNode.NodeMaster:= NodeMaster;
      if ( not Assigned( GeometryNode )) then
        P3DData.P3DShaderNodeLib.FindNode( 'geometry' ).Clone( FGeometryNode );
      GeometryNode.NodeMaster:= NodeMaster;
      p:= RootNode.FindInput( 'VertexPosition' );
      if ( Assigned( p )) then
        p.Connected:= GeometryNode.FindOutput( 'Attribute' );
      p:= RootNode.FindInput( 'Color' );
      if ( Assigned( p )) then
        p.Connected:= GeometryNode.FindOutput( 'Color' );
    end;
end;

function TP3DShaderBuilder.GetGeometryNode: TP3DNodeClone;
begin
  Result:= TP3DNodeClone( FGeometryNode.AsValue );
end;

function TP3DShaderBuilder.GetRootNode: TP3DNodeClone;
begin
  Result:= TP3DNodeClone( FRootNode.AsValue );
end;

procedure TP3DShaderBuilder.SetGeometryNode(AValue: TP3DNodeClone);
begin
  if ( Assigned( GeometryNode )) then begin
    if ( Assigned( NodeMaster )) then
      NodeMaster.RemoveNode( GeometryNode );
    GeometryNode.Free;
  end;
  FGeometryNode.AsValue:= AValue;
  //FNeedsUpdate:= True;
  if ( Assigned( GeometryNode ) and Assigned( NodeMaster )) then begin
    GeometryNode.NodeMaster:= NodeMaster;
    NodeMaster.AddNode( GeometryNode );
  end;
end;

procedure TP3DShaderBuilder.SetRootNode(AValue: TP3DNodeClone);
begin
  if ( Assigned( RootNode )) then begin
    if ( Assigned( NodeMaster )) then
      NodeMaster.RemoveNode( RootNode );
    RootNode.Free;
  end;
  FRootNode.AsValue:= AValue;
  //FNeedsUpdate:= True;
  if ( Assigned( RootNode ) and Assigned( NodeMaster )) then begin
    RootNode.NodeMaster:= NodeMaster;
    NodeMaster.AddNode( RootNode );
  end;
end;

constructor TP3DShaderBuilder.Create;
begin
  inherited Create;
  FNodeTree:= TP3DShaderNodeTree.Create( 'NodeTree', smText );
  //FNodeTree.OnChange:= @NodesChange;
  FNodeTree.NodeMaster:= NodeMaster;
  FNodeTree.Root:= P3DData;
  FRootNode:= TP3DStreamableContainer.Create( 'RootNode', TP3DNodeClone, smText );
  //FRootNode.OnChange:= @NodeChange;
  FRootNode.Root:= P3DData;
  FGeometryNode:= TP3DStreamableContainer.Create( 'GeometryNode', TP3DNodeClone, smText );
  //FGeometryNode.OnChange:= @NodeChange;
  FGeometryNode.Root:= P3DData;
  FUniforms:= TP3DShaderNodeTree.Create( 'Uniforms', smText );
  FUniforms.NodeMaster:= NodeMaster;
  //FUniforms.OnChange:= @NodesChange;
  FUniforms.Root:= P3DData;
end;

destructor TP3DShaderBuilder.Destroy;
begin
  FreeAndNil( FNodeTree );
  FreeAndNil( FRootNode );
  FreeAndNil( FGeometryNode );
  FreeAndNil( FUniforms );
  inherited Destroy;
end;

procedure TP3DShaderBuilder.MakeRootNode(AName: String);
var
  nd: TP3DNode;
begin
  nd:= P3DData.P3DShaderNodeLib.FindNode( AName );
  if ( Assigned( nd )) then
    MakeRootNode( P3DData.P3DShaderNodeLib.FindNode( AName ))
  else
    raise Exception.Create( 'Cannot make copy to material root node. The specified node was not found: ' + AName );
end;

procedure TP3DShaderBuilder.MakeRootNode(ANode: TP3DNode);
begin
  if ( Assigned( ANode )) then
    begin
      ANode.Clone( FRootNode );
      SetupRootNode;
    end
  else
    raise Exception.Create( 'Cannot make copy to material root node. The passed node was not assigned!' );
end;

procedure TP3DShaderBuilder.BuildShader(AExtensions: TStringList; AOwner: TP3DStreamableContainer);
var
  sh: TP3DShaderCompiled;
  i: Integer;
  Buffer, frag, vert: TP3DShaderBuffer;
  Socket: TP3DNodeSocketClone;
  s: String;
begin
  try
    sh:= TP3DShaderCompiled.Create;
    RootNode.Libs.Clear;
    for s in AExtensions do
      RootNode.AddLib( s );
    if ( Assigned( RootNode )) then
      for Socket in RootNode.Outputs do
        if ( Socket.CloneOf.SocketType = 'text' ) then
          begin
            Buffer:= TP3DShaderBuffer.Create;
            Buffer.Name:= Socket.CloneOf.Name;
            Buffer.Code:= TP3DShaderNodeSocketClone( Socket ).GetOutput( Buffer );
            sh.Buffers.Add( Buffer );
            log_info( message_header( Self ) + '<details><summary>Built ' + Buffer.Name + ' from chunks <br/></summary><pre><code class="glsl">' + P3DTextToHTML( Buffer.Code ) + '</code></pre></details>' );
          end;
  except
    on E: Exception do
      log_exception( message_header( Self ) + E.Message );
  end;

  try
    frag:= sh.FindBuffer( '__fshader' );
    vert:= sh.FindBuffer( '__vshader' );
    if ( Assigned( frag ) and Assigned( vert )) then
      TP3DShader.CreateFromVertexAndFragmentShader( vert.Code, frag.Code, AOwner );
  finally
    sh.Free;
  end;
end;

{ TP3DPlotListSetting }

function TP3DPlotListSetting.GetShader: TP3DShader;
begin
  Result:= TP3DShader( FShader.AsValue );
end;

constructor TP3DPlotListSetting.Create(const AOwnedByUser: Boolean);
begin
  inherited Create( AOwnedByUser );
  FShader:= TP3DStreamableContainer.Create( 'Shader', TP3DShader );
  FShader.Root:= P3DData;
end;

destructor TP3DPlotListSetting.Destroy;
begin
  FShader.Free;
  inherited Destroy;
end;

procedure TP3DPlotListSetting.Append(Item: TP3DPlotCommandSetting);
begin
  if ( Item is TP3DPlotCommandSettingShader ) then
    if ( not Assigned( BuildShader )) then
      FBuildShader:= Item as TP3DPlotCommandSettingShader
    else
      raise Exception.Create( 'You can add only one setting that defines a shader!' );
  inherited Append(Item);
end;

procedure TP3DPlotListSetting.Clear;
begin
  FBuildShader:= nil;
  inherited Clear;
end;

procedure TP3DPlotListSetting.UpdateShader;
var
  ShaderBuilder: TP3DShaderBuilder;
  Extensions: TStringList;
  i: Integer;
  Mat: TP3DMaterialNode;
begin
  if ( not Assigned( Shader )) then
    if ( Assigned( FBuildShader )) then
      FBuildShader.BuildShader( FShader )
    else begin
      ShaderBuilder:= TP3DShaderBuilder.Create;
      Extensions:= TStringList.Create;
      ShaderBuilder.MakeRootNode( 'output' );
      for i:= 0 to Count -1 do begin
        Items[ i ].GetExtensions( Extensions );

        WriteLn( Extensions.Text );
        Items[ i ].MakeShaderModifications( Self, i, ShaderBuilder );
      end;
      WriteLn( Extensions.Text );
      ShaderBuilder.BuildShader( Extensions, FShader );
      if ( Assigned( Shader )) then
        WriteLn( 'Shader assigned' )
      else
        WriteLn( 'No shader' );
      Extensions.Free;
    end;
end;

procedure TP3DPlotListSetting.Execute(AContext: TP3DPlotCommandContext);
begin
  if ( not Assigned( Shader )) then
    UpdateShader;
  if ( Assigned( Shader )) then begin
    AContext.Shader:= Shader;
    Shader.Enable;
    inherited Execute( AContext );
  end;

end;

{ TP3DShaderNodeFragmentList }

function TP3DShaderNodeFragmentList.GetStringOutput(ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer): String;
var
  Fragment: TP3DShaderNodeFragment;
begin
  Result:= '';
  for Fragment in Self do
    Result+= Fragment.GetStringOutput( ACloneSocket, Buffer );
end;



{ TP3DPlotCommandScene }

constructor TP3DPlotCommandScene.Create(AScene: TP3DScene);
begin
  inherited Create;
  FScene:= AScene;
end;

procedure TP3DPlotCommandScene.Execute(AContext: TP3DPlotCommandContext);
begin
  //inherited Execute(AContext);
  if ( Assigned( FScene )) then
    FScene.FPlotList.Execute( AContext );
end;



{$ENDIF}
