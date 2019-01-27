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

{ TP3DPlotCommandUniformTransformView }

constructor TP3DPlotCommandUniformTransformView.Create(AValue: TP3DTransform);
begin
  inherited Create( 'view', AValue );
end;

procedure TP3DPlotCommandUniformTransformView.GetExtensions(Extension: TStringList);
begin
  Extension.Add( 'camera' );
end;


{ TP3DPlotCommandContext }

constructor TP3DPlotCommandContext.Create;
begin
  inherited Create;
  FSettingsDefault:= TP3DDefaultStack.Create;
  FSettingsDefault.OwnsObjects:= False;
end;

destructor TP3DPlotCommandContext.Destroy;
begin
  FreeAndNil( FSettingsDefault );
  inherited Destroy;
end;

{ TP3DPlotCommandPopDefault }

procedure TP3DPlotCommandPopDefault.Execute(AContext: TP3DPlotCommandContext);
begin
  AContext.SettingsDefault.Pop;
end;

{ TP3DDefaultStack }

procedure TP3DDefaultStack.Execute(AContext: TP3DPlotCommandContext);
var
  Item: TP3DPlotCommandPushDefault;
begin
  for Item in Self do
    Item.ShaderSettings.Execute( AContext );
end;

procedure TP3DDefaultStack.GetExtensions(Extension: TStringList);
var
  Item: TP3DPlotCommandPushDefault;
begin
  for Item in Self do
    Item.ShaderSettings.GetExtensions( Extension );
end;

procedure TP3DDefaultStack.MakeShaderModifications(ASender: TP3DPlotListSetting; ASenderIdx: Integer; ABuilder: TP3DShaderBuilder);
var
  Item: TP3DPlotCommandPushDefault;
begin
  for Item in Self do
    Item.ShaderSettings.MakeShaderModifications( ASender, Item.ShaderSettings.Count, ABuilder );
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







{ TP3DShaderBuilder }


procedure TP3DShaderBuilder.SetupRootNode;
var
  p: TP3DNodeSocketClone;
begin
  if ( Assigned( RootNode )) then
    begin
      RootNode.NodeMaster:= NodeMaster;
      if ( not Assigned( GeometryNode )) then
        P3DShaderNodeLib.FindNode( 'geometry' ).Clone( FGeometryNode );
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
  FRootNode:= TP3DStreamableContainer.Create( 'RootNode', TP3DNodeClone, smText );
  //FRootNode.OnChange:= @NodeChange;
  FGeometryNode:= TP3DStreamableContainer.Create( 'GeometryNode', TP3DNodeClone, smText );
  //FGeometryNode.OnChange:= @NodeChange;
  FUniforms:= TP3DShaderNodeTree.Create( 'Uniforms', smText );
  FUniforms.NodeMaster:= NodeMaster;
  //FUniforms.OnChange:= @NodesChange;
  Properties.Add([ FNodeTree, FRootNode, FGeometryNode, FUniforms ]);
end;

destructor TP3DShaderBuilder.Destroy;
begin
  {FreeAndNil( FNodeTree );
  FreeAndNil( FRootNode );
  FreeAndNil( FGeometryNode );
  FreeAndNil( FUniforms );}
  inherited Destroy;
end;

procedure TP3DShaderBuilder.MakeRootNode(AName: String);
var
  nd: TP3DNode;
begin
  nd:= P3DShaderNodeLib.FindNode( AName );
  if ( Assigned( nd )) then
    MakeRootNode( P3DShaderNodeLib.FindNode( AName ))
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
  FShader.Root:= P3DCoreContainers; //TODO: Change this
end;

destructor TP3DPlotListSetting.Destroy;
begin
  FreeAndNil( FShader );
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

procedure TP3DPlotListSetting.UpdateShader(AContext: TP3DPlotCommandContext);
var
  ShaderBuilder: TP3DShaderBuilder;
  Extensions: TStringList;
  i: Integer;
begin
  if ( not Assigned( Shader )) then
    if ( Assigned( FBuildShader )) then
      FBuildShader.BuildShader( FShader )
    else begin
      ShaderBuilder:= TP3DShaderBuilder.Create;
      Extensions:= TStringList.Create;
      ShaderBuilder.MakeRootNode( 'output' );

      AContext.SettingsDefault.GetExtensions( Extensions );
      AContext.SettingsDefault.MakeShaderModifications( Self, 0, ShaderBuilder );

      for i:= 0 to Count -1 do begin
        Items[ i ].GetExtensions( Extensions );
        Items[ i ].MakeShaderModifications( Self, i, ShaderBuilder );
      end;

      WriteLn( Extensions.Text );
      ShaderBuilder.BuildShader( Extensions, FShader );
      if ( Assigned( Shader )) then
        WriteLn( 'Shader assigned' )
      else
        WriteLn( 'No shader' );
      Extensions.Free;
      FreeAndNil( ShaderBuilder );
    end;
end;

procedure TP3DPlotListSetting.GetExtensions(Extension: TStringList);
var
  i: Integer;
begin
  for i:= 0 to Count -1 do
    Items[ i ].GetExtensions( Extension );
end;

procedure TP3DPlotListSetting.MakeShaderModifications(ASender: TP3DPlotListSetting; ASenderIdx: Integer; ABuilder: TP3DShaderBuilder);
var
  i: Integer;
begin
  for i:= 0 to Count -1 do
    Items[ i ].MakeShaderModifications( Self, i, ABuilder );
end;

procedure TP3DPlotListSetting.ExecuteAndBuildShader(AContext: TP3DPlotCommandContext);
begin
  if ( not Assigned( Shader )) then
    UpdateShader( AContext );
  if ( Assigned( Shader )) then begin
    AContext.Shader:= Shader;
    Shader.Enable;
    AContext.SettingsDefault.Execute( AContext );
    Execute( AContext );
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







{$ENDIF}
