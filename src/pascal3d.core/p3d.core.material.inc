//p3d.core.material.inc
{$IFDEF INTERFACE}
  type
     TP3DMaterial = class;

     { TP3DMaterialMap }

     TP3DMaterialMap = class ( TP3DNodeClone )
       protected
         FHasTransparency: Boolean;
         FMap: TP3DStreamablePointer;
         FParentMaterial: TP3DStreamablePointer;
         FTexChannel: Integer;

         function GetMap: TP3DTexture;
         function GetName: String;
         function GetParentMaterial: TP3DMaterial;
         procedure SetMap( AValue: TP3DTexture );
         procedure SetTexChannel(AValue: Integer); virtual;

       public
         constructor Create( const AOwner: IP3DStreamableContainer = nil ); override;
         destructor Destroy; override;

       published
         property TexChannel: Integer read FTexChannel write SetTexChannel;
         property Map: TP3DTexture read GetMap write SetMap;
         property ParentMaterial: TP3DMaterial read GetParentMaterial;
         property HasTransparency: Boolean read FHasTransparency;
         property Name: String read GetName;
     end;

     TP3DMaterialSubsetDict = specialize TFPGMap < String, TP3DShader >;

     TP3DMaterialMapClass = class of TP3DMaterialMap;


     {TP3DMaterialMapContainer = specialize gP3DStreamableContainer < TP3DMaterialMap >;
     TP3DMaterialMapPointer = specialize gP3DStreamablePointer < TP3DMaterialMap >;
     //TP3DMaterialMapContainerList = specialize gP3DStreamableContainerList < TP3DMaterialMap, TP3DMaterialMapContainer >;
     TP3DMaterialMapPointerList = specialize gP3DStreamableList < TP3DMaterialMap, TP3DMaterialMapPointer >;
     }
     { TP3DMaterialMapList }

     TP3DMaterialMapContainerList = class( specialize gP3DShaderNodeTree < TP3DMaterialMap > {specialize gP3DStreamableContainerList < TP3DMaterialMap >})
       private
         FMapClass: TP3DMaterialMapClass;
         FParentMaterial: TP3DMaterial;

       public
         procedure Notify(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction); override;

         property ParentMaterial: TP3DMaterial read FParentMaterial write FParentMaterial;
         //needs map class
     end;

     TP3DMaterial = class ( TP3DDataBlock, IP3DAnimatable )
       private
         FMaps: TP3DMaterialMapContainerList;
         FShader: TP3DStreamableContainer;
         FAction: TP3DStreamablePointer;

         function GetAction: TP3DAction;

         function GetHasTransparency: Boolean; virtual;
         procedure SetAction(AValue: TP3DAction);
         function GetShader: TP3DShader; virtual;
         procedure SetShader( AValue: TP3DShader ); virtual;
         function FindMap( AMap: TP3DTexture ): Integer;

       public
         class function GetContainer(ALib: TP3DLibrary): TP3DPropertyAccess; override;

         constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
         destructor Destroy; override;

         procedure PassToShader( const ActiveSubset: String = '' ); virtual;
         procedure BuildShader; virtual; abstract;

       published
         property Maps: TP3DMaterialMapContainerList read FMaps write FMaps;
         property Shader: TP3DShader read GetShader write SetShader;
         property HasTransparency: Boolean read GetHasTransparency;
         property Action: TP3DAction read GetAction write SetAction;
     end;

     {TP3DMaterialContainer = specialize gP3DStreamableContainer < TP3DMaterial >;
     TP3DMaterialPointer = specialize gP3DStreamablePointer < TP3DMaterial >;
     TP3DMaterialPointerList = specialize gP3DStreamableList < TP3DMaterial, TP3DMaterialPointer >;
     }
     TP3DMaterialContainerList = specialize gP3DNamedStreamableContainerList < TP3DMaterial >;

     {$INCLUDE p3d.core.material.deprecated.inc}
     { TP3DMaterialNode }

     TP3DMaterialNode = class ( TP3DMaterial )
       protected type
         TOnPassUniform = procedure ( Material: TP3DMaterialNode; Uniform: TP3DNodeClone ) of object;

       protected
         FOnPassUniform: TOnPassUniform;
         FAutoRecompile: Boolean;
         FNodeMaster: TP3DNodeMaster;
         //FTexNodes: TP3DShaderNodeTree;
         FRootNode: TP3DStreamableContainer;
         FGeometryNode: TP3DStreamableContainer;
         FNodeTree: TP3DShaderNodeTree;
         FUniforms: TP3DShaderNodeTree;

         function GetGeometryNode: TP3DNodeClone;
         procedure SetGeometryNode(AValue: TP3DNodeClone);
         function GetRootNode: TP3DNodeClone;
         procedure SetRootNode(AValue: TP3DNodeClone);
         procedure SetupRootNode;
         procedure MapsChange( ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction );
         procedure NodesChange( ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction );
         procedure NodeChange( ANewValue: TP3DStreamable );
         procedure UpdateTexNodes;
         function GetShader: TP3DShader; override;
         procedure SocketHandler( Sender: TP3DNodeSocketClone );

       public
         constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
         destructor Destroy; override;

         procedure MakeRootNode( AName: String );
         procedure MakeRootNode( ANode: TP3DNode );
         procedure BuildShader; override;
         procedure PassToShader(const ActiveSubset: String=''); override;
         function FindUniform( AName: String ): TP3DNodeClone;

         property RootNode: TP3DNodeClone read GetRootNode write SetRootNode;
         property GeometryNode: TP3DNodeClone read GetGeometryNode write SetGeometryNode;
         //property TexNodes: TP3DShaderNodeTree read FTexNodes;
         property NodeTree: TP3DShaderNodeTree read FNodeTree;
         property Uniforms: TP3DShaderNodeTree read FUniforms;
         property NodeMaster: TP3DNodeMaster read FNodeMaster;
         property AutoRecompile: Boolean read FAutoRecompile write FAutoRecompile;
         property OnPassUniform: TOnPassUniform read FOnPassUniform write FOnPassUniform;
     end;

  var
    P3DMaterialActive: TP3DMaterial = nil;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{$INCLUDE p3d.core.material.deprecated.inc}


{ TP3DMaterialMapShader }

procedure TP3DMaterialMap.SetMap(AValue: TP3DTexture);
var
  S: TP3DNodeSocketClone;
  n: String;
begin
  FMap.AsValue:= AValue;
  if ( Assigned( ParentMaterial )) then begin
    S:= FindInput( 'Map' );
    if ( S is TP3DShaderNodeSocketClone ) then begin
      n:= IntToStr( ParentMaterial.FindMap( AValue ));
      if ( n <> TP3DShaderNodeSocketClone( S ).ValueProp.AsString ) then
        TP3DShaderNodeSocketClone( S ).ValueProp.AsString:= n;
    end;
  end;
end;

procedure TP3DMaterialMap.SetTexChannel(AValue: Integer);
begin
  if FTexChannel=AValue then Exit;
  FTexChannel:=AValue;
end;


function TP3DMaterialMap.GetName: String;
begin
  if ( Assigned( Map )) then
    Result:= Map.Name
  else
    Result:= '<None>';
end;

function TP3DMaterialMap.GetParentMaterial: TP3DMaterial;
begin
  Result:= TP3DMaterial( FParentMaterial.AsValue );
end;

function TP3DMaterialMap.GetMap: TP3DTexture;
begin
  Result:= TP3DTexture( FMap.AsValue );
end;

constructor TP3DMaterialMap.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FTexChannel:= 0;
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'TexChannel', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Name' ));
  FMap:= TP3DStreamablePointer.Create( 'Map', TP3DTexture, smText );
  FParentMaterial:= TP3DStreamablePointer.Create( 'ParentMaterial', TP3DMaterial );
  FParentMaterial.AsValue:= TP3DStreamable( TP3DPropAccessStreamable( Owner.GetContainer.Context.GetInstance ).Context.GetInstance );

  Properties.Add([ FParentMaterial, FMap ]);
end;

{
function TP3DMaterialMap.SaveToDOM(AParent: TDOMElement): TDOMElement;
var
  i: Integer;
begin
  Result:= AParent.OwnerDocument.CreateElement( 'map' );
  AParent.AppendChild( Result );
  for i:= 0 to Properties.Count - 1 do
    Result.SetAttribute( Properties[ i ].Name, Properties[ i ].AsString );
end;
}

destructor TP3DMaterialMap.Destroy;
begin
  if ( Assigned( FMap ) and Assigned( Map )) then
    Map:= nil;
  inherited Destroy;
end;

{ TP3DMaterial }

procedure TP3DMaterial.SetShader(AValue: TP3DShader);
begin
  FShader.AsValue:= AValue;
end;

function TP3DMaterial.FindMap(AMap: TP3DTexture): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Maps.Count - 1 do
    if ( Maps[ i ].Map = AMap ) then begin
      Result:= i;
      break;
    end;
end;


class function TP3DMaterial.GetContainer(ALib: TP3DLibrary): TP3DPropertyAccess;
begin
  Result:= ALib.Properties.GetPropByName( 'Materials' );
end;

function TP3DMaterial.GetAction: TP3DAction;
begin
  Result:= TP3DAction( FAction.AsValue );
end;

function TP3DMaterial.GetHasTransparency: Boolean;
begin
  Result:= False;
end;

procedure TP3DMaterial.SetAction(AValue: TP3DAction);
begin
  FAction.AsValue:= AValue;
end;

function TP3DMaterial.GetShader: TP3DShader;
begin
  Result:= TP3DShader( FShader.AsValue );
end;

constructor TP3DMaterial.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );

  Maps:= TP3DMaterialMapContainerList.Create( 'Maps', smText );
  Maps.ParentMaterial:= Self;
  Maps.FItemClass:= TP3DMaterialMap;
  Properties.Add( Maps );
  FShader:= TP3DStreamableContainer.Create( 'Shader', TP3DShader );
  Properties.Add( FShader );
end;

destructor TP3DMaterial.Destroy;
begin
  //Owner.Materials.Remove( Self );
  WriteLn( 'Destroying Material: ' + Name );
  inherited Destroy;
end;

procedure TP3DMaterial.PassToShader(const ActiveSubset: String);
var
  i: Integer;
  decl: TP3DShaderDecl;
  S: TP3DNodeSocketClone;
  n: LongInt;
begin
  P3DMaterialActive:= Self;
  if ( Assigned( Shader )) then
    begin
      Shader.Enable;

      for i:= 0 to Maps.Count - 1 do
        if ( Shader.Uniforms.FindByName( 'tex' + IntToStr( i ), decl )) then
          begin
            glActiveTexture( GL_TEXTURE0 + i ); cle( Self );
            {glBindTexture( Maps[ i ].Map.Target, Maps[ i ].Map.GLTexture ); cle( Self );}
            S:= Maps[ i ].FindInput( 'Map' );
            if ( Assigned( Maps[ i ].Map ) and Assigned( S )) then begin
              n:= StrToInt( S.ValueProp.AsString );
              Maps[ i ].Map.Bind( n );
              decl.AsInt:= n;
            end else begin
              glBindTexture( GL_TEXTURE_2D, 0 );
              decl.AsInt:= i;
            end;
          end;
      Inc( i );
      while ( Shader.Uniforms.FindByName( 'tex' + IntToStr( i ), decl )) do
        begin
          glActiveTexture( GL_TEXTURE0 + i ); cle( Self );
          glBindTexture( GL_TEXTURE_2D, 0 );
          decl.AsInt:= i;
        end;
    end;
end;

{ TP3DMaterialMapContainerList }

procedure TP3DMaterialMapContainerList.Notify(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction);
begin
  inherited Notify( ASender, AItem, AAction );
end;

{ TP3DMaterialNode }

function TP3DMaterialNode.GetGeometryNode: TP3DNodeClone;
begin
  Result:= TP3DNodeClone( FGeometryNode.AsValue );
end;

procedure TP3DMaterialNode.SetGeometryNode(AValue: TP3DNodeClone);
begin
  if ( Assigned( GeometryNode )) then begin
    if ( Assigned( NodeMaster )) then
      NodeMaster.RemoveNode( GeometryNode );
    GeometryNode.Free;
  end;
  FGeometryNode.AsValue:= AValue;
  FNeedsUpdate:= True;
  if ( Assigned( GeometryNode ) and Assigned( NodeMaster )) then begin
    GeometryNode.NodeMaster:= NodeMaster;
    NodeMaster.AddNode( GeometryNode );
  end;
end;

function TP3DMaterialNode.GetRootNode: TP3DNodeClone;
begin
  Result:= TP3DNodeClone( FRootNode.AsValue );
end;

procedure TP3DMaterialNode.SetRootNode(AValue: TP3DNodeClone);
begin
  if ( Assigned( RootNode )) then begin
    if ( Assigned( NodeMaster )) then
      NodeMaster.RemoveNode( RootNode );
    RootNode.Free;
  end;
  FRootNode.AsValue:= AValue;
  FNeedsUpdate:= True;
  if ( Assigned( RootNode ) and Assigned( NodeMaster )) then begin
    RootNode.NodeMaster:= NodeMaster;
    NodeMaster.AddNode( RootNode );
  end;
end;

procedure TP3DMaterialNode.SetupRootNode;
begin
  if ( Assigned( RootNode )) then
    begin
      RootNode.NodeMaster:= NodeMaster;
      if ( not Assigned( GeometryNode )) then
        P3DData.P3DShaderNodeLib.FindNode( 'geometry' ).Clone( FGeometryNode );
      GeometryNode.NodeMaster:= NodeMaster;
    end;
end;

procedure TP3DMaterialNode.MapsChange(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction);
var
  Nd: TP3DNode;
begin
  FNeedsUpdate:= True;
  UpdateTexNodes;
  //TP3DMaterialMap( AItem ).;

  {if ( Assigned( AItem )) then
    if ( AAction = actAdd ) then
      begin
        Nd:= P3DData.P3DShaderNodeLib.FindNode( 'texture2D' );
        Result:= Nd.Clone( TexNodes );
        Result.FindInput( 'texture2D' ).ValueProp.AsString:= IntToStr( Index );
        if ( not Assigned( Nd )) then
          exit;
        NdC:= Nd.Clone( TexNodes );
        NdC.NodeMaster:= NodeMaster
        if ( Assigned( NodeMaster )) then
          NodeMaster.AddNode( NdC );
      end
    else
      begin
        TP3DNodeClone( AItem ).NodeMaster:= nil;
        if ( Assigned( NodeMaster )) then
          NodeMaster.RemoveNode( TP3DNodeClone( AItem ))
      end;}
  {UpdateTexNodes;

  if ( AAction = actAdd ) then
    TP3DNodeClone( AItem ).NodeMaster:= NodeMaster
  else
    TP3DNodeClone( AItem ).NodeMaster:= nil;

  if ( Assigned( NodeMaster )) then
    case AAction of
      actAdd: NodeMaster.AddNode( TP3DNodeClone( AItem ));
      actDelete, actExtract, actClear: NodeMaster.RemoveNode( TP3DNodeClone( AItem ));
    end;}

end;

procedure TP3DMaterialNode.NodesChange(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction);
begin
  FNeedsUpdate:= True;

  {if ( AAction = actAdd ) then
    TP3DNodeClone( AItem ).NodeMaster:= NodeMaster
  else if ( Assigned( AItem )) then
    TP3DNodeClone( AItem ).NodeMaster:= nil;}

  if ( Assigned( NodeMaster )) then
    case AAction of
      actAdd: NodeMaster.AddNode( TP3DNodeClone( AItem ));
      actDelete, actExtract, actClear: NodeMaster.RemoveNode( TP3DNodeClone( AItem ));
    end;
end;

procedure TP3DMaterialNode.NodeChange(ANewValue: TP3DStreamable);
begin
  FNeedsUpdate:= True;
end;

procedure TP3DMaterialNode.UpdateTexNodes;
var
  i: Integer;
  Nd: TP3DNode;
  TexNd: TP3DNodeClone;
  s: TP3DNodeSocketClone;

  {function AddTexNode( Index: Integer ): TP3DNodeClone;
  begin
    //if ( TexNodes.Count <= Index ) then
      begin
        Result:= Nd.Clone( TexNodes );
        Result.FindInput( 'texture2D' ).ValueProp.AsString:= IntToStr( Index );
        Result.NodeMaster:= NodeMaster;
        if ( Assigned( NodeMaster )) then
          NodeMaster.AddNode( Result );
        //TexNodes.Add( Result )
      end
    {else
      begin
        Result:= TexNodes[ Index ];
        Result.FindInput( 'texture2D' ).ValueProp.AsString:= IntToStr( Index );
      end;}
  end;}

begin
  for i:= 0 to Maps.Count - 1 do begin
    s:= Maps[ i ].FindInput( 'Map' );
    if ( s is TP3DShaderNodeSocketClone ) then
      TP3DShaderNodeSocketClone( s ).ValueProp.AsString:= IntToStr( FindMap( Maps[ i ].Map ));
  end;

  {Nd:= P3DData.P3DShaderNodeLib.FindNode( 'texture2D' );
  if ( not Assigned( Nd )) then
    exit;
  //TexNodes.Clear;
  for i:= 0 to Maps.Count - 1 do
    if ( i >= TexNodes.Count ) then
      AddTexNode( i );
  FNeedsUpdate:= True;
  Inc( i );
  while ( i < TexNodes.Count - 1 ) do
    begin
      TexNd:= TexNodes[ i ];
      TexNodes.Delete( i );
      TexNd.Free;
    end;}
end;

function TP3DMaterialNode.GetShader: TP3DShader;
begin
  if ( NeedsUpdate ) then
    BuildShader;
  Result:= inherited GetShader;
end;

procedure TP3DMaterialNode.SocketHandler(Sender: TP3DNodeSocketClone);
begin
  FNeedsUpdate:= True;
end;

function TP3DMaterialNode.FindUniform(AName: String): TP3DNodeClone;
var
  UnName: String;
  Node: TP3DNodeClone;
begin
  Result:= nil;
  for Node in Uniforms do
    begin
      UnName:= Node.FindInput( 'UnName' ).ValueProp.AsString;
      if ( UnName = AName ) then begin
        Result:= Node;
        Break;
      end;
    end;
end;

constructor TP3DMaterialNode.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FNodeMaster:= TP3DNodeMaster.Create;

  FNodeTree:= TP3DShaderNodeTree.Create( 'NodeTree', smText );
  FNodeTree.OnChange:= @NodesChange;
  FNodeTree.NodeMaster:= NodeMaster;
  FRootNode:= TP3DStreamableContainer.Create( 'RootNode', TP3DNodeClone, smText );
  FRootNode.OnChange:= @NodeChange;
  FGeometryNode:= TP3DStreamableContainer.Create( 'GeometryNode', TP3DNodeClone, smText );
  FGeometryNode.OnChange:= @NodeChange;
  //FTexNodes:= TP3DShaderNodeTree.Create( 'TexNodes', smText );
  //FTexNodes.NodeMaster:= NodeMaster;
  Maps.NodeMaster:= NodeMaster;
  FUniforms:= TP3DShaderNodeTree.Create( 'Uniforms', smText );
  FUniforms.NodeMaster:= NodeMaster;
  FUniforms.OnChange:= @NodesChange;
  Properties.Add([ FNodeTree, FRootNode, FGeometryNode, {FTexNodes,} FUniforms ]);
  Maps.OnChange:= @NodesChange;
  FNeedsUpdate:= True;
  FAutoRecompile:= True;

  NodeMaster.AddSocketSetConnectedHandler( @SocketHandler );
  NodeMaster.AddSocketSetValueHandler( @SocketHandler );
end;

destructor TP3DMaterialNode.Destroy;
begin
  if ( Assigned( Maps )) then
    begin
      Maps.OnChange:= nil;
      Maps.AfterChange:= nil;
    end;
  if ( Self = P3DMaterialActive ) then
    P3DMaterialActive:= nil;
  inherited Destroy;
  FreeAndNil( FNodeMaster );
end;

procedure TP3DMaterialNode.MakeRootNode(AName: String);
var
  nd: TP3DNode;
begin
  nd:= P3DData.P3DShaderNodeLib.FindNode( AName );
  if ( Assigned( nd )) then
    MakeRootNode( P3DData.P3DShaderNodeLib.FindNode( AName ))
  else
    raise Exception.Create( 'Cannot make copy to material root node. The specified node was not found: ' + AName );
end;

procedure TP3DMaterialNode.MakeRootNode(ANode: TP3DNode);
begin
  if ( Assigned( ANode )) then
    begin
      ANode.Clone( FRootNode );
      SetupRootNode;
    end
  else
    raise Exception.Create( 'Cannot make copy to material root node. The passed node was not assigned!' );
end;

procedure TP3DMaterialNode.BuildShader;
var
  sh: TP3DShaderCompiled;
  i: Integer;
  Buffer, frag, vert: TP3DShaderBuffer;
  Socket: TP3DNodeSocketClone;
begin
  try
    sh:= //MatNode.NodeTree.Compile;
    TP3DShaderCompiled.Create;
    if ( Assigned( RootNode )) then
      for Socket in RootNode.Outputs do
        if ( Socket.CloneOf.SocketType = 'text' ) then
          begin
            Buffer:= TP3DShaderBuffer.Create;
            Buffer.Name:= Socket.CloneOf.Name;
            Buffer.Code:= TP3DShaderNodeSocketClone( Socket ).GetOutput( Buffer );
            sh.Buffers.Add( Buffer );
            P3DLog.LogInfoXML( Self, '<details><summary>Built ' + Buffer.Name + ' from chunks <br/></summary><pre><code class="glsl">' + P3DTextToHTML( Buffer.Code ) + '</code></pre></details>' );
          end;
  except
    on E: Exception do
      P3DLog.LogException( Self, E );
  end;
  FNeedsUpdate:= False;
  //for i:= 0 to sh.Buffers.Count - 1 do
  //  P3DSaveStringToFile( '/tmp/' + sh.Buffers[ i ].Name + '.glsl', sh.Buffers[ i ].Code );

  try
    frag:= sh.FindBuffer( 'fshader' );
    vert:= sh.FindBuffer( 'vshader' );
    if ( Assigned( frag ) and Assigned( vert )) then
      TP3DShader.CreateFromVertexAndFragmentShader( vert.Code, frag.Code, FShader );
  finally
    sh.Free;
  end;
end;

procedure TP3DMaterialNode.PassToShader(const ActiveSubset: String);
var
  Node: TP3DNodeClone;
  UnName: String;
  UnValue: TP3DPropertyAccess;
  decl: TP3DShaderDecl;
begin
  inherited PassToShader(ActiveSubset);
  if ( FNeedsUpdate ) then
    BuildShader;
  if ( Assigned( Shader )) then
    for Node in Uniforms do
      begin
        if ( Assigned( FOnPassUniform )) then
          FOnPassUniform( Self, Node );
        UnName:= Node.FindInput( 'UnName' ).ValueProp.AsString;
        UnValue:= Node.FindInput( 'Value' ).ValueProp;
        if ( Shader.Uniforms.FindByName( UnName, decl )) then
          begin
            case decl.VarType of
              GL_INT,
              GL_SAMPLER_1D,
              GL_SAMPLER_2D,
              GL_SAMPLER_3D,
              GL_SAMPLER_CUBE:
                if ( UnValue is TP3DPropAccessInt ) then
                  decl.AsInt:= TP3DPropAccessInt( UnValue ).AsValue
                else
                  decl.AsInt:= StrToInt( UnValue.AsString );
              GL_FLOAT:
                if ( UnValue is TP3DPropAccessFloat ) then
                  decl.AsFloat:= TP3DPropAccessFloat( UnValue ).AsValue
                else
                  decl.AsFloat:= StrToFloat( UnValue.AsString );
              GL_FLOAT_VEC2:
                if ( UnValue is TP3DPropAccessVec2 ) then
                  decl.AsVec2:= TP3DPropAccessVec2( UnValue ).AsValue
                else
                  decl.AsVec2:= vec2( UnValue.AsString );
              GL_FLOAT_VEC3:
                if ( UnValue is TP3DPropAccessVec3 ) then
                  decl.AsVec3:= TP3DPropAccessVec3( UnValue ).AsValue
                else
                  decl.AsVec3:= vec3( UnValue.AsString );
              GL_FLOAT_VEC4:
                if ( UnValue is TP3DPropAccessVec4 ) then
                  decl.AsVec4:= TP3DPropAccessVec4( UnValue ).AsValue
                else
                  decl.AsVec4:= vec4( UnValue.AsString );
              {GL_FLOAT_MAT2: Result:= 'mat2';
              GL_FLOAT_MAT3: Result:= 'mat3';
              GL_FLOAT_MAT4: Result:= 'mat4';
              GL_FLOAT_MAT2x3: Result:= 'mat2x3';
              GL_FLOAT_MAT2x4: Result:= 'mat2x3';
              GL_FLOAT_MAT3x2: Result:= 'mat3x2';
              GL_FLOAT_MAT3x4: Result:= 'mat3x4';
              GL_FLOAT_MAT4x2: Result:= 'mat4x2';
              GL_FLOAT_MAT4x3: Result:= 'mat4x3';}
            end;
          end;
      end;
end;



{$ENDIF}

{$IFDEF FINALIZATION}
  P3DMaterialActive:= nil;
{$ENDIF}

