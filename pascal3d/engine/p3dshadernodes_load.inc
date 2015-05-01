
procedure LoadVariableFromDOMNode( Module: String; SNV: TP3DShaderNodeVariableList; Node: TDOMNode );
  var
    Item: TP3DShaderNodeVariable;
    name: TDOMNode;
    vartype: TDOMNode;
    required: TDOMNode;
begin
  Item:= nil;
  case ( Node.NodeName ) of
    '#text':
      begin
        Item:= TP3DShaderNodeVariableInline.Create;
        //if ( Node.HasChildNodes ) then
        //  if ( Node.FirstChild.NodeName = '#text' ) then
        //     TP3DShaderNodeVariableInline( Item ).Text:= Node.FirstChild.NodeValue;
        TP3DShaderNodeVariableInline( Item ).Text:= StringReplace( Node.NodeValue, LineEnding, '', [rfReplaceAll]);
        WriteLn( 'inline: ' + Copy( TP3DShaderNodeVariableInline( Item ).Text, 1, 10 ) + '...' );
      end;
    'br':
    begin
      Item:= TP3DShaderNodeVariableInline.Create;
      TP3DShaderNodeVariableInline( Item ).Text:= LineEnding;
      WriteLn( 'inline: <br>' );
    end;
    'input':
      begin
        name:= Node.Attributes.GetNamedItem( 'name' );
        required:= Node.Attributes.GetNamedItem( 'required' );
        vartype:= Node.Attributes.GetNamedItem( 'type' );
        Item:= TP3DShaderNodeVariableInput.Create;

        if ( not ( Assigned( name ))) then
          raise Exception.Create( Format( 'Error while compiling %s. Input must have a name!', [ Module ]));
        TP3DShaderNodeVariableInput( Item ).Name:= name.NodeValue;

        if ( not ( Assigned( vartype ))) then
          TP3DShaderNodeVariableInput( Item ).VarType:= 'any'
        else
          TP3DShaderNodeVariableInput( Item ).VarType:= vartype.NodeValue;

        if ( Assigned( required ) AND ( required.NodeValue = 'yes' )) then
          TP3DShaderNodeVariableInput( Item ).Required:= True
        else
          TP3DShaderNodeVariableInput( Item ).Required:= False;

        if( Node.HasChildNodes ) then
          LoadVariablesFromDOMNode( Module, TP3DShaderNodeVariableInput( Item ).Fragments, Node );
        WriteLn( 'input ', TP3DShaderNodeVariableInput( Item ).Name, ':', Item.VarType, ' required: ', BoolToStr( TP3DShaderNodeVariableInput( Item ).Required ));
      end;
    'exists':
      begin
        name:= Node.Attributes.GetNamedItem( 'name' );
        if ( not ( Assigned( name ))) then
          raise Exception.Create( Format( 'Error while compiling %s. Exists must have a name!', [ Module ]));
        Item:= TP3DShaderNodeVariableExists.Create;
        TP3DShaderNodeVariableExists( Item ).Name:= name.NodeValue;
        if( Node.HasChildNodes ) then
          LoadVariablesFromDOMNode( Module, TP3DShaderNodeVariableExists( Item ).Fragments, Node );
        WriteLn( 'exists ', TP3DShaderNodeVariableExists( Item ).Name );
      end;
    'link':
      begin
        name:= Node.Attributes.GetNamedItem( 'name' );
        if ( not ( Assigned( name ))) then
          raise Exception.Create( Format( 'Error while compiling %s. Link must have a name!', [ Module ]));
        Item:= TP3DShaderNodeVariableLink.Create;
        TP3DShaderNodeVariableLink( Item ).Target:= name.NodeValue;
        if( Node.HasChildNodes ) then
          LoadVariablesFromDOMNode( Module, TP3DShaderNodeVariableLink( Item ).Fragments, Node );

        WriteLn( 'link: ' + Copy( TP3DShaderNodeVariableLink( Item ).Target, 1, 10 ) + '...' );
      end;
    '#comment':
      //DO NOTHING
  else
    raise Exception.Create( Format( 'Error while compiling %s. %s is not defined in this context!', [ Module, Node.NodeName ]));
  end;
  if ( Assigned( Item )) then
    SNV.Add( Item );
end;

procedure LoadVariablesFromDOMNode( Module: String; SNV: TP3DShaderNodeVariableList; Node: TDOMNode );
var
  child: TDOMNode;
begin
  if ( Node.HasChildNodes ) then
    begin
      child:= Node.FirstChild;
      while ( Assigned( child )) do
        begin
          LoadVariableFromDOMNode( Module, SNV, child );
          child:= child.NextSibling;
        end;
    end;
end;

function ShaderNodeFromDOMNode( Module: String; Node: TDOMNode ): TP3DShaderNode;

var
  name: TDOMNode;
begin
  Result:= TP3DShaderNode.Create;
  if ( Node.HasAttributes ) then
    begin
      name:= Node.Attributes.GetNamedItem( 'name' );
      Result.Name:= name.NodeValue;
    end
  else
    raise Exception.Create( Format( 'Error while compiling %s. Modules must have a name!', [ Module ]));
  Result.Module:= Module;
  WriteLn( 'Module: "', Module, '.', Result.Name, '"' );
  LoadVariablesFromDOMNode( Module, Result.Fragments, Node );
end;


procedure ProcessFile( NodeList: TP3DShaderNodeList; FN: String );

var
  F: TXMLDocument;
  i: Integer;
  sn: TP3DShaderNode;
  Module: String;
begin
  XMLRead.ReadXMLFile( F, FN );
  Module:= ExtractFileNameOnly( FN );

  if ( F.DocumentElement.NodeName = 'p3dshader' ) then
    for i:= 0 to F.DocumentElement.ChildNodes.Count - 1 do
      begin
        if ( F.DocumentElement.ChildNodes[ i ].NodeName = 'node' ) then
          begin
            sn:= ShaderNodeFromDOMNode( Module, F.DocumentElement.ChildNodes[ i ]);
            NodeList.Add( sn );
          end;
      end;
  F.Free;
end;
