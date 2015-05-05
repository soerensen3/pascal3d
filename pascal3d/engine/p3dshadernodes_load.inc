procedure LoadVariableFromDOMNode( Module: String; SNV: TP3DShaderNodeVariableList; Node: TDOMNode; SN: TP3DShaderNode );
  var
    Item: TP3DShaderNodeVariable;
    name: TDOMNode;
    vartype: TDOMNode;
    required: TDOMNode;
    list: TDOMNode;
    target: TDOMNode;
    Inp: TP3DShaderNodeInput;
begin
  Item:= nil;
  case ( Node.NodeName ) of
    '#text':
      begin
        Item:= TP3DShaderNodeVariableInline.Create( SN );
        //if ( Node.HasChildNodes ) then
        //  if ( Node.FirstChild.NodeName = '#text' ) then
        //     TP3DShaderNodeVariableInline( Item ).Text:= Node.FirstChild.NodeValue;
        TP3DShaderNodeVariableInline( Item ).Text:= StringReplace( Node.NodeValue, LineEnding, '', [rfReplaceAll]);
        WriteLn( 'inline: ' + Copy( TP3DShaderNodeVariableInline( Item ).Text, 1, 10 ) + '...' );
      end;
    'br':
    begin
      Item:= TP3DShaderNodeVariableInline.Create( SN );
      TP3DShaderNodeVariableInline( Item ).Text:= LineEnding;
      WriteLn( 'inline: <br>' );
    end;
    'link':
      begin
        target:= Node.Attributes.GetNamedItem( 'target' );
        list:= Node.Attributes.GetNamedItem( 'list' );
        if ( not ( Assigned( target ))) then
          raise Exception.Create( Format( 'Error while compiling %s. Link must have a target!', [ Module ]));
        Item:= TP3DShaderNodeVariableLink.Create( SN );
        TP3DShaderNodeVariableLink( Item ).Target:= target.NodeValue;
        if ( Assigned( list )) then
          TP3DShaderNodeVariableLink( Item ).List:= list.NodeValue = 'yes';
        if( Node.HasChildNodes ) then
          LoadVariablesFromDOMNode( Module, TP3DShaderNodeVariableLink( Item ).Fragments, Node, SN );

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

procedure LoadVariablesFromDOMNode( Module: String; SNV: TP3DShaderNodeVariableList; Node: TDOMNode; SN: TP3DShaderNode );
var
  child: TDOMNode;
begin
  if ( Node.HasChildNodes ) then
    begin
      child:= Node.FirstChild;
      while ( Assigned( child) and ( child.NodeName = 'input' )) do
        child:= child.NextSibling;
      while ( Assigned( child )) do
        begin
          LoadVariableFromDOMNode( Module, SNV, child, SN );
          child:= child.NextSibling;
        end;
    end;
end;

procedure LoadInputsFromDOMNode( Module: String; SN: TP3DShaderNode; Node: TDOMNode );
var
  child: TDOMNode;
  name: TDOMNode;
  vartype: TDOMNode;
  target: TDOMNode;
  Inp: TP3DShaderNodeInput;
begin
  if ( Node.HasChildNodes ) then
    begin
      child:= Node.FirstChild;
      while ( Assigned( child) and ( child.NodeName = 'input' )) do
        begin
          name:= Child.Attributes.GetNamedItem( 'name' );
          vartype:= Child.Attributes.GetNamedItem( 'type' );
          target:= Child.Attributes.GetNamedItem( 'target' );
          if ( not ( Assigned( name ))) then
            raise Exception.Create( Format( 'Error while compiling %s. Input must have a name!', [ Module ]));
          Inp:= TP3DShaderNodeInput.Create;
          Inp.FName:= name.NodeValue;
          if ( Assigned( vartype )) then
            Inp.VarType:= vartype.NodeValue;
          if ( Assigned( target )) then
            Inp.Target:= target.NodeValue;
          SN.Inputs.Add( Inp );

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
  LoadInputsFromDOMNode( Module, Result, Node );
  LoadVariablesFromDOMNode( Module, Result.Fragments, Node, Result );
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

  if ( F.DocumentElement.NodeName = 'p3dnodes' ) then
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
