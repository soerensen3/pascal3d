procedure LoadVariableFromDOMNode( Module: TP3DShaderModule; SNV: TP3DShaderNodeFragmentList; Node: TDOMNode; SN: TP3DShaderNode );
  var
    Item: TP3DShaderNodeFragment;
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
        Item:= TP3DShaderNodeFragmentInline.Create( SN );
        //if ( Node.HasChildNodes ) then
        //  if ( Node.FirstChild.NodeName = '#text' ) then
        //     TP3DShaderNodeFragmentInline( Item ).Text:= Node.FirstChild.NodeValue;
        TP3DShaderNodeFragmentInline( Item ).Text:= StringReplace( Node.NodeValue, LineEnding, '', [rfReplaceAll]);
        {$IFDEF DEBUG}
        WriteLn( 'inline: ' + Copy( TP3DShaderNodeFragmentInline( Item ).Text, 1, 10 ) + '...' );
        {$ENDIF}
      end;
    'br':
    begin
      Item:= TP3DShaderNodeFragmentInline.Create( SN );
      TP3DShaderNodeFragmentInline( Item ).Text:= LineEnding;
      {$IFDEF DEBUG}
      WriteLn( 'inline: <br>' );
      {$ENDIF}
    end;
    'link':
      begin
        target:= Node.Attributes.GetNamedItem( 'target' );
        list:= Node.Attributes.GetNamedItem( 'list' );
        if ( not ( Assigned( target ))) then
          raise Exception.Create( Format( 'Error while compiling %s. Link must have a target!', [ Module ]));
        Item:= TP3DShaderNodeFragmentLink.Create( SN );
        TP3DShaderNodeFragmentLink( Item ).Target:= target.NodeValue;
        if ( Assigned( list )) then
          TP3DShaderNodeFragmentLink( Item ).List:= list.NodeValue = 'yes';
        if( Node.HasChildNodes ) then
          LoadVariablesFromDOMNode( Module, TP3DShaderNodeFragmentLink( Item ).Fragments, Node, SN );

        {$IFDEF DEBUG}
        WriteLn( 'link: ' + Copy( TP3DShaderNodeFragmentLink( Item ).Target, 1, 10 ) + '...' );
        {$ENDIF}
      end;
    '#comment':
      //DO NOTHING
  else
    raise Exception.Create( Format( 'Error while compiling %s. %s is not defined in this context!', [ Module, Node.NodeName ]));
  end;
  if ( Assigned( Item )) then
    SNV.Add( Item );
end;

procedure LoadVariablesFromDOMNode( Module: TP3DShaderModule; SNV: TP3DShaderNodeFragmentList; Node: TDOMNode; SN: TP3DShaderNode );
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

procedure LoadModuleInputFromDOMNode( Module: TP3DShaderModule; Node: TDOMNode );
var
  child: TDOMNode;
  name: TDOMNode;
  vartype: TDOMNode;
  Inp: TP3DShaderNodeInput;
begin
  name:= Node.Attributes.GetNamedItem( 'name' );
  vartype:= Node.Attributes.GetNamedItem( 'type' );
  if ( not ( Assigned( name ))) then
    raise Exception.Create( Format( 'Error while compiling %s. Input must have a name!', [ Module ]));
  Inp:= TP3DShaderNodeInput.Create;
  Inp.FName:= name.NodeValue;
  if ( Assigned( vartype )) then
    Inp.VarType:= vartype.NodeValue;
  Module.Inputs.Add( Inp );
end;

function ShaderNodeFromDOMNode( Module: TP3DShaderModule; Node: TDOMNode ): TP3DShaderNode;
var
  name: TDOMNode;
  unique: TDOMNode;
begin
  Result:= TP3DShaderNode.Create;
  name:= Node.Attributes.GetNamedItem( 'name' );
  unique:= Node.Attributes.GetNamedItem( 'unique' );
  if ( Assigned( name )) then
    Result.Name:= name.NodeValue
  else
    raise Exception.Create( Format( 'Error while compiling %s. Modules must have a name!', [ Module ]));
  if ( Assigned( unique )) then
    Result.Unique:= unique.NodeValue = 'yes';
  Result.Module:= Module;
  {$IFDEF DEBUG}
  WriteLn( 'Module: "', Module.Name, '.', Result.Name, '"' );
  {$ENDIF}
  //LoadInputsFromDOMNode( Module, Result, Node );
  LoadVariablesFromDOMNode( Module, Result.Fragments, Node, Result );
end;


function ProcessFile(FN: String): TP3DShaderModule;

var
  F: TXMLDocument;
  i: Integer;
  sn: TP3DShaderNode;
begin
  XMLRead.ReadXMLFile( F, FN );
  Result:= TP3DShaderModule.Create;
  Result.Name:= ExtractFileNameOnly( FN );

  if ( F.DocumentElement.NodeName = 'p3dnodes' ) then
    for i:= 0 to F.DocumentElement.ChildNodes.Count - 1 do
      begin
        if ( F.DocumentElement.ChildNodes[ i ].NodeName = 'node' ) then
          begin
            sn:= ShaderNodeFromDOMNode( Result, F.DocumentElement.ChildNodes[ i ]);
            Result.Nodes.Add( sn );
          end
        else if ( F.DocumentElement.ChildNodes[ i ].NodeName = 'input' ) then
          LoadModuleInputFromDOMNode( Result, F.DocumentElement.ChildNodes[ i ]);
      end;
  F.Free;
end;
