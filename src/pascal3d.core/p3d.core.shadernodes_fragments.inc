{ p3dshadernodes_fragments.inc }

{$IFDEF INTERFACE}

{ TP3DShaderNodeFragment }
// Base Type for all Fragments
TP3DShaderNodeFragment = class
  private
    function InternalGetStringOutput( ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer ): String; virtual;

  public
    constructor Create;

    // Parse the node and return string output
    function GetStringOutput( ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer ): String; virtual;
end;

TP3DShaderNodeFragmentList = specialize gP3DCustomObjectList < TP3DShaderNodeFragment >;

{ TP3DShaderNodeFragmentInline }
{
 inline text is just written as a child element of any other node
 ...
 <node name="fshader">
 #version 330
 </node>
 ...
}

{ TP3DShaderNodeFragmentInput }

TP3DShaderNodeFragmentInput = class( TP3DShaderNodeFragment )
  private
    FDefines: TStringList;
    FInputName: String;

  public
    constructor Create;
    destructor Destroy; override;

    function InternalGetStringOutput( ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer ): String; override;

    property InputName: String read FInputName write FInputName;
    property Defines: TStringList read FDefines;
end;

TP3DShaderNodeFragmentInline = class( TP3DShaderNodeFragment )
  private
    FText: String;

  public
    function InternalGetStringOutput( ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer ): String; override;

    property Text: String read FText write FText;
end;

{ TP3DShaderNodeFragmentIfDef }

TP3DShaderNodeFragmentIfDef = class( TP3DShaderNodeFragment )
  private
    FCheckDefs: TStringList;
    FFragments: TP3DShaderNodeFragmentList;

    function InternalGetStringOutput(ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer): String; override;

  public
    constructor Create;
    destructor Destroy; override;

    function GetStringOutput( ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer ): String; override;

    property CheckDefs: TStringList read FCheckDefs;
    property Fragments: TP3DShaderNodeFragmentList read FFragments write FFragments;
end;

{ TP3DShaderNodeFragmentSingleOnly }

TP3DShaderNodeFragmentSingleOnly = class( TP3DShaderNodeFragment )
  private
    FFragments: TP3DShaderNodeFragmentList;

    function InternalGetStringOutput(ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer): String; override;
    function InhibitOutput( ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer ): Boolean;
    function Serialize( ACloneSocket: TP3DNodeSocketClone ): String;

  public
    constructor Create;
    destructor Destroy; override;

    function GetStringOutput( ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer ): String; override;

    property Fragments: TP3DShaderNodeFragmentList read FFragments write FFragments;
end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DShaderNodeFragmentInline }

function TP3DShaderNodeFragmentInline.InternalGetStringOutput(ACloneSocket: TP3DNodeSocketClone;
  Buffer: TP3DShaderBuffer): String;
begin
  Result:= //'/*' + ACloneSocket.Node.CloneOf.Name + '.' + ACloneSocket.CloneOf.Name + '-->*/' +
           Text
           //+ '/*<--' + ACloneSocket.Node.CloneOf.Name + '.' + ACloneSocket.CloneOf.Name + '*/'
end;

{ TP3DShaderNodeFragment }

function TP3DShaderNodeFragment.InternalGetStringOutput(ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer ): String;
begin
  Result:= '';
end;

constructor TP3DShaderNodeFragment.Create;
begin
  inherited Create;
end;

function TP3DShaderNodeFragment.GetStringOutput(ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer): String;
begin
  Result:= InternalGetStringOutput( ACloneSocket, Buffer );
  //WriteLn( 'Getting fragement output: "', Result, '"' );
end;


{ TP3DShaderNodeFragmentIfDef }

function TP3DShaderNodeFragmentIfDef.InternalGetStringOutput(ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer): String;
var
  Frag: TP3DShaderNodeFragment;
begin
  Result:= '';
  for Frag in Fragments do
    Result+= Frag.GetStringOutput( ACloneSocket, Buffer );
end;

constructor TP3DShaderNodeFragmentIfDef.Create;
begin
  inherited;
  Fragments:= TP3DShaderNodeFragmentList.Create;
  FCheckDefs:= TStringList.Create;
end;

destructor TP3DShaderNodeFragmentIfDef.Destroy;
begin
  FCheckDefs.Free;
  Fragments.Clear;
  Fragments.Free;
  inherited Destroy;
end;

function TP3DShaderNodeFragmentIfDef.GetStringOutput(ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer ): String;

  function FindAllDefines: Boolean;
  var
    S: String;
  begin
    Result:= True;
    for S in CheckDefs do
      if ( Buffer.Defines.IndexOf( S ) < 0 ) then
        begin
          Result:= False;
          Break;
        end;
  end;

begin
  //WriteLn( 'IFDEF ', Caption, ' defines=', Buffer.Defines.CommaText, ' defined?=', Buffer.Defines.IndexOf( Caption ) > -1 );
  if ( FindAllDefines ) then
    Result:= InternalGetStringOutput( ACloneSocket, Buffer )
  else
    Result:= '';

  {else
    WriteLn( 'ifdef failed: ', Caption, ' was not defined!' );}
end;

{ TP3DShaderNodeFragmentSingleOnly }

function TP3DShaderNodeFragmentSingleOnly.InternalGetStringOutput(ACloneSocket: TP3DNodeSocketClone;
  Buffer: TP3DShaderBuffer): String;
begin
  Result:= inherited InternalGetStringOutput(ACloneSocket, Buffer);
end;


function TP3DShaderNodeFragmentSingleOnly.InhibitOutput(ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer): Boolean;
var
  i: Integer;
begin
  Result:= ( Buffer.Declarations.IndexOf( Serialize( ACloneSocket )) > -1 );
  //for i:= 0 to Buffer.Declarations.Count - 1 do
  //  WriteLn( Buffer.Declarations[ i ]);
  //WriteLn( 'Inhibit ', Serialize( ACloneSocket ), ': ', Result );
end;

function TP3DShaderNodeFragmentSingleOnly.Serialize(ACloneSocket: TP3DNodeSocketClone): String;
begin
  Result:= IntToHex( Int64( Self ), 8 ) + ':' + Self.ClassName + '_' + ACloneSocket.ValueProp.Name + '=' + ACloneSocket.ValueProp.AsString;
end;

constructor TP3DShaderNodeFragmentSingleOnly.Create;
begin
  inherited;
  Fragments:= TP3DShaderNodeFragmentList.Create;
end;

destructor TP3DShaderNodeFragmentSingleOnly.Destroy;
begin
  Fragments.Clear;
  Fragments.Free;
  inherited Destroy;
end;

function TP3DShaderNodeFragmentSingleOnly.GetStringOutput( ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer ): String;
begin
  if ( not InhibitOutput( ACloneSocket, Buffer )) then
    Result:= InternalGetStringOutput( ACloneSocket, Buffer );
  Buffer.RegisterFragment( Self, TP3DShaderNodeSocketClone( ACloneSocket ));
end;

{ TP3DShaderNodeFragmentInput }

constructor TP3DShaderNodeFragmentInput.Create;
begin
  inherited Create;
  FDefines:= TStringList.Create;
end;

destructor TP3DShaderNodeFragmentInput.Destroy;
begin
  FDefines.Free;
  inherited Destroy;
end;

function TP3DShaderNodeFragmentInput.InternalGetStringOutput(ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer): String;
var
  i: Integer;
  Defs: String;
  Socket: TP3DNodeSocketClone;
  S: TP3DShaderNodeSocket;
  inp, filter: String;
  Inputs, Types: TP3DNodeSocketClonePointerList;
  Nd: TP3DNode;
  T: TP3DShaderNodeType;

begin
  Defs:= Buffer.Defines.Text;
  Buffer.Defines.AddStrings( Defines );
  Result:= '';
  inp:= ExtractWord( 1, InputName, [ ' ', #8 ]);
  filter:= ExtractWord( 2, InputName, [ ' ', #8 ]);
  if ( inp = 'all' ) then
    begin
      Inputs:= TP3DNodeSocketClonePointerList.Create( 'Inputs' );
      Inputs.Root:= ACloneSocket.Root;
      Inputs.Context:= ACloneSocket;
      Types:= TP3DNodeSocketClonePointerList.Create( 'Types' );
      Types.Root:= ACloneSocket.Root;
      Types.Context:= ACloneSocket;
      for i:= 0 to ACloneSocket.Node.Inputs.Count - 1 do begin
        TP3DShaderNodeSocketClone( ACloneSocket.Node.Inputs[ i ]).GetAllInputs( Buffer, filter, Inputs, Types );
      end;
      if ( Assigned( Inputs )) then
        for i:= Types.Count - 1 downto 0 do
          begin
            //if ( Types[ i ].CloneOf.SocketType = filter ) then
            T:= P3DData.P3DShaderNodeLib.FindType( TP3DShaderNodeSocket( Types[ i ].CloneOf ).SocketType );
            if ( Assigned( T )) then
              Result+= T.GetOutputAsType( Buffer, Types[ i ], filter );
          end;
        for i:= Inputs.Count - 1 downto 0 do
          begin
            //if ( Inputs[ i ].CloneOf.Name = filter ) then
              Result+= TP3DShaderNodeSocket( Inputs[ i ].CloneOf ).GetOutput( Buffer, Inputs[ i ]);

      //P3DLog.LogInfo( Self, 'getting all inputs output of: ' + ACloneSocket.Node.CloneOf.Name + '.' + ACloneSocket.CloneOf.Name + ' = "' + Result + '"' );
          end;
      Types.Free;
      Inputs.Free;
    end
  else if ( InputName = 'inject' ) then begin
    if ( Assigned( ACloneSocket.Connected )) then
      Result:= TP3DShaderNodeSocketClone( ACloneSocket.Connected ).GetOutput( Buffer )
    else
      Result:= ACloneSocket.ValueProp.AsString;
    {if ( ACloneSocket.CloneOf.SocketType = 'text' ) then begin
      Result:= ACloneSocket.ValueProp.AsString
      ;P3DLog.LogInfo( Self, 'getting text output of: ' + ACloneSocket.Node.CloneOf.Name + '.' + ACloneSocket.CloneOf.Name + ' = "' + Result + '"' );
    end else

      begin
        Nd:= P3DData.P3DShaderNodeLib.FindNode( ACloneSocket.CloneOf.SocketType );
        if ( not Assigned( Nd )) then
          raise Exception.Create( 'Could not find node for socket type!' );
        S:= TP3DShaderNodeSocket( Nd.Outputs.GetByName( 'fromvalue' ));
        if ( not Assigned( S )) then
          raise Exception.Create( 'Could not find output fromvalue!' );
        Result:= S.GetOutput( Buffer, ACloneSocket );
        P3DLog.LogInfo( Self, 'injecting ' + ACloneSocket.Node.CloneOf.Name + '.' + ACloneSocket.CloneOf.Name + ' into output of: ' + Nd.Name + '.' + S.Name + ' = "' + Result + '"' );
      end;}
    //P3DLog.LogInfo( Self, 'injecting ' + ACloneSocket.Node.CloneOf.Name + '.' + ACloneSocket.CloneOf.Name + ' = "' + Result + '"' );
  end else
    begin
      Socket:= ACloneSocket.Node.FindInput( InputName );
      if ( Socket is TP3DShaderNodeSocketClone ) then
        Result:= TP3DShaderNodeSocket( Socket.CloneOf ).GetOutput( Buffer, Socket )
      else
        raise Exception.Create( 'Can not get output of fragment. Socket name "' + InputName + '" was not found!' );
      //P3DLog.LogInfo( Self, 'getting output of: ' + ACloneSocket.Node.CloneOf.Name + '.' + ACloneSocket.CloneOf.Name + ' = "' + Result + '"' );
    end;
  Buffer.Defines.Text:= Defs;
end;


{$ENDIF}

