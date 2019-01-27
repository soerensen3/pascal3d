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

{ TP3DShaderNodeFragmentList }

TP3DShaderNodeFragmentList = class( specialize gP3DCustomObjectList < TP3DShaderNodeFragment >)
  function GetStringOutput( ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer ): String; virtual;
end;

{ TP3DShaderNodeFragmentSelector }

TP3DShaderNodeFragmentSelector = class( TP3DShaderNodeFragment )
  private
    FDefines: TStringList;
    FInputName: String;
    FOutputOverride: String;
    FWildCard: Boolean;
    procedure SetInputName(AValue: String);

  public
    constructor Create;
    destructor Destroy; override;

    function InternalGetStringOutput( ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer ): String; override;

    property InputName: String read FInputName write SetInputName;
    property OutputOverride: String read FOutputOverride write FOutputOverride;
    property Defines: TStringList read FDefines;
end;

TP3DShaderNodeFragmentInline = class( TP3DShaderNodeFragment )
  private
    FText: String;

  public
    function InternalGetStringOutput( ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer ): String; override;

    property Text: String read FText write FText;
end;

{ TP3DShaderNodeFragmentWithFragments }

TP3DShaderNodeFragmentWithFragments = class( TP3DShaderNodeFragment )
  private
    FFragments: TP3DShaderNodeFragmentList;

    function InternalGetStringOutput(ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer): String; override;

  public
    constructor Create;
    destructor Destroy; override;

    property Fragments: TP3DShaderNodeFragmentList read FFragments write FFragments;
end;

{ TP3DShaderNodeFragmentIfDef }

TP3DShaderNodeFragmentIfDef = class( TP3DShaderNodeFragmentWithFragments )
  private
    FCheckDefs: TStringList;
    FInvert: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function GetStringOutput( ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer ): String; override;

    property CheckDefs: TStringList read FCheckDefs;
    property Invert: Boolean read FInvert;
end;

{ TP3DShaderNodeFragmentIfConn }

TP3DShaderNodeFragmentIfConn = class( TP3DShaderNodeFragmentWithFragments )
  private
    FCheckDefs: TStringList;
    FInputName: String;
    FInvert: Boolean;

  public
    function GetStringOutput( ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer ): String; override;
    property InputName: String read FInputName;
    property Invert: Boolean read FInvert;
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


{ TP3DShaderNodeFragmentWithFragments }

function TP3DShaderNodeFragmentWithFragments.InternalGetStringOutput(ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer): String;
begin
  Result:= Fragments.GetStringOutput( ACloneSocket, Buffer );
end;

constructor TP3DShaderNodeFragmentWithFragments.Create;
begin
  inherited;
  Fragments:= TP3DShaderNodeFragmentList.Create;
end;

destructor TP3DShaderNodeFragmentWithFragments.Destroy;
begin
  Fragments.Clear;
  Fragments.Free;
  inherited Destroy;
end;


{ TP3DShaderNodeFragmentIfDef }

constructor TP3DShaderNodeFragmentIfDef.Create;
begin
  inherited;
  FCheckDefs:= TStringList.Create;
end;

destructor TP3DShaderNodeFragmentIfDef.Destroy;
begin
  FCheckDefs.Free;
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
          //log_info( 'ifdef failed: ' + S + ' was not defined!' );
          Result:= False;
          Break;
        end;
  end;

  function FindAllDefinesInverted: Boolean;
  var
    S: String;
  begin
    Result:= True;
    for S in CheckDefs do
      if ( Buffer.Defines.IndexOf( S ) > 0 ) then
        begin
          log_info( 'ifndef failed: ' + S + ' was defined!' );
          Result:= False;
          Break;
        end;
  end;

var
  condition: Boolean;
begin
  //WriteLn( 'IFDEF ', Caption, ' defines=', Buffer.Defines.CommaText, ' defined?=', Buffer.Defines.IndexOf( Caption ) > -1 );
  if ( Invert ) then
    condition:= FindAllDefinesInverted
  else
    condition:= FindAllDefines;
  if ( condition ) then
    Result:= InternalGetStringOutput( ACloneSocket, Buffer )
  else
    Result:= '';

  {else
    WriteLn( 'ifdef failed: ', Caption, ' was not defined!' );}
end;

{ TP3DShaderNodeFragmentIfConn }

function TP3DShaderNodeFragmentIfConn.GetStringOutput(ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer): String;
var
  inp: TP3DNodeSocketClone;
  condition: Boolean;
begin
  Result:= '';
  inp:= ACloneSocket.Node.FindInput( InputName );
  if ( not Assigned( inp )) then
    raise Exception.Create( 'Input ' + InputName + ' not found in ' + ACloneSocket.Node.CloneOf.Name );
  condition:= Assigned( inp.Connected );
  if ( Invert ) then
    condition:= not condition;
  if ( condition ) then
    Result:= inherited GetStringOutput(ACloneSocket, Buffer);
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

{ TP3DShaderNodeFragmentSelector }

procedure TP3DShaderNodeFragmentSelector.SetInputName(AValue: String);
begin
  if FInputName=AValue then Exit;
  FInputName:=AValue;
  FWildCard:= FInputName.Contains( '*' );
end;

constructor TP3DShaderNodeFragmentSelector.Create;
begin
  inherited Create;
  FDefines:= TStringList.Create;
end;

destructor TP3DShaderNodeFragmentSelector.Destroy;
begin
  FDefines.Free;
  inherited Destroy;
end;

function TP3DShaderNodeFragmentSelector.InternalGetStringOutput(ACloneSocket: TP3DNodeSocketClone; Buffer: TP3DShaderBuffer): String;
var
  i: Integer;
  Defs: String;
  Socket: TP3DNodeSocketClone;
  Socket2: TP3DNodeSocket;
  Inputs, Types: TP3DNodeSocketClonePointerList;
  Libs: TP3DNodeSocketPointerList;
  T: TP3DShaderNodeType;

begin
  Defs:= Buffer.Defines.Text;
  Buffer.Defines.AddStrings( Defines );
  Result:= '';
  if ( FWildCard ) then
    begin
      try
        Inputs:= TP3DNodeSocketClonePointerList.Create( 'Inputs' );
        Inputs.Root:= ACloneSocket.Root;
        Inputs.Context:= ACloneSocket;
        Types:= TP3DNodeSocketClonePointerList.Create( 'Types' );
        Types.Root:= ACloneSocket.Root;
        Types.Context:= ACloneSocket;
        Libs:= TP3DNodeSocketPointerList.Create( 'Libs' );
        Libs.Root:= ACloneSocket.Root;
        Libs.Context:= ACloneSocket;
        for Socket in ACloneSocket.Node.Inputs.EnumerateWildcard( InputName ) do
          TP3DShaderNodeSocketClone( Socket ).GetAllInputs( Buffer, OutputOverride, Inputs, Types, Libs );
        for Socket in Types.Reversed do
          begin
            T:= P3DShaderNodeLib.FindType( TP3DShaderNodeSocket( Socket.CloneOf ).SocketType );
            if ( Assigned( T )) then
              Result+= T.GetOutputAsType( Buffer, Socket, OutputOverride );
          end;
        WriteLn( 'Inputs: ', Inputs.Count, ' Types: ', Types.Count );
        for Socket in Inputs.Reversed do
          Result+= TP3DShaderNodeSocket( Socket.CloneOf ).GetOutput( Buffer, Socket );
        for Socket2 in Libs{.Reversed} do
          Result+= TP3DShaderNodeSocket( Socket2 ).Fragments.GetStringOutput( Socket, Buffer );

      finally
        Types.Free;
        Inputs.Free;
        Libs.Free;
      end;
    end
  else if ( InputName = 'inject' ) then begin
    if ( Assigned( ACloneSocket.Connected )) then
      Result:= TP3DShaderNodeSocketClone( ACloneSocket.Connected ).GetOutput( Buffer )
    else
      Result:= ACloneSocket.ValueProp.AsString;
  end else
    begin
      Socket:= ACloneSocket.Node.FindInput( InputName );
      if ( Socket is TP3DShaderNodeSocketClone ) then
        Result:= TP3DShaderNodeSocket( Socket.CloneOf ).GetOutput( Buffer, Socket )
      else
        raise Exception.Create( 'Can not get output of fragment. Socket name "' + InputName + '" was not found in ' + ACloneSocket.Node.CloneOf.Name + '!' );
    end;
  Buffer.Defines.Text:= Defs;
end;


{$ENDIF}

