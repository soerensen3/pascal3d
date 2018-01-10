unit p3dpropaccess;


interface
  uses Classes, SysUtils, fpjson, Math, strutils, fgl, typinfo;

  type
    TP3DJSONLoader = class;

    IP3DLoadJSON = interface;

    {$DEFINE INTERFACE}
    {$INCLUDE p3dgenerics.inc}
    {$INCLUDE p3dpropaccess.inc}
    {$INCLUDE p3dpropaccess_basetypes.inc}
    {$INCLUDE p3dstreamable.inc}

    {$UNDEF INTERFACE}


    //..........................................................................

    //p3djson.inc
    //..........................................................................


  function Enclosed( var S: String; sStart: Char; sEnd: Char; const TrimStr: Boolean = True ): Boolean;
  function PathGetNextFragment( var APath: String; out ArrayIdxStr: String ): String;


    //..........................................................................

    //p3dlibrary.inc
    //..........................................................................

    type

      { TP3DLibrary }

      TP3DLibrary = class ( TP3DStreamable, IP3DLoadJSON )
        private
          FJSONLoader: TP3DJSONLoader;

          function GetJSONLoader: TP3DJSONLoader;
          function GetFileAge: LongInt;

        public
          constructor Create( const AOwner: IP3DStreamableContainer = nil; const ARoot: IP3DLoadJSON = nil ); override;
          destructor Destroy; override;

          property JSONLoader: TP3DJSONLoader read FJSONLoader write FJSONLoader;
      end;

    //..........................................................................
implementation

function Enclosed( var S: String; sStart: Char; sEnd: Char; const TrimStr: Boolean = True ): Boolean;
begin
  if ( TrimStr ) then
    S:= Trim( S );
  Result:= ( Length( S ) > 1 ) and ( S[ 1 ] = sStart ) and ( S[ Length( S )] = sEnd );
  if ( Result ) then
    S:= Copy( S, 2, Length( S ) - 2 );
end;

function Copy2SymbSkipString( var S: String; Ch: Char ): String;
var
  i: Integer;
begin
  i:= 1;
  while i <= Length( S ) do
    begin
      if ( S[ i ] = '"' ) then
        begin
          Inc( i );
          while ( i <= Length( S )) do
            begin
              if ( S[ i ] = '"' ) then
                if (( i = 1 ) or ( S[ i - 1 ] <> '\' )) then
                  break;
              Inc( i );
            end;
        end
      else if ( S[ i ] = Ch ) then
        break;
      Inc( i );
    end;
  Result:= Copy( S, 1, i - 1 );
  S:= Copy( S, i + 1, Length( S ));
end;

function PathGetNextFragment( var APath: String; out ArrayIdxStr: String ): String;
var
  n: Integer;
begin
  Result:= Copy2SymbSkipString( APath, '.' );
  n:= Pos( '[', Result );
  if ( n > 0 ) then
    begin
      ArrayIdxStr:= Result;
      Result:= Copy2SymbDel( ArrayIdxStr, '[' );
      ArrayIdxStr:= Trim( Copy2Symb( ArrayIdxStr, ']' ));
    end
  else
    ArrayIdxStr:= '';
end;


{ TP3DLibrary }

function TP3DLibrary.GetJSONLoader: TP3DJSONLoader;
begin
  Result:= FJSONLoader;
end;

function TP3DLibrary.GetFileAge: LongInt;
begin
  Result:= JSONLoader.FileAge;
end;

constructor TP3DLibrary.Create(const AOwner: IP3DStreamableContainer;
  const ARoot: IP3DLoadJSON);
begin
  inherited Create(AOwner, ARoot);
  JSONLoader:= TP3DJSONLoader.Create;
end;

destructor TP3DLibrary.Destroy;
begin
  inherited Destroy;
  FreeAndNil( FJSONLoader );
end;

{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dgenerics.inc}
{$INCLUDE p3dpropaccess.inc}
{$INCLUDE p3dpropaccess_basetypes.inc}
{$INCLUDE p3dstreamable.inc}
{$UNDEF IMPLEMENTATION}

//..........................................................................

//p3djson.inc
//..........................................................................

procedure TP3DJSONLoader.SetJSON(AValue: TJSONData);
begin
  if FJSON=AValue then Exit;

  if ( Assigned( FJSON )) then
    FJSON.Free;
  FJSON:= AValue;
  FFileAge:= GetTickCount64;
end;

procedure TP3DJSONLoader.BeginRead;
begin
  FIsLoading:= True;
end;

procedure TP3DJSONLoader.EndRead;
var
  D: TP3DDeferredLoad;
begin
  FIsLoading:= False;
  for D in DeferredLoading do
    D.DoLoad;
  DeferredLoading.Clear;
end;

constructor TP3DJSONLoader.Create;
begin
  FDeferredLoading:= TP3DDeferredLoadList.Create( True );
end;

destructor TP3DJSONLoader.Destroy;
begin
  FDeferredLoading.Free;
  JSON:= nil;
  inherited Destroy;
end;

// Try to find path in properties
// Exit if valid!
// Find JSONPath to next fragment
// raise Exception if not found
// Load last found property
// return
function TP3DJSONLoader.GetPropByPath( APath: String; const DoLoad: Boolean = False ): TP3DPropertyAccess;
var
  RestString, IdxString, PropString: String;
  LastFound: TP3DPropertyAccess;
  data: TJSONData;
begin
  if ( Assigned( Root )) then
    begin
      Result:= Root.Properties.GetPropByPath( APath, RestString, LastFound );
      if ( Assigned( Result ) or ( not DoLoad )) then
        exit;
      data:= JSON.FindPath( APath );

      while (( not Assigned( Result )) and ( RestString > '' )) do
        begin
          PropString:= PathGetNextFragment( RestString, IdxString );
          if ( not Assigned( LastFound )) then
            raise Exception.CreateFmt( 'The specified path "%s.%s" seems to be invalid.', [ APath, PropString ]);
          if ( not Assigned( data )) then
            raise Exception.CreateFmt( 'Could not find path "%s.%s" in json file.', [ APath, PropString ]);
          LastFound.AsJSON:= data;
          if ( IdxString > '' ) then
            PropString:= PropString + '[' + IdxString + ']';
          data:= data.FindPath( PropString );
          Result:= LastFound.GetChildPropByPath( PropString, RestString, LastFound );
        end;
    end
  else
    Result:= nil;
end;

procedure TP3DJSONLoader.ReadJSON;
begin
  BeginRead;
  if ( Assigned( Root )) then
    Root.AsJSON:= JSON;
  EndRead;
end;

//..........................................................................


initialization
  DebugList:= TP3DDebugList.Create;

finalization
  DebugList.Free;

end.

