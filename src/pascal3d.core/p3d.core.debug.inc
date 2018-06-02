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
  P3DLog.LogInfoXML( nil, 'Initialized OpenGL <p class="messageheader">' + debuginfo + '</p>');
end;

procedure cle( Sender: TObject; const AddMsg: String = ''  ); inline;
begin
  P3DCheckLastError( Sender, AddMsg );
end;


var
  P3DAssertSender: TObject = nil;

procedure P3DAssert(const M, F: ShortString; L: LongInt; E: Pointer);
begin
  P3DLog.LogException( P3DAssertSender, Format( '%s(%d): %s', [F,L,M]));
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

{ TP3DNodeSocketCloneContainerList }

function TP3DNodeSocketCloneContainerList.SaveToJSON: TJSONData;
var
  a: TJSONArray;
  i: Integer;
  json: TJSONData;
begin
  case StoreMode of
    smText:
      begin
        a:= TJSONArray.Create();
        for i:= 0 to Count - 1 do
          begin
            if ( not AsValues[ i ].IsDefault ) then begin
              json:= GetArrayItemAsProp( i ).SaveToJSON;
              if ( Assigned( json )) then
                a.Add( json );
            end;
          end;
        Result:= a;
      end
    else
      Result:= nil;
  end;
end;

procedure TP3DNodeSocketCloneContainerList.LoadJSON(AValue: TJSONData);
var
  i: Integer;
  Item: TJSONData;
begin
  if ( AValue.JSONType = jtArray ) then begin
    for i:= 0 to TJSONArray( AValue ).Count - 1 do
      LoadItemFromJSON( 0, AValue.Items[ i ]);
  end else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ 'jtArray', JSONTypeName( AValue.JSONType )]);
end;

procedure TP3DNodeSocketCloneContainerList.LoadItemFromJSON(Index: Integer; AData: TJSONData);
var
  PropName: TJSONStringType;
  i: Integer;
begin
  PropName:= AData.FindPath( 'Name' ).AsString;
  for i:= 0 to Count - 1 do
    if ( AsValues[ i ].Name = PropName ) then begin
      AsValues[ i ].AsJSON:= AData;
      break;
    end;
end;


{$ENDIF}
