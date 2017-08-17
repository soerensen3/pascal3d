//p3dviewport.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}

  { TP3DViewport }

  TP3DViewport = record
    Left, Top: Integer;
    Width, Height: Integer
  end;

  { TP3DViewportStack }

  TP3DViewportStack = class
    private
      FScreen: TP3DViewport;
      VPs: array of TP3DViewport;
      function GetCount: Integer;
      function GetVP( Index: Integer ): TP3DViewport;
      procedure SetScreen(AValue: TP3DViewport);
      procedure SetVP( Index: Integer ; AValue: TP3DViewport);
      procedure ApplyVP( VP: TP3DViewport );

    public
      function Push( Left, Top, Width, Height: Integer ): Integer;
      function Push( VP: TP3DViewport ): Integer;
      function Pop: TP3DViewport;
      function Peek: TP3DViewport;
      procedure DebugShowVP( Color: TVec4 );
      function DumpViewportInfo(): String;

      property VP[ Index: Integer ]: TP3DViewport read GetVP write SetVP; default;
      property Count: Integer read GetCount;
      property Screen: TP3DViewport read FScreen write SetScreen;
  end;

function P3DViewport( Left, Top, Width, Height: Integer ): TP3DViewport; inline;
function P3DViewportClipMatrix( X, Y, Width, Height: Float ; VP: TP3DViewport): TMat4;


{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DViewportStack }

function TP3DViewportStack.GetCount: Integer;
begin
  Result:= Length( VPs );
end;

function TP3DViewportStack.GetVP( Index: Integer ): TP3DViewport;
begin
  Result:= VPs[ Index ];
end;

procedure TP3DViewportStack.SetScreen(AValue: TP3DViewport);
begin
  FScreen:= AValue;
  if ( Count = 0 ) then
    ApplyVP( Screen );
end;

procedure TP3DViewportStack.SetVP( Index: Integer ; AValue: TP3DViewport);
begin
  VPs[ Index ]:= AValue;
  if ( Index = Count - 1 ) then
    ApplyVP( AValue );
end;

procedure TP3DViewportStack.ApplyVP(VP: TP3DViewport);
begin
  glViewport( VP.Left, {VPs[ 0 ].Height-}VP.Top{-VP.Height}, VP.Width, VP.Height ); cle( Self );
end;

function TP3DViewportStack.Push(Left, Top, Width, Height: Integer): Integer;
var
  len: Integer;
begin
  len:= Length( VPs );
  SetLength( VPs, len + 1 );
  VPs[ len ].Left:= Left;
  VPs[ len ].Top:= Top;
  VPs[ len ].Width:= Width;
  VPs[ len ].Height:= Height;
  ApplyVP( VPs[ len ]);
  Result:= len;
end;

function TP3DViewportStack.Push(VP: TP3DViewport): Integer;
var
  len: Integer;
begin
  len:= Length( VPs );
  SetLength( VPs, len + 1 );
  VPs[ len ]:= VP;
  ApplyVP( VP );
  Result:= len;
end;

function TP3DViewportStack.Pop: TP3DViewport;
begin
  if ( Count > 0 ) then
    begin
      Result:= VPs[ Count - 1 ];
      SetLength( VPs, Count - 1 );
      ApplyVP( Peek );
    end
  else
    ApplyVP( Screen );
end;

function TP3DViewportStack.Peek: TP3DViewport;
begin
  if ( Count > 0 ) then
    Result:= VPs[ Count - 1 ]
  else
    Result:= Screen;
end;

procedure TP3DViewportStack.DebugShowVP(Color: TVec4);
begin
  glClearColor( Color.R, Color.G, Color.B, Color.A );
  glClear( GL_COLOR_BUFFER_BIT ); cle( Self );
end;

function TP3DViewportStack.DumpViewportInfo: String;
begin
  Result:= Format( 'Active Viewport: [%d,%d,%d,%d]', [ Peek.Left, Peek.Top, Peek.Width, Peek.Height ]);
end;

function P3DViewport(Left, Top, Width, Height: Integer): TP3DViewport;
begin
  Result.Left:= Left;
  Result.Top:= Top;
  Result.Width:= Width;
  Result.Height:= Height;
end;

function P3DViewportClipMatrix( X, Y, Width, Height: Float ; VP: TP3DViewport): TMat4;
var
  t, s: TVec2;
begin
  s:= vec2( VP.Width / Width, VP.Height / Height );
  t:= vec2(( VP.Width + 2 * ( VP.Left - X )) / Width ,
           ( VP.Height + 2 * ( VP.Top - Y )) / Height );
  Result:= mat4( mat2scale( s ));
  Result._30:= t.x;
  Result._31:= t.y;
end;

{$ENDIF}

{$IFDEF INITIALIZATION}
  if ( not Assigned( P3DViewports )) then
    P3DViewports:= TP3DViewportStack.Create;
{$ENDIF}

{$IFDEF FINALIZATION}
  if ( Assigned( P3DViewports )) then
    FreeAndNil( P3DViewports );
{$ENDIF}
