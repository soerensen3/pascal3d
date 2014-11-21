unit p3dviewport;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  dglOpenGL;

type

  { TP3DViewport }

  TP3DViewport = record
    Left, Top: Integer;
    Width, Height: Integer
  end;

  { TP3DViewportStack }

  TP3DViewportStack = class
    private
      VPs: array of TP3DViewport;
      function GetCount: Integer;
      function GetVP( Index: Integer ): TP3DViewport;
      procedure SetVP( Index: Integer ; AValue: TP3DViewport);
      procedure ApplyVP( VP: TP3DViewport );

    public
      procedure Push( Left, Top, Width, Height: Integer );
      procedure Push( VP: TP3DViewport );
      function Pop: TP3DViewport;
      function Peek: TP3DViewport;

      property VP[ Index: Integer ]: TP3DViewport read GetVP write SetVP;
      property Count: Integer read GetCount;
  end;

var
  P3DViewports: TP3DViewportStack;

implementation

{ TP3DViewportStack }

function TP3DViewportStack.GetCount: Integer;
begin
  Result:= Length( VPs );
end;

function TP3DViewportStack.GetVP( Index: Integer ): TP3DViewport;
begin
  Result:= VPs[ Index ];
end;

procedure TP3DViewportStack.SetVP( Index: Integer ; AValue: TP3DViewport);
begin
  VPs[ Index ]:= AValue;
end;

procedure TP3DViewportStack.ApplyVP(VP: TP3DViewport);
begin
  glViewport( VP.Left, VP.Top, VP.Width, VP.Height );
end;

procedure TP3DViewportStack.Push(Left, Top, Width, Height: Integer);
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
end;

procedure TP3DViewportStack.Push(VP: TP3DViewport);
var
  len: Integer;
begin
  len:= Length( VPs );
  SetLength( VPs, len + 1 );
  VPs[ len ]:= VP;
  ApplyVP( VP );
end;

function TP3DViewportStack.Pop: TP3DViewport;
begin
  if ( Count > 0 ) then
    begin
      Result:= VPs[ Count - 1 ];
      if ( Count > 1 ) then
        begin
          SetLength( VPs, Count - 1 );
          ApplyVP( Peek );
        end;
    end;
end;

function TP3DViewportStack.Peek: TP3DViewport;
begin
  if ( Count > 0 ) then
    Result:= VPs[ Count - 1 ];
end;

initialization
  P3DViewports:= TP3DViewportStack.Create;

finalization
  P3DViewports.Free;


end.

