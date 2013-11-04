unit GameLib;

{$mode objfpc}{$H+}
{.$DEFINE DEBUG_COLLISIONS}

interface

  uses
    Classes, SysUtils, fgl, Graphics, Math;

  type

    { TGameObject }

    TGameObject = class ( TPersistent )
      private
        FGraphic: TPicture;
        FX: Real;
        FY: Real;

      public
        constructor Create;
        destructor Destroy; override;

        procedure Process; virtual;

        property Graphic: TPicture read FGraphic write FGraphic;

      published
        property X: Real read FX write FX;
        property Y: Real read FY write FY;
    end;

    TEnemy = class ( TGameObject )

    end;

    { TEnemyAdvanced }

    TEnemyAdvanced = class ( TEnemy )
      private
        FSpeed: Integer;

      public
        constructor Create;

        procedure Process; override;

        property Speed: Integer read FSpeed write FSpeed;
    end;

    { TEnemyHard }

    TEnemyHard = class ( TEnemy )

    private
      FDirection: Real;
      FSpeed: Integer;
    public
      constructor Create;

      procedure Process; override;

      property Speed: Integer read FSpeed write FSpeed;
      property Direction: Real read FDirection write FDirection;
    end;

    TGameObjectList = specialize TFPGList < TGameObject >;

    { TRuler }

    TRuler = class ( TPersistent )
      private
        FLeft: Integer;

      published
        property Left: Integer read FLeft write FLeft;
        procedure Draw( X, Y, W, H: Integer; Canvas: TCanvas );
    end;

    { TGamePainter }

    TGamePainter = class ( TPersistent )
      private
        FGameObjects: TGameObjectList;
        FLeft: Real;
        FRuler: TRuler;
        FScreen: TBitmap;

      public
        constructor Create;
        destructor Destroy; override;

      published
        property GameObjects: TGameObjectList read FGameObjects write FGameObjects;
        property Screen: TBitmap read FScreen write FScreen;
        property Ruler: TRuler read FRuler write FRuler;
        procedure Draw;
        property Left: Real read FLeft write FLeft;
    end;

    procedure DeleteGameObjects( List: TGameObjectList );

    function CheckCollision(Obj1, Obj2: TGameObject; const Tolerance: Integer = 0 ): Boolean;
    function CheckCollisionList( Obj: TGameObject; List: TGameObjectList; const Tolerance: Integer = 0 ): Integer;


implementation

uses GameVars;


procedure DeleteGameObjects(List: TGameObjectList);
var
  i: Integer;
begin
  if ( Assigned( List )) then
    for i:= 0 to List.Count - 1 do
      List[ i ].Free;
  List.Clear;
end;

function CheckCollision(Obj1, Obj2: TGameObject; const Tolerance: Integer
  ): Boolean;
begin
  Result:= not (
    ( Obj1.X + Obj1.Graphic.Width < Obj2.X + Tolerance ) or
    ( Obj1.X > Obj2.X + Obj2.Graphic.Width - Tolerance ) or
    ( Obj1.Y + Obj1.Graphic.Height < Obj2.Y + Tolerance ) or
    ( Obj1.Y > Obj2.Y + Obj1.Graphic.Height - Tolerance ));
end;

function CheckCollisionList(Obj: TGameObject; List: TGameObjectList;
  const Tolerance: Integer): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to List.Count - 1 do
    if ( Obj <> List[ i ]) then
      if ( CheckCollision( Obj, List[ i ], Tolerance )) then
        begin
          Result:= i;
          break;
        end;
end;

{ TEnemyHard }

constructor TEnemyHard.Create;
begin
  inherited;
  FSpeed:= 20;
  FDirection:= PI;
end;

procedure TEnemyHard.Process;
begin
  inherited Process;
  FDirection:= PI + sin( GetTickCount / 5000 ) * PI / 4;
  FX+= cos( Direction ) * Speed * dT / 1000;
  FY+= sin( Direction ) * Speed * dT / 1000;
end;

{ TEnemyAdvanced }

constructor TEnemyAdvanced.Create;
begin
  inherited;
  Speed:= 20;
end;

procedure TEnemyAdvanced.Process;
begin
  inherited Process;
  FX-= Speed * dT / 1000;
end;

{ TRuler }

procedure TRuler.Draw(X, Y, W, H: Integer; Canvas: TCanvas);
var
  i: Integer;
begin
  with ( Canvas ) do
    begin
      Brush.Style:= bsSolid;
      Brush.Color:= $C4DBB2;
      Pen.Color:= $59664E;
      Pen.Style:= psSolid;
      Pen.Width:= 2;

      Rectangle( X, Y, W, H );
      Brush.Style:= bsClear;
      for i:= 0 to W div 50 do
        if (( i * 50 + Self.Left - Self.Left mod 50 ) mod 100 = 0 ) then
          begin
            Line( i * 50 + X - Self.Left mod 50, Y, i * 50 + X - Self.Left mod 50, Y + 30 );
            TextOut( i * 50 + X - Self.Left mod 50, 0, IntToStr( i * 50 + Self.Left - Self.Left mod 50 ));
          end
        else
          Line( i * 50 + X - Self.Left mod 50, Y, i * 50 + X - Self.Left mod 50, Y + 20 );
    end;
end;

{ TGameObject }

constructor TGameObject.Create;
begin
  inherited;
//  Graphic:= TPicture.Create;
end;

destructor TGameObject.Destroy;
begin
//  Graphic.Free;
  inherited Destroy;
end;

procedure TGameObject.Process;
begin

end;

{ TGamePainter }

constructor TGamePainter.Create;
begin
  inherited;
  Screen:= TBitmap.Create;
  Ruler:= TRuler.Create;
  GameObjects:= TGameObjectList.Create;
end;

destructor TGamePainter.Destroy;
begin
  Ruler.Free;
  Screen.Free;
  DeleteGameObjects( GameObjects );
  GameObjects.Free;
  inherited Destroy;
end;


procedure TGamePainter.Draw;
var
  i: Integer;
begin
  with ( Screen.Canvas ) do
    begin
      Brush.Style:= bsSolid;
      Brush.Color:= clWhite;
      Pen.Style:= psClear;
      Rectangle( 0, 0, Width, Height );

      Ruler.Left:= Round( Left );
      Ruler.Draw( 0, 0, Width, 30, Screen.Canvas );

      for i:= 0 to GameObjects.Count - 1 do
        begin
          {$IFDEF DEBUG_COLLISIONS}
          Rectangle( GameObjects[ i ].X - Round( Left ), GameObjects[ i ].Y + 50,
            GameObjects[ i ].X - Round( Left ) + GameObjects[ i ].Graphic.Width, GameObjects[ i ].Y + 50 + GameObjects[ i ].Graphic.Height );
          Rectangle( GameObjects[ i ].X - Round( Left ) + CollisionTolerance, GameObjects[ i ].Y + 50 + CollisionTolerance,
            GameObjects[ i ].X - Round( Left ) + GameObjects[ i ].Graphic.Width - CollisionTolerance, GameObjects[ i ].Y + 50 + GameObjects[ i ].Graphic.Height - CollisionTolerance );
          {$ENDIF}
          Draw( Round( GameObjects[ i ].X - Left ), Round( GameObjects[ i ].Y + 50 ), GameObjects[ i ].Graphic.Graphic );
        end;
    end;
end;

end.

