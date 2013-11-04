unit GameVars;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GameLib, Game, Graphics;

const
  CollisionTolerance = 15;

var
  Player: TGameObject;
  GamePainter: TGamePainter;
  TheGame: TGame;

  PlayerSprite: TPicture;
  Enemy1Sprite: TPicture;
  Enemy2Sprite: TPicture;
  Enemy3Sprite: TPicture;

  dT: Integer;
  T: Integer;

implementation

end.

