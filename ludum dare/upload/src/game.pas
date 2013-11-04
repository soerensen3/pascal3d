unit Game;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GameLib, LCLIntf, Graphics, Math;

type

  { TGame }

  TGame = class ( TPersistent )
    private
      FGameOver: Boolean;
      FSpeed: Real;
      procedure SetGameOver(AValue: Boolean);
    published
      procedure Init;
      procedure Process;
      procedure Deinit;
      procedure NewGame;
      procedure ShowInfo;

      property Speed: Real read FSpeed write FSpeed;
      property GameOver: Boolean read FGameOver write SetGameOver;
  end;

implementation

uses GameVars, cmd;

{ TGame }

procedure TGame.SetGameOver( AValue: Boolean );
begin
  FGameOver:= AValue;
  if ( FGameOver ) then
    begin
      Speed:= 0;
      OnWrite( 'The Rabbit is out of the screen or you bumped into an enemy! Game over! Your score is ' + IntToStr( Round( GamePainter.Left )));
    end;
end;

procedure TGame.Init;
begin
  T:= GetTickCount;
  GamePainter:= TGamePainter.Create;

  PlayerSprite:= TPicture.Create;
  PlayerSprite.LoadFromFile( 'img/hase.png' );

  Enemy1Sprite:= TPicture.Create;
  Enemy1Sprite.LoadFromFile( 'img/gegner_1.png' );

  Enemy2Sprite:= TPicture.Create;
  Enemy2Sprite.LoadFromFile( 'img/gegner_2.png' );

  Enemy3Sprite:= TPicture.Create;
  Enemy3Sprite.LoadFromFile( 'img/gegner_3.png' );

  ShowInfo;
//  NewGame;
end;

procedure TGame.Process;
  procedure CheckCollisions;
  var
    i: Integer;
    obj: TGameObject;
  begin
    for i:= GamePainter.GameObjects.Count - 1 downto 0 do
      if ( GamePainter.GameObjects[ i ] <> Player ) then
        if ( CheckCollision( Player, GamePainter.GameObjects[ i ], CollisionTolerance )) then
          GameOver:= True
        else if ( GamePainter.GameObjects[ i ].X + GamePainter.GameObjects[ i ].Graphic.Width < GamePainter.Left ) then
          begin
            obj:= GamePainter.GameObjects[ i ];
//            OnWrite( Format( 'Enemy No. %d deleted! Position %d,%d', [ i, Round( obj.X ), Round( obj.Y )]));
            GamePainter.GameObjects.Delete( i );
            FreeAndNil( obj );
          end;
  end;

var
  Enemy: TGameObject;
  maxenemy: Integer;
  i: Integer;
begin
  dT:= GetTickCount - T;
  T:= GetTickCount;

  if ( Speed > 0 ) then
    begin
      Speed:= 20 + GamePainter.Left / 50;

      //Move Screen
      GamePainter.Left:= GamePainter.Left + Speed * dT / 1000;

      //Move Enemies
      for i:= 0 to GamePainter.GameObjects.Count - 1 do
        GamePainter.GameObjects[ i ].Process;

      //Spawn Enemies
      if ( Round( GamePainter.Left ) mod Round( 20000 / Speed ) = 0 ) then
        begin
          maxenemy:= Min( 3, Round( GamePainter.Left ) div 1000 );
          case ( Random( maxenemy )) of
            0:
              begin
                Enemy:= TEnemy.Create;
                Enemy.Graphic:= Enemy1Sprite;
              end;
            1:
              begin
                Enemy:= TEnemyAdvanced.Create;
                ( Enemy as TEnemyAdvanced ).Speed:= 10 + Random( 20 );
                Enemy.Graphic:= Enemy2Sprite;
              end;
            2:
              begin
                Enemy:= TEnemyHard.Create;
                ( Enemy as TEnemyHard ).Speed:= 10 + Random( 20 );
                Enemy.Graphic:= Enemy3Sprite;
              end;
          end;
          GamePainter.GameObjects.Add( Enemy );
          with ( Enemy ) do
            begin
              repeat
                X:= Round( GamePainter.Left ) + GamePainter.Screen.Width + 200 + Random( 100 ); //+200 to not spawn inside of the player
                Y:= Random( GamePainter.Screen.Height - Enemy1Sprite.Height ) - 50;
              until ( CheckCollisionList( Enemy, GamePainter.GameObjects, CollisionTolerance ) = -1 );
            end;

        end;

      //Player out of the screen?
      if (( Player.X + Player.Graphic.Width < GamePainter.Left ) or
        ( Player.X > GamePainter.Left + GamePainter.Screen.Width )) then
        GameOver:= True;

      //CheckCollisions and get rid of enemies which are out of the screen
      CheckCollisions;
    end;
  GamePainter.Draw;
end;

procedure TGame.Deinit;
begin
  PlayerSprite.Free;
  Enemy1Sprite.Free;
  Enemy2Sprite.Free;

  GamePainter.Free;
end;

procedure TGame.NewGame;
var
  Enemy: TEnemyHard;
begin
  DeleteGameObjects( GamePainter.GameObjects );
  Player:= nil;

  Player:= TGameObject.Create;
  Player.Graphic:= PlayerSprite;
  Player.X:= -Player.Graphic.Width;
  GamePainter.GameObjects.Add( Player );

  Speed:= 20;
  GamePainter.Left:= -300;
  GameOver:= False;
end;

procedure TGame.ShowInfo;
begin
  OnWrite(
    'Welcome to "Make a binky"' + #13#10 +
    'You control a rabbit by typing commands.' + #13#10 +
    'Try to avoid to get outside of' + #13#10 +
    'the screen or to bump into enemies.' + #13#10 +
    '-----------------------' + #13#10 +
    'List of possible commands' + #13#10 +
    '  start - will start a new game' + #13#10 +
    '  pause - will pause the current' + #13#10 + '    game or continue if paused' + #13#10 +
    '  set X Y - will set the rabbit to ' + #13#10 + '    the position - e.g. set 500 50' + #13#10 + #13#10 +
    'That''s all folks!' + #13#10 +
    '-----------------------' );
end;

end.

