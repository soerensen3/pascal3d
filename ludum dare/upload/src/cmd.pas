unit cmd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GameVars, strutils;

function ParseCmd(S: String): String;
var
  OnWrite: procedure ( OutPut: String ) of object;

implementation

function ParseCmd(S: String): String;
var
  com: String;
  p1, p2, p3: String;
begin
  com:= ExtractWord( 1, S, [ ' ' ]);

  Result:= '';
  case com of
    'set':
      begin
        p1:= ExtractWord( 2, S, [ ' ' ]);
        p2:= ExtractWord( 3, S, [ ' ' ]);
        try
          Player.X:= StrToInt( p1 );
          Player.Y:= StrToInt( p2 );

        except
          on E: Exception do
            Result:= 'Syntax Error.';
        end;
      end;
    'pause':
      if ( not TheGame.GameOver ) then
        begin
          if ( TheGame.Speed <> 0 ) then
            TheGame.Speed:= 0
          else
            TheGame.Speed:= 1;
        end;
    'start':
      begin
        p1:= ExtractWord( 2, S, [ ' ' ]);
        //ignore p1 for now
        TheGame.NewGame;
      end
    else
      Result:= 'Error. Command "' + com + '" not found.';
  end;
end;

end.

