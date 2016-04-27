unit p3dgui_forms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, p3dMath, p3dgui, p3dgui_stdctrls, p3devents;

type

  { TP3DForm }

  TP3DForm = class( TP3DGroupBox )
    public
      class function HasShadow: Boolean; override;

    private
      procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer ); override;
      procedure MouseMove(X, Y: Integer); override;

  end;

implementation

{ TP3DForm }

class function TP3DForm.HasShadow: Boolean;
begin
  Result:= True;
end;

procedure TP3DForm.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  if ( mb1 and ( gcisMouseOver in InputState )) then
    BringToFront;
end;

procedure TP3DForm.MouseMove(X, Y: Integer);
begin
  inherited MouseMove( X, Y );
  if ( gcisMouseBtn1Down in InputState ) then
    begin
      Left:= Left + P3DInput.Mouse.DX;
      Top:= Top + P3DInput.Mouse.DY;
    end;
end;

end.

