unit p3dgui_focuscontrol;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  p3dgui;

type

  { TP3DFocusControl }

  TP3DFocusControl = class ( TP3DGraphicControl )
    protected
      function GetFocused: Boolean;
      procedure SetFocused( AValue: Boolean ); virtual;

    published
      property Focused: Boolean read GetFocused write SetFocused;
  end;

var
  FocusedControl: TP3DFocusControl;

implementation

{ TP3DFocusControl }

function TP3DFocusControl.GetFocused: Boolean;
begin
  Result:= Self = FocusedControl;
end;

procedure TP3DFocusControl.SetFocused(AValue: Boolean);
begin
  if ( AValue ) then
    FocusedControl:= Self
  else
    FocusedControl:= nil;
end;

end.

