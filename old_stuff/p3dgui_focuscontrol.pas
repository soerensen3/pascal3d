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
      procedure SetInputState(AValue: TP3DGCInputFlags); override;

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
  if ( AValue = Focused ) then
    exit;
  if ( AValue ) then
    FocusedControl:= Self
  else
    FocusedControl:= nil;
end;

procedure TP3DFocusControl.SetInputState(AValue: TP3DGCInputFlags);
begin
  if ( not Focused ) then
    if ( gcisMouseBtn1Down in AValue ) then
      Focused:= True;

  inherited SetInputState(AValue);
end;

end.

