//GUI Constants

{$IFDEF INTERFACE}
var
  PrimaryText: TP3DColorRGBA =                  ( FCoord: ( $00 / 255, $00 / 255, $00 / 255, 0.87 ));
  SecondaryText: TP3DColorRGBA =                ( FCoord: ( $00 / 255, $00 / 255, $00 / 255, 0.54 ));
  DisabledText: TP3DColorRGBA =                 ( FCoord: ( $00 / 255, $00 / 255, $00 / 255, 0.38 ));
  Primary50, Primary100, Primary200,
    Primary300, Primary400, Primary500,
    Primary600, Primary700, Primary800,
    Primary900: TP3DColorRGBA;
  Secondary50, Secondary100, Secondary200,
    Secondary300, Secondary400, Secondary500,
    Secondary600, Secondary700, Secondary800,
    Secondary900: TP3DColorRGBA;
  Accent50, Accent100, Accent200,
    Accent300, Accent400, Accent500,
    Accent600, Accent700, Accent800,
    Accent900: TP3DColorRGBA;

{$ENDIF}

procedure P3DGUIInitColors;
{$IFDEF IMPLEMENTATION}
begin
  Primary50:=                           Grey50;
  Primary100:=                          Grey100;
  Primary200:=                          Grey200;
  Primary300:=                          Grey300;
  Primary400:=                          Grey400;
  Primary500:=                          Grey500;
  Primary600:=                          Grey600;
  Primary700:=                          Grey700;
  Primary800:=                          Grey800;
  Primary900:=                          Grey900;

  Secondary50:=                         amber50;
  Secondary100:=                        amber100;
  Secondary200:=                        amber200;
  Secondary300:=                        amber300;
  Secondary400:=                        amber400;
  Secondary500:=                        amber500;
  Secondary600:=                        amber600;
  Secondary700:=                        amber700;
  Secondary800:=                        amber800;
  Secondary900:=                        amber900;


  Accent50:=                            BlueGrey50;
  Accent100:=                           BlueGrey100;
  Accent200:=                           BlueGrey200;
  Accent300:=                           BlueGrey300;
  Accent400:=                           BlueGrey400;
  Accent500:=                           BlueGrey500;
  Accent600:=                           BlueGrey600;
  Accent700:=                           BlueGrey700;
  Accent800:=                           BlueGrey800;
  Accent900:=                           BlueGrey900;
end;



{$ENDIF}
