{$IFDEF INTERFACE}
type

  { TP3DColorRGBA }

  {TP3DColorRGBA = TVec4
    function Fade( Opacity: Float ): TP3DColorRGBA;
  end;

  { TP3DColorRGB }

  TP3DColorRGB = object ( TVec3 )
  end;}
  TP3DColorRGBA = TVec4;
  TP3DColorRGB = TVec3;

const
  Black: TP3DColorRGBA =                        ( FCoord: ( $00 / 255, $00 / 255, $00 / 255, 1.0 ));
  White: TP3DColorRGBA =                        ( FCoord: ( $ff / 255, $ff / 255, $ff / 255, 1.0 ));

  //Source: 2016 https://material.google.com/style/color.html#color-color-palette
  Red50: TP3DColorRGBA =                        ( FCoord: ( $ff / 255, $eb / 255, $ee / 255, 1.0 ));
  Red100: TP3DColorRGBA =                       ( FCoord: ( $ff / 255, $cd / 255, $d2 / 255, 1.0 ));
  Red200: TP3DColorRGBA =                       ( FCoord: ( $ef / 255, $9a / 255, $9a / 255, 1.0 ));
  Red300: TP3DColorRGBA =                       ( FCoord: ( $e5 / 255, $73 / 255, $73 / 255, 1.0 ));
  Red400: TP3DColorRGBA =                       ( FCoord: ( $ef / 255, $53 / 255, $50 / 255, 1.0 ));
  Red500: TP3DColorRGBA =                       ( FCoord: ( $f4 / 255, $43 / 255, $36 / 255, 1.0 ));
  Red600: TP3DColorRGBA =                       ( FCoord: ( $e5 / 255, $39 / 255, $35 / 255, 1.0 ));
  Red700: TP3DColorRGBA =                       ( FCoord: ( $d3 / 255, $2f / 255, $2f / 255, 1.0 ));
  Red800: TP3DColorRGBA =                       ( FCoord: ( $c6 / 255, $28 / 255, $28 / 255, 1.0 ));
  Red900: TP3DColorRGBA =                       ( FCoord: ( $b7 / 255, $1c / 255, $1c / 255, 1.0 ));
  RedA100: TP3DColorRGBA =                      ( FCoord: ( $ff / 255, $8a / 255, $80 / 255, 1.0 ));
  RedA200: TP3DColorRGBA =                      ( FCoord: ( $ff / 255, $52 / 255, $52 / 255, 1.0 ));
  RedA400: TP3DColorRGBA =                      ( FCoord: ( $ff / 255, $17 / 255, $44 / 255, 1.0 ));
  RedA700: TP3DColorRGBA =                      ( FCoord: ( $d5 / 255, $00 / 255, $00 / 255, 1.0 ));

  Pink50: TP3DColorRGBA =                       ( FCoord: ( $fc / 255, $e4 / 255, $ec / 255, 1.0 ));
  Pink100: TP3DColorRGBA =                      ( FCoord: ( $f8 / 255, $bb / 255, $d0 / 255, 1.0 ));
  Pink200: TP3DColorRGBA =                      ( FCoord: ( $f4 / 255, $8f / 255, $b1 / 255, 1.0 ));
  Pink300: TP3DColorRGBA =                      ( FCoord: ( $f0 / 255, $62 / 255, $92 / 255, 1.0 ));
  Pink400: TP3DColorRGBA =                      ( FCoord: ( $ec / 255, $40 / 255, $7a / 255, 1.0 ));
  Pink500: TP3DColorRGBA =                      ( FCoord: ( $e9 / 255, $1e / 255, $63 / 255, 1.0 ));
  Pink600: TP3DColorRGBA =                      ( FCoord: ( $d8 / 255, $1b / 255, $60 / 255, 1.0 ));
  Pink700: TP3DColorRGBA =                      ( FCoord: ( $c2 / 255, $18 / 255, $5b / 255, 1.0 ));
  Pink800: TP3DColorRGBA =                      ( FCoord: ( $ad / 255, $14 / 255, $57 / 255, 1.0 ));
  Pink900: TP3DColorRGBA =                      ( FCoord: ( $88 / 255, $0e / 255, $4f / 255, 1.0 ));
  PinkA100: TP3DColorRGBA =                     ( FCoord: ( $ff / 255, $80 / 255, $ab / 255, 1.0 ));
  PinkA200: TP3DColorRGBA =                     ( FCoord: ( $ff / 255, $40 / 255, $81 / 255, 1.0 ));
  PinkA400: TP3DColorRGBA =                     ( FCoord: ( $f5 / 255, $00 / 255, $57 / 255, 1.0 ));
  PinkA700: TP3DColorRGBA =                     ( FCoord: ( $c5 / 255, $11 / 255, $62 / 255, 1.0 ));

  Purple50: TP3DColorRGBA =                     ( FCoord: ( $f3 / 255, $e5 / 255, $f5 / 255, 1.0 ));
  Purple100: TP3DColorRGBA =                    ( FCoord: ( $e1 / 255, $be / 255, $e7 / 255, 1.0 ));
  Purple200: TP3DColorRGBA =                    ( FCoord: ( $ce / 255, $93 / 255, $d8 / 255, 1.0 ));
  Purple300: TP3DColorRGBA =                    ( FCoord: ( $ba / 255, $68 / 255, $c8 / 255, 1.0 ));
  Purple400: TP3DColorRGBA =                    ( FCoord: ( $ab / 255, $47 / 255, $bc / 255, 1.0 ));
  Purple500: TP3DColorRGBA =                    ( FCoord: ( $9c / 255, $27 / 255, $b0 / 255, 1.0 ));
  Purple600: TP3DColorRGBA =                    ( FCoord: ( $8e / 255, $24 / 255, $aa / 255, 1.0 ));
  Purple700: TP3DColorRGBA =                    ( FCoord: ( $7b / 255, $1f / 255, $a2 / 255, 1.0 ));
  Purple800: TP3DColorRGBA =                    ( FCoord: ( $6a / 255, $1b / 255, $9a / 255, 1.0 ));
  Purple900: TP3DColorRGBA =                    ( FCoord: ( $4a / 255, $14 / 255, $8c / 255, 1.0 ));
  PurpleA100: TP3DColorRGBA =                   ( FCoord: ( $ea / 255, $80 / 255, $fc / 255, 1.0 ));
  PurpleA200: TP3DColorRGBA =                   ( FCoord: ( $e0 / 255, $40 / 255, $fb / 255, 1.0 ));
  PurpleA400: TP3DColorRGBA =                   ( FCoord: ( $d5 / 255, $00 / 255, $f9 / 255, 1.0 ));
  PurpleA700: TP3DColorRGBA =                   ( FCoord: ( $aa / 255, $00 / 255, $ff / 255, 1.0 ));

  DeepPurple50: TP3DColorRGBA =                 ( FCoord: ( $ed / 255, $e7 / 255, $f6 / 255, 1.0 ));
  DeepPurple100: TP3DColorRGBA =                ( FCoord: ( $d1 / 255, $c4 / 255, $e9 / 255, 1.0 ));
  DeepPurple200: TP3DColorRGBA =                ( FCoord: ( $b3 / 255, $9d / 255, $db / 255, 1.0 ));
  DeepPurple300: TP3DColorRGBA =                ( FCoord: ( $95 / 255, $75 / 255, $cd / 255, 1.0 ));
  DeepPurple400: TP3DColorRGBA =                ( FCoord: ( $7e / 255, $57 / 255, $c2 / 255, 1.0 ));
  DeepPurple500: TP3DColorRGBA =                ( FCoord: ( $67 / 255, $3a / 255, $b7 / 255, 1.0 ));
  DeepPurple600: TP3DColorRGBA =                ( FCoord: ( $5e / 255, $35 / 255, $b1 / 255, 1.0 ));
  DeepPurple700: TP3DColorRGBA =                ( FCoord: ( $51 / 255, $2d / 255, $a8 / 255, 1.0 ));
  DeepPurple800: TP3DColorRGBA =                ( FCoord: ( $45 / 255, $27 / 255, $a0 / 255, 1.0 ));
  DeepPurple900: TP3DColorRGBA =                ( FCoord: ( $31 / 255, $1b / 255, $92 / 255, 1.0 ));
  DeepPurpleA100: TP3DColorRGBA =               ( FCoord: ( $b3 / 255, $88 / 255, $ff / 255, 1.0 ));
  DeepPurpleA200: TP3DColorRGBA =               ( FCoord: ( $7c / 255, $4d / 255, $ff / 255, 1.0 ));
  DeepPurpleA400: TP3DColorRGBA =               ( FCoord: ( $65 / 255, $1f / 255, $ff / 255, 1.0 ));
  DeepPurpleA700: TP3DColorRGBA =               ( FCoord: ( $62 / 255, $00 / 255, $ea / 255, 1.0 ));

  Indigo50: TP3DColorRGBA =                     ( FCoord: ( $e8 / 255, $ea / 255, $f6 / 255, 1.0 ));
  Indigo100: TP3DColorRGBA =                    ( FCoord: ( $c5 / 255, $ca / 255, $e9 / 255, 1.0 ));
  Indigo200: TP3DColorRGBA =                    ( FCoord: ( $9f / 255, $a8 / 255, $da / 255, 1.0 ));
  Indigo300: TP3DColorRGBA =                    ( FCoord: ( $79 / 255, $86 / 255, $cb / 255, 1.0 ));
  Indigo400: TP3DColorRGBA =                    ( FCoord: ( $5c / 255, $6b / 255, $c0 / 255, 1.0 ));
  Indigo500: TP3DColorRGBA =                    ( FCoord: ( $3f / 255, $51 / 255, $b5 / 255, 1.0 ));
  Indigo600: TP3DColorRGBA =                    ( FCoord: ( $39 / 255, $49 / 255, $ab / 255, 1.0 ));
  Indigo700: TP3DColorRGBA =                    ( FCoord: ( $30 / 255, $3f / 255, $9f / 255, 1.0 ));
  Indigo800: TP3DColorRGBA =                    ( FCoord: ( $28 / 255, $35 / 255, $93 / 255, 1.0 ));
  Indigo900: TP3DColorRGBA =                    ( FCoord: ( $1a / 255, $23 / 255, $7e / 255, 1.0 ));
  IndigoA100: TP3DColorRGBA =                   ( FCoord: ( $8c / 255, $9e / 255, $ff / 255, 1.0 ));
  IndigoA200: TP3DColorRGBA =                   ( FCoord: ( $53 / 255, $6d / 255, $fe / 255, 1.0 ));
  IndigoA400: TP3DColorRGBA =                   ( FCoord: ( $3d / 255, $5a / 255, $fe / 255, 1.0 ));
  IndigoA700: TP3DColorRGBA =                   ( FCoord: ( $30 / 255, $4f / 255, $fe / 255, 1.0 ));

  Blue50: TP3DColorRGBA =                       ( FCoord: ( $e3 / 255, $f2 / 255, $fd / 255, 1.0 ));
  Blue100: TP3DColorRGBA =                      ( FCoord: ( $bb / 255, $de / 255, $fb / 255, 1.0 ));
  Blue200: TP3DColorRGBA =                      ( FCoord: ( $90 / 255, $ca / 255, $f9 / 255, 1.0 ));
  Blue300: TP3DColorRGBA =                      ( FCoord: ( $64 / 255, $b5 / 255, $f6 / 255, 1.0 ));
  Blue400: TP3DColorRGBA =                      ( FCoord: ( $42 / 255, $a5 / 255, $f5 / 255, 1.0 ));
  Blue500: TP3DColorRGBA =                      ( FCoord: ( $21 / 255, $96 / 255, $f3 / 255, 1.0 ));
  Blue600: TP3DColorRGBA =                      ( FCoord: ( $1e / 255, $88 / 255, $e5 / 255, 1.0 ));
  Blue700: TP3DColorRGBA =                      ( FCoord: ( $19 / 255, $76 / 255, $d2 / 255, 1.0 ));
  Blue800: TP3DColorRGBA =                      ( FCoord: ( $15 / 255, $65 / 255, $c0 / 255, 1.0 ));
  Blue900: TP3DColorRGBA =                      ( FCoord: ( $0d / 255, $47 / 255, $a1 / 255, 1.0 ));
  BlueA100: TP3DColorRGBA =                     ( FCoord: ( $82 / 255, $b1 / 255, $ff / 255, 1.0 ));
  BlueA200: TP3DColorRGBA =                     ( FCoord: ( $44 / 255, $8a / 255, $ff / 255, 1.0 ));
  BlueA400: TP3DColorRGBA =                     ( FCoord: ( $29 / 255, $79 / 255, $ff / 255, 1.0 ));
  BlueA700: TP3DColorRGBA =                     ( FCoord: ( $29 / 255, $62 / 255, $ff / 255, 1.0 ));

  LightBlue50: TP3DColorRGBA =                  ( FCoord: ( $e1 / 255, $f5 / 255, $fe / 255, 1.0 ));
  LightBlue100: TP3DColorRGBA =                 ( FCoord: ( $b3 / 255, $e5 / 255, $fc / 255, 1.0 ));
  LightBlue200: TP3DColorRGBA =                 ( FCoord: ( $81 / 255, $d4 / 255, $fa / 255, 1.0 ));
  LightBlue300: TP3DColorRGBA =                 ( FCoord: ( $4f / 255, $c3 / 255, $f7 / 255, 1.0 ));
  LightBlue400: TP3DColorRGBA =                 ( FCoord: ( $29 / 255, $b6 / 255, $f6 / 255, 1.0 ));
  LightBlue500: TP3DColorRGBA =                 ( FCoord: ( $03 / 255, $a9 / 255, $f4 / 255, 1.0 ));
  LightBlue600: TP3DColorRGBA =                 ( FCoord: ( $03 / 255, $9b / 255, $e5 / 255, 1.0 ));
  LightBlue700: TP3DColorRGBA =                 ( FCoord: ( $02 / 255, $88 / 255, $d1 / 255, 1.0 ));
  LightBlue800: TP3DColorRGBA =                 ( FCoord: ( $02 / 255, $77 / 255, $bd / 255, 1.0 ));
  LightBlue900: TP3DColorRGBA =                 ( FCoord: ( $01 / 255, $57 / 255, $9b / 255, 1.0 ));
  LightBlueA100: TP3DColorRGBA =                ( FCoord: ( $80 / 255, $d8 / 255, $ff / 255, 1.0 ));
  LightBlueA200: TP3DColorRGBA =                ( FCoord: ( $40 / 255, $c4 / 255, $ff / 255, 1.0 ));
  LightBlueA400: TP3DColorRGBA =                ( FCoord: ( $00 / 255, $b0 / 255, $ff / 255, 1.0 ));
  LightBlueA700: TP3DColorRGBA =                ( FCoord: ( $00 / 255, $91 / 255, $ea / 255, 1.0 ));

  Cyan50: TP3DColorRGBA =                       ( FCoord: ( $e0 / 255, $f7 / 255, $fa / 255, 1.0 ));
  Cyan100: TP3DColorRGBA =                      ( FCoord: ( $b2 / 255, $eb / 255, $f2 / 255, 1.0 ));
  Cyan200: TP3DColorRGBA =                      ( FCoord: ( $80 / 255, $de / 255, $ea / 255, 1.0 ));
  Cyan300: TP3DColorRGBA =                      ( FCoord: ( $4d / 255, $d0 / 255, $e1 / 255, 1.0 ));
  Cyan400: TP3DColorRGBA =                      ( FCoord: ( $26 / 255, $c6 / 255, $da / 255, 1.0 ));
  Cyan500: TP3DColorRGBA =                      ( FCoord: ( $00 / 255, $bc / 255, $d4 / 255, 1.0 ));
  Cyan600: TP3DColorRGBA =                      ( FCoord: ( $00 / 255, $ac / 255, $c1 / 255, 1.0 ));
  Cyan700: TP3DColorRGBA =                      ( FCoord: ( $00 / 255, $97 / 255, $a7 / 255, 1.0 ));
  Cyan800: TP3DColorRGBA =                      ( FCoord: ( $00 / 255, $83 / 255, $8f / 255, 1.0 ));
  Cyan900: TP3DColorRGBA =                      ( FCoord: ( $00 / 255, $60 / 255, $64 / 255, 1.0 ));
  CyanA100: TP3DColorRGBA =                     ( FCoord: ( $84 / 255, $ff / 255, $ff / 255, 1.0 ));
  CyanA200: TP3DColorRGBA =                     ( FCoord: ( $18 / 255, $ff / 255, $ff / 255, 1.0 ));
  CyanA400: TP3DColorRGBA =                     ( FCoord: ( $00 / 255, $e5 / 255, $ff / 255, 1.0 ));
  CyanA700: TP3DColorRGBA =                     ( FCoord: ( $00 / 255, $b8 / 255, $d4 / 255, 1.0 ));

  Teal50: TP3DColorRGBA =                       ( FCoord: ( $e0 / 255, $f2 / 255, $f1 / 255, 1.0 ));
  Teal100: TP3DColorRGBA =                      ( FCoord: ( $b2 / 255, $df / 255, $db / 255, 1.0 ));
  Teal200: TP3DColorRGBA =                      ( FCoord: ( $80 / 255, $cb / 255, $c4 / 255, 1.0 ));
  Teal300: TP3DColorRGBA =                      ( FCoord: ( $4d / 255, $b6 / 255, $ac / 255, 1.0 ));
  Teal400: TP3DColorRGBA =                      ( FCoord: ( $26 / 255, $a6 / 255, $9a / 255, 1.0 ));
  Teal500: TP3DColorRGBA =                      ( FCoord: ( $00 / 255, $96 / 255, $88 / 255, 1.0 ));
  Teal600: TP3DColorRGBA =                      ( FCoord: ( $00 / 255, $89 / 255, $7b / 255, 1.0 ));
  Teal700: TP3DColorRGBA =                      ( FCoord: ( $00 / 255, $79 / 255, $6b / 255, 1.0 ));
  Teal800: TP3DColorRGBA =                      ( FCoord: ( $00 / 255, $69 / 255, $5c / 255, 1.0 ));
  Teal900: TP3DColorRGBA =                      ( FCoord: ( $00 / 255, $4d / 255, $40 / 255, 1.0 ));
  TealA100: TP3DColorRGBA =                     ( FCoord: ( $a7 / 255, $ff / 255, $eb / 255, 1.0 ));
  TealA200: TP3DColorRGBA =                     ( FCoord: ( $64 / 255, $ff / 255, $da / 255, 1.0 ));
  TealA400: TP3DColorRGBA =                     ( FCoord: ( $1d / 255, $e9 / 255, $b6 / 255, 1.0 ));
  TealA700: TP3DColorRGBA =                     ( FCoord: ( $00 / 255, $bf / 255, $a5 / 255, 1.0 ));

  Green50: TP3DColorRGBA =                      ( FCoord: ( $e8 / 255, $f5 / 255, $e9 / 255, 1.0 ));
  Green100: TP3DColorRGBA =                     ( FCoord: ( $c8 / 255, $e6 / 255, $c9 / 255, 1.0 ));
  Green200: TP3DColorRGBA =                     ( FCoord: ( $a5 / 255, $d6 / 255, $a7 / 255, 1.0 ));
  Green300: TP3DColorRGBA =                     ( FCoord: ( $81 / 255, $c7 / 255, $84 / 255, 1.0 ));
  Green400: TP3DColorRGBA =                     ( FCoord: ( $66 / 255, $bb / 255, $6a / 255, 1.0 ));
  Green500: TP3DColorRGBA =                     ( FCoord: ( $4c / 255, $af / 255, $50 / 255, 1.0 ));
  Green600: TP3DColorRGBA =                     ( FCoord: ( $43 / 255, $a0 / 255, $47 / 255, 1.0 ));
  Green700: TP3DColorRGBA =                     ( FCoord: ( $38 / 255, $8e / 255, $3c / 255, 1.0 ));
  Green800: TP3DColorRGBA =                     ( FCoord: ( $2e / 255, $7d / 255, $32 / 255, 1.0 ));
  Green900: TP3DColorRGBA =                     ( FCoord: ( $1b / 255, $5e / 255, $20 / 255, 1.0 ));
  GreenA100: TP3DColorRGBA =                    ( FCoord: ( $b9 / 255, $f6 / 255, $ca / 255, 1.0 ));
  GreenA200: TP3DColorRGBA =                    ( FCoord: ( $69 / 255, $f0 / 255, $ae / 255, 1.0 ));
  GreenA400: TP3DColorRGBA =                    ( FCoord: ( $00 / 255, $e6 / 255, $76 / 255, 1.0 ));
  GreenA700: TP3DColorRGBA =                    ( FCoord: ( $00 / 255, $c8 / 255, $53 / 255, 1.0 ));

  LightGreen50: TP3DColorRGBA =                 ( FCoord: ( $f1 / 255, $f8 / 255, $e9 / 255, 1.0 ));
  LightGreen100: TP3DColorRGBA =                ( FCoord: ( $dc / 255, $ed / 255, $c8 / 255, 1.0 ));
  LightGreen200: TP3DColorRGBA =                ( FCoord: ( $c5 / 255, $e1 / 255, $a5 / 255, 1.0 ));
  LightGreen300: TP3DColorRGBA =                ( FCoord: ( $ae / 255, $d5 / 255, $81 / 255, 1.0 ));
  LightGreen400: TP3DColorRGBA =                ( FCoord: ( $9c / 255, $cc / 255, $65 / 255, 1.0 ));
  LightGreen500: TP3DColorRGBA =                ( FCoord: ( $8b / 255, $c3 / 255, $4a / 255, 1.0 ));
  LightGreen600: TP3DColorRGBA =                ( FCoord: ( $7c / 255, $b3 / 255, $42 / 255, 1.0 ));
  LightGreen700: TP3DColorRGBA =                ( FCoord: ( $68 / 255, $9f / 255, $38 / 255, 1.0 ));
  LightGreen800: TP3DColorRGBA =                ( FCoord: ( $55 / 255, $8b / 255, $2f / 255, 1.0 ));
  LightGreen900: TP3DColorRGBA =                ( FCoord: ( $33 / 255, $69 / 255, $1e / 255, 1.0 ));
  LightGreenA100: TP3DColorRGBA =               ( FCoord: ( $cc / 255, $ff / 255, $90 / 255, 1.0 ));
  LightGreenA200: TP3DColorRGBA =               ( FCoord: ( $b2 / 255, $ff / 255, $59 / 255, 1.0 ));
  LightGreenA400: TP3DColorRGBA =               ( FCoord: ( $76 / 255, $ff / 255, $03 / 255, 1.0 ));
  LightGreenA700: TP3DColorRGBA =               ( FCoord: ( $64 / 255, $dd / 255, $17 / 255, 1.0 ));

  Lime50: TP3DColorRGBA =                       ( FCoord: ( $f9 / 255, $fb / 255, $e7 / 255, 1.0 ));
  Lime100: TP3DColorRGBA =                      ( FCoord: ( $f0 / 255, $f4 / 255, $c3 / 255, 1.0 ));
  Lime200: TP3DColorRGBA =                      ( FCoord: ( $e6 / 255, $ee / 255, $9c / 255, 1.0 ));
  Lime300: TP3DColorRGBA =                      ( FCoord: ( $dc / 255, $e7 / 255, $75 / 255, 1.0 ));
  Lime400: TP3DColorRGBA =                      ( FCoord: ( $d4 / 255, $e1 / 255, $57 / 255, 1.0 ));
  Lime500: TP3DColorRGBA =                      ( FCoord: ( $cd / 255, $dc / 255, $39 / 255, 1.0 ));
  Lime600: TP3DColorRGBA =                      ( FCoord: ( $c0 / 255, $ca / 255, $33 / 255, 1.0 ));
  Lime700: TP3DColorRGBA =                      ( FCoord: ( $af / 255, $b4 / 255, $2b / 255, 1.0 ));
  Lime800: TP3DColorRGBA =                      ( FCoord: ( $9e / 255, $9d / 255, $24 / 255, 1.0 ));
  Lime900: TP3DColorRGBA =                      ( FCoord: ( $82 / 255, $77 / 255, $17 / 255, 1.0 ));
  LimeA100: TP3DColorRGBA =                     ( FCoord: ( $f4 / 255, $ff / 255, $81 / 255, 1.0 ));
  LimeA200: TP3DColorRGBA =                     ( FCoord: ( $ee / 255, $ff / 255, $41 / 255, 1.0 ));
  LimeA400: TP3DColorRGBA =                     ( FCoord: ( $c6 / 255, $ff / 255, $00 / 255, 1.0 ));
  LimeA700: TP3DColorRGBA =                     ( FCoord: ( $ae / 255, $ea / 255, $00 / 255, 1.0 ));

  Yellow50: TP3DColorRGBA =                     ( FCoord: ( $ff / 255, $fd / 255, $e7 / 255, 1.0 ));
  Yellow100: TP3DColorRGBA =                    ( FCoord: ( $ff / 255, $f9 / 255, $c4 / 255, 1.0 ));
  Yellow200: TP3DColorRGBA =                    ( FCoord: ( $ff / 255, $f5 / 255, $9d / 255, 1.0 ));
  Yellow300: TP3DColorRGBA =                    ( FCoord: ( $ff / 255, $f1 / 255, $76 / 255, 1.0 ));
  Yellow400: TP3DColorRGBA =                    ( FCoord: ( $ff / 255, $ee / 255, $58 / 255, 1.0 ));
  Yellow500: TP3DColorRGBA =                    ( FCoord: ( $ff / 255, $eb / 255, $3b / 255, 1.0 ));
  Yellow600: TP3DColorRGBA =                    ( FCoord: ( $fd / 255, $d8 / 255, $35 / 255, 1.0 ));
  Yellow700: TP3DColorRGBA =                    ( FCoord: ( $fb / 255, $c0 / 255, $2d / 255, 1.0 ));
  Yellow800: TP3DColorRGBA =                    ( FCoord: ( $f9 / 255, $a8 / 255, $25 / 255, 1.0 ));
  Yellow900: TP3DColorRGBA =                    ( FCoord: ( $f5 / 255, $7f / 255, $17 / 255, 1.0 ));
  YellowA100: TP3DColorRGBA =                   ( FCoord: ( $ff / 255, $ff / 255, $8d / 255, 1.0 ));
  YellowA200: TP3DColorRGBA =                   ( FCoord: ( $ff / 255, $ff / 255, $00 / 255, 1.0 ));
  YellowA400: TP3DColorRGBA =                   ( FCoord: ( $ff / 255, $ea / 255, $00 / 255, 1.0 ));
  YellowA700: TP3DColorRGBA =                   ( FCoord: ( $ff / 255, $d6 / 255, $00 / 255, 1.0 ));

  Amber50: TP3DColorRGBA =                      ( FCoord: ( $ff / 255, $f8 / 255, $e1 / 255, 1.0 ));
  Amber100: TP3DColorRGBA =                     ( FCoord: ( $ff / 255, $ec / 255, $b3 / 255, 1.0 ));
  Amber200: TP3DColorRGBA =                     ( FCoord: ( $ff / 255, $e0 / 255, $82 / 255, 1.0 ));
  Amber300: TP3DColorRGBA =                     ( FCoord: ( $ff / 255, $d5 / 255, $4f / 255, 1.0 ));
  Amber400: TP3DColorRGBA =                     ( FCoord: ( $ff / 255, $ca / 255, $28 / 255, 1.0 ));
  Amber500: TP3DColorRGBA =                     ( FCoord: ( $ff / 255, $c1 / 255, $07 / 255, 1.0 ));
  Amber600: TP3DColorRGBA =                     ( FCoord: ( $ff / 255, $b3 / 255, $00 / 255, 1.0 ));
  Amber700: TP3DColorRGBA =                     ( FCoord: ( $ff / 255, $a0 / 255, $00 / 255, 1.0 ));
  Amber800: TP3DColorRGBA =                     ( FCoord: ( $ff / 255, $8f / 255, $00 / 255, 1.0 ));
  Amber900: TP3DColorRGBA =                     ( FCoord: ( $ff / 255, $6f / 255, $00 / 255, 1.0 ));
  AmberA100: TP3DColorRGBA =                    ( FCoord: ( $ff / 255, $e5 / 255, $7f / 255, 1.0 ));
  AmberA200: TP3DColorRGBA =                    ( FCoord: ( $ff / 255, $d7 / 255, $40 / 255, 1.0 ));
  AmberA400: TP3DColorRGBA =                    ( FCoord: ( $ff / 255, $c4 / 255, $00 / 255, 1.0 ));
  AmberA700: TP3DColorRGBA =                    ( FCoord: ( $ff / 255, $ab / 255, $00 / 255, 1.0 ));

  Orange50: TP3DColorRGBA =                     ( FCoord: ( $ff / 255, $f3 / 255, $e0 / 255, 1.0 ));
  Orange100: TP3DColorRGBA =                    ( FCoord: ( $ff / 255, $e0 / 255, $b2 / 255, 1.0 ));
  Orange200: TP3DColorRGBA =                    ( FCoord: ( $ff / 255, $cc / 255, $80 / 255, 1.0 ));
  Orange300: TP3DColorRGBA =                    ( FCoord: ( $ff / 255, $b7 / 255, $4d / 255, 1.0 ));
  Orange400: TP3DColorRGBA =                    ( FCoord: ( $ff / 255, $a7 / 255, $26 / 255, 1.0 ));
  Orange500: TP3DColorRGBA =                    ( FCoord: ( $ff / 255, $98 / 255, $00 / 255, 1.0 ));
  Orange600: TP3DColorRGBA =                    ( FCoord: ( $fb / 255, $8c / 255, $00 / 255, 1.0 ));
  Orange700: TP3DColorRGBA =                    ( FCoord: ( $f5 / 255, $7c / 255, $00 / 255, 1.0 ));
  Orange800: TP3DColorRGBA =                    ( FCoord: ( $ef / 255, $6c / 255, $00 / 255, 1.0 ));
  Orange900: TP3DColorRGBA =                    ( FCoord: ( $e6 / 255, $51 / 255, $00 / 255, 1.0 ));
  OrangeA100: TP3DColorRGBA =                   ( FCoord: ( $ff / 255, $d1 / 255, $80 / 255, 1.0 ));
  OrangeA200: TP3DColorRGBA =                   ( FCoord: ( $ff / 255, $ab / 255, $40 / 255, 1.0 ));
  OrangeA400: TP3DColorRGBA =                   ( FCoord: ( $ff / 255, $91 / 255, $00 / 255, 1.0 ));
  OrangeA700: TP3DColorRGBA =                   ( FCoord: ( $ff / 255, $6d / 255, $00 / 255, 1.0 ));

  DeepOrange50: TP3DColorRGBA =                 ( FCoord: ( $fb / 255, $e9 / 255, $e7 / 255, 1.0 ));
  DeepOrange100: TP3DColorRGBA =                ( FCoord: ( $ff / 255, $cc / 255, $bc / 255, 1.0 ));
  DeepOrange200: TP3DColorRGBA =                ( FCoord: ( $ff / 255, $ab / 255, $91 / 255, 1.0 ));
  DeepOrange300: TP3DColorRGBA =                ( FCoord: ( $ff / 255, $8a / 255, $65 / 255, 1.0 ));
  DeepOrange400: TP3DColorRGBA =                ( FCoord: ( $ff / 255, $70 / 255, $43 / 255, 1.0 ));
  DeepOrange500: TP3DColorRGBA =                ( FCoord: ( $ff / 255, $57 / 255, $22 / 255, 1.0 ));
  DeepOrange600: TP3DColorRGBA =                ( FCoord: ( $f4 / 255, $51 / 255, $1e / 255, 1.0 ));
  DeepOrange700: TP3DColorRGBA =                ( FCoord: ( $e6 / 255, $4a / 255, $19 / 255, 1.0 ));
  DeepOrange800: TP3DColorRGBA =                ( FCoord: ( $d8 / 255, $43 / 255, $15 / 255, 1.0 ));
  DeepOrange900: TP3DColorRGBA =                ( FCoord: ( $bf / 255, $36 / 255, $0c / 255, 1.0 ));
  DeepOrangeA100: TP3DColorRGBA =               ( FCoord: ( $ff / 255, $9e / 255, $80 / 255, 1.0 ));
  DeepOrangeA200: TP3DColorRGBA =               ( FCoord: ( $ff / 255, $6e / 255, $40 / 255, 1.0 ));
  DeepOrangeA400: TP3DColorRGBA =               ( FCoord: ( $ff / 255, $3d / 255, $00 / 255, 1.0 ));
  DeepOrangeA700: TP3DColorRGBA =               ( FCoord: ( $dd / 255, $2c / 255, $00 / 255, 1.0 ));

  Brown50: TP3DColorRGBA =                      ( FCoord: ( $ef / 255, $eb / 255, $e9 / 255, 1.0 ));
  Brown100: TP3DColorRGBA =                     ( FCoord: ( $d7 / 255, $cc / 255, $c8 / 255, 1.0 ));
  Brown200: TP3DColorRGBA =                     ( FCoord: ( $bc / 255, $aa / 255, $a4 / 255, 1.0 ));
  Brown300: TP3DColorRGBA =                     ( FCoord: ( $a1 / 255, $88 / 255, $7f / 255, 1.0 ));
  Brown400: TP3DColorRGBA =                     ( FCoord: ( $8d / 255, $6e / 255, $63 / 255, 1.0 ));
  Brown500: TP3DColorRGBA =                     ( FCoord: ( $79 / 255, $55 / 255, $48 / 255, 1.0 ));
  Brown600: TP3DColorRGBA =                     ( FCoord: ( $6d / 255, $4c / 255, $41 / 255, 1.0 ));
  Brown700: TP3DColorRGBA =                     ( FCoord: ( $5d / 255, $40 / 255, $37 / 255, 1.0 ));
  Brown800: TP3DColorRGBA =                     ( FCoord: ( $4e / 255, $34 / 255, $2e / 255, 1.0 ));
  Brown900: TP3DColorRGBA =                     ( FCoord: ( $3e / 255, $27 / 255, $23 / 255, 1.0 ));

  Grey50: TP3DColorRGBA =                       ( FCoord: ( $fa / 255, $fa / 255, $fa / 255, 1.0 ));
  Grey100: TP3DColorRGBA =                      ( FCoord: ( $f5 / 255, $f5 / 255, $f5 / 255, 1.0 ));
  Grey200: TP3DColorRGBA =                      ( FCoord: ( $ee / 255, $ee / 255, $ee / 255, 1.0 ));
  Grey300: TP3DColorRGBA =                      ( FCoord: ( $e0 / 255, $e0 / 255, $e0 / 255, 1.0 ));
  Grey400: TP3DColorRGBA =                      ( FCoord: ( $bd / 255, $bd / 255, $bd / 255, 1.0 ));
  Grey500: TP3DColorRGBA =                      ( FCoord: ( $9e / 255, $9e / 255, $9e / 255, 1.0 ));
  Grey600: TP3DColorRGBA =                      ( FCoord: ( $75 / 255, $75 / 255, $75 / 255, 1.0 ));
  Grey700: TP3DColorRGBA =                      ( FCoord: ( $61 / 255, $61 / 255, $61 / 255, 1.0 ));
  Grey800: TP3DColorRGBA =                      ( FCoord: ( $42 / 255, $42 / 255, $42 / 255, 1.0 ));
  Grey900: TP3DColorRGBA =                      ( FCoord: ( $21 / 255, $21 / 255, $21 / 255, 1.0 ));

  BlueGrey50: TP3DColorRGBA =                   ( FCoord: ( $ec / 255, $ef / 255, $f1 / 255, 1.0 ));
  BlueGrey100: TP3DColorRGBA =                  ( FCoord: ( $cf / 255, $d8 / 255, $dc / 255, 1.0 ));
  BlueGrey200: TP3DColorRGBA =                  ( FCoord: ( $b0 / 255, $be / 255, $c5 / 255, 1.0 ));
  BlueGrey300: TP3DColorRGBA =                  ( FCoord: ( $90 / 255, $a4 / 255, $ae / 255, 1.0 ));
  BlueGrey400: TP3DColorRGBA =                  ( FCoord: ( $78 / 255, $90 / 255, $9c / 255, 1.0 ));
  BlueGrey500: TP3DColorRGBA =                  ( FCoord: ( $60 / 255, $7d / 255, $8b / 255, 1.0 ));
  BlueGrey600: TP3DColorRGBA =                  ( FCoord: ( $54 / 255, $6e / 255, $7a / 255, 1.0 ));
  BlueGrey700: TP3DColorRGBA =                  ( FCoord: ( $45 / 255, $5a / 255, $64 / 255, 1.0 ));
  BlueGrey800: TP3DColorRGBA =                  ( FCoord: ( $37 / 255, $47 / 255, $4f / 255, 1.0 ));
  BlueGrey900: TP3DColorRGBA =                  ( FCoord: ( $26 / 255, $32 / 255, $38 / 255, 1.0 ));

  {operator * ( a, b: TP3DColorRGBA ) c : TP3DColorRGBA;
  operator * ( a: TP3DColorRGBA; b: TVec4 ) c: TP3DColorRGBA;
  operator * ( a: TVec4; b: TP3DColorRGBA ) c: TP3DColorRGBA;

  operator := ( a: TVec4 ) b: TP3DColorRGBA;

  operator * ( a, b: TP3DColorRGB ) c : TP3DColorRGB;
  operator * ( a: TP3DColorRGB; b: TVec3 ) c: TP3DColorRGB;
  operator * ( a: TVec3; b: TP3DColorRGB ) c: TP3DColorRGB;

  operator := ( a: TVec3 ) b: TP3DColorRGB;}

type
  TP3DPropAccessColorRGBA = specialize gP3DPropAccessVec < TP3DColorRGBA, Float, TP3DPropAccessFloat >;
  TP3DPropAccessColorRGB = specialize gP3DPropAccessVec < TP3DColorRGB, Float, TP3DPropAccessFloat >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DColorRGBA }
{
function TP3DColorRGBA.Fade( Opacity: Float ): TP3DColorRGBA;
begin
  Result:= vec4( XYZ, A * Opacity );
end;

operator*(a, b: TP3DColorRGBA)c: TP3DColorRGBA;
begin
  c:= TP3DColorRGBA( TVec4( a ) * TVec4( b ));
end;

operator*(a: TP3DColorRGBA; b: TVec4)c: TP3DColorRGBA;
begin
  c:= TP3DColorRGBA( TVec4( a ) * b );
end;

operator*(a: TVec4; b: TP3DColorRGBA)c: TP3DColorRGBA;
begin
  c:= TP3DColorRGBA( a * TVec4( b ));
end;

operator:=(a: TVec4)b: TP3DColorRGBA;
begin
  b.R:= a.R;
  b.G:= a.G;
  b.B:= a.B;
  b.A:= a.A;
end;


operator*(a, b: TP3DColorRGB)c: TP3DColorRGB;
begin
  c:= TP3DColorRGB( TVec3( a ) * TVec3( b ));
end;

operator*(a: TP3DColorRGB; b: TVec3)c: TP3DColorRGB;
begin
  c:= TP3DColorRGB( TVec3( a ) * b );
end;

operator*(a: TVec3; b: TP3DColorRGB)c: TP3DColorRGB;
begin
  c:= TP3DColorRGB( a * TVec3( b ));
end;

operator:=(a: TVec3)b: TP3DColorRGB;
begin
  b.R:= a.R;
  b.G:= a.G;
  b.B:= a.B;
end;

operator=(A, B: TVec2): Boolean;
begin
  Result:= A.Compare( B );
end;
}

{$ENDIF}

