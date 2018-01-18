unit RButtons;

interface
  uses
    Core,
    RVCL,
    RTypes,
    Types,
    RUtils,
    Timers;

  type
    TButtonStyle = ( bsGlowHorizontal, bsGlowVertical, bsFlat );
    TRevButton = class( TRevFocusControl )
      private
        FColor,
        FColorHover,
        FColorDown,
        FFontColor: Cardinal;
        FCaption: String;
        F_curCol: TColor;
        FState: Integer;
        FFontSize: Integer;
        FFontName: String;
        FButtonStyle: TButtonStyle;

        procedure Set_curCol( const Value: Cardinal );

        property _curCol: Cardinal read F_curCol.Col write Set_curCol;

      public
        procedure Draw; override;
        constructor Create( AOwner: TBaseObject; AEngine: TEngine; _Manager: TVCLManager; const Parent: TRevGraphicControl = nil );
        procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer ); override;
        procedure MouseUp( mb1, mb2, mb3: Boolean; X, Y: Integer ); override;
        procedure MouseMove( IsOver: Boolean; X, Y: Integer ); override;
        destructor Destroy; override;

      published
        property FontSize: Integer read FFontSize write FFontSize;
        property Color: Cardinal read FColor write FColor;
        property ColorHover: Cardinal read FColorHover write FColorHover;
        property ColorDown: Cardinal read FColorDown write FColorDown;
        property FontColor: Cardinal read FFontColor write FFontColor;
        property FontName: String read FFontName write FFontName;
        property Caption: String read FCaption write FCaption;
        property ButtonStyle: TButtonStyle read FButtonStyle write FButtonStyle;
    end;

implementation

{ TButton }

constructor TRevButton.Create( AOwner: TBaseObject; AEngine: TEngine; _Manager: TVCLManager; const Parent: TRevGraphicControl = nil );

begin
  inherited;
  Color:= $7F003F7F;
  ColorHover:= $7FFFFFFF;
  ColorDown:= $7F000F3F;
  FontColor:= $3FFFFFFF;
  F_curCol.Col:= Color;
  FontSize:= 16;
end;

destructor TRevButton.Destroy;
begin
{  if ( _curTmrR <> nil ) then
    _curTmrR.Free;
  if ( _curTmrG <> nil ) then
    _curTmrG.Free;
  if ( _curTmrB <> nil ) then
    _curTmrB.Free;
  if ( _curTmrA <> nil ) then
    _curTmrA.Free;
}
  inherited;
end;

procedure TRevButton.Draw;
var
  Col: Cardinal;
  Col2: Cardinal;
begin
{  if ( WasDown ) then
    Col:= FColorDown
  else if ( WasOver ) then
    Col:= FColorHover
  else
    Col:= FColor;}
  Col:= _curCol;

  if ( ButtonStyle = bsFlat ) then
    begin
      Canvas.RenderRect( 0, 0, Width - 1, Height - 1, _curCol, _curCol, _curCol, _curCol );
      if ( WasDown ) then
        Canvas.Render3DFrame( 0, 0, Width - 1, Height - 1, _curCol, _curCol, _curCol, _curCol, -128 )
      else
        Canvas.Render3DFrame( 0, 0, Width - 1, Height - 1, _curCol, _curCol, _curCol, _curCol, 64 );
    end
  else
    begin
      if ( ButtonStyle = bsGlowHorizontal ) then
        begin
          Canvas.RenderRect( 0, 0, Width, Height div 2,
            Brightness( _curCol, 64 ), Brightness( _curCol, 64 ), Brightness( _curCol, 0 ), Brightness( _curCol, 0 ));
          Canvas.RenderRect( 0, Height div 2, Width, Height,
            Brightness( _curCol, -64 ), Brightness( _curCol, -64 ), Brightness( _curCol, -32 ), Brightness( _curCol, -32 ));
        end
      else
        begin
          Canvas.RenderRect( 0, 0, Width div 2, Height,
            Brightness( _curCol, 64 ), Brightness( _curCol, 0 ), Brightness( _curCol, 64 ), Brightness( _curCol, 0 ));
          Canvas.RenderRect( Width div 2, 0, Width, Height,
            Brightness( _curCol, -64 ), Brightness( _curCol, -32 ), Brightness( _curCol, -64 ), Brightness( _curCol, -32 ));
        end;
    end;

  if ( HasFocus ) then
    begin
      Col2:= RUtils.Color( $FF, $FF, $FF, $66 ).Col;
      Canvas.RenderLine( 4, 4, Width - 4, 4, Col2, Col2 );
      Canvas.RenderLine( 4, Height - 5, Width - 4, Height - 5, Col2, Col2 );
      Canvas.RenderLine( 4, 4, 4, Height - 4, Col2, Col2 );
      Canvas.RenderLine( Width - 5, 4, Width - 5, Height - 4, Col2, Col2 );
    end;
  Canvas.Font.Color:= FontColor;
  Canvas.Font.Size:= FontSize;
  Canvas.Font.Name:= FontName;
  Canvas.RenderText( Caption, Point(( Round( Width ) - Canvas.TextWidth( FCaption )) div 2, 0 ));
end;

procedure TRevButton.MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer );
begin
  inherited;
//  if ( FState <> 3 ) then
//    _curCol:= FColorDown;
//  FState:= 3;
end;

procedure TRevButton.MouseMove( IsOver: Boolean; X, Y: Integer );
begin
  inherited;
  if ( not WasDown ) then
    begin
      if ( IsOver ) then
        begin
          if ( FState <> 1 ) then
            F_curCol.Col:= FColorHover;
          FState:= 1;
        end
      else
        begin
          if ( FState <> 2 ) then
            F_curCol.Col:= FColor;
          FState:= 2;
        end;
    end;
end;

procedure TRevButton.MouseUp( mb1, mb2, mb3: Boolean; X, Y: Integer );
begin
  inherited;
  if ( FState <> 4 ) then
    _curCol:= ( Ord( WasOver ) * FColorHover + Ord( not WasOver ) * FColor );
  FState:= 4;
end;

procedure TRevButton.Set_curCol( const Value: Cardinal );
var
  C: TColor;
begin
{  if ( _curTmrR <> nil ) then
    _curTmrR.Free;
  _curTmrR:= nil;
  if ( _curTmrG <> nil ) then
    _curTmrG.Free;
  _curTmrG:= nil;
  if ( _curTmrB <> nil ) then
    _curTmrB.Free;
  _curTmrB:= nil;
  if ( _curTmrA <> nil ) then
    _curTmrA.Free;
  _curTmrA:= nil;}

//  C.Col:= Value;
//  Rev_Fade( Engine, @F_curCol.r, C.r, 1 );
//  Rev_Fade( Engine, @F_curCol.g, C.g, 1 );
//  Rev_Fade( Engine, @F_curCol.b, C.b, 1 );
//  Rev_Fade( Engine, @F_curCol.a, C.a, 1 );

{  _curTmrR:= Rev_Fade( Engine, @F_curCol.r, C.r, 1 );
  _curTmrG:= Rev_Fade( Engine, @F_curCol.g, C.g, 1 );
  _curTmrB:= Rev_Fade( Engine, @F_curCol.b, C.b, 1 );
  _curTmrA:= Rev_Fade( Engine, @F_curCol.a, C.a, 1 );}
//  TTimedFade.Create( nil, Engine, PByte( @F_curCol.r ), TColor( Value ).r, 100 );
//  TTimedFade.Create( nil, Engine, PByte( @F_curCol.g ), TColor( Value ).g, 10000 );
//  TTimedFade.Create( nil, Engine, PByte( @F_curCol.b ), TColor( Value ).b, 100 );
//  TTimedFade.Create( nil, Engine, PByte( @F_curCol.a ), TColor( Value ).a, 100 );
  F_curCol.Col:= Value;
end;

initialization
  Rev_RegisterClass( TRevButton );

end.
 