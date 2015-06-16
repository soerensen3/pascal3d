unit RImage;

interface
  uses
    RVCL,
    RBase,
    RTypes;
  type
    TImage = class( TRevGraphicControl )
      private
        FTexture: TTexture;

      public
        procedure Draw; override;

        property Texture: TTexture read FTexture write FTexture;
    end;

implementation

uses Core;

{ TImage }

procedure TImage.Draw;
begin
  inherited;
  if (( Texture <> nil ) and ( Engine.Painter <> nil )) then
    Engine.Painter.RenderRect2D( Vector( FScreenLeft, FScreenTop, 0.0 ),
      Vector( FScreenLeft + FScreenWidth, FScreenTop + FScreenHeight, 0.0 ),
      $FFFFFFFF, 0.0, 0.0, 1.0, 1.0, @Texture.TexInfo );
end;

initialization
  Rev_RegisterClass( TImage );

end.
 