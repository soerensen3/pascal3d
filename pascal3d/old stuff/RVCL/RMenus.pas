unit RMenus;

interface
  uses
    Core,
    Classes,
    Math,
    SysUtils,
    RVCL;

  type
    TMenuItemBuffer = class;
    TRevPopupMenu = class;

    TMenuItem = class( TPersistent )
      private
        FCaption: String;
        FOnClick: TNotifyEvent;

      public
        SubMenus: TMenuItemBuffer;
        constructor Create; 
        destructor Destroy; override;

      published
        property Caption: String read FCaption write FCaption;
        property OnClick: TNotifyEvent read FOnClick write FOnClick;
    end;

    TMenuItemBuffer = class( TList )
      protected
        FOnChange: TNotifyEvent;

        function Get( Index: Integer ): TMenuItem;
        procedure Put( Index: Integer; Item: TMenuItem );

      public
        Parent: TMenuItem;

        constructor Create( AParent: TMenuItem );
        destructor Destroy; override;
        procedure Clear; override;
        procedure Delete( Index: Integer ); overload;
        procedure Delete( Item: TMenuItem ); overload;
        procedure MoveTo( Index: Integer; Controls: TMenuItemBuffer );
        function Add( Item: TMenuItem ): Integer; overload;
        function Add( Caption: String ): Integer; overload;

        property Items[ Index: Integer ]: TMenuItem read Get write Put; default;
        property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

    TRevPopupMenu = class( TRevFocusControl )
      private
        FItemHeight: Integer;
        FSelIdx: Integer;
        FSelColor: Cardinal;
        FFontColor: Cardinal;
        FSelFontColor: Cardinal;
        FColor: Cardinal;
        FMenus: TMenuItemBuffer;
        FFreeMenu: Boolean;
        FAutoSize: Boolean;

        procedure OnChange( Sender: TObject );
        procedure SetItemHeight( const Value: Integer );
        procedure SetMenus( const Value: TMenuItemBuffer );

      protected
        procedure SetFocus( const Value: Boolean ); override;

      public
        Expand: TRevPopupMenu;

        constructor Create( AOwner: TBaseObject; AEngine: TEngine; _Manager: TVCLManager; const Parent: TRevGraphicControl = nil );
        destructor Destroy; override;

        procedure Draw; override;

        procedure MouseMove( IsOver: Boolean; X: Integer; Y: Integer ); override;
        procedure MouseClick( mb1: Boolean; mb2: Boolean; mb3: Boolean;
          X: Integer; Y: Integer ); override;
        procedure PopUp( x, y: Integer );

        property Menus: TMenuItemBuffer read FMenus write SetMenus;

      published
        property ItemHeight: Integer read FItemHeight write SetItemHeight;
        property Color: Cardinal read FColor write FColor;
        property SelColor: Cardinal read FSelColor write FSelColor;
        property FontColor: Cardinal read FFontColor write FFontColor;
        property SelFontColor: Cardinal read FSelFontColor write FSelFontColor;
        property AutoSize: Boolean read FAutoSize write FAutoSize;
    end;

implementation

{ TPopupMenu }

constructor TRevPopupMenu.Create( AOwner: TBaseObject; AEngine: TEngine;
  _Manager: TVCLManager; const Parent: TRevGraphicControl = nil );
begin
  inherited;
  FMenus:= TMenuItemBuffer.Create( nil );
  Menus.OnChange:= OnChange;
  Color:= $20FFFFFF;
  SelColor:= $80000000;
  FontColor:= $80000000;
  SelFontColor:= $80FFFFFF;
  ItemHeight:= 16;
  FFreeMenu:= True;
  Visible:= False;
  FAutoSize:= True;
end;

destructor TRevPopupMenu.Destroy;
begin
//  if ( FFreeMenu ) then
  FreeAndNil( FMenus );
//  else
//  Menus.OnChange:= nil;
  FreeAndNil( Expand );
  inherited;
end;

procedure TRevPopupMenu.Draw;
var
  I: Integer;
begin
  inherited;
  if ( not HasFocus ) then
    Visible:= False;
  if ( Menus = nil ) then
    exit;

  Canvas.RenderShade( 5, 5, FScreenWidth + 5, FScreenHeight + 5, 10, $00000000, $40000000 );    
  for I:= 0 to Menus.Count - 1 do
    begin
      Canvas.Font.Size:= ItemHeight;
      if ( I = FSelIdx ) then
        begin
          Canvas.RenderRect( 0, I * ItemHeight, Width - 1, ( I + 1 ) * ItemHeight, SelColor, SelColor, SelColor, SelColor );
          Canvas.Font.Color:= SelFontColor;
        end
      else
        begin
          Canvas.RenderRect( 0, I * ItemHeight, Width - 1, ( I + 1 ) * ItemHeight, Color, Color, Color, Color );
          Canvas.Font.Color:= FontColor;
        end;

      Canvas.RenderText( Menus[ I ].Caption, Point( 0, I * ItemHeight ));
      if ( Menus[ I ].SubMenus.Count > 0 ) then
        Canvas.RenderText( '>', Point( Round( Width - Canvas.TextWidth( '>' )),
          I * ItemHeight ) );
    end;
end;

procedure TRevPopupMenu.MouseClick( mb1, mb2, mb3: Boolean; X, Y: Integer );
begin
  inherited;
  if ( mb1 ) then
    if ( Menus.Count > FSelIdx ) then
      if ( FSelIdx > -1 ) then
        begin
          if ( Assigned( Menus.Items[ FSelIdx ].OnClick )) then
            Menus.Items[ FSelIdx ].OnClick( Menus.Items[ FSelIdx ]);
        end;
  HasFocus:= False;
end;

procedure TRevPopupMenu.MouseMove( IsOver: Boolean; X, Y: Integer );
begin
  inherited;
  if ( Menus = nil ) then
    exit;
  if ( not IsOver ) then
    begin
      if ( Expand = nil ) then
        FSelIdx:= -1;
      exit;
    end;
  if ( Int( Y / ItemHeight ) = FSelIdx ) then
    exit;

  FreeAndNil( Expand );
  FSelIdx:= Round( Int( Y / ItemHeight ));
  if ( Menus.Count > FSelIdx ) then
    if ( Menus[ FSelIdx ].SubMenus.Count > 0 ) then
      begin
        Expand:= TRevPopupMenu.Create( Self, Engine, Manager );
        Expand.Skin:= Skin;
        Expand.Left:= Left + Width;
        Expand.Top:= Top + FSelIdx * ItemHeight;
        Expand.Menus:= Menus[ FSelIdx ].SubMenus;
      end;
end;

procedure TRevPopupMenu.OnChange( Sender: TObject );
var
  I: Integer;
begin
  if ( Menus = nil ) then
    exit;
  Height:= Menus.Count * ItemHeight;
  Width:= 10;
  if ( FAutoSize ) then
    For I:= 0 to Menus.Count - 1 do
      Width:= Max( Width, Manager.DefaultFont.TextWidth( Menus[ I ].Caption + '>>', ItemHeight ));
end;

procedure TRevPopupMenu.PopUp( x, y: Integer );
begin
  Left:= x;
  Top:= y;
  HasFocus:= True;
  Visible:= True;
  BringToFront;
end;

procedure TRevPopupMenu.SetFocus( const Value: Boolean );
begin
  inherited;
  if ( not Value ) then
    Visible:= False;
end;

procedure TRevPopupMenu.SetItemHeight( const Value: Integer );
begin
  FItemHeight:= Value;
  OnChange( Self );
end;

procedure TRevPopupMenu.SetMenus( const Value: TMenuItemBuffer );
begin
  FreeAndNil( FMenus );
  FMenus:= Value;
  FFreeMenu:= False;
  FMenus.OnChange:= OnChange;
  OnChange( Self );
end;

{ TMenuItemBuffer }

function TMenuItemBuffer.Add( Item: TMenuItem ): Integer;
begin
  Result:= inherited Add( Item );
  if ( Assigned( OnChange )) then
    OnChange( Self );
end;

function TMenuItemBuffer.Add( Caption: String ): Integer;
var
  M: TMenuItem;
begin
  M:= TMenuItem.Create;
  M.Caption:= Caption;
  Result:= Add( M );
end;

procedure TMenuItemBuffer.Clear;
begin
  while ( Count > 0 ) do
    begin
      Get( Count - 1 ).Free;
      Delete( Count - 1 );
    end;
  inherited;
  if ( Assigned( OnChange )) then
    OnChange( Self );
end;

constructor TMenuItemBuffer.Create( AParent: TMenuItem );
begin
  inherited Create;
  Parent:= AParent;
end;

procedure TMenuItemBuffer.Delete( Item: TMenuItem );
begin
  Delete( IndexOf( Item ));
end;

procedure TMenuItemBuffer.Delete( Index: Integer );
begin
  inherited Delete( Index );
  if ( Assigned( OnChange )) then
    OnChange( Self );
end;

destructor TMenuItemBuffer.Destroy;
begin
  Clear;
  inherited;
end;

function TMenuItemBuffer.Get( Index: Integer ): TMenuItem;
begin
  Result:= TMenuItem( inherited Get( Index ));
end;

procedure TMenuItemBuffer.MoveTo( Index: Integer;
  Controls: TMenuItemBuffer );
begin
  Controls.Add( Get( Index ));
  inherited Delete( Index );
end;

procedure TMenuItemBuffer.Put( Index: Integer; Item: TMenuItem );
begin
  inherited Put( Index, Item );
  if ( Assigned( OnChange )) then
    OnChange( Self );
end;

{ TMenuItem }

constructor TMenuItem.Create;
begin
  inherited;
  SubMenus:= TMenuItemBuffer.Create( Self );
end;

destructor TMenuItem.Destroy;
begin
  FreeAndNil( SubMenus );
  inherited;
end;

initialization
  Rev_RegisterClass( TRevPopupMenu );


end.
