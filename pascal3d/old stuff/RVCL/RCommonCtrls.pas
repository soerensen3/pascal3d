unit RCommonCtrls;

interface
  uses
    Core,
    Classes,
    SysUtils,
    Types,
    RStdCtrls,
    RButtons,
    Input,
    RMenus,
    RScrollBars,
    Math,
    RTypes,
    RVCL;

  type
    TFPSGraph = class( TRevGraphicControl )
      private
        FFontColor: Cardinal;
        FColor: Cardinal;

      public
        procedure Draw; override;

      published
        property Color: Cardinal read FColor write FColor;
        property FontColor: Cardinal read FFontColor write FFontColor;
    end;

    TRevLabel = class( TRevGraphicControl )
      private
        FFontColor: Cardinal;
        FFontSize: Integer;
        FColor: Cardinal;
        FText: String;
        FShowBorder: Boolean;

      public
        procedure Draw; override;
        constructor Create( AOwner: TBaseObject; AEngine: TEngine; _OwnerCtrlBuf: TControlBuffer; _Manager: TVCLManager );
        destructor Destroy; override;

      published
        property Color: Cardinal read FColor write FColor;
        property FontColor: Cardinal read FFontColor write FFontColor;
        property FontSize: Integer read FFontSize write FFontSize;
        property Text: String read FText write FText;
        property ShowBorder: Boolean read FShowBorder write FShowBorder;
    end;

    TRevProgressBar = class( TRevGraphicControl )
      private
        FProgress: Integer;
        FMax: Single;
        FMin: Single;
        FFontColor: Cardinal;
        FColor: Cardinal;
        FBackColor: Cardinal;

        function GetPercentDone: Single;

      public
        procedure Draw; override;

      published
        property Min: Single read FMin write FMin;
        property Max: Single read FMax write FMax;
        property Progress: Integer read FProgress write FProgress;
        property PercentDone: Single read GetPercentDone;
        property Color: Cardinal read FColor write FColor;
        property BackColor: Cardinal read FBackColor write FBackColor;
        property FontColor: Cardinal read FFontColor write FFontColor;
    end;

    TTreeNode = class( TPersistent )
      private
        FItems: array[ 0..16768 ] of TTreeNode;
        FCaption: String;
        FCount: Integer;
        FExpanded: Boolean;
        FLevel: Integer;
        FParent: TTreeNode;
        FValue: String;
        FIdx: Integer;
        FReadOnly: Boolean;
        FObject: TObject;
        FOnSetValue: TNotifyEvent;
        FDropdownList: TStringList;

        function GetItem( Index: Integer ): TTreeNode;
        procedure SetItem( Index: Integer; const Value: TTreeNode );
        procedure SetValue( const Value: String );
        function GetVisible: Boolean;

      public
        constructor Create( AParent: TTreeNode );
        destructor Destroy; override;

        procedure Clear;
        function Add( Item: TTreeNode ): Integer; overload;
        function Add( Caption: String ): Integer; overload;
        function IndexOf( Item: TTreeNode ): Integer;
        procedure Delete( Index: Integer ); overload;
        procedure Delete( Item: TTreeNode ); overload;
        procedure Insert( Index: Integer; Item: TTreeNode );
        function GetItemIndex: Integer;
        function GetSubItemCount: Integer;
        function GetItemWithIndex( Index: Integer ): TTreeNode;
        procedure Expand( const Recursive: Boolean = False );

        property Items[ Index: Integer ]: TTreeNode read GetItem write SetItem; default;
        property Count: Integer read FCount;
        property DropDownList: TStringList read FDropdownList;

      published
        property Caption: String read FCaption write FCaption;
        property Value: String read FValue write SetValue;
        property Expanded: Boolean read FExpanded write FExpanded;
        property Level: Integer read FLevel;
        property Parent: TTreeNode read FParent;
        property ReadOnly: Boolean read FReadOnly write FReadOnly;
        property Obj: TObject read FObject write FObject;
        property OnSetValue: TNotifyEvent read FOnSetValue write FOnSetValue;
        property Visible: Boolean read GetVisible;
    end;

    TValEdit = class( TRevGraphicControl )
      private
        FEdit: TRevEdit;
        FButton: TRevButton;
        FItem: TTreeNode;
        FDropDown: TRevPopupMenu;

        procedure SetItem( const Value: TTreeNode );
        procedure SetHeight( const Value: Integer ); override;
        procedure KeyDown( Sender: TObject );
        procedure ButtonClick( Sender: TRevGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
        procedure OnItemClick( Sender: TObject );

      public
        constructor Create( AOwner: TBaseObject; AEngine: TEngine; _OwnerCtrlBuf: TControlBuffer; _Manager: TVCLManager );

      published
        property Item: TTreeNode read FItem write SetItem;
    end;

    TRevTreeView = class( TRevFocusControl )
      private
        FRootNode: TTreeNode;
        FColorSel: Cardinal;
        FColorSelText: Cardinal;
        FColorText: Cardinal;
        FColor: Cardinal;
        FItemHeight: Integer;
        FSelection: TTreeNode;
        FRowSelect: Boolean;
        FValueList: TRevGraphicControl;
        FItemList: TRevGraphicControl;
        FScrollBar: TRevScrollBar;
        FShowValues: Boolean;
        FCanEditValues: Boolean;
        FValEdit: TValEdit;
        FOnSelectionChange: TNotifyEvent;

        procedure BoxMouseClick( Sender: TRevGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
        procedure DrawItems( Sender: TRevGraphicControl; OffSetX, OffSetY, _Width, _Height: Single );
        procedure DrawValues( Sender: TRevGraphicControl; OffSetX, OffSetY, _Width, _Height: Single );
        procedure SetShowValues( const Value: Boolean );
        procedure SetCanEditValues( const Value: Boolean );
        procedure SetSelection( const Value: TTreeNode );

      public
        constructor Create( AOwner: TBaseObject; AEngine: TEngine; _OwnerCtrlBuf: TControlBuffer; _Manager: TVCLManager );
        destructor Destroy; override;

        property RootNode: TTreeNode read FRootNode;

      published
        property ItemHeight: Integer read FItemHeight write FItemHeight;
        property Color: Cardinal read FColor write FColor;
        property ColorText: Cardinal read FColorText write FColorText;
        property ColorSel: Cardinal read FColorSel write FColorSel;
        property ColorSelText: Cardinal read FColorSelText write FColorSelText;
        property Selection: TTreeNode read FSelection write SetSelection;
        property RowSelect: Boolean read FRowSelect write FRowSelect;
        property ShowValues: Boolean read FShowValues write SetShowValues;
        property CanEditValues: Boolean read FCanEditValues write SetCanEditValues;
        property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    end;

implementation

{ TProgressBar }

procedure TRevProgressBar.Draw;
begin
  inherited;
  Canvas.RenderRect( GetPercentDone * ( Width - 1 ), 0, Width - 1, Height - 1, BackColor, BackColor, BackColor, BackColor );
  Canvas.Render3DFrame( GetPercentDone * ( Width - 1 ), 0, Width - 1, Height - 1, BackColor, BackColor, BackColor, BackColor, -64 );
  Canvas.Render3DFrame( GetPercentDone * ( Width - 2 ), 1, Width - 2, Height - 2, BackColor, BackColor, BackColor, BackColor, -64 );
  Canvas.RenderRect( 0, 0, GetPercentDone * ( Width - 1 ), Height - 1, Color, Color, Color, Color );
  Canvas.Render3DFrame( 0, 0, GetPercentDone * ( Width - 1 ), Height - 1, Color, Color, Color, Color, 64 );
  Canvas.Render3DFrame( 1, 1, GetPercentDone * ( Width - 2 ), Height - 2, Color, Color, Color, Color, 64 );
end;

function TRevProgressBar.GetPercentDone: Single;
begin
  Result:= ( Progress - FMin ) / ( FMax - FMin );
end;

{ TFPSGraph }

procedure TFPSGraph.Draw;
var
  vp: TViewPort;
begin
  inherited;
  Canvas.RenderRect( 0, 0, Width - 1, Height - 1, $80000000, $80000000, $80000000, $80000000 );
  Canvas.RenderLineRect( 0, 0, Width - 1, Height - 1, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF );
  Canvas.RenderLineRect( 1, 1, Width - 2, Height - 2, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF );
  Canvas.Font.Size:= 32;
  Canvas.Font.Color:= $80FFFFFF;
  Canvas.RenderTextAdv( 'FPS: $$color:800080FF$$$size:36$' + FloatToStrF( Engine.FPS, ffFixed, 7, 2 ), Point( 2, 8 ));
  Canvas.Font.Size:= 16;
  Canvas.Font.Color:= $80008040;
  Canvas.RenderTextAdv( 'Average FPS: $$color:80808080$' + FloatToStrF( Engine.AverageFPS, ffFixed, 7, 2 ), Point( 2, 48 ));
  Canvas.Font.Size:= 32;
  Canvas.Font.Color:= $80FFFFFF;
  Canvas.RenderTextAdv( 'TimeGone: $$color:800080FF$$$size:36$' + IntToStr( Engine.TimeGone ), Point( 2, 60 ));
  vp:= Engine.DisplayDriver.GetViewport;
  Canvas.Font.Size:= 16;
  Canvas.Font.Color:= $80008040;
  Canvas.RenderTextAdv( 'viewport: ' + #13#10 + '($$color:80808080$ X>' + IntToStr( vp.X ) + ', Y>' +
                                                                   IntToStr( vp.Y ) + ', W>' +
                                                                   IntToStr( vp.Width ) + ', H>' +
                                                                   IntToStr( vp.Height ) + '$$color:XXXXXXXX$ )',
                                                                   Point( 2, 86 ));

{  RenderTextAdv( 'Cam Position: ' + #13#10 + '( $$color:80808080$' + FloatToStrF( Engine.DisplayDriver.vCamPos.x, ffFixed, 7, 2 ) + ', ' +
                                                       FloatToStrF( Engine.DisplayDriver.vCamPos.y, ffFixed, 7, 2 ) + ', ' +
                                                       FloatToStrF( Engine.DisplayDriver.vCamPos.y, ffFixed, 7, 2 ) + '$$color:XXXXXXXX$ )',
                                                       Point( 2, 86 ), 16, $80008040 );}
end;

{ TTreeNode }

function TTreeNode.Add( Item: TTreeNode ): Integer;
begin
  Result:= -1;
  if ( Item = nil ) then
    exit;
  Items[ FCount ]:= Item;
  Item.FLevel:= Level + 1; 
  Result:= FCount;
  Inc( FCount );
end;

function TTreeNode.Add( Caption: String ): Integer;
begin
  Result:= Add( TTreeNode.Create( Self ));
  if ( Result > -1 ) then
    Items[ Result ].Caption:= Caption;
end;

procedure TTreeNode.Delete( Index: Integer );
var
  I: Integer;
begin
  if (( Index < 0 ) or ( Index >= Count )) then
    exit;
  FreeAndNil( FItems[ Index ]);
  For I:= Index to FCount - 2 do
    Items[ Index ]:= Items[ Index + 1 ];
end;

procedure TTreeNode.Clear;
var
  I: Integer;
begin
  For I:= 0 to FCount - 1 do
    FreeAndNil( FItems[ I ]);
  FCount:= 0;
end;

procedure TTreeNode.Delete( Item: TTreeNode );
var
  I: Integer;
begin
  I:= IndexOf( Item );
  Delete( I );
end;

function TTreeNode.GetItem( Index: Integer ): TTreeNode;
begin
  if (( Index < 0 ) or ( Index >= Count )) then
    exit;
  Result:= FItems[ Index ];
end;

function TTreeNode.IndexOf( Item: TTreeNode ): Integer;
var
  I: Integer;
begin
  Result:= -1;
  For I:= 0 to Count - 1 do
    if ( Items[ I ] = Item ) then
      begin
        Result:= I;
        break;
      end;
end;

procedure TTreeNode.Insert( Index: Integer; Item: TTreeNode );
var
  I: Integer;
begin
  if (( Index < 0 ) or ( Index >= Count )) then
    exit;

  For I:= Index to FCount do
    Items[ Index + 1 ]:= Items[ Index ];
  Items[ Index ]:= Item;
end;

procedure TTreeNode.SetItem( Index: Integer; const Value: TTreeNode );
begin
  FItems[ Index ]:= Value;
end;

constructor TTreeNode.Create( AParent: TTreeNode );
begin
  inherited Create;
  FCount:= 0;
  FCaption:= '';
  FExpanded:= False;
  FParent:= AParent;
  if ( FParent <> nil ) then
    FLevel:= Parent.Level + 1;
  FDropDownList:= TStringList.Create;
end;

function TTreeNode.GetItemIndex: Integer;
var
  I: Integer;
begin
  Result:= 0;
  if ( Parent = nil ) then
    exit;
  if ( not Parent.Expanded ) then
    begin
      Result:= -1;
      exit;
    end;

  For I:= Parent.IndexOf( Self ) - 1 downto 0 do
    Result:= Result + Items[ I ].GetSubItemCount + 1;

  I:= Parent.GetItemIndex;
    if ( I > -1 ) then
      Result:= Result + I
    else
      Result:= -1;
end;

function TTreeNode.GetSubItemCount: Integer;
var
  I: Integer;
begin
  Result:= 0;
  if ( not Expanded ) then
    exit;
  For I:= 0 to Count - 1 do
    Result:= Result + Items[ I ].GetSubItemCount + 1;
end;

function TTreeNode.GetItemWithIndex( Index: Integer ): TTreeNode;
var
  I: Integer;
begin
  Result:= nil;
  if ( not Expanded ) then
    exit;
  For I:= 0 to Count - 1 do
    if ( Items[ I ].GetItemIndex = Index ) then
      begin
        Result:= Items[ I ];
        break;
      end
    else
      if ( Items[ I ].Expanded ) then
        begin
          Result:= Items[ I ].GetItemWithIndex( Index );
//          Start:= Start + Items[ I ].GetSubItemCount;
          if ( Result <> nil ) then
            break;
        end;

end;

procedure TTreeNode.SetValue(const Value: String);
begin
  FValue:= Value;
  if ( Assigned( OnSetValue )) then
    OnSetValue( Self );
end;

destructor TTreeNode.Destroy;
begin
  FDropdownList.Free;
  inherited;
end;

function TTreeNode.GetVisible: Boolean;
  function IsVisible( Node: TTreeNode ): Boolean;
  begin
    if ( Node.Parent = nil ) then
      Result:= True
    else if ( Node.Parent.Expanded ) then
      Result:= IsVisible( Node.Parent )
    else
      Result:= False;
  end;
begin
  Result:= IsVisible( Self );
end;

procedure TTreeNode.Expand( const Recursive: Boolean = False );
var
  I: Integer;
begin
  Self.Expanded:= True;
  if ( Recursive ) then
    For I:= 0 to Count - 1 do
      Items[ I ].Expand( True );
end;

{ TTreeView }

constructor TRevTreeView.Create( AOwner: TBaseObject; AEngine: TEngine;
  _OwnerCtrlBuf: TControlBuffer; _Manager: TVCLManager );
begin
  inherited;
  FRootNode:= TTreeNode.Create( nil );
  FRootNode.Caption:= 'Root';
  FRootNode.Expanded:= True;
  FColor:= $80003088;
  FColorText:= $9FFFFFFF;
  FColorSel:= $80FF0000;
  FColorSelText:= $FF000000;
  FRowSelect:= True;
  FItemHeight:= 16;
  FItemList:= TRevFocusControl.Create( Self, AEngine, Self.Controls, Manager );
  FItemList.OnDraw:= DrawItems;
  FItemList.OnMouseClick:= BoxMouseClick;
  FValueList:= TRevGraphicControl.Create( Self, AEngine, Self.Controls, Manager );
  FValueList.OnDraw:= DrawValues;
  FValueList.OnMouseClick:= BoxMouseClick;
  FCanEditValues:= True;
  FValEdit:= TValEdit.Create( Self, AEngine, FValueList.Controls, Manager );
  FScrollBar:= TRevScrollBar.Create( Self, AEngine, Self.Controls, Manager );
  FScrollBar.Align:= alRight;
  FScrollBar.Min:= 0;
  FScrollBar.Max:= 0;
  FScrollBar.PageSize:= 1.0;
  ShowValues:= True;
end;

destructor TRevTreeView.Destroy;
begin
  FRootNode.Free;
  inherited;
end;

procedure TRevTreeView.BoxMouseClick( Sender: TRevGraphicControl; mb1, mb2,
  mb3: Boolean; X, Y: Integer );
var
  BtnWidth,
  XOffSet,
  YOffSet: Integer;
  Index: Integer;
  N: TTreeNode;
  BrkAll: Boolean;
  CurItem: Integer;

  function GetObject( Node: TTreeNode ): TTreeNode;
  var
    I: Integer;
  begin
    Result:= nil;
    For I:= 0 to Node.Count - 1 do
      begin
        Inc( CurItem );
        if ( BrkAll ) then
          break
        else if ( CurItem > Index ) then
          BrkAll:= True
        else if ( CurItem = Index ) then
          begin
            Result:= Node[ I ];
            BrkAll:= True;
            break;
          end
        else if ( Node[ I ].Expanded ) then
          begin
            Result:= GetObject( Node[ I ]);
            if ( Result <> nil ) then
              begin
                BrkAll:= True;
                break;
              end;
          end;
      end;
  end;
begin
  inherited;

  Index:= Y div ItemHeight - 1;

  CurItem:= -1;
  N:= GetObject( RootNode ); //GetObject( RootNode, Index, _s );

  BtnWidth:= Round( 0.8 * ItemHeight );
  XOffSet:= Round( ItemHeight * 0.2 ) + ( N.Level - 1 ) * ItemHeight;
  YOffSet:= Round( ItemHeight * 0.1 );

  if (( mb1 ) and ( X >= XOffSet ) and ( Y - Index * ItemHeight >= YOffSet ) and
      ( X <= XOffSet + BtnWidth ) and ( Y - Index * ItemHeight >= YOffSet + BtnWidth )) then
    begin
      N.Expanded:= not N.Expanded;
      if ( Selection <> nil ) then
        if ( not Selection.Visible ) then
          Selection:= nil
        else
          begin
//            Selection.FIdx:= Selection.GetItemIndex + 1;
//            Selection:= Selection;
            Selection:= N;
          end;

{          begin
            if ( N.Expanded ) then
              Selection.FIdx:= Selection.FIdx + N.GetSubItemCount;
          end;}
    end
  else
    begin
      N.FIdx:= Index + 1;
      Selection:= N;
    end;
end;

procedure TRevTreeView.DrawItems( Sender: TRevGraphicControl; OffSetX, OffSetY,
  _Width, _Height: Single );
var
  YPos: Integer;
  OffSet: Integer;
  nDrawn: Integer;
  procedure DrawItem( Item: TTreeNode; Level: Integer );
    procedure DrawExpandButton( Expand: Boolean );
    var
//      _Skin: TTexture;
      BtnWidth: Integer;
      XOffSet,
      YOffSet,
      XCenter: Single;
      YCenter: Single;
    begin
//      _Skin:= Skin;
//      Skin:= '';

      BtnWidth:= Round( 0.8 * ItemHeight );
      XOffSet:= Round( ItemHeight * 0.2 ) + Level * ItemHeight;
      YOffSet:= Round( ItemHeight * 0.1 );
      XCenter:= Round( ItemHeight * 0.4 );
      YCenter:= Round( ItemHeight * 0.5 );

//      Canvas.RenderShade( XOffSet + 2, YPos + YOffSet + 2, XOffSet + BtnWidth + 1, YPos + BtnWidth + 1, 5, $00000000, $80000000 );
      FItemList.Canvas.RenderRect( XOffSet, YPos + YOffSet, XOffSet + BtnWidth - 1, YPos + BtnWidth - 1, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF );
      FItemList.Canvas.RenderLineRect( XOffSet, YPos + YOffSet, XOffSet + BtnWidth - 1, YPos + BtnWidth - 1, $FF000000, $FF000000, $FF000000, $FF000000 );
//      FItemList.RenderLineRect( XOffSet - 1, YPos - 1 + YOffSet, XOffSet + BtnWidth, YPos + BtnWidth, $80000000, $80000000, $80000000, $80000000 );
//      FItemList.RenderLineRect( XOffSet + 1, YPos + 1 + YOffSet, XOffSet + BtnWidth + 2, YPos + BtnWidth + 2, $80000000, $80000000, $80000000, $80000000 );
      FItemList.Canvas.RenderLine( XOffSet + 2, YPos + YCenter - 1, XOffSet + BtnWidth - 2, YPos + YCenter - 1, $FF000000, $FF000000 );
      if ( not Expand ) then
        FItemList.Canvas.RenderLine( XOffSet + XCenter, YPos + 2 + YOffSet, XOffSet + XCenter, YPos + BtnWidth - 2, $FF000000, $FF000000 );
//      Skin:= _Skin;
    end;
  var
    I: Integer;
  begin
    FItemList.Canvas.Font.Size:= ItemHeight;
    for I:= 0 to Item.Count - 1 do
      begin
        Inc( YPos, ItemHeight );
        Inc( nDrawn );
        OffSet:= Round( ItemHeight * 1.5 ) + Level * ItemHeight;
        if ( Item[ I ] = Selection ) then
          if ( RowSelect ) then
            FItemList.Canvas.RenderRect( 0, YPos, Width - 1, YPos + ItemHeight, ColorSel, ColorSel, ColorSel, ColorSel )
          else
            FItemList.Canvas.RenderRect( OffSet, YPos, OffSet + Manager.DefaultFont.TextWidthAdv( Item[ I ].Caption, ItemHeight ), YPos + ItemHeight, ColorSel, ColorSel, ColorSel, ColorSel );

        FItemList.Canvas.Font.Color:= $80000000;
//        FItemList.Canvas.RenderTextAdv( Item[ I ].Caption, Point( OffSet + 2, YPos + 2 ));
        FItemList.Canvas.Font.Color:= ColorText;
        FItemList.Canvas.RenderTextAdv( Item[ I ].Caption, Point( OffSet, YPos ));
        if ( Item[ I ].Count > 0 ) then
          DrawExpandButton( Item[ I ].Expanded );
        if ( Item[ I ].Expanded ) then
          DrawItem( Item[ I ], Level + 1 );
      end;
  end;
begin
  YPos:= -Round( FScrollBar.Position );
  nDrawn:= 0;
  FItemList.Canvas.RenderRect( 0, 0, FItemList.Width - 1, FItemList.Height - 1, Color, Color, Color, Color );
  if ( HasFocus ) then
    FItemList.Canvas.RenderLineRect( 2, 2, FItemList.Width - 3, FItemList.Height - 3, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF );  
  DrawItem( RootNode, 0 );

  FScrollBar.Max:= nDrawn * ItemHeight;
  FScrollBar.PageSize:= Min( 1, Height / Max( 1, FScrollBar.Max ));
  inherited;
end;

procedure TRevTreeView.DrawValues( Sender: TRevGraphicControl; OffSetX, OffSetY,
  _Width, _Height: Single );
var
  YPos: Integer;

  procedure DrawItem( Item: TTreeNode );
  var
    I: Integer;
  begin
    FValueList.Canvas.Font.Size:= ItemHeight;

    for I:= 0 to Item.Count - 1 do
      begin
        Inc( YPos, ItemHeight );

        if (( Item[ I ] = Selection ) and (( not CanEditValues ) or ( Item[ I ].ReadOnly ))) then
          FValueList.Canvas.RenderRect( 0, YPos, FValueList.Width - 1, YPos + ItemHeight, ColorSel, ColorSel, ColorSel, ColorSel );
        if (( not CanEditValues ) or ( Item[ I ] <> Selection ) or ( Item[ I ].ReadOnly )) then
          begin
//            FValueList.Canvas.Font.Color:= $80000000;
//            FValueList.Canvas.RenderText( Item[ I ].Value, Point( 7, YPos + 2 ));
            FValueList.Canvas.Font.Color:= ColorText;
            FValueList.Canvas.RenderText( Item[ I ].Value, Point( 5, YPos ));
          end;
        if ( Item[ I ].Expanded ) then
          DrawItem( Item[ I ]);
      end;
  end;
begin
  YPos:= 0;
  FValueList.Canvas.RenderRect( 0, 0, FValueList.Width - 1, FValueList.Height - 1, Color, Color, Color, Color );
  DrawItem( RootNode );  
end;

procedure TRevTreeView.SetShowValues( const Value: Boolean );
begin
  FShowValues:= Value;
  case ( Value ) of
    False:
      begin
        FValueList.Visible:= False;
        FItemList.Align:= alClient;
      end;
    True:
      begin
        FValueList.Visible:= True;
        FValueList.Align:= alClient;
        FItemList.Align:= alLeft;
        FItemList.Width:= 100;
        FValueList.Width:= Width - 100;
        if ( Selection <> nil ) then
          begin
            FValEdit.Visible:= CanEditValues;
            FValEdit.Top:= Selection.FIdx * ItemHeight;
          end;
        FValEdit.Left:= 0;
        FValEdit.Width:= Width - 100;
        FValEdit.Height:= ItemHeight;
      end;
  end;
end;

procedure TRevTreeView.SetCanEditValues( const Value: Boolean );
begin
  FCanEditValues:= Value;
end;

procedure TRevTreeView.SetSelection( const Value: TTreeNode );
begin
  FSelection:= Value;
  if (( FSelection = nil ) or ( FSelection.ReadOnly )) then
    begin
      FValEdit.Visible:= False;
      exit;
    end;
  FValEdit.Top:= FSelection.FIdx * ItemHeight;
  FValEdit.Width:= Width - 100;
  FValEdit.Visible:= FCanEditValues;
  FValEdit.Item:= FSelection;
  if ( Assigned( FOnSelectionChange )) then
    FOnSelectionChange( FValEdit.Item );
end;

{ TValEdit }

procedure TValEdit.ButtonClick( Sender: TRevGraphicControl; mb1, mb2,
  mb3: Boolean; X, Y: Integer );
var
  I: Integer;
begin
  FDropDown.Width:= Width;
  FDropDown.Color:= $FFFFFFFF;
  FDropDown.PopUp( FScreenLeft, FScreenTop + Round( Height ));
end;

constructor TValEdit.Create( AOwner: TBaseObject; AEngine: TEngine;
  _OwnerCtrlBuf: TControlBuffer; _Manager: TVCLManager );
begin
  inherited;
  FEdit:= TRevEdit.Create( Self, AEngine, Self.Controls, Manager );
  FButton:= TRevButton.Create( Self, AEngine, Self.Controls, Manager );
  FButton.Width:= Height;
  FEdit.Align:= alClient;
  FEdit.OnKeyDown:= KeyDown;
  FButton.Align:= alRight;
  FButton.Caption:= 'V';
  FDropDown:= TRevPopupMenu.Create( Self, AEngine, _Manager.Controls, _Manager );
  FButton.OnMouseClick:= ButtonClick;
end;

procedure TValEdit.KeyDown( Sender: TObject );
begin
  if ( Manager.GameInput.Keys[ IK_RETURN ]) then
    Item.Value:= FEdit.Text;
end;

procedure TValEdit.OnItemClick( Sender: TObject );
begin
  if ( Sender is TMenuItem ) then
    begin
      FEdit.Text:= TMenuItem( Sender ).Caption;
      Item.Value:= FEdit.Text;      
    end;
end;

procedure TValEdit.SetHeight( const Value: Integer );
begin
  inherited;
  if ( FButton <> nil ) then
    FButton.Width:= Height;
end;

procedure TValEdit.SetItem( const Value: TTreeNode );
var
  I: Integer;
begin
  FItem:= Value;
  FEdit.Text:= FItem.Value;
  FEdit.SelectAll;
  FEdit.HasFocus:= True;

  FButton.Visible:= Item.DropDownList.Count > 0;
  FDropDown.Menus.Clear;
  For I:= 0 to Item.DropDownList.Count - 1 do
    FDropDown.Menus[ FDropDown.Menus.Add( Item.DropDownList[ I ])].OnClick:= OnItemClick;
end;

{ TLabel }

constructor TRevLabel.Create(AOwner: TBaseObject; AEngine: TEngine;
  _OwnerCtrlBuf: TControlBuffer; _Manager: TVCLManager);
begin
  inherited;
  FFontColor:= $FFFFFFFF;
  FFontSize:= 16;
end;

destructor TRevLabel.Destroy;
begin

  inherited;
end;

procedure TRevLabel.Draw;
begin
  if ( ShowBorder ) then
    begin
      Canvas.RenderRect( 0, 0, Width - 1, Height - 1, $80000000, $80000000, $80000000, $80000000 );
      Canvas.RenderLineRect( 0, 0, Width - 1, Height - 1, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF );
      Canvas.RenderLineRect( 1, 1, Width - 2, Height - 2, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF );
    end;
  Canvas.Font.Size:= FontSize;
  Canvas.Font.Color:= FFontColor;
  Canvas.RenderTextAdv( Text, Point( 2, Round( Height - FontSize ) div 2 ));
  inherited;
end;

initialization
  Rev_RegisterClasses([ TFPSGraph, TRevLabel, TRevProgressBar, TRevTreeView, TValEdit ]);

end.
