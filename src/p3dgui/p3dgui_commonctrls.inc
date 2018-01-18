//p3dgui_commoncontrols.inc
//part of p3dgui.pas

{$IFDEF INTERFACE}

type

  { TP3DListItem }

  TP3DListItem = class( TPersistent )
    private
      FCaption: String;
      FData: Pointer;
      FSymbolText: String;
      FText: TP3DTextBmp;
      FRenderPos: TVec2;
      Symbol: TP3DTextBmp;

      procedure SetCaption(AValue: String);
      procedure SetSymbolText(AValue: String);

    public
      constructor Create;
      destructor Destroy; override;

      property Data: Pointer read FData write FData;
      property RenderPos: TVec2 read FRenderPos;

    published
      property Caption: String read FCaption write SetCaption;
      property Text: TP3DTextBmp read FText write FText;
      property SymbolText: String read FSymbolText write SetSymbolText;
  end;

  TP3DCustomListItemList = specialize gP3DCustomObjectList < TP3DListItem >;

  { TP3DListItemList }

  TP3DListItemList = class ( TP3DCustomListItemList )
    function Add( Caption: String; Data: Pointer ): Integer; overload;
    function Add( Caption: String; Data: Pointer; SymbolText: String ): Integer; overload;
  end;

  { TP3DTreeNode }

  TP3DTreeNodeList = class;

  TP3DTreeNode = class( TP3DListItem )
    private
      FChildren: TP3DTreeNodeList;
      FExpanded: Boolean;
      FParent: TP3DTreeNode;
      procedure SetParent(AValue: TP3DTreeNode);

    public
      constructor Create( const AParent: TP3DTreeNode = nil );
      destructor Destroy; override;

    published
      property Children: TP3DTreeNodeList read FChildren write FChildren;
      property Expanded: Boolean read FExpanded write FExpanded;
      property Parent: TP3DTreeNode read FParent write SetParent;
  end;

  TP3DCustomTreeNodeList = specialize gP3DCustomObjectList < TP3DTreeNode >;

  { TP3DTreeNodeList }

  TP3DTreeNodeList = class ( TP3DCustomTreeNodeList )
    private
      FTreeNode: TP3DTreeNode;

    public
      constructor Create( ATreeNode: TP3DTreeNode );

      function Add( Caption: String; Data: Pointer; const Expanded: Boolean = False; const Symbol: String = '' ): Integer; overload;
      procedure Delete(Index: Integer); override;
      function Find( Caption: String ): Integer;
      function FindByData( Data: Pointer ): Integer;
      function FindByData( Data: Pointer; Recurse: Boolean ): TP3DTreeNode;
      procedure ExpandAll( const Recurse: Boolean = True );

      property TreeNode: TP3DTreeNode read FTreeNode;
  end;

  { TP3DCustomItemView }

  TP3DCustomItemView = class ( TP3DGraphicControl )
    private
      FAllowDrag: Boolean;
      FFont: TP3DCanvasFont;
      FPageScrollX: Single;
      FPageScrollY: Single;
      FPreset: TP3DButtonPreset;
      FScrollBarV: TP3DScrollBar;
      FScrollBarH: TP3DScrollBar;
      FScrollBarWidth: Float;
      FShowSymbols: Boolean;
      FSymbolsFontName: String;

      procedure SetPageScrollX(AValue: Single);
      procedure SetPageScrollY(AValue: Single);
      procedure SetScrollBarWidth(AValue: Float);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      class function IsFocusControl: Boolean; override;

      procedure Draw; override;
      procedure OnScroll( Sender: TObject );
      procedure MouseWheel( deltax, deltay: Integer); override;
      procedure MouseMove( X, Y: Integer ); override;

    published
      property Preset: TP3DButtonPreset read FPreset write FPreset;
      property Font: TP3DCanvasFont read FFont write FFont;
      property ScrollBarV: TP3DScrollBar read FScrollBarV write FScrollBarV;
      property ScrollBarH: TP3DScrollBar read FScrollBarH write FScrollBarH;
      property ShowSymbols: Boolean read FShowSymbols write FShowSymbols;
      property SymbolsFontName: String read FSymbolsFontName write FSymbolsFontName;
      property ScrollBarWidth: Float read FScrollBarWidth write SetScrollBarWidth;
      property AllowDrag: Boolean read FAllowDrag write FAllowDrag;
      property PageScrollX: Single read FPageScrollX write SetPageScrollX;
      property PageScrollY: Single read FPageScrollY write SetPageScrollY;
  end;

  { TP3DTreeView }

  TP3DListViewStyle = ( vsList, vsIcon );
  TP3DTreeViewSelectionChangeEvent = procedure ( var NewSelection: TP3DTreeNode ) of object;
  TP3DTreeView = class( TP3DCustomItemView )
    private
      FChildren: TP3DTreeNodeList;
      FDragItem: TP3DTreeNode;
      FOnSelectionChange: TP3DTreeViewSelectionChangeEvent;
      FSelection: TP3DTreeNode;

      SymbolMinus: TP3DTextBmp;
      SymbolPlus: TP3DTextBmp;

      procedure SetSelection( AValue: TP3DTreeNode ); virtual;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure Draw; override;
      procedure MouseClick( mb1, mb2, mb3: Boolean; X, Y: Integer ); override;
      procedure MouseDblClick(mb1, mb2, mb3: Boolean; X, Y: Integer); override;
      procedure BeginDrag; override;
      procedure DragCancel(Sender: TP3DGraphicControl; X, Y: Integer); override;
      procedure KeyboardAction; override;

      function NodeAtPos(p: TVec2): TP3DTreeNode;

    published
      property Children: TP3DTreeNodeList read FChildren write FChildren;
      property Selection: TP3DTreeNode read FSelection write SetSelection;
      property OnSelectionChange: TP3DTreeViewSelectionChangeEvent read FOnSelectionChange write fOnSelectionChange;
      property DragItem: TP3DTreeNode read FDragItem;
  end;

  { TP3DListView }

  TP3DListView = class;
  TP3DListViewSelectionChangeEvent = procedure ( Sender: TP3DListView; var NewSelection: TP3DListItem ) of object;
  TP3DListView = class ( TP3DCustomItemView )
    private
      FChildren: TP3DListItemList;
      FDragItem: TP3DListItem;
      FOnSelectionChange: TP3DListViewSelectionChangeEvent;
      FSelection: TP3DListItem;
      FViewStyle: TP3DListViewStyle;
      procedure SetSelection(AValue: TP3DListItem);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      function ItemAtPos( p: TVec2 ): TP3DListItem;

      procedure Draw; override;
      function DrawListItem( p: TVec2; Node: TP3DListItem ): TVec2; virtual;
      procedure MouseClick( mb1, mb2, mb3: Boolean; X, Y: Integer ); override;
      procedure BeginDrag; override;
      procedure DragCancel(Sender: TP3DGraphicControl; X, Y: Integer); override;
      procedure KeyboardAction; override;

    published
      property Children: TP3DListItemList read FChildren write FChildren;
      property Selection: TP3DListItem read FSelection write SetSelection;
      property OnSelectionChange: TP3DListViewSelectionChangeEvent read FOnSelectionChange write fOnSelectionChange;
      property ViewStyle: TP3DListViewStyle read FViewStyle write FViewStyle;
      property DragItem: TP3DListItem read FDragItem;
  end;

  TP3DListViewFile = class;

  { TP3DTreeViewDirectory }

  TP3DTreeViewDirectory = class ( TP3DTreeView )
    private
      FDirectory: String;
      FListViewFile: TP3DListViewFile;
      FMask: String;
      FShowFiles: Boolean;

      procedure SetDirectory(AValue: String);
      procedure SetListViewFile(AValue: TP3DListViewFile);
      procedure SetMask(AValue: String);
      procedure SetShowFiles(AValue: Boolean);
      procedure SetSelection(AValue: TP3DTreeNode); override;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      procedure Update;
      function GetItemFileName( Item: TP3DTreeNode; const Relative: Boolean = False ): AnsiString;
      function FindItemByFileName( FileName: String; Relative: Boolean ): TP3DTreeNode;

    published
      property Directory: String read FDirectory write SetDirectory;
      property ShowFiles: Boolean read FShowFiles write SetShowFiles;
      property Mask: String read FMask write SetMask;
      property ListViewFile: TP3DListViewFile read FListViewFile write SetListViewFile;
  end;

  { TP3DListViewFile }

  TP3DListViewFile = class ( TP3DListView )
    private
      FAllowOpenP3D: Boolean;
      FDirectory: String;
      FMask: String;
      FShowFolderUp: Boolean;
      FTreeViewDirectory: TP3DTreeViewDirectory;

      procedure SetDirectory(AValue: String);
      procedure SetMask(AValue: String);
      procedure SetShowFolderUp(AValue: Boolean);
      procedure SetTreeViewDirectory(AValue: TP3DTreeViewDirectory);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      procedure Update;
      procedure MouseDblClick(mb1, mb2, mb3: Boolean; X, Y: Integer); override;

    published
      property Directory: String read FDirectory write SetDirectory;
      property TreeViewDirectory: TP3DTreeViewDirectory read FTreeViewDirectory write SetTreeViewDirectory;
      property ShowFolderUp: Boolean read FShowFolderUp write SetShowFolderUp;
      property Mask: String read FMask write SetMask;
      property AllowOpenP3D: Boolean read FAllowOpenP3D write FAllowOpenP3D;
  end;

  { TP3DTreeViewObjects }

  TP3DTreeViewObjects = class ( TP3DTreeView )
    private
      FObj: TP3DStreamablePointer;
      FShowDesignerPackages: Boolean;

      function GetObj: TP3DStreamable;
      procedure SetObj(AValue: TP3DStreamable);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      procedure Update;

    published
      property Obj: TP3DStreamable read GetObj write SetObj;
      property ShowDesignerPackages: Boolean read FShowDesignerPackages write FShowDesignerPackages;
  end;

  { TP3DTreeViewProperty }

  { TP3DTreeViewProperties }

  TP3DTreeViewProperties = class ( TP3DTreeView )
    private
      FMaxDepth: Integer;
      FObj: TP3DStreamable;

      procedure SetMaxDepth(AValue: Integer);
      procedure SetObj(AValue: TP3DStreamable);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      procedure Update;

    published
      property Obj: TP3DStreamable read FObj write SetObj;
      property MaxDepth: Integer read FMaxDepth write SetMaxDepth;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

procedure TP3DCustomItemView.SetScrollBarWidth(AValue: Float);
begin
  if FScrollBarWidth=AValue then Exit;
  FScrollBarWidth:=AValue;
  ScrollBarH.WidthHeight:= vec2( ScrollBarWidth );
end;

procedure TP3DCustomItemView.SetPageScrollX(AValue: Single);
begin
  if FPageScrollX=AValue then Exit;
  FPageScrollX:=AValue;
end;

procedure TP3DCustomItemView.SetPageScrollY(AValue: Single);
begin
  if FPageScrollY=AValue then Exit;
  FPageScrollY:=AValue;
end;

constructor TP3DCustomItemView.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  SymbolsFontName:= 'Pascal3D-Symbols';
  Font:= TP3DCanvasFont.Create;
  Font.Size:= 20;
  Preset:= TP3DButtonPreset.Create;
  Preset.Color:= Primary50;//vec4( vec3( 0.93 ), 1 );
  //Preset.OutlineColor:= Accent900;//vec4( 225/255, 230/255, 234/255, 1 );
  Preset.FontColor:= Accent50;//vec4( vec3( 0 ), 1 );
  ScrollBarV:= TP3DScrollBar.Create( Self.Controls );
  ScrollBarV.Kind:= p3dsbVertical;
  ScrollBarV.Align:= alRight;
  ScrollBarV.OnChange:= @OnScroll;
  ScrollBarH:= TP3DScrollBar.Create( Self.Controls );
  ScrollBarH.Kind:= p3dsbHorizontal;
  ScrollBarH.Align:= alBottom;
  ScrollBarH.OnChange:= @OnScroll;
  ScrollBarWidth:= 10;
end;

destructor TP3DCustomItemView.Destroy;
begin
  Font.Free;
  Preset.Free;
  inherited Destroy;
end;

class function TP3DCustomItemView.IsFocusControl: Boolean;
begin
  Result:= True;
end;

procedure TP3DCustomItemView.Draw;
begin
  inherited Draw;
  Canvas.RenderRect( Controls.PageScrollPixel, Canvas.WidthHeight - 1 + Controls.PageScrollPixel, Preset.Color, Preset.Color, Preset.Color, Preset.Color );
  Canvas.RenderLineRect( Controls.PageScrollPixel, Canvas.WidthHeight - 1 + Controls.PageScrollPixel, Preset.OutlineColor, Preset.OutlineColor, Preset.OutlineColor, Preset.OutlineColor );
  Canvas.RenderRectShadowInner( Controls.PageScrollPixel, Canvas.WidthHeight - 1 + Controls.PageScrollPixel, 2, 5, vec4( Primary900.xyz, 0.05 ));

  if ( Focused ) then
    Canvas.RenderLineRect( vec2( 4 ) + Controls.PageScrollPixel, Canvas.WidthHeight - 4 + Controls.PageScrollPixel, vec4( 0.5, 0.5, 0.5, 0.5 ));
end;

procedure TP3DCustomItemView.OnScroll(Sender: TObject);
begin
  {if ( Sender = ScrollBarV ) then
    Canvas.Scroll.Y:= Round( ScrollBarV.Position )
  else if ( Sender = ScrollBarH ) then
    Canvas.Scroll.X:= Round( ScrollBarH.Position );}
  PageScrollX:= ScrollBarH.Percentage;
  PageScrollY:= ScrollBarV.Percentage;
end;

procedure TP3DCustomItemView.MouseWheel(deltax, deltay: Integer);
begin
  inherited MouseWheel( deltax, deltay );
  if ( gcisMouseOver in InputState ) then
    begin
      ScrollBarH.Position:= ScrollBarH.Position + deltax * P3DGUIManager.ScrollSpeed;
      ScrollBarV.Position:= ScrollBarV.Position + deltay * P3DGUIManager.ScrollSpeed
    end;
end;

procedure TP3DCustomItemView.MouseMove(X, Y: Integer);
begin
  inherited MouseMove( X, Y);
  if ( not Assigned( DragDropSrc ) and ( gcisMouseBtn1Down in InputState ) and
       AllowDrag and (( P3DInput.Mouse.DX <> 0 ) or ( P3DInput.Mouse.DY <> 0 ))) then
    BeginDrag();
end;

{ TP3DTreeView }

procedure TP3DTreeView.SetSelection(AValue: TP3DTreeNode);
begin
  if FSelection=AValue then Exit;

  FSelection:=AValue;

  if ( Assigned( OnSelectionChange )) then
    OnSelectionChange( AValue );
end;

constructor TP3DTreeView.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  Children:= TP3DTreeNodeList.Create( nil );

  SymbolPlus:= TP3DTextBmp.CreateSimple( P3DSymbolPlus, P3DFontManagerBmp.Fonts[ 'Pascal3D-Symbols' ], Font.Size );
  SymbolMinus:= TP3DTextBmp.CreateSimple( P3DSymbolMinus, P3DFontManagerBmp.Fonts[ 'Pascal3D-Symbols' ], Font.Size );
end;

destructor TP3DTreeView.Destroy;
begin
  Children.Clear;
  Children.Free;
  SymbolPlus.Free;
  SymbolMinus.Free;
  inherited Destroy;
end;

procedure TP3DTreeView.Draw;
  function DrawNode( p: TVec2; Node: TP3DTreeNode; var MaxX: Float ): TVec2;
  var
    Child: TP3DTreeNode;
    LineHeight: Float;
  begin
    Result:= p;
    Node.FRenderPos:= p;
    LineHeight:= Font.Size * 1.2;
    p-= vec2( ScrollBarH.Position, ScrollBarV.Position );
    if ( not Assigned( Node.Text )) then
      Canvas.GetValidText( Node.Caption, Node.FText, P3DFontManager[ Font.Name, Font.Size ]);
    Canvas.Font.Color:= Accent900;
    if ( Node = DragItem ) then
      begin
        Canvas.Font.Color:= Accent50;
        Canvas.RenderRect( p, vec2( ClientRect.Right - ScrollBarWidth, p.y + LineHeight ),
                           Secondary900, Secondary400, Secondary600, Secondary900 );
      end
    else if ( Node = Selection ) then
      begin
        Canvas.Font.Color:= Accent50;
        Canvas.RenderRect( p, vec2( ClientRect.Right - ScrollBarWidth, p.y + LineHeight ),
                           Accent900, Accent400, Accent600, Accent900 );
 //Preset.OutlineColor, vec4( Preset.OutlineColor.xyz, 0 ), vec4( Preset.OutlineColor.xyz, 0 ), Preset.OutlineColor );
      end;
    if ( ShowSymbols ) then
      begin
        if ( not Assigned( Node.Symbol )) then
          Canvas.GetValidText( Node.SymbolText, Node.Symbol, P3DFontManager[ SymbolsFontName, Font.Size ]);
        Canvas.RenderText( Node.Symbol, p + vec2( Font.Size, 0 ));
      end;

    Canvas.RenderText( Node.Text, p + vec2( Font.Size, 0 ) * ( 1.5 + Ord( ShowSymbols )));

    MaxX:= Max( MaxX, Node.FRenderPos.x + Font.Size * ( 1 + Ord( ShowSymbols )) + Node.Text.Width );
    Result.y:= Result.y + LineHeight;
    if ( Node.Children.Count > 0 ) then
      if ( Node.Expanded ) then
        begin
          Canvas.RenderText( SymbolMinus, p );
          for Child in Node.Children do
            Result.y:= DrawNode( vec2( Result.x + Font.Size, Result.y ), Child, MaxX ).y;
        end
      else
        Canvas.RenderText( SymbolPlus, p );
  end;

var
  Child: TP3DTreeNode;
  p: TVec2;
  MaxX: Float;

begin
  inherited Draw;

  p:= vec2( ClientRect.Left + 5, ClientRect.Top + 5 );
  MaxX:= 0.0;
  for Child in Children do
    p:= DrawNode( p, Child, MaxX );

  if ( MaxX > 0 ) then
    ScrollBarH.ClientPageSize:= ( ClientRect.Width - ScrollBarWidth - 10 ) / ( MaxX )
  else
    ScrollBarH.ClientPageSize:= 1;
  ScrollBarH.Max:= MaxX * ( 1 - ScrollBarH.ClientPageSize );

  if ( p.Y > 0 ) then
    ScrollBarV.ClientPageSize:= ( ClientRect.Height - ScrollBarWidth - 10 ) / ( p.Y )
  else
    ScrollBarV.ClientPageSize:= 1;
  if ( ScrollBarV.ClientPageSize = 1 ) then
    Canvas.Scroll.Y:= 0;
  ScrollBarV.Max:= p.Y * ( 1 - ScrollBarV.ClientPageSize );
end;

procedure TP3DTreeView.MouseClick(mb1, mb2, mb3: Boolean; X, Y: Integer);
var
  Node: TP3DTreeNode;
begin
  inherited MouseClick(mb1, mb2, mb3, X, Y);
  if ( mb1 ) then
    begin
      Node:= NodeAtPos( vec2( X + ScrollBarH.Position, Y + ScrollBarV.Position ));
      if ( Assigned( Node )) then
        begin
          if ( P3DRect( Node.FRenderPos.x, Node.FRenderPos.y, Font.Size, Node.Text.Height ).PtInRect( vec2( X + ScrollBarH.Position, Y + ScrollBarV.Position ))) then
            Node.Expanded:= not Node.Expanded
          else
            Selection:= Node;
        end
      else
        Selection:= nil;
    end;
end;

procedure TP3DTreeView.MouseDblClick(mb1, mb2, mb3: Boolean; X, Y: Integer);
var
  Item: TP3DTreeNode;
begin
  inherited MouseDblClick(mb1, mb2, mb3, X, Y);
  if ( mb1 ) then
    begin
      Item:= NodeAtPos( vec2( X + Scroll.X, Y + Scroll.Y ));
      if ( Assigned( Item ) and ( Item.Children.Count > 0 )) then
        Item.Expanded:= not Item.Expanded;
    end;
end;

procedure TP3DTreeView.BeginDrag;
begin
  inherited BeginDrag();
  FDragItem:= NodeAtPos( vec2( P3DInput.Mouse.X, P3DInput.Mouse.Y ) - vec2( Canvas.Left, Canvas.Top ) + Canvas.Scroll );
end;

procedure TP3DTreeView.DragCancel(Sender: TP3DGraphicControl; X, Y: Integer);
begin
  FDragItem:= nil;
end;

procedure TP3DTreeView.KeyboardAction;
  procedure Select( n: Integer );
  var
    NewSelection: TP3DTreeNode = nil;
    Previous: TP3DTreeNode = nil;

    function Find( Ch: TP3DTreeNodeList ): Boolean;
    var
      i: Integer;
      Child: TP3DTreeNode;

      procedure SelectPrevious;
      begin

      end;

      procedure SelectNext;
      begin
        if (( Child.Expanded ) and ( Child.Children.Count > 0 )) then
          NewSelection:= Child.Children[ 0 ]
        else if ( Ch.Count > i + 1 ) then
          NewSelection:= Ch[ i + 1 ];
      end;

    begin
      Result:= False;
      Child:= nil;
      for i:= 0 to Ch.Count - 1 do
        begin
          if ( i > 0 ) then
            Previous:= Child;
          Child:= Ch[ i ];
          if ( Child = Selection ) then
            begin
              Result:= True;
              if ( n < 0 ) then
                Selection:= Previous
              else
                SelectNext;
              break;
            end
          else if ( Child.Expanded ) then
            begin
              Result:= Find( Child.Children );
              if ( Result ) then
                begin
                  if ( not Assigned( NewSelection )) then
                    begin
                      if ( n < 0 ) then
                        NewSelection:= Child
                      else if ( Ch.Count > i + 1 ) then
                        NewSelection:= Ch[ i + 1 ];
                    end;
                  break;
                end;
            end;
          if ( Result ) then
            break;
        end;
    end;

  begin
    if ( Selection = nil ) then
      Selection:= Children[ 0 ]
    else
      begin
        Find( Children );
        if ( Assigned( NewSelection )) then
          Selection:= NewSelection;
      end;
  end;

  procedure SearchBackward;
    function FindLast( N: TP3DTreeNode ): TP3DTreeNode;
    begin
      Result:= n;
      while ( Result.Expanded ) and ( Result.Children.Count > 0 ) do
        Result:= Result.Children[ N.Children.Count - 1 ];
    end;

  var
    ch: TP3DTreeNodeList;
    n: Integer;
  begin
    if ( Selection = nil ) then
      Selection:= Children[ 0 ]
    else
      begin
        if ( Assigned( Selection.Parent )) then
          ch:= Selection.Parent.Children
        else
          ch:= Children;

        n:= ch.IndexOf( Selection );
        if ( n = 0 ) then
          begin
            if ( Assigned( Selection.Parent )) then
              Selection:= Selection.Parent;
          end
        else if ( n > 0 ) then
          Selection:= FindLast( ch[ n - 1 ]);
      end;
  end;

  procedure SearchForward;
    function FindNext( N: TP3DTreeNode ): TP3DTreeNode;
    var
      ch: TP3DTreeNodeList;
      i: Integer;
    begin
      Result:= n.Parent;
      while ( Assigned( Result.Parent ) and ( Result.Parent.Children.IndexOf( Result ) = Result.Parent.Children.Count -1 )) do
        Result:= Result.Parent;
      if ( Assigned( Result.Parent )) then
        ch:= Result.Parent.Children
      else
        ch:= Children;

      i:= ch.IndexOf( Result );
      if ( i >= 0 ) then
        Result:= ch[ i + 1 ];
    end;

  var
    ch: TP3DTreeNodeList;
    n: Integer;
  begin
    if ( Selection = nil ) then
      Selection:= Children[ 0 ]
    else if ( Selection.Expanded ) and ( Selection.Children.Count > 0 ) then
      Selection:= Selection.Children[ 0 ]
    else
      begin
        if ( Assigned( Selection.Parent )) then
          ch:= Selection.Parent.Children
        else
          ch:= Children;

        n:= ch.IndexOf( Selection );
        if ( n = ch.Count - 1 ) then
          begin
            if ( Assigned( Selection.Parent )) then
              Selection:= FindNext( Selection );
          end
        else if ( n >= 0 ) then
          Selection:= ch[ n + 1 ];
      end;
  end;

begin
  inherited KeyboardAction;
  if (( not Focused ) or ( Children.Count = 0 )) then
    exit;
  if ( P3DInput.Keyboard.DKeys[ P3DK_UP ] = ksPressed ) then
    SearchBackward;
  if ( P3DInput.Keyboard.DKeys[ P3DK_DOWN ] = ksPressed ) then
    SearchForward;
  if ( Assigned( Selection )) then
    begin
      if ( P3DInput.Keyboard.DKeys[ P3DK_LEFT ] = ksPressed ) then
        Selection.Expanded:= False;
      if ( P3DInput.Keyboard.DKeys[ P3DK_RIGHT ] = ksPressed ) then
        Selection.Expanded:= True;
    end;
end;

function TP3DTreeView.NodeAtPos(p: TVec2): TP3DTreeNode;
var
  LineHeight: Float;

  function FindNodeRecurse( NodeList: TP3DTreeNodeList ): TP3DTreeNode;
  var
    Child: TP3DTreeNode;
  begin
    Result:= nil;
    for Child in NodeList do
      if ( P3DRect( Child.FRenderPos.x, Child.FRenderPos.y,
                    ClientRect.Width - Child.FRenderPos.x, LineHeight ).PtInRect( p )) then
        begin
          Result:= Child;
          break;
        end
      else if ( Child.Expanded ) then
        begin
          Result:= FindNodeRecurse( Child.Children );
          if ( Assigned( Result )) then
            break;
        end;
  end;
begin
  LineHeight:= Font.Size * 1.2;
  Result:= FindNodeRecurse( Children );
end;

{ TP3DTreeNode }

procedure TP3DTreeNode.SetParent(AValue: TP3DTreeNode);
begin
  if FParent=AValue then Exit;

  if ( Assigned( Parent )) then
    Parent.Children.Remove( Self );

  FParent:=AValue;

  if ( Assigned( Parent )) then
    Parent.Children.Add( Self );
end;

constructor TP3DTreeNode.Create(const AParent: TP3DTreeNode);
begin
  inherited Create;
  Children:= TP3DTreeNodeList.Create( Self );
  Parent:= AParent;
end;

destructor TP3DTreeNode.Destroy;
begin
  Children.Clear;
  Children.Free;
  inherited Destroy;
end;

{ TP3DTreeViewDirectory }

procedure TP3DTreeViewDirectory.SetDirectory(AValue: String);
begin
  if FDirectory=AValue then Exit;
  FDirectory:=AValue;
  Update;
end;

procedure TP3DTreeViewDirectory.SetListViewFile(AValue: TP3DListViewFile);
begin
  if FListViewFile=AValue then Exit;

  if ( Assigned( FListViewFile )) then
    FListViewFile.FTreeViewDirectory:= nil;

  FListViewFile:=AValue;

  if ( Assigned( FListViewFile )) then
    begin
      FListViewFile.FTreeViewDirectory:= Self;
      if ( Assigned( Selection )) then
        FListViewFile.Directory:= GetItemFileName( Selection )
      else
        FListViewFile.Directory:= Directory;
    end;
end;

procedure TP3DTreeViewDirectory.SetMask(AValue: String);
begin
  if FMask=AValue then Exit;
  FMask:=AValue;
  Update;
end;

procedure TP3DTreeViewDirectory.SetShowFiles(AValue: Boolean);
begin
  if FShowFiles=AValue then Exit;
  FShowFiles:=AValue;
  Update;
end;

procedure TP3DTreeViewDirectory.SetSelection(AValue: TP3DTreeNode);
var
  Dir: String;
begin
  inherited SetSelection(AValue);
  if ( Assigned( ListViewFile )) then
    begin
      Dir:= GetItemFileName( Selection, False );
      if ( FileExistsUTF8( Dir )) then
        ListViewFile.Directory:= Dir;
    end;
end;

constructor TP3DTreeViewDirectory.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FMask:= '*.*';
  Directory:= P3DSearchPaths.BaseDir.FileName;
end;


procedure TP3DTreeViewDirectory.Update;
  procedure ListFolderFiles( dir: String; BaseNode: TP3DTreeNode );
  var
    i: Integer;
    Node: TP3DTreeNode;
    Files: TStringList;
    Ext: String;
  begin
    Files:= P3DListFolderDirectories( dir, False, True );
    for i:= 0 to Files.Count - 1 do
      begin
        if ( Assigned( BaseNode )) then
          Node:= BaseNode.Children[ BaseNode.Children.Add( Files[ i ], nil )]
        else
          Node:= Children[ Children.Add( Files[ i ], nil )];
        Node.Parent:= BaseNode;
        Node.SymbolText:= P3DSymbolFolder;
        ListFolderFiles( AppendPathDelim( dir ) + Node.Caption + DirectorySeparator, Node );
      end;
    Files.Free;
    if ( ShowFiles ) then
      begin
        Files:= P3DListFolderFiles( AppendPathDelim( dir ) + Mask, False, True );
        //Result.Append( Files.Text );
        for i:= 0 to Files.Count - 1 do
          begin
            if ( Assigned( BaseNode )) then
              Node:= BaseNode.Children[ BaseNode.Children.Add( Files[ i ], nil )]
            else
              Node:= Children[ Children.Add( Files[ i ], nil )];
            Ext:= lowercase( ExtractFileExt( Files[ i ]));
            case Ext of
              '.p3d': Node.SymbolText:= P3DSymbolLibrary;
              '.p3dmesh': Node.SymbolText:= P3DSymbolMesh;
              '.bmp', '.jpg', '.jpeg', '.png': Node.SymbolText:= P3DSymbolTexture;
              '.frag', '.vert', '.glsl', '.pmd': Node.SymbolText:= P3DSymbolShader;
              '.ttf': Node.SymbolText:= P3DSymbolFont;
              else Node.SymbolText:= P3DSymbolFile;
            end;
            Node.Parent:= BaseNode;
          end;
        Files.Free;
      end;
  end;

begin
  Children.Clear;
  ListFolderFiles( Directory, nil );
end;

function TP3DTreeViewDirectory.GetItemFileName(Item: TP3DTreeNode;
  const Relative: Boolean): AnsiString;
var
  ThisItem: TP3DTreeNode;
begin
  ThisItem:= Item;
  Result:= '';
  while ( Assigned( ThisItem )) do
    begin
      Result:= ThisItem.Caption + DirectorySeparator + Result;
      ThisItem:= ThisItem.Parent;
    end;
  if ( not Relative ) then
    Result:= ExpandFileNameUTF8( Directory ) + Result;
  RemoveTrailingChars( Result, [ DirectorySeparator ]);
end;

function TP3DTreeViewDirectory.FindItemByFileName(FileName: String;
  Relative: Boolean): TP3DTreeNode;
var
  RelName: String;
  i: Integer;
  n: Integer;
begin
  if ( not Relative ) then
    RelName:= ExtractRelativepath( Directory, FileName )
  else
    RelName:= FileName;
  n:= Children.Find( ExtractWord( 1, RelName, [ DirectorySeparator ]));
  if ( n >= 0 ) then
    Result:= Children[ n ]
  else
    exit;
  for i:= 2 to WordCount( RelName, [ DirectorySeparator ]) do
    if (( not Assigned( Result )) or ( not Assigned( Result.Children ))) then
      break
    else
      begin
        n:= Result.Children.Find( ExtractWord( i, RelName, [ DirectorySeparator ]));
        if ( n >= 0 ) then
          Result:= Result.Children[ n ]
        else
          Result:= nil;
      end;
end;

{ TP3DTreeNodeList }

constructor TP3DTreeNodeList.Create(ATreeNode: TP3DTreeNode);
begin
  inherited Create;
  FTreeNode:= ATreeNode;
end;

function TP3DTreeNodeList.Add(Caption: String; Data: Pointer;
  const Expanded: Boolean; const Symbol: String): Integer;
var
  Node: TP3DTreeNode;
begin
  Node:= TP3DTreeNode.Create;
  Node.Caption:= Caption;
  Node.Data:= Data;
  Node.Expanded:= Expanded;
  Node.SymbolText:= Symbol;
  Node.FParent:= TreeNode;
  Result:= inherited Add( Node );
end;

procedure TP3DTreeNodeList.Delete(Index: Integer);
begin
  Items[ Index ].FParent:= nil;
  inherited Delete( Index );
end;

function TP3DTreeNodeList.Find(Caption: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( Items[ i ].Caption = Caption ) then
      begin
        Result:= i;
        break;
      end;
end;

function TP3DTreeNodeList.FindByData(Data: Pointer): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( Items[ i ].Data = Data ) then
      begin
        Result:= i;
        break;
      end;
end;

function TP3DTreeNodeList.FindByData( Data: Pointer; Recurse: Boolean ): TP3DTreeNode;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to Count - 1 do
    if ( Items[ i ].Data = Data ) then
      begin
        Result:= Items[ i ];
        break;
      end
    else if ( Recurse ) then
      begin
        Result:= Items[ i ].Children.FindByData( Data, Recurse );
        if ( Assigned( Result )) then
          break;
      end;
end;

procedure TP3DTreeNodeList.ExpandAll(const Recurse: Boolean);
var
  Node: TP3DTreeNode;
begin
  for Node in Self do
    begin
      Node.Expanded:= True;
      if ( Recurse ) then
        Node.Children.ExpandAll( True );
    end;
end;

{ TP3DListView }

procedure TP3DListView.SetSelection(AValue: TP3DListItem);
begin
  if ( Assigned( OnSelectionChange )) then
    OnSelectionChange( Self, AValue );

  if FSelection= AValue then Exit;
  FSelection:= AValue;
end;

constructor TP3DListView.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  ScrollBarH.Visible:= False;
  Children:= TP3DListItemList.Create;
  Font.Size:= 32;
end;

destructor TP3DListView.Destroy;
begin
  Children.Clear;
  Children.Free;
  inherited Destroy;
end;

function TP3DListView.ItemAtPos(p: TVec2): TP3DListItem;
  function FindItem( ItemList: TP3DListItemList ): TP3DListItem;
  var
    Child: TP3DListItem;
  begin
    Result:= nil;
    for Child in ItemList do
      begin
        if ( not Assigned( Child.Text )) then
          Canvas.GetValidText( Child.Caption, Child.FText, P3DFontManager[ Font.Name, Font.Size ]);
        if ( P3DRect( Child.FRenderPos.x, Child.FRenderPos.y,
                      ClientRect.Width - Child.FRenderPos.x, Child.Text.Height ).PtInRect( p )) then
          begin
            Result:= Child;
            break;
          end;
      end;
  end;
begin
  Result:= FindItem( Children );
end;

procedure TP3DListView.Draw;
  function DrawList( p: TVec2; Node: TP3DListItem; var MaxX: Float ): TVec2;
  begin
    Result:= p;
    Node.FRenderPos:= p;
    p-= vec2( ScrollBarH.Position, ScrollBarV.Position );
    if ( not Assigned( Node.Text )) then
      Canvas.GetValidText( Node.Caption, Node.FText, P3DFontManager[ Font.Name, Font.Size ]);
    Canvas.Font.Color:= Accent900;
    if ( Node = DragItem ) then
      begin
        Canvas.Font.Color:= Accent50;
        Canvas.RenderRect( p, vec2( ClientRect.Right - ScrollBarWidth, p.y + Node.Text.Height),
          Secondary900, Secondary400, Secondary600, Secondary900 );
          //Preset.OutlineColor, vec4( Preset.OutlineColor.xyz, 0 ), vec4( Preset.OutlineColor.xyz, 0 ), Preset.OutlineColor );
      end
    else if ( Node = Selection ) then
      begin
        Canvas.Font.Color:= Accent50;
        Canvas.RenderRect( p, vec2( ClientRect.Right - ScrollBarWidth, p.y + Node.Text.Height),
          Accent900, Accent400, Accent600, Accent900 );
          //Preset.OutlineColor, vec4( Preset.OutlineColor.xyz, 0 ), vec4( Preset.OutlineColor.xyz, 0 ), Preset.OutlineColor );
      end;
    if ( ShowSymbols ) then
      begin
        if ( not Assigned( Node.Symbol )) then
          Canvas.GetValidText( Node.SymbolText, Node.Symbol, P3DFontManager[ SymbolsFontName, Font.Size ]);
        Canvas.RenderText( Node.Symbol, p );
      end;
    Canvas.RenderText( Node.Text, p + vec2( Font.Size, 0 ) * Ord( ShowSymbols ));
    MaxX:= Max( MaxX, p.x + Font.Size * Ord( ShowSymbols ) + Node.Text.Width );
    Result.y:= Result.y + Node.Text.Height;
  end;

var
  Child: TP3DListItem;
  p, p2: TVec2;
  MaxX: Float;

begin
  inherited Draw;

  p:= vec2( ClientRect.Left + 5, ClientRect.Top + 5 );
  MaxX:= 0.0;
  for Child in Children do
    begin
      p2:= DrawListItem( p, Child );
      p.y:= p2.y;
      MaxX:= max( MaxX, p2.x );
    end;

  if ( MaxX > 0 ) then
    ScrollBarH.ClientPageSize:= ( ClientRect.Width - ScrollBarWidth - 10 ) / ( MaxX )
  else
    ScrollBarH.ClientPageSize:= 1;
  ScrollBarH.Max:= MaxX * ( 1 - ScrollBarH.ClientPageSize );

  if ( p.Y > 0 ) then
    ScrollBarV.ClientPageSize:= ( ClientRect.Height - 10 ) / ( p.Y )
  else
    ScrollBarV.ClientPageSize:= 1;
  if ( ScrollBarV.ClientPageSize = 1 ) then
    Canvas.Scroll.Y:= 0;
  ScrollBarV.Max:= p.Y * ( 1 - ScrollBarV.ClientPageSize );
end;

function TP3DListView.DrawListItem(p: TVec2; Node: TP3DListItem): TVec2;
begin
  Node.FRenderPos:= p;
  p-= vec2( ScrollBarH.Position, ScrollBarV.Position );

  if ( not Assigned( Node.Text )) then
    Canvas.GetValidText( Node.Caption, Node.FText, P3DFontManager[ Font.Name, Font.Size ]);
  Canvas.Font.Color:= Accent900;
  if ( Node = DragItem ) then
    begin
      Canvas.Font.Color:= Accent50;
      Canvas.RenderRect( p, vec2( ClientRect.Right - ScrollBarWidth, p.y + Node.Text.Height),
        Secondary900, Secondary400, Secondary600, Secondary900 );
    end
  else if ( Node = Selection ) then
    begin
      Canvas.Font.Color:= Accent50;
      Canvas.RenderRect( p, vec2( ClientRect.Right - ScrollBarWidth, p.y + Node.Text.Height),
        Accent900, Accent400, Accent600, Accent900 );
        //Preset.OutlineColor, vec4( Preset.OutlineColor.xyz, 0 ), vec4( Preset.OutlineColor.xyz, 0 ), Preset.OutlineColor );
    end;
  if ( ShowSymbols ) then
    begin
      if ( not Assigned( Node.Symbol )) then
        Canvas.GetValidText( Node.SymbolText, Node.Symbol, P3DFontManager[ SymbolsFontName, Font.Size ]);
      Canvas.RenderText( Node.Symbol, p );
    end;
  Canvas.RenderText( Node.Text, p + vec2( Font.Size, 0 ) * Ord( ShowSymbols ));
  Result.x:= Node.FRenderPos.x + Font.Size * Ord( ShowSymbols ) + Node.Text.Width;
  Result.y:= Node.FRenderPos.y + Node.Text.Height;
end;

procedure TP3DListView.MouseClick(mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  inherited MouseClick( mb1, mb2, mb3, X, Y );

  if ( mb1 ) then
    Selection:= ItemAtPos( vec2( X + ScrollBarH.Position, Y + ScrollBarV.Position ));
end;

procedure TP3DListView.BeginDrag;
begin
  inherited BeginDrag;
  FDragItem:= ItemAtPos( vec2( P3DInput.Mouse.X, P3DInput.Mouse.Y ) + vec2( ScrollBarH.Position - Canvas.Left, ScrollBarV.Position - Canvas.Top ) + Canvas.Scroll );
end;

procedure TP3DListView.DragCancel(Sender: TP3DGraphicControl; X, Y: Integer);
begin
  inherited DragCancel(Sender, X, Y);
  FDragItem:= nil;
end;

procedure TP3DListView.KeyboardAction;
  procedure Select( n: Integer );
  begin
    n:= Min( Children.Count - 1, Max( 0, n ));
    Selection:= Children[ n ];
  end;

begin
  inherited KeyboardAction;
  if (( not Focused ) or ( Children.Count = 0 )) then
    exit;
  if ( P3DInput.Keyboard.DKeys[ P3DK_UP ] = ksPressed ) then
    Select( Children.IndexOf( Selection ) - 1 );
  if ( P3DInput.Keyboard.DKeys[ P3DK_DOWN ] = ksPressed ) then
    Select( Children.IndexOf( Selection ) + 1 );
end;


{ TP3DListItem }

procedure TP3DListItem.SetCaption(AValue: String);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  if ( Assigned( FText )) then
    FreeAndNil( FText );
end;

procedure TP3DListItem.SetSymbolText(AValue: String);
begin
  if FSymbolText=AValue then Exit;
  FSymbolText:=AValue;
  if ( Assigned( Symbol )) then
    FreeAndNil( Symbol );
end;

constructor TP3DListItem.Create;
begin
  inherited;
  FText:= nil;
  Symbol:= nil;
  SymbolText:= '';
end;

destructor TP3DListItem.Destroy;
begin
  //WriteLn( '<[' + IntToHex( Integer( Pointer( Self )), 16 ) + '].Caption = '+ Caption + ':TP3DListItem>.Destroy' );
  if ( Assigned( FText )) then
    FreeAndNil( FText );
  if ( Assigned( Symbol )) then
    begin
      Symbol.Free;
      Symbol:= nil;
    end;
  inherited Destroy;
end;

{ TP3DListItemList }

function TP3DListItemList.Add(Caption: String; Data: Pointer): Integer;
var
  Node: TP3DListItem;
begin
  Node:= TP3DListItem.Create;
  Node.Caption:= Caption;
  Node.Data:= Data;
  Result:= inherited Add( Node )
end;

function TP3DListItemList.Add( Caption: String; Data: Pointer; SymbolText: String ): Integer;
var
  Node: TP3DListItem;
begin
  Node:= TP3DListItem.Create;
  Node.Caption:= Caption;
  Node.Data:= Data;
  Node.SymbolText:= SymbolText;
  Result:= inherited Add( Node )
end;

{ TP3DListViewFile }

procedure TP3DListViewFile.SetDirectory(AValue: String);
begin
  if FDirectory=AValue then Exit;

  if ( not DirectoryExistsUTF8( AValue )) then
    begin
    if ( not ( AllowOpenP3D and FileExistsUTF8( AValue )
         and ( lowercase( ExtractFileExt( AValue )) = '.p3d' ))) then
      exit;
    end;
    //AValue:= CleanAndExpandDirectory( AValue );
  FDirectory:= CleanAndExpandFilename( AValue );
  if ( Assigned( TreeViewDirectory )) then
    TreeViewDirectory.FSelection:= TreeViewDirectory.FindItemByFileName( Directory, False );
  Update;
end;

procedure TP3DListViewFile.SetMask(AValue: String);
begin
  if FMask=AValue then Exit;
  FMask:=AValue;
  Update;
end;

procedure TP3DListViewFile.SetShowFolderUp(AValue: Boolean);
begin
  if FShowFolderUp=AValue then Exit;
  FShowFolderUp:=AValue;
  Update;
end;

procedure TP3DListViewFile.SetTreeViewDirectory(AValue: TP3DTreeViewDirectory);
begin
  if FTreeViewDirectory=AValue then Exit;

  if ( Assigned( FTreeViewDirectory )) then
    FTreeViewDirectory.FListViewFile:= nil;

  FTreeViewDirectory:=AValue;

  if ( Assigned( FTreeViewDirectory )) then
    begin
      FTreeViewDirectory.FListViewFile:= Self;
      if ( Assigned( FTreeViewDirectory.Selection )) then
        Directory:= FTreeViewDirectory.GetItemFileName( FTreeViewDirectory.Selection )
      else
        Directory:= FTreeViewDirectory.Directory;
    end;
end;

constructor TP3DListViewFile.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FMask:= '*.*';
  Directory:= P3DSearchPaths.BaseDir.FileName;
end;

procedure TP3DListViewFile.Update;
  procedure ListP3DFile( FN: String );
  var
    i: Integer;
    name: String;
    json, scenes: TJSONData;
    F: TStringList;
  begin
    F:= TStringList.Create;
    try
      F.LoadFromFile( FN );
      json:= fpjson.GetJSON( F.Text );
      if ( ShowFolderUp and
        ( not ( Assigned( TreeViewDirectory )
        and ( TreeViewDirectory.Directory = Directory )))) then
        Children[ Children.Add( '<-' + ExtractFileName( FN ), nil )].SymbolText:= P3DSymbolLibrary;

      scenes:= json.GetPath( 'Scenes' );
      for i:= 0 to scenes.Count - 1 do
        begin
          name:= scenes.Items[ i ].GetPath( 'Name' ).AsString;
          if ( name > '' ) then
            Children.Add( name, nil, P3DSymbolScene );
        end;
    finally
      json.Free;
      F.Free;
    end;
  end;

  procedure ListFolderFiles( dir: String );
  var
    i: Integer;
    Item: TP3DListItem;
    Files: TStringList;
    Ext: String;
  begin
    if ( ShowFolderUp and
      ( not ( Assigned( TreeViewDirectory )
      and ( TreeViewDirectory.Directory = Directory )))) then
      begin
        Item:= Children[ Children.Add( '..', nil )];
        Item.SymbolText:= P3DSymbolFolder;
      end;

    Files:= P3DListFolderDirectories( AppendPathDelim( dir ) + '*', False, True );
    for i:= 0 to Files.Count - 1 do
      begin
        Item:= Children[ Children.Add( Files[ i ], nil )];
        Item.SymbolText:= P3DSymbolFolder;
      end;
    Files.Free;
    Files:= P3DListFolderFiles( AppendPathDelim( dir ) + Mask, False, True );
    for i:= 0 to Files.Count - 1 do
      begin
        Item:= Children[ Children.Add( Files[ i ], nil )];
        Ext:= lowercase( ExtractFileExt( Files[ i ]));
        case Ext of
          '.p3d': Item.SymbolText:= P3DSymbolLibrary;
          '.p3dmesh': Item.SymbolText:= P3DSymbolMesh;
          '.bmp', '.jpg', '.jpeg', '.png': Item.SymbolText:= P3DSymbolTexture;
          '.frag', '.vert', '.glsl', '.pmd': Item.SymbolText:= P3DSymbolShader;
          '.ttf': Item.SymbolText:= P3DSymbolFont;
          else Item.SymbolText:= P3DSymbolFile;
        end;
      end;
    Files.Free;
  end;

begin
  Children.Clear;
  Selection:= nil;
  if ( DirectoryExistsUTF8( Directory )) then // is path a directory
    ListFolderFiles( Directory )
  else if ( lowercase( ExtractFileExt( Directory )) = '.p3d' ) then
    ListP3DFile( Directory )
  else
    Children.Add( 'Error: invalid directory', nil );
end;

procedure TP3DListViewFile.MouseDblClick(mb1, mb2, mb3: Boolean; X, Y: Integer);
var
  Dir: String;
begin
  inherited MouseDblClick(mb1, mb2, mb3, X, Y);
  if ( Assigned( Selection )) then
    begin
      Dir:= AppendPathDelim( Directory ) + Selection.Caption;
      Directory:= Dir;
    end;
end;

{ TP3DTreeViewObjects }

function TP3DTreeViewObjects.GetObj: TP3DStreamable;
begin
  FObj.AsValue;
end;

procedure TP3DTreeViewObjects.SetObj(AValue: TP3DStreamable);
begin
  if ( FObj.AsValue = AValue ) then
    Exit;
  FObj.AsValue:= AValue;
  Update;
end;

constructor TP3DTreeViewObjects.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FObj:= TP3DStreamablePointer.Create( 'Obj', TP3DStreamable );
  Properties.Add( FObj );
end;

procedure TP3DTreeViewObjects.Update;

  {procedure AddObjectList( List: TP3DStreamableListBase; Parent: TP3DTreeNodeList; const Recurse: Boolean = False );
  var
    Item: TP3DObject;
    n: Integer;
  begin
    for Item in List do
      begin
        if ( P3DDataBlockCache.IsValid( Item )) then
          n:= Parent.Add( Item.Name, Item )
        else
          begin
            Parent.Add( 'Error: Orphaned datablock', nil );
            break;
          end;
        if ( Assigned( Item.Data )) then
          begin
            if ( Item.Data is TP3DMesh ) then
              Parent[ n ].SymbolText:= P3DSymbolMesh
//            else if ( Item.Data is TP3DFont ) then
//              Parent[ n ].SymbolText:= 'U'
            else if ( Item.Data is TP3DCamera ) then
              Parent[ n ].SymbolText:= P3DSymbolCamera
            else if ( Item.Data is TP3DMaterialBase ) then
              Parent[ n ].SymbolText:= P3DSymbolMaterial
            else if ( Item.Data is TP3DScene ) then
              Parent[ n ].SymbolText:= P3DSymbolScene
            else if ( Item.Data is TP3DLight ) then
              Parent[ n ].SymbolText:= P3DSymbolLight
            else if ( Item.Data is TP3DShader ) then
              Parent[ n ].SymbolText:= P3DSymbolShader
            else if ( Item.Data is TP3DTexture ) then
              Parent[ n ].SymbolText:= P3DSymbolTexture
            else if ( Item.Data is TP3DArmature ) then
              Parent[ n ].SymbolText:= P3DSymbolArmature
            else if ( Item.Data is TP3DTileGrid ) then
              Parent[ n ].SymbolText:= P3DSymbolGrid
//            else if ( Item.Data is TP3DJoint) then
//              Parent[ n ].SymbolText:= P3DSymbolArmature
            //else if ( Item.Data is TP3DData ) then
            //  Parent[ n ].SymbolText:= TP3DSymbolLibrary
            else;
          end
        else
          Parent[ n ].SymbolText:= 'R';
        if ( Recurse ) then
          AddObjectList( Item.Children, Parent[ n ].Children, Recurse );
      end;
  end;

  procedure AddObjects( List: TP3DObjectList; Parent: TP3DTreeNodeList; const Recurse: Boolean = False );
  var
    Item: TP3DObject;
    n: Integer;
  begin
    for Item in List do
      begin
        if ( P3DDataBlockCache.IsValid( Item )) then
          n:= Parent.Add( Item.Name, Item )
        else
          begin
            Parent.Add( 'Error: Orphaned datablock', nil );
            break;
          end;
        if ( Assigned( Item.Data )) then
          begin
            if ( Item.Data is TP3DMesh ) then
              Parent[ n ].SymbolText:= P3DSymbolMesh
//            else if ( Item.Data is TP3DFont ) then
//              Parent[ n ].SymbolText:= 'U'
            else if ( Item.Data is TP3DCamera ) then
              Parent[ n ].SymbolText:= P3DSymbolCamera
            else if ( Item.Data is TP3DMaterialBase ) then
              Parent[ n ].SymbolText:= P3DSymbolMaterial
            else if ( Item.Data is TP3DScene ) then
              Parent[ n ].SymbolText:= P3DSymbolScene
            else if ( Item.Data is TP3DLight ) then
              Parent[ n ].SymbolText:= P3DSymbolLight
            else if ( Item.Data is TP3DShader ) then
              Parent[ n ].SymbolText:= P3DSymbolShader
            else if ( Item.Data is TP3DTexture ) then
              Parent[ n ].SymbolText:= P3DSymbolTexture
            else if ( Item.Data is TP3DArmature ) then
              Parent[ n ].SymbolText:= P3DSymbolArmature
            else if ( Item.Data is TP3DTileGrid ) then
              Parent[ n ].SymbolText:= P3DSymbolGrid
//            else if ( Item.Data is TP3DJoint) then
//              Parent[ n ].SymbolText:= P3DSymbolArmature
            //else if ( Item.Data is TP3DData ) then
            //  Parent[ n ].SymbolText:= TP3DSymbolLibrary
            else;
          end
        else
          Parent[ n ].SymbolText:= 'R';
        if ( Recurse ) then
          AddObjectList( Item.Children, Parent[ n ].Children, Recurse );
      end;
  end;

  procedure AddSceneList( SceneList: TP3DSceneList; Parent: TP3DTreeNodeList );
  var
    Scene: TP3DScene;
    n: Integer;
  begin
    for Scene in SceneList do
      begin
        //WriteLn( Scene.Owner.Scenes = P3DDataBlockCache[ P3DDataBlockCache.FindBlock( Scene )].Datablock.GetContainer( Scene.Owner ));
        //WriteLn( Scene.Owner.Scenes.DumpObjectList );
        if ( P3DDataBlockCache.IsValid( Scene )) then
          n:= Parent.Add( Scene.Name, Scene )
        else
          Parent.Add( 'Error: Orphaned datablock', nil );
        Parent[ n ].SymbolText:= P3DSymbolScene;
        AddObjectList( Scene.Objects, Parent[ n ].Children, True );
      end;
  end;

  procedure AddCameraList( List: TP3DCameraList; Parent: TP3DTreeNodeList );
  var
    Cam: TP3DCamera;
    n: LongInt;
  begin
    for Cam in List do
      begin
        if ( P3DDataBlockCache.IsValid( Cam )) then
          n:= Parent.Add( Cam.Name, Cam )
        else
          Parent.Add( 'Error: Orphaned datablock', nil );
        Parent[ n ].SymbolText:= P3DSymbolCamera;
      end;
  end;

  procedure AddRestJointList( List: TP3DRestJointList; Parent: TP3DTreeNodeList; const Recurse: Boolean = False );
  var
    Joint: TP3DRestJoint;
    n: LongInt;
  begin
    for Joint in List do
      begin
        if ( Recurse and Assigned( Joint.Parent )) then
          continue;
        n:= Parent.Add( Joint.Name, Joint );
        Parent[ n ].SymbolText:= P3DSymbolArmature;
        if ( Recurse ) then
          AddRestJointList( Joint.Children, Parent[ n ].Children, Recurse );
      end;
  end;

  procedure AddDataList( Data: TP3DLibrary; Parent: TP3DTreeNodeList ); forward;

  procedure AddLibraryList( List: TP3DLibraryList; Parent: TP3DTreeNodeList );
  var
    Lib: TP3DLibrary;
    n: LongInt;
  begin
    for Lib in List do
      if ( ShowDesignerPackages or ( not Lib.IsDesignerPackage )) then
        begin
          n:= Parent.Add( ExtractFileName( Lib.JSONLoader.FileName ), Lib );
          Parent[ n ].SymbolText:= P3DSymbolLibrary;

          AddDataList( Lib, Parent[ n ].Children );
        end;
  end;


  procedure AddLightList( List: TP3DLightList; Parent: TP3DTreeNodeList );
  var
    Light: TP3DLight;
    n: LongInt;
  begin
    for Light in List do
      begin
        if ( P3DDataBlockCache.IsValid( Light )) then
          n:= Parent.Add( Light.Name, Light )
        else
          Parent.Add( 'Error: Orphaned datablock', nil );
        Parent[ n ].SymbolText:= P3DSymbolLight;
      end;
  end;

  procedure AddMaterialList( List: TP3DMaterialList; Parent: TP3DTreeNodeList );
  var
    Mat: TP3DMaterial;
    n: LongInt;
  begin
    for Mat in List do
      begin
        if ( P3DDataBlockCache.IsValid( Mat )) then
          n:= Parent.Add( Mat.Name, Mat )
        else
          Parent.Add( 'Error: Orphaned datablock', nil );
        Parent[ n ].SymbolText:= P3DSymbolMaterial;
      end;
  end;

  procedure AddMeshList( List: TP3DMeshList; Parent: TP3DTreeNodeList );
  var
    Mesh: TP3DMesh;
    n: LongInt;
  begin
    for Mesh in List do
      begin
        if ( P3DDataBlockCache.IsValid( Mesh )) then
          n:= Parent.Add( Mesh.Name, Mesh )
        else
          Parent.Add( 'Error: Orphaned datablock', nil );
        Parent[ n ].SymbolText:= P3DSymbolMesh;
      end;
  end;

  procedure AddShaderList( List: TP3DShaderList; Parent: TP3DTreeNodeList );
  var
    Shader: TP3DShader;
    n: LongInt;
  begin
    for Shader in List do
      begin
        if ( P3DDataBlockCache.IsValid( Shader )) then
          n:= Parent.Add( Shader.Name, Shader )
        else
          Parent.Add( 'Error: Orphaned datablock', nil );
        Parent[ n ].SymbolText:= P3DSymbolShader;
      end;
  end;

  procedure AddTextureList( List: TP3DTextureList; Parent: TP3DTreeNodeList );
  var
    Tex: TP3DTexture;
    n: LongInt;
  begin
    for Tex in List do
      begin
        if ( P3DDataBlockCache.IsValid( Tex )) then
          n:= Parent.Add( Tex.Name, Tex )
        else
          Parent.Add( 'Error: Orphaned datablock', nil );
        Parent[ n ].SymbolText:= P3DSymbolTexture;
      end;
  end;

  procedure AddArmatureList( List: TP3DArmatureList; Parent: TP3DTreeNodeList; const Recurse: Boolean = False );
  var
    Armature: TP3DArmature;
    n: LongInt;
  begin
    for Armature in List do
      begin
        if ( P3DDataBlockCache.IsValid( Armature )) then
          n:= Parent.Add( Armature.Name, Armature )
        else
          Parent.Add( 'Error: Orphaned datablock', nil );
        Parent[ n ].SymbolText:= P3DSymbolArmature;
        if ( Recurse ) then
          AddRestJointList( Armature.Joints, Parent[ n ].Children, Recurse );
      end;
  end;

  procedure AddActionList( List: TP3DActionList; Parent: TP3DTreeNodeList );
  var
    Action: TP3DAction;
    n: LongInt;
  begin
    for Action in List do
      begin
        if ( P3DDataBlockCache.IsValid( Action )) then
          n:= Parent.Add( Action.Name, Action )
        else
          Parent.Add( 'Error: Orphaned datablock', nil );
        Parent[ n ].SymbolText:= P3DSymbolArmature;
      end;
  end;


  procedure AddFontList( List: TP3DFontBmpList; Parent: TP3DTreeNodeList );
  var
    Font: TP3DFontBmp;
    n: LongInt;
  begin
    for Font in List do
      begin
        if ( P3DDataBlockCache.IsValid( Font )) then
          n:= Parent.Add( Font.Name, Font )
        else
          Parent.Add( 'Error: Orphaned datablock', nil );
        Parent[ n ].SymbolText:= P3DSymbolFont;
      end;
  end;

  procedure AddGridList( List: TP3DTileGridList; Parent: TP3DTreeNodeList );
  var
    Grid: TP3DTileGrid;
    n: LongInt;
  begin
    for Grid in List do
      begin
        if ( P3DDataBlockCache.IsValid( Grid )) then
          n:= Parent.Add( Grid.Name, Grid )
        else
          Parent.Add( 'Error: Orphaned datablock', nil );
        Parent[ n ].SymbolText:= P3DSymbolGrid;
      end;
  end;

  procedure AddDataList( Data: TP3DLibrary; Parent: TP3DTreeNodeList );
  var
    Node: TP3DTreeNodeList;
  begin
    Node:= Parent[ Parent.Add( 'Cameras', Data.Cameras, False, P3DSymbolCamera )].Children;
    AddCameraList( Data.Cameras, Node );

    if ( Data = P3DData ) then
      begin
        Node:= Parent[ Parent.Add( 'Libraries', P3DData.Libraries, False, P3DSymbolLibrary )].Children;
        AddLibraryList( P3DData.Libraries, Node );
      end;

    //Node:= Parent[ Parent.Add( 'Joints', Data.Joints, False, P3DSymbolArmature )].Children;
    //AddJointList( Data.Joints, Node );

    Node:= Parent[ Parent.Add( 'Lights', Data.Lights, False, P3DSymbolLight )].Children;
    AddLightList( Data.Lights, Node );

    Node:= Parent[ Parent.Add( 'Materials', Data.Materials, False, P3DSymbolMaterial )].Children;
    AddMaterialList( Data.Materials, Node );

    Node:= Parent[ Parent.Add( 'Meshes', Data.Meshes, False, P3DSymbolMesh )].Children;
    AddMeshList( Data.Meshes, Node );

    Node:= Parent[ Parent.Add( 'Objects', Data.Objects, False, P3DSymbolObject )].Children;
    AddObjects( Data.Objects, Node );

    Node:= Parent[ Parent.Add( 'Scenes', Data.Scenes, False, P3DSymbolScene )].Children;
    AddSceneList( Data.Scenes, Node );

    Node:= Parent[ Parent.Add( 'Shaders', Data.Shaders, False, P3DSymbolShader )].Children;
    AddShaderList( Data.Shaders, Node );

    Node:= Parent[ Parent.Add( 'Textures', Data.Textures, False, P3DSymbolTexture )].Children;
    AddTextureList( Data.Textures, Node );

    Node:= Parent[ Parent.Add( 'Armatures', Data.Armatures, False, P3DSymbolArmature )].Children;
    AddArmatureList( Data.Armatures, Node, True );

    Node:= Parent[ Parent.Add( 'Actions', Data.Actions, False, P3DSymbolArmature )].Children;
    AddActionList( Data.Actions, Node );

    Node:= Parent[ Parent.Add( 'Fonts', Data.Fonts, False, P3DSymbolFont )].Children;
    AddFontList( Data.Fonts, Node );

    Node:= Parent[ Parent.Add( 'Grids', Data.Grids, False, P3DSymbolGrid )].Children;
    AddGridList( Data.Grids, Node );
  end;}

  function GetSymbolClass( Item: TP3DStreamableClass ): Char;
  begin
    if ( Item.InheritsFrom( TP3DObject )) then
      Result:= P3DSymbolObject
    else if ( Item.InheritsFrom( TP3DMesh )) then
      Result:= P3DSymbolMesh
    else if ( Item.InheritsFrom( TP3DCamera )) then
      Result:= P3DSymbolCamera
    else if ( Item.InheritsFrom( TP3DMaterial )) then
      Result:= P3DSymbolMaterial
    else if ( Item.InheritsFrom( TP3DScene )) then
      Result:= P3DSymbolScene
    else if ( Item.InheritsFrom( TP3DLight )) then
      Result:= P3DSymbolLight
    else if ( Item.InheritsFrom( TP3DShader )) then
      Result:= P3DSymbolShader
    else if ( Item.InheritsFrom( TP3DTexture )) then
      Result:= P3DSymbolTexture
    else if ( Item.InheritsFrom( TP3DArmature )) then
      Result:= P3DSymbolArmature
    else if ( Item.InheritsFrom( TP3DAction )) then
      Result:= P3DSymbolArmature
    else if ( Item.InheritsFrom( TP3DTileGrid )) then
      Result:= P3DSymbolGrid
    else if ( Item.InheritsFrom( TP3DJoint)) then
      Result:= P3DSymbolArmature
    else if ( Item.InheritsFrom( TP3DLibrary )) then
      Result:= P3DSymbolLibrary
    else if ( Item.InheritsFrom( TP3DFontBmp )) then
      Result:= P3DSymbolFont
    else
      Result:= P3DSymbolFile;
  end;

  function GetSymbol( Item: TP3DStreamable ): Char;
  begin
    if ( Item is TP3DObject ) then
      Result:= GetSymbol( TP3DObject( Item ).Data )
    else if ( Item is TP3DMesh ) then
      Result:= P3DSymbolMesh
    else if ( Item is TP3DCamera ) then
      Result:= P3DSymbolCamera
    else if ( Item is TP3DMaterial ) then
      Result:= P3DSymbolMaterial
    else if ( Item is TP3DScene ) then
      Result:= P3DSymbolScene
    else if ( Item is TP3DLight ) then
      Result:= P3DSymbolLight
    else if ( Item is TP3DShader ) then
      Result:= P3DSymbolShader
    else if ( Item is TP3DTexture ) then
      Result:= P3DSymbolTexture
    else if ( Item is TP3DArmature ) then
      Result:= P3DSymbolArmature
    else if ( Item is TP3DAction ) then
      Result:= P3DSymbolArmature
    else if ( Item is TP3DTileGrid ) then
      Result:= P3DSymbolGrid
    else if ( Item is TP3DJoint) then
      Result:= P3DSymbolArmature
    else if ( Item is TP3DLibrary ) then
      Result:= P3DSymbolLibrary
    else if ( Item is TP3DFontBmp ) then
      Result:= P3DSymbolFont
    else
      Result:= P3DSymbolFile;
  end;

  procedure AddStreamableList( List: TP3DStreamableListBase; Parent: TP3DTreeNodeList ); forward;

  procedure AddStreamableProps( List: TP3DPropertyList; Parent: TP3DTreeNodeList);
  var
    Item: TP3DPropertyAccess;
  begin
    for Item in List do
      if ( Item is TP3DStreamableListBase ) then
        AddStreamableList( TP3DStreamableListBase( Item ), Parent );
  end;

  procedure AddStreamable( Item: TP3DStreamable; Parent: TP3DTreeNodeList);
  begin
    if ( Item is TP3DNamedStreamable ) then
      AddStreamableProps( Item.Properties, Parent[ Parent.Add( TP3DNamedStreamable( Item ).Name, Item, False, GetSymbol( Item ))].Children );
  end;

  procedure AddStreamableList( List: TP3DStreamableListBase; Parent: TP3DTreeNodeList );
  var
    Item: TP3DStreamable;
    Node: TP3DTreeNodeList;
  begin
    Node:= Parent[ Parent.Add( List.Name, List, False, GetSymbolClass( List.ItemClass ))].Children;
    for Item in List do
      AddStreamable( Item, Node );
  end;

var
  SelectionData: Pointer;

begin
  if ( Assigned( Selection )) then
    begin
      SelectionData:= Selection.Data;
      FSelection:= nil;
    end
  else
    SelectionData:= nil;

  Children.Clear;
  if ( Assigned( Obj )) then
    AddStreamable( Obj, Children )
  else
    AddStreamable( P3DData, Children );

  if ( Assigned( SelectionData )) then
    Selection:= Children.FindByData( SelectionData, True );
end;

{ TP3DTreeViewProperty }

procedure TP3DTreeViewProperties.SetObj(AValue: TP3DStreamable);
begin
  if FObj=AValue then Exit;
  FObj:=AValue;
  Update;
end;

constructor TP3DTreeViewProperties.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FMaxDepth:= 5;
end;

procedure TP3DTreeViewProperties.SetMaxDepth(AValue: Integer);
begin
  if FMaxDepth=AValue then Exit;
  FMaxDepth:=AValue;
  Update;
end;

procedure TP3DTreeViewProperties.Update;
  procedure AddProps( Props: TP3DPropertyList; NodeLst: TP3DTreeNodeList; Level: Integer ); forward;
  procedure AddProp( Prop: TP3DPropertyAccess; NodeLst: TP3DTreeNodeList; Level: Integer );
  var
    n, i: Integer;
  begin
    if (( MaxDepth < Level ) or ( not Assigned( Prop ))) then
      exit;
    n:= NodeLst.Add( Prop.Name + ': ' + Prop.ClassName, nil );
    if (( Prop is IP3DPropAccessStreamable ) and ( Assigned(( Prop as IP3DPropAccessStreamable ).GetValueAsStreamable ))) then
      AddProps(( Prop as IP3DPropAccessStreamable ).GetValueAsStreamable.Properties, NodeLst[ n ].Children, Level + 1 )
    else if ( Prop is TP3DPropertyAccessArray ) then
      for i:= 0 to TP3DPropertyAccessArray( Prop ).Count - 1 do
        AddProp( TP3DPropertyAccessArray( Prop ).GetArrayItemAsProp( i ), NodeLst[ n ].Children, Level + 1 )
    else
      WriteLn( Prop.ClassName );
  end;

  procedure AddProps( Props: TP3DPropertyList; NodeLst: TP3DTreeNodeList; Level: Integer );
  var
    Prop: TP3DPropertyAccess;
  begin
    //TODO: Fix this: if (( MaxDepth < Level ) or ( not Assigned( Prop ))) then
      exit;
    if ( not Assigned( Props )) then
      WriteLn( 'Error' )
    else if ( not Assigned( NodeLst )) then
      WriteLn( 'Error 2' )
    else
      for Prop in Props do
        AddProp( Prop, NodeLst, Level + 1 );
  end;

begin
  Children.Clear;
  if ( Assigned( Obj )) then
    AddProps( Obj.Properties, Children, 1 )
  else
    Children.Add( '<None:TP3DStreamable>', nil );
end;

{$ENDIF}

