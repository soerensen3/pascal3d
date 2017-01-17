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

    public
      constructor Create( const AParent: TP3DTreeNode = nil );
      destructor Destroy; override;

    published
      property Children: TP3DTreeNodeList read FChildren write FChildren;
      property Expanded: Boolean read FExpanded write FExpanded;
      property Parent: TP3DTreeNode read FParent write FParent;
  end;

  TP3DCustomTreeNodeList = specialize gP3DCustomObjectList < TP3DTreeNode >;

  { TP3DTreeNodeList }

  TP3DTreeNodeList = class ( TP3DCustomTreeNodeList )
    function Add( Caption: String; Data: Pointer; const Expanded: Boolean = False; const Symbol: String = '' ): Integer; overload;
    function Find( Caption: String ): Integer;
    function FindByData( Data: Pointer ): Integer;
    function FindByData( Data: Pointer; Recurse: Boolean ): TP3DTreeNode;
    procedure ExpandAll( const Recurse: Boolean = True );
  end;

  { TP3DCustomItemView }

  TP3DCustomItemView = class ( TP3DGraphicControl )
    private
      FAllowDrag: Boolean;
      FFont: TP3DCanvasFont;
      FPreset: TP3DButtonPreset;
      FScrollBarV: TP3DScrollBar;
      FScrollBarH: TP3DScrollBar;
      FScrollBarWidth: Float;
      FShowSymbols: Boolean;
      FSymbolsFontName: String;

      procedure SetScrollBarWidth(AValue: Float);

    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;
      destructor Destroy; override;

      class function IsFocusControl: Boolean; override;

      procedure Draw; override;
      procedure OnScroll( Sender: TObject );
      procedure MouseWheel(delta: Integer); override;
      procedure MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer); override;

    published
      property Preset: TP3DButtonPreset read FPreset write FPreset;
      property Font: TP3DCanvasFont read FFont write FFont;
      property ScrollBarV: TP3DScrollBar read FScrollBarV write FScrollBarV;
      property ScrollBarH: TP3DScrollBar read FScrollBarH write FScrollBarH;
      property ShowSymbols: Boolean read FShowSymbols write FShowSymbols;
      property SymbolsFontName: String read FSymbolsFontName write FSymbolsFontName;
      property ScrollBarWidth: Float read FScrollBarWidth write SetScrollBarWidth;
      property AllowDrag: Boolean read FAllowDrag write FAllowDrag;
  end;

  { TP3DTreeView }

  TP3DListViewStyle = ( vsList, vsIcon );
  TP3DTreeViewSelectionChangeEvent = procedure ( var NewSelection: TP3DTreeNode ) of object;
  TP3DTreeView = class( TP3DCustomItemView )
    private
      FChildren: TP3DTreeNodeList;
      FOnSelectionChange: TP3DTreeViewSelectionChangeEvent;
      FSelection: TP3DTreeNode;

      SymbolMinus: TP3DTextBmp;
      SymbolPlus: TP3DTextBmp;

      procedure SetSelection( AValue: TP3DTreeNode ); virtual;

    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;
      destructor Destroy; override;

      procedure Draw; override;
      procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer ); override;
      procedure MouseDblClick(mb1, mb2, mb3: Boolean; X, Y: Integer); override;

      function NodeAtPos(p: TVec2): TP3DTreeNode;

    published
      property Children: TP3DTreeNodeList read FChildren write FChildren;
      property Selection: TP3DTreeNode read FSelection write SetSelection;
      property OnSelectionChange: TP3DTreeViewSelectionChangeEvent read FOnSelectionChange write fOnSelectionChange;
  end;

  { TP3DListView }

  TP3DListView = class;
  TP3DListViewSelectionChangeEvent = procedure ( Sender: TP3DListView; var NewSelection: TP3DListItem ) of object;
  TP3DListView = class ( TP3DCustomItemView )
    private
      FChildren: TP3DListItemList;
      FOnSelectionChange: TP3DListViewSelectionChangeEvent;
      FSelection: TP3DListItem;
      FViewStyle: TP3DListViewStyle;
      procedure SetSelection(AValue: TP3DListItem);

    public
      constructor Create( const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil ); override;
      destructor Destroy; override;

      function ItemAtPos( p: TVec2 ): TP3DListItem;

      procedure Draw; override;
      function DrawListItem( p: TVec2; Node: TP3DListItem ): TVec2; virtual;
      procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer ); override;

    published
      property Children: TP3DListItemList read FChildren write FChildren;
      property Selection: TP3DListItem read FSelection write SetSelection;
      property OnSelectionChange: TP3DListViewSelectionChangeEvent read FOnSelectionChange write fOnSelectionChange;
      property ViewStyle: TP3DListViewStyle read FViewStyle write FViewStyle;
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
      constructor Create(const AData: TP3DLibrary= nil; const AParent: TP3DGraphicControl= nil); override;

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
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

      procedure Update;
      procedure MouseDblClick(mb1, mb2, mb3: Boolean; X, Y: Integer); override;

    published
      property Directory: String read FDirectory write SetDirectory;
      property TreeViewDirectory: TP3DTreeViewDirectory read FTreeViewDirectory write SetTreeViewDirectory;
      property ShowFolderUp: Boolean read FShowFolderUp write SetShowFolderUp;
      property Mask: String read FMask write SetMask;
      property AllowOpenP3D: Boolean read FAllowOpenP3D write FAllowOpenP3D;
  end;

  { TP3DTreeViewActors }

  TP3DTreeViewActors = class ( TP3DTreeView )
    private
      FObj: TPersistent;
      FShowDesignerPackages: Boolean;

      procedure SetObj(AValue: TPersistent);

    public
      procedure Update;

    published
      property Obj: TPersistent read FObj write SetObj;
      property ShowDesignerPackages: Boolean read FShowDesignerPackages write FShowDesignerPackages;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

procedure TP3DCustomItemView.SetScrollBarWidth(AValue: Float);
begin
  if FScrollBarWidth=AValue then Exit;
  FScrollBarWidth:=AValue;
  ScrollBarH.Height:= ScrollBarWidth;
  ScrollBarV.Width:= ScrollBarWidth;
end;

constructor TP3DCustomItemView.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  SymbolsFontName:= 'Pascal3D-Symbols';
  Font:= TP3DCanvasFont.Create;
  Font.Size:= 20;
  Preset:= TP3DButtonPreset.Create;
  Preset.Color:= Primary50;//vec4( vec3( 0.93 ), 1 );
  //Preset.OutlineColor:= Accent900;//vec4( 225/255, 230/255, 234/255, 1 );
  Preset.FontColor:= Accent50;//vec4( vec3( 0 ), 1 );
  ScrollBarV:= TP3DScrollBar.Create( nil, Self );
  ScrollBarV.Kind:= p3dsbVertical;
  ScrollBarV.Align:= alRight;
  ScrollBarV.OnChange:= @OnScroll;
  ScrollBarH:= TP3DScrollBar.Create( nil, Self );
  ScrollBarH.Kind:= p3dsbHorizontal;
  ScrollBarH.Align:= alBottom;
  ScrollBarH.OnChange:= @OnScroll;
  ScrollBarWidth:= 10;
end;

destructor TP3DCustomItemView.Destroy;
begin
  Font.Free;
  Preset.Free;
  if ( P3DData.IsValid( ScrollBarV )) then
    ScrollBarV.Free;
  if ( P3DData.IsValid( ScrollBarH )) then
    ScrollBarH.Free;
  inherited Destroy;
end;

class function TP3DCustomItemView.IsFocusControl: Boolean;
begin
  Result:= True;
end;

procedure TP3DCustomItemView.Draw;
begin
  inherited Draw;
  Canvas.RenderRect( Scroll, vec2( Width, Height ) - 1 + Scroll, Preset.Color, Preset.Color, Preset.Color, Preset.Color );
  Canvas.RenderLineRect( Scroll, vec2( Width, Height ) - 1 + Scroll, Preset.OutlineColor, Preset.OutlineColor, Preset.OutlineColor, Preset.OutlineColor );

  if ( Focused ) then
    Canvas.RenderLineRect( vec2( 4 ) + Scroll, vec2( Width, Height ) - 4 + Scroll, vec4( 0.5, 0.5, 0.5, 0.5 ));
end;

procedure TP3DCustomItemView.OnScroll(Sender: TObject);
begin
  if ( Sender = ScrollBarV ) then
    Canvas.Scroll.Y:= Round( ScrollBarV.Position )
  else if ( Sender = ScrollBarH ) then
    Canvas.Scroll.X:= Round( ScrollBarH.Position );
end;

procedure TP3DCustomItemView.MouseWheel(delta: Integer);
begin
  inherited MouseWheel( delta );
  if ( gcisMouseOver in InputState ) then
    ScrollBarV.Position:= ScrollBarV.Position + delta * P3DGUIManager.ScrollSpeed;
end;

procedure TP3DCustomItemView.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  inherited MouseDown(mb1, mb2, mb3, X, Y);
  if ( mb1 and AllowDrag ) then
    BeginDrag();
end;

{ TP3DTreeView }

procedure TP3DTreeView.SetSelection(AValue: TP3DTreeNode);
begin
  if FSelection=AValue then Exit;

  if ( Assigned( OnSelectionChange )) then
    OnSelectionChange( AValue );
  FSelection:=AValue;
end;

constructor TP3DTreeView.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  Children:= TP3DTreeNodeList.Create;

  SymbolPlus:= TP3DTextBmp.CreateSimple( P3DSymbolPlus, P3DFontManagerBmp.Fonts[ 'Pascal3D-Symbols' ], Font.Size );
  SymbolMinus:= TP3DTextBmp.CreateSimple( P3DSymbolMinus, P3DFontManagerBmp.Fonts[ 'Pascal3D-Symbols' ], Font.Size );
end;

destructor TP3DTreeView.Destroy;
begin
  Children.Clear( True );
  Children.Free;
  SymbolPlus.Free;
  SymbolMinus.Free;
  inherited Destroy;
end;

procedure TP3DTreeView.Draw;
  function DrawNode( p: TVec2; Node: TP3DTreeNode; var MaxX: Float ): TVec2;
  var
    Child: TP3DTreeNode;
  begin
    Result:= p;
    Node.FRenderPos:= p;
    if ( not Assigned( Node.Text )) then
      Canvas.GetValidText( Node.Caption, Node.FText, P3DFontManager[ Font.Name, Font.Size ]);
    Canvas.Font.Color:= Accent900;
    if ( Node = Selection ) then
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
        Canvas.RenderText( Node.Symbol, p + vec2( Font.Size, 0 ));
      end;

    Canvas.RenderText( Node.Text, p + vec2( Font.Size, 0 ) * ( 1.5 + Ord( ShowSymbols )));
    MaxX:= Max( MaxX, p.x + Font.Size * ( 1 + Ord( ShowSymbols )) + Node.Text.Width );
    Result.y:= Result.y + Node.Text.Height;
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
    ScrollBarH.PageSize:= ( ClientRect.Width - ScrollBarWidth - 10 ) / ( MaxX )
  else
    ScrollBarH.PageSize:= 1;
  ScrollBarH.Max:= MaxX * ( 1 - ScrollBarH.PageSize );

  if ( p.Y > 0 ) then
    ScrollBarV.PageSize:= ( ClientRect.Height - ScrollBarWidth - 10 ) / ( p.Y )
  else
    ScrollBarV.PageSize:= 1;
  if ( ScrollBarV.PageSize = 1 ) then
    Canvas.Scroll.Y:= 0;
  ScrollBarV.Max:= p.Y * ( 1 - ScrollBarV.PageSize );
end;

procedure TP3DTreeView.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer);
var
  Node: TP3DTreeNode;
begin
  inherited MouseDown(mb1, mb2, mb3, X, Y);
  if ( P3DInput.Mouse.Buttons[ 0 ] and P3DInput.Mouse.DButtons[ 0 ]) then
    begin
      Node:= NodeAtPos( vec2( X + Scroll.X, Y + Scroll.Y ));
      if ( Assigned( Node )) then
        begin
          if ( P3DRect( Node.FRenderPos.x, Node.FRenderPos.y, Font.Size, Node.Text.Height ).PtInRect( vec2( X + Scroll.X, Y + Scroll.Y ))) then
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

function TP3DTreeView.NodeAtPos(p: TVec2): TP3DTreeNode;
  function FindNodeRecurse( NodeList: TP3DTreeNodeList ): TP3DTreeNode;
  var
    Child: TP3DTreeNode;
  begin
    Result:= nil;
    for Child in NodeList do
      if ( P3DRect( Child.FRenderPos.x, Child.FRenderPos.y,
                    ClientRect.Width - Child.FRenderPos.x, Child.Text.Height ).PtInRect( p )) then
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
  Result:= FindNodeRecurse( Children );
end;

{ TP3DTreeNode }


constructor TP3DTreeNode.Create(const AParent: TP3DTreeNode);
begin
  inherited Create;
  Children:= TP3DTreeNodeList.Create;
  Parent:= AParent;
end;

destructor TP3DTreeNode.Destroy;
begin
  Children.Clear( True );
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

constructor TP3DTreeViewDirectory.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  FMask:= '*.*';
  Directory:= P3DSearchPaths.BaseDir;
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
  Children.Clear( True );
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
  Result:= inherited Add( Node );
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

constructor TP3DListView.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  ScrollBarH.Visible:= False;
  Children:= TP3DListItemList.Create;
  Font.Size:= 32;
end;

destructor TP3DListView.Destroy;
begin
  Children.Clear( True );
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
    if ( not Assigned( Node.Text )) then
      Canvas.GetValidText( Node.Caption, Node.FText, P3DFontManager[ Font.Name, Font.Size ]);
    Canvas.Font.Color:= Accent900;
    if ( Node = Selection ) then
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
    ScrollBarH.PageSize:= ( ClientRect.Width - ScrollBarWidth - 10 ) / ( MaxX )
  else
    ScrollBarH.PageSize:= 1;
  ScrollBarH.Max:= MaxX * ( 1 - ScrollBarH.PageSize );

  if ( p.Y > 0 ) then
    ScrollBarV.PageSize:= ( ClientRect.Height - 10 ) / ( p.Y )
  else
    ScrollBarV.PageSize:= 1;
  if ( ScrollBarV.PageSize = 1 ) then
    Canvas.Scroll.Y:= 0;
  ScrollBarV.Max:= p.Y * ( 1 - ScrollBarV.PageSize );
end;

function TP3DListView.DrawListItem(p: TVec2; Node: TP3DListItem): TVec2;
begin
  Node.FRenderPos:= p;
  if ( not Assigned( Node.Text )) then
    Canvas.GetValidText( Node.Caption, Node.FText, P3DFontManager[ Font.Name, Font.Size ]);
  Canvas.Font.Color:= Accent900;
  if ( Node = Selection ) then
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
  Result.x:= p.x + Font.Size * Ord( ShowSymbols ) + Node.Text.Width;
  Result.y:= p.y + Node.Text.Height;
end;

procedure TP3DListView.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  inherited MouseDown(mb1, mb2, mb3, X, Y);

  if ( P3DInput.Mouse.Buttons[ 0 ] and P3DInput.Mouse.DButtons[ 0 ]) then
    Selection:= ItemAtPos( vec2( X + Scroll.X, Y + Scroll.Y ));
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

constructor TP3DListViewFile.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  FMask:= '*.*';
  Directory:= P3DSearchPaths.BaseDir;
end;

procedure TP3DListViewFile.Update;
  procedure ListP3DFile( FN: String );
  var
    XML: TXMLDocument;
    list: TDOMNodeList;
    i: Integer;
    name: String;
  begin
    ReadXMLFile( XML, FN );
    try
      if ( ShowFolderUp and
        ( not ( Assigned( TreeViewDirectory )
        and ( TreeViewDirectory.Directory = Directory )))) then
        Children[ Children.Add( '<-' + ExtractFileName( FN ), nil )].SymbolText:= P3DSymbolLibrary;

      list:= XML.GetElementsByTagName( 'scene' );
      for i:= 0 to list.Count - 1 do
        begin
          name:= TDOMElement( list[ i ]).AttribStrings[ 'name' ];
          if ( name > '' ) then
            Children.Add( name, nil, P3DSymbolScene );
        end;
    finally
      XML.Free;
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
  Children.Clear( True );
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

{ TP3DTreeViewActors }

procedure TP3DTreeViewActors.SetObj(AValue: TPersistent);
begin
  if FObj=AValue then Exit;
  FObj:=AValue;
  Update;
end;

procedure TP3DTreeViewActors.Update;

  procedure AddList( List: TP3DActorList; Parent: TP3DTreeNodeList; const Recurse: Boolean = False );
  var
    Item: TP3DActor;
    n: Integer;
  begin
    for Item in List do
      begin
        if ( P3DData.IsValid( Item )) then
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
            //else if ( Item.Data is TP3DData ) then
            //  Parent[ n ].SymbolText:= TP3DSymbolLibrary
            else;
          end
        else
          Parent[ n ].SymbolText:= 'R';
        if ( Recurse ) then
          AddList( Item.Children, Parent[ n ].Children );
      end;
  end;

  procedure AddSceneList( SceneList: TP3DSceneList; Parent: TP3DTreeNodeList );
  var
    Scene: TP3DScene;
    n: Integer;
  begin
    for Scene in SceneList do
      begin
        if ( P3DData.IsValid( Scene )) then
          n:= Parent.Add( Scene.Name, Scene )
        else
          Parent.Add( 'Error: Orphaned datablock', nil );
        Parent[ n ].SymbolText:= P3DSymbolScene;
        AddList( Scene.Objects, Parent[ n ].Children, True );
      end;
  end;

  procedure AddCameraList( List: TP3DCameraList; Parent: TP3DTreeNodeList );
  var
    Cam: TP3DCamera;
    n: LongInt;
  begin
    for Cam in List do
      begin
        if ( P3DData.IsValid( Cam )) then
          n:= Parent.Add( Cam.Name, Cam )
        else
          Parent.Add( 'Error: Orphaned datablock', nil );
        Parent[ n ].SymbolText:= P3DSymbolCamera;
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
          n:= Parent.Add( ExtractFileName( Lib.FileWatch.FileName ), Lib );
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
        if ( P3DData.IsValid( Light )) then
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
        if ( P3DData.IsValid( Mat )) then
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
        if ( P3DData.IsValid( Mesh )) then
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
        if ( P3DData.IsValid( Shader )) then
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
        if ( P3DData.IsValid( Tex )) then
          n:= Parent.Add( Tex.Name, Tex )
        else
          Parent.Add( 'Error: Orphaned datablock', nil );
        Parent[ n ].SymbolText:= P3DSymbolTexture;
      end;
  end;

  procedure AddArmatureList( List: TP3DArmatureList; Parent: TP3DTreeNodeList );
  var
    Armature: TP3DArmature;
    n: LongInt;
  begin
    for Armature in List do
      begin
        if ( P3DData.IsValid( Armature )) then
          n:= Parent.Add( Armature.Name, Armature )
        else
          Parent.Add( 'Error: Orphaned datablock', nil );
        Parent[ n ].SymbolText:= P3DSymbolArmature;
      end;
  end;

  procedure AddActionList( List: TP3DArmatureActionList; Parent: TP3DTreeNodeList );
  var
    Action: TP3DArmatureAction;
    n: LongInt;
  begin
    for Action in List do
      begin
        if ( P3DData.IsValid( Action )) then
          n:= Parent.Add( Action.Name, Action )
        else
          Parent.Add( 'Error: Orphaned datablock', nil );
        Parent[ n ].SymbolText:= P3DSymbolArmature;
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

    Node:= Parent[ Parent.Add( 'Lights', Data.Lights, False, P3DSymbolLight )].Children;
    AddLightList( Data.Lights, Node );

    Node:= Parent[ Parent.Add( 'Materials', Data.Materials, False, P3DSymbolMaterial )].Children;
    AddMaterialList( Data.Materials, Node );

    Node:= Parent[ Parent.Add( 'Meshes', Data.Meshes, False, P3DSymbolMesh )].Children;
    AddMeshList( Data.Meshes, Node );

    Node:= Parent[ Parent.Add( 'Objects', Data.Objects, False, P3DSymbolActor )].Children;
    AddList( Data.Objects, Node );

    Node:= Parent[ Parent.Add( 'Scenes', Data.Scenes, False, P3DSymbolScene )].Children;
    AddSceneList( Data.Scenes, Node );

    Node:= Parent[ Parent.Add( 'Shaders', Data.Shaders, False, P3DSymbolShader )].Children;
    AddShaderList( Data.Shaders, Node );

    Node:= Parent[ Parent.Add( 'Textures', Data.Textures, False, P3DSymbolTexture )].Children;
    AddTextureList( Data.Textures, Node );

    Node:= Parent[ Parent.Add( 'Armatures', Data.Armatures, False, P3DSymbolArmature )].Children;
    AddArmatureList( Data.Armatures, Node );

    Node:= Parent[ Parent.Add( 'Actions', Data.Actions, False, P3DSymbolArmature )].Children;
    AddActionList( Data.Actions, Node );
  end;

var
  SelectionData: Pointer;

begin
  if ( Assigned( Selection )) then
    begin
      SelectionData:= Selection.Data;
      Selection:= nil;
    end
  else
    SelectionData:= nil;
  Children.Clear( True );
  if ( Assigned( Obj )) then
    begin
      if ( Obj is TP3DLibrary ) then
        AddDataList( TP3DLibrary( Obj ), Children )
      else if ( Obj is TP3DScene ) then
        AddList( TP3DScene( Obj ).Objects, Children )
      else if ( Obj is TP3DActor ) then
        AddList( TP3DActor( Obj ).Children, Children );
    end
  else
    AddDataList( P3DData, Children );

  if ( Assigned( SelectionData )) then
    Selection:= Children.FindByData( SelectionData, True );
end;

{$ENDIF}

