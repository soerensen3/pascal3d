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
      FText: TP3DText;
      FRenderPos: TVec2;
      Symbol: TP3DText;

      procedure SetCaption(AValue: String);
      procedure SetSymbolText(AValue: String);

    public
      constructor Create;

      property Data: Pointer read FData write FData;

    published
      property Caption: String read FCaption write SetCaption;
      property Text: TP3DText read FText write FText;
      property SymbolText: String read FSymbolText write SetSymbolText;
  end;

  TP3DCustomListItemList = specialize gP3DCustomObjectList < TP3DListItem >;

  { TP3DTreeNodeList }

  { TP3DListItemList }

  TP3DListItemList = class ( TP3DCustomListItemList )
    function Add( Caption: String; Data: Pointer ): Integer; overload;
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
    function Add( Caption: String; Data: Pointer; const Expanded: Boolean = False ): Integer; overload;
    function Find( Caption: String ): Integer;
  end;

  { TP3DCustomItemView }

  TP3DCustomItemView = class ( TP3DGraphicControl )
    private
      FFont: TP3DCanvasFont;
      FPreset: TP3DButtonPreset;
      FScrollBarV: TP3DScrollBar;
      FScrollBarH: TP3DScrollBar;
      FScrollBarWidth: Float;
      FShowSymbols: Boolean;
      FSymbolsFontName: String;

      procedure SetScrollBarWidth(AValue: Float);

    public
      constructor Create( const AOwner: TP3DObjectList=nil; const AParent: TP3DGraphicControl=nil );
      destructor Destroy; override;

      class function IsFocusControl: Boolean; override;

      procedure Draw; override;
      procedure OnScroll( Sender: TObject );
      procedure MouseWheel(delta: Integer); override;

    published
      property Preset: TP3DButtonPreset read FPreset write FPreset;
      property Font: TP3DCanvasFont read FFont write FFont;
      property ScrollBarV: TP3DScrollBar read FScrollBarV write FScrollBarV;
      property ScrollBarH: TP3DScrollBar read FScrollBarH write FScrollBarH;
      property ShowSymbols: Boolean read FShowSymbols write FShowSymbols;
      property SymbolsFontName: String read FSymbolsFontName write FSymbolsFontName;
      property ScrollBarWidth: Float read FScrollBarWidth write SetScrollBarWidth;
  end;

  { TP3DTreeView }

  TP3DTreeView = class( TP3DCustomItemView )
    private
      FChildren: TP3DTreeNodeList;
      FSelection: TP3DTreeNode;

      SymbolMinus: TP3DText;
      SymbolPlus: TP3DText;

      procedure SetSelection( AValue: TP3DTreeNode ); virtual;

    public
      constructor Create( const AOwner: TP3DObjectList=nil; const AParent: TP3DGraphicControl=nil );
      destructor Destroy; override;

      procedure Draw; override;
      procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer ); override;
      procedure MouseDblClick(mb1, mb2, mb3: Boolean; X, Y: Integer); override;

      function NodeAtPos(p: TVec2): TP3DTreeNode;

    published
      property Children: TP3DTreeNodeList read FChildren write FChildren;
      property Selection: TP3DTreeNode read FSelection write SetSelection;
  end;

  { TP3DListView }

  TP3DListView = class ( TP3DCustomItemView )
    private
      FChildren: TP3DListItemList;
      FSelection: TP3DListItem;

    public
      constructor Create(const AOwner: TP3DObjectList=nil; const AParent: TP3DGraphicControl=nil);
      destructor Destroy; override;

      function ItemAtPos(p: TVec2): TP3DListItem;

      procedure Draw; override;
      procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer ); override;

    published
      property Children: TP3DListItemList read FChildren write FChildren;
      property Selection: TP3DListItem read FSelection write FSelection;
  end;

  TP3DListViewFile = class;

  { TP3DTreeViewDirectory }

  TP3DTreeViewDirectory = class ( TP3DTreeView )
    private
      FDirectory: String;
      FListViewFile: TP3DListViewFile;
      FShowFiles: Boolean;

      procedure SetDirectory(AValue: String);
      procedure SetListViewFile(AValue: TP3DListViewFile);
      procedure SetShowFiles(AValue: Boolean);
      procedure SetSelection(AValue: TP3DTreeNode); override;

    public
      procedure Update;
      function GetItemFileName( Item: TP3DTreeNode; const Relative: Boolean = False ): String;
      function FindItemByFileName( FileName: String; Relative: Boolean ): TP3DTreeNode;

    published
      property Directory: String read FDirectory write SetDirectory;
      property ShowFiles: Boolean read FShowFiles write SetShowFiles;
      property ListViewFile: TP3DListViewFile read FListViewFile write SetListViewFile;
  end;

  { TP3DListViewFile }

  TP3DListViewFile = class ( TP3DListView )
    private
      FDirectory: String;
      FShowFolderUp: Boolean;
      FTreeViewDirectory: TP3DTreeViewDirectory;

      procedure SetDirectory(AValue: String);
      procedure SetShowFolderUp(AValue: Boolean);
      procedure SetTreeViewDirectory(AValue: TP3DTreeViewDirectory);

    public
      procedure Update;
      procedure MouseDblClick(mb1, mb2, mb3: Boolean; X, Y: Integer); override;

    published
      property Directory: String read FDirectory write SetDirectory;
      property TreeViewDirectory: TP3DTreeViewDirectory read FTreeViewDirectory write SetTreeViewDirectory;
      property ShowFolderUp: Boolean read FShowFolderUp write SetShowFolderUp;
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

constructor TP3DCustomItemView.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
begin
  inherited;
  SymbolsFontName:= 'Pascal3D-Symbols';
  Font:= TP3DCanvasFont.Create;
  Font.Size:= 16;
  Preset:= TP3DButtonPreset.Create;
  Preset.Color:= vec4( vec3( 0.93 ), 1 );
  Preset.OutlineColor:= vec4( 165/255, 165/255, 237/255, 1 );
  Preset.FontColor:= vec4( vec3( 0 ), 1 );
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
  ScrollBarV.Free;
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

{ TP3DTreeView }

procedure TP3DTreeView.SetSelection(AValue: TP3DTreeNode);
begin
  if FSelection=AValue then Exit;
  FSelection:=AValue;
end;

constructor TP3DTreeView.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
begin
  inherited;
  Children:= TP3DTreeNodeList.Create;

  SymbolPlus:= p3dTextSimple( 'H', P3DFontManager[ 'Pascal3D-Symbols', Font.Size ]);
  SymbolMinus:= p3dTextSimple( 'F', P3DFontManager[ 'Pascal3D-Symbols', Font.Size ]);
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
      Node.Text:= p3dTextSimple( Node.Caption, P3DFontManager[ Font.Name, Font.Size ]);
    if ( Node = Selection ) then
      begin
        Canvas.RenderRect( p, vec2( ClientRect.Right - ScrollBarWidth, p.y + Node.Text.Height),
          Preset.OutlineColor, vec4( Preset.OutlineColor.xyz, 0 ), vec4( Preset.OutlineColor.xyz, 0 ), Preset.OutlineColor );
      end;
    if ( ShowSymbols ) then
      begin
        if ( not Assigned( Node.Symbol )) then
          Node.Symbol:= p3dTextSimple( Node.SymbolText, P3DFontManager[ SymbolsFontName, Font.Size ]);
        Canvas.RenderText( Node.Symbol, p + vec2( Font.Size, 0 ));
      end;
    Canvas.RenderText( Node.Text, p + vec2( Font.Size, 0 ) * ( 1 + Ord( ShowSymbols )));
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
  if ( Assigned( Text )) then
    FreeAndNil( FText );
  Symbol.Free;
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
      if ( DirectoryExistsUTF8( Dir )) then
        ListViewFile.Directory:= Dir;
    end;
end;


procedure TP3DTreeViewDirectory.Update;
  procedure ListFolderFiles( dir: String; BaseNode: TP3DTreeNode );
  var
    i: Integer;
    Node: TP3DTreeNode;
    Files: TStringList;
  begin
    Files:= P3DListFolderDirectories( dir, False, True );
    for i:= 0 to Files.Count - 1 do
      begin
        if ( Assigned( BaseNode )) then
          Node:= BaseNode.Children[ BaseNode.Children.Add( Files[ i ], nil )]
        else
          Node:= Children[ Children.Add( Files[ i ], nil )];
        Node.Parent:= BaseNode;
        Node.SymbolText:= 'P';
        ListFolderFiles( AppendPathDelim( dir ) + Node.Caption + DirectorySeparator, Node );
      end;
    Files.Free;
    if ( ShowFiles ) then
      begin
        Files:= P3DListFolderFiles( AppendPathDelim( dir ) + '*', False, True );
        //Result.Append( Files.Text );
        for i:= 0 to Files.Count - 1 do
          begin
            if ( Assigned( BaseNode )) then
              Node:= BaseNode.Children[ BaseNode.Children.Add( Files[ i ], nil )]
            else
              Node:= Children[ Children.Add( Files[ i ], nil )];
            Node.SymbolText:= 'Q';
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
  const Relative: Boolean): String;
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
    Result:= AppendPathDelim( Directory ) + Result;
  ChompPathDelim( Result );
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

function TP3DTreeNodeList.Add( Caption: String; Data: Pointer;
  const Expanded: Boolean ): Integer;
var
  Node: TP3DTreeNode;
begin
  Node:= TP3DTreeNode.Create;
  Node.Caption:= Caption;
  Node.Data:= Data;
  Node.Expanded:= Expanded;
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

{ TP3DListView }

constructor TP3DListView.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
begin
  inherited;
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
          Child.Text:= p3dTextSimple( Child.Caption, P3DFontManager[ Font.Name, Font.Size ]);
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
      Node.Text:= p3dTextSimple( Node.Caption, P3DFontManager[ Font.Name, Font.Size ]);
    if ( Node = Selection ) then
      begin
        Canvas.RenderRect( p, vec2( ClientRect.Right - ScrollBarWidth, p.y + Node.Text.Height),
          Preset.OutlineColor, vec4( Preset.OutlineColor.xyz, 0 ), vec4( Preset.OutlineColor.xyz, 0 ), Preset.OutlineColor );
      end;
    if ( ShowSymbols ) then
      begin
        if ( not Assigned( Node.Symbol )) then
          Node.Symbol:= p3dTextSimple( Node.SymbolText, P3DFontManager[ SymbolsFontName, Font.Size ]);
        Canvas.RenderText( Node.Symbol, p );
      end;
    Canvas.RenderText( Node.Text, p + vec2( Font.Size, 0 ) * Ord( ShowSymbols ));
    MaxX:= Max( MaxX, p.x + Font.Size * Ord( ShowSymbols ) + Node.Text.Width );
    Result.y:= Result.y + Node.Text.Height;
  end;

var
  Child: TP3DListItem;
  p: TVec2;
  MaxX: Float;

begin
  inherited Draw;

  p:= vec2( ClientRect.Left + 5, ClientRect.Top + 5 );
  MaxX:= 0.0;
  for Child in Children do
    p:= DrawList( p, Child, MaxX );

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
  SymbolText:= '';
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

{ TP3DListViewFile }

procedure TP3DListViewFile.SetDirectory(AValue: String);
begin
  if FDirectory=AValue then Exit;
  FDirectory:=AValue;
  if ( Assigned( TreeViewDirectory )) then
    TreeViewDirectory.FSelection:= TreeViewDirectory.FindItemByFileName( Directory, False );
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

procedure TP3DListViewFile.Update;
  procedure ListFolderFiles( dir: String );
  var
    i: Integer;
    Item: TP3DListItem;
    Files: TStringList;
  begin
    if ( ShowFolderUp and
      ( not ( Assigned( TreeViewDirectory )
      and ( TreeViewDirectory.Directory = Directory )))) then
      begin
        Item:= Children[ Children.Add( '..', nil )];
        Item.SymbolText:= 'P';
      end;

    Files:= P3DListFolderDirectories( AppendPathDelim( dir ) + '*', False, True );
    for i:= 0 to Files.Count - 1 do
      begin
        Item:= Children[ Children.Add( Files[ i ], nil )];
        Item.SymbolText:= 'P';
      end;
    Files.Free;
    Files:= P3DListFolderFiles( AppendPathDelim( dir ) + '*', False, True );
    for i:= 0 to Files.Count - 1 do
      begin
        Item:= Children[ Children.Add( Files[ i ], nil )];
        Item.SymbolText:= 'Q';
      end;
    Files.Free;
  end;

begin
  Children.Clear( True );
  Selection:= nil;
  ListFolderFiles( Directory );
end;

procedure TP3DListViewFile.MouseDblClick(mb1, mb2, mb3: Boolean; X, Y: Integer);
var
  Dir: String;
begin
  inherited MouseDblClick(mb1, mb2, mb3, X, Y);
  if ( Assigned( Selection )) then
    begin
      Dir:= AppendPathDelim( Directory ) + Selection.Caption;
      if ( DirectoryExistsUTF8( Dir )) then
        Directory:= CleanAndExpandDirectory( Dir );
    end;
end;


{$ENDIF}

