{ p3d.ide_toolbar.inc }

{$IFDEF INTERFACE}
type

  { TP3DIDETabControl }

  TP3DIDETabControl = class ( TP3DGraphicControl )
    protected
      FTabControl: TP3DStreamablePointer;
      FToolbar: TP3DStreamablePointer;
      FActiveAsset: TP3DStreamablePointer;

      function GetTabControl: TP3DTabControl;
      function GetToolbar: TP3DToolbar;
      procedure SetActiveEditor( Sender: TObject );
      procedure EditorsChange( Sender: TObject );
      procedure TabCtrlBtnClick( AButtonIdx: Integer; AButtonCaption: String );
      procedure ToolbarBtnClick( AButtonIdx: Integer; AButtonCaption: String );
      function GetActiveAsset: TP3DPropertyEditorStreamable;
      procedure SetActiveAsset(AValue: TP3DPropertyEditorStreamable);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      property TabControl: TP3DTabControl read GetTabControl;
      property Toolbar: TP3DToolbar read GetToolbar;
      property ActiveAsset: TP3DPropertyEditorStreamable read GetActiveAsset write SetActiveAsset;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DIDETabControl }

function TP3DIDETabControl.GetToolbar: TP3DToolbar;
begin
  Result:= TP3DToolbar( FToolbar.AsValue );
end;

procedure TP3DIDETabControl.SetActiveEditor(Sender: TObject);
var
  ed: TP3DEditor;
begin
  ed:= P3DMainIntf.Editors[ P3DMainIntf.ActiveEditor ];

  if ( not Assigned( ed )) then
    Toolbar.Buttons.Text:= ''
  else begin
    Toolbar.Buttons.Text:= ed.Buttons;
    ActiveAsset.Prop:= ed.Prop;
  end;
end;

procedure TP3DIDETabControl.EditorsChange(Sender: TObject);
var
  S: String;
  ed: TP3DEditor;
  Btn: TP3DGroupButton;
begin
  S:= '';
  for ed in P3DMainIntf.Editors do
    S+= ed.Caption + LineEnding;
  S:= Copy( S, 1, Length( S ) - Length( LineEnding  ));
  TabControl.Buttons.Text:= S;
  Btn:= TP3DGroupButton( TabControl.Controls[ P3DMainIntf.ActiveEditor ]);
  if ( Assigned( Btn )) then
    Btn.Down:= True;
end;

procedure TP3DIDETabControl.TabCtrlBtnClick(AButtonIdx: Integer; AButtonCaption: String);
begin
  P3DMainIntf.SetActiveEditor( Self, AButtonIdx );
  SetActiveEditor( nil );
end;

procedure TP3DIDETabControl.ToolbarBtnClick(AButtonIdx: Integer; AButtonCaption: String);
var
  ed: TP3DEditor;
begin
  ed:= P3DMainIntf.Editors[ P3DMainIntf.ActiveEditor ];
  if ( Assigned( ed )) then
    ed.ToolbarBtnClick( AButtonIdx, AButtonCaption );
end;

function TP3DIDETabControl.GetActiveAsset: TP3DPropertyEditorStreamable;
begin
  Result:= TP3DPropertyEditorStreamable( FActiveAsset.AsValue );
end;

procedure TP3DIDETabControl.SetActiveAsset(AValue: TP3DPropertyEditorStreamable);
begin
  FActiveAsset.AsValue:= AValue;
end;

function TP3DIDETabControl.GetTabControl: TP3DTabControl;
begin
  Result:= TP3DTabControl( FTabControl.AsValue );
end;

constructor TP3DIDETabControl.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  Align:= alTop;
  FTabControl:= TP3DStreamablePointer.Create( 'TabControl', TP3DTabControl );
  Properties.Add( FTabControl );
  FTabControl.AsValue:= TP3DTabControl.Create( Controls );
  TabControl.OnButtonClick:= @TabCtrlBtnClick;
  {TabControl.Buttons.Text:= P3DSymbolFile + LineEnding +
                            P3DSymbolScene + LineEnding +
                            P3DSymbolMesh + LineEnding +
                            P3DSymbolMaterial + LineEnding +
                            P3DSymbolTexture + LineEnding;}
  TabControl.Height:= 32;
  TabControl.Font.Size:= 24;
  TabControl.Align:= alTop;
  TabControl.Font.Name:= P3DSymbolFontName;

  FActiveAsset:= TP3DStreamablePointer.Create( 'ActiveAsset', TP3DPropertyEditorStreamable );
  ActiveAsset:= TP3DPropertyEditorStreamable.Create( Controls );
  ActiveAsset.Align:= alTop;

  FToolbar:= TP3DStreamablePointer.Create( 'Toolbar', TP3DToolbar );
  Properties.Add( FToolbar );
  FToolbar.AsValue:= TP3DToolbar.Create( Controls );
  Toolbar.OnButtonClick:= @ToolbarBtnClick;
  {Toolbar.Buttons.Text:= P3DSymbolFileNew + LineEnding +
                         P3DSymbolFileOpen + LineEnding +
                         P3DSymbolFileSave + LineEnding +
                         P3DSymbolFileClose;}
  Toolbar.Height:= 40;
  Toolbar.Font.Size:= 32;
  Toolbar.Align:= alTop;
  Toolbar.Font.Name:= P3DSymbolFontName;
  Toolbar.Color:= White;

  P3DMainIntf.AddEditorChangeHandler( @EditorsChange );
  EditorsChange( nil );
  SetActiveEditor( nil );

  PageModeY:= pmExpand;
end;

destructor TP3DIDETabControl.Destroy;
begin
  P3DMainIntf.RemoveEditorChangeHandler( @EditorsChange );
  ActiveAsset.Prop:= nil;
  inherited Destroy;
end;


{$ENDIF}

