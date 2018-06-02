{ include.inc }

{$IFDEF INTERFACE}

TP3DNodeEditorIDEIntf = class ( TObject )
  private
    FLastUpdate: Integer;
    FNeedsUpdate: Boolean;
    FViewMode: TP3DDataViewerMode;
    FUpdating: Boolean;

    PropEd: TP3DPropertyEditorString;
    Edit: TP3DEdit;

    procedure SetActiveDataBlock(AValue: TP3DDataBlock);
    procedure SetActiveLibrary(AValue: TP3DLibrary);
    procedure SetActiveScene(AValue: TP3DScene);
    procedure SetViewMode(AValue: TP3DDataViewerMode);
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure OnEditKeyDown( Sender: TObject );

  public
    constructor CreateBlock(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

    function NewLibrary( const AFileName: String = 'Untitled%d.p3d' ): TP3DLibrary;
    function NewScene( const AName: String = 'NewScene%d' ): TP3DScene;
    procedure Paint(); override;
    procedure CheckLastChange;
    procedure Update;
    function FindFirstLib: TP3DLibrary;
    function FindFirstScene: TP3DScene;

  published
    property ActiveScene: TP3DScene read FActiveScene write SetActiveScene;
    property ActiveLibrary: TP3DLibrary read FActiveLibrary write SetActiveLibrary;
    property ActiveDataBlock: TP3DDataBlock read FActiveDataBlock write SetActiveDataBlock;
    property ViewMode: TP3DDataViewerMode read FViewMode write SetViewMode;
    property LastUpdate: Integer read FLastUpdate;
    property NeedsUpdate: Boolean read FNeedsUpdate;
end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{$ENDIF}

