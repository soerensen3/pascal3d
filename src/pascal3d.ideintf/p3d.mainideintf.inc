{ p3dmainideintf.inc }

{$IFDEF INTERFACE}

type
  TP3DMainIntfHandlerType = (
    mihtSelectionChange,
    mihtActiveLibraryChange,
    mihtActiveSceneChange,
    mihtActiveMeshChange,
    mihtActiveMaterialChange,
    mihtActiveEditorChange,
    mihtActiveTextureChange,
    mihtActiveEffectChange,
    mihtEditorsChange,
    mihtFileOpenDlg,
    mihtFileSaveDlg,
    mihtFileActiveClose,
    mihtMaterialNew,
    mihtMaterialDelete,
    mihtMaterialMapNew,
    mihtMeshAssignMaterial,
    mihtTextureNew,
    mihtTextureDelete,
    mihtEffectNew,
    mihtEffectDelete,
    mihtLibraryChange,
    mihtSceneChange,
    mihtObjectChange,
    mihtMeshChange,
    mihtTextureChange,
    mihtMaterialChange
  );

  { TP3DPopupDictionary }

  TP3DPopupDictionary = class( specialize TFPGMap < String, TP3DPopupMenu > )
    function FindClassPopup( AClass: TClass; var APopup: TP3DPopupMenu ): Boolean;
  end;

  { TP3DEditor }

  TP3DEditor = class ( TP3DStreamable )
    protected
      FProp: TP3DStreamablePointer;
      FButtons: String;
      FCaption: String;
      FName: String;

      function GetProperty: TP3DStreamable; virtual; abstract;
      procedure SetProperty( AValue: TP3DStreamable ); virtual; abstract;

    public
      procedure ToolbarBtnClick( AButtonIdx: Integer; AButtonCaption: String ); virtual;

      property Name: String read FName;
      property Caption: String read FCaption;
      property Buttons: String read FButtons;
      property Prop: TP3DStreamablePointer read FProp;
  end;

  { gP3DEditor }

  generic gP3DEditor < T: TP3DStreamable > = class ( TP3DEditor )
    protected
      FPopup: TP3DStreamablePointer;
      FPopupList: TP3DStreamablePointer;

      function GetPopup: TP3DPopupMenu;
      function GetPopupList: TP3DPopupMenu;
      procedure PopupClick( Sender: TP3DMenuItem ); virtual;
      procedure PopupListClick( Sender: TP3DMenuItem ); virtual;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      property Popup: TP3DPopupMenu read GetPopup;
      property PopupList: TP3DPopupMenu read GetPopupList;
  end;

  TP3DEditorContainerList = specialize gP3DStreamableContainerList < TP3DEditor >;

  { TP3DMainIntf }

  TP3DMainIntf = class ( TP3DJSONRoot )
    protected
      FPopups: TP3DPopupDictionary;
      FHandlers: array[ TP3DMainIntfHandlerType ] of TMethodList;
      FSelection: TP3DStreamablePointer;
      FActiveDataBlock: TP3DStreamablePointer;
      FActiveLibrary: TP3DStreamablePointer;
      FActiveScene: TP3DStreamablePointer;
      FActiveMesh: TP3DStreamablePointer;
      FActiveMaterial: TP3DStreamablePointer;
      FActiveTexture: TP3DStreamablePointer;
      FActiveEffect: TP3DStreamablePointer;

      FActiveEditor: Integer;
      FEditors: TP3DEditorContainerList;

      procedure AddHandler( HandlerType: TP3DMainIntfHandlerType;
                            const AMethod: TMethod; AsLast: Boolean = False );
      procedure RemoveHandler( HandlerType: TP3DMainIntfHandlerType;
                               const AMethod: TMethod );
      procedure DoCallNotifyHandler( HandlerType: TP3DMainIntfHandlerType;
                                     ASender: TObject);
      function GetActiveTexture: TP3DTexture;
      function GetSelection: TP3DStreamable;
      function GetActiveLibrary: TP3DLibrary;
      function GetActiveMesh: TP3DMesh;
      function GetActiveMaterial: TP3DMaterial;
      function GetActiveScene: TP3DScene;
      function GetActiveEffect: TP3DEffect;

      procedure EditorChange( ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction );

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure AddSetSelectionHandler( const AHandler: TNotifyEvent; AsLast: Boolean = False );
      procedure RemoveSetSelectionHandler( const AHandler: TNotifyEvent );
      procedure SetSelection( ASender: TObject; AValue: TP3DStreamable);

      procedure AddSetActiveSceneHandler( const AHandler: TNotifyEvent; AsLast: Boolean = False );
      procedure RemoveSetActiveSceneHandler( const AHandler: TNotifyEvent );
      procedure SetActiveScene( ASender: TObject; AValue: TP3DStreamable);

      procedure AddSetActiveMeshHandler( const AHandler: TNotifyEvent; AsLast: Boolean = False );
      procedure RemoveSetActiveMeshHandler( const AHandler: TNotifyEvent );
      procedure SetActiveMesh( ASender: TObject; AValue: TP3DStreamable);

      procedure AddSetActiveLibraryHandler( const AHandler: TNotifyEvent; AsLast: Boolean = False );
      procedure RemoveSetActiveLibraryHandler( const AHandler: TNotifyEvent );
      procedure SetActiveLibrary( ASender: TObject; AValue: TP3DStreamable);

      procedure AddSetActiveMaterialHandler( const AHandler: TNotifyEvent; AsLast: Boolean = False );
      procedure RemoveSetActiveMaterialHandler( const AHandler: TNotifyEvent );
      procedure SetActiveMaterial( ASender: TObject; AValue: TP3DStreamable);

      procedure AddSetActiveTextureHandler( const AHandler: TNotifyEvent; AsLast: Boolean = False );
      procedure RemoveSetActiveTextureHandler( const AHandler: TNotifyEvent );
      procedure SetActiveTexture( ASender: TObject; AValue: TP3DStreamable);

      procedure AddSetActiveEffectHandler( const AHandler: TNotifyEvent; AsLast: Boolean = False );
      procedure RemoveSetActiveEffectHandler( const AHandler: TNotifyEvent );
      procedure SetActiveEffect( ASender: TObject; AValue: TP3DStreamable);

      procedure AddSetActiveEditorHandler( const AHandler: TNotifyEvent; AsLast: Boolean = False );
      procedure RemoveSetActiveEditorHandler( const AHandler: TNotifyEvent );
      procedure SetActiveEditor( ASender: TObject; AValue: Integer );

      procedure AddEditorChangeHandler( const AHandler: TNotifyEvent; AsLast: Boolean = False );
      procedure RemoveEditorChangeHandler( const AHandler: TNotifyEvent );

      procedure AddFileOpenDlgHandler( const AHandler: TNotifyEvent; AsLast: Boolean = False );
      procedure RemoveFileOpenDlgHandler( const AHandler: TNotifyEvent );
      procedure ShowFileOpenDlg( ASender: TObject );

      procedure AddFileSaveDlgHandler( const AHandler: TNotifyEvent; AsLast: Boolean = False );
      procedure RemoveFileSaveDlgHandler( const AHandler: TNotifyEvent );
      procedure ShowFileSaveDlg( ASender: TObject );

      procedure AddMaterialNewHandler( const AHandler: TNotifyEvent; AsLast: Boolean = False );
      procedure RemoveMaterialNewHandler( const AHandler: TNotifyEvent );
      procedure NewMaterial( ASender: TObject );

      procedure AddMaterialDeleteHandler( const AHandler: TNotifyEvent; AsLast: Boolean = False );
      procedure RemoveMaterialDeleteHandler( const AHandler: TNotifyEvent );
      procedure DeleteMaterial( ASender: TObject );

      procedure AddMaterialMapNewHandler( const AHandler: TNotifyEvent; AsLast: Boolean = False );
      procedure RemoveMaterialMapNewHandler( const AHandler: TNotifyEvent );
      procedure NewMaterialMap( ASender: TObject );

      procedure AddMeshAssignMaterialHandler( const AHandler: TNotifyEvent; AsLast: Boolean = False );
      procedure RemoveMeshAssignMaterialHandler( const AHandler: TNotifyEvent );
      procedure AssignMeshMaterial( ASender: TObject );

      procedure AddTextureNewHandler( const AHandler: TNotifyEvent; AsLast: Boolean = False );
      procedure RemoveTextureNewHandler( const AHandler: TNotifyEvent );
      procedure NewTexture( ASender: TObject );

      procedure AddEffectNewHandler( const AHandler: TNotifyEvent; AsLast: Boolean = False );
      procedure RemoveEffectNewHandler( const AHandler: TNotifyEvent );
      procedure NewEffect( ASender: TObject; AEffect: TP3DEffect );

      procedure AddEffectDeleteHandler( const AHandler: TNotifyEvent; AsLast: Boolean = False );
      procedure RemoveEffectDeleteHandler( const AHandler: TNotifyEvent );
      procedure DeleteEffect( ASender: TObject );

      procedure RegisterPopup( AClass: TClass; APopup: TP3DPopupMenu );
      procedure UnregisterPopup( AClass: TClass );

      property Selection: TP3DStreamable read GetSelection;
      property ActiveMesh: TP3DMesh read GetActiveMesh;
      property ActiveMaterial: TP3DMaterial read GetActiveMaterial;
      property ActiveTexture: TP3DTexture read GetActiveTexture;
      property ActiveScene: TP3DScene read GetActiveScene;
      property ActiveLibrary: TP3DLibrary read GetActiveLibrary;
      property ActiveEffect: TP3DEffect read GetActiveEffect;
      property ActiveEditor: Integer read FActiveEditor;
      property Editors: TP3DEditorContainerList read FEditors;
      property Popups: TP3DPopupDictionary read FPopups write FPopups;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DMainIntf }

procedure TP3DMainIntf.SetSelection(ASender: TObject; AValue: TP3DStreamable);
begin
  if ( Selection = AValue ) then
    Exit;

  FSelection.AsValue:= AValue;

  DoCallNotifyHandler( mihtSelectionChange, ASender );
end;

procedure TP3DMainIntf.AddSetActiveSceneHandler(const AHandler: TNotifyEvent; AsLast: Boolean);
begin
  AddHandler( mihtActiveSceneChange, TMethod( AHandler ), AsLast );
end;

procedure TP3DMainIntf.RemoveSetActiveSceneHandler(const AHandler: TNotifyEvent);
begin
  RemoveHandler( mihtActiveSceneChange, TMethod( AHandler ));
end;

procedure TP3DMainIntf.SetActiveScene(ASender: TObject; AValue: TP3DStreamable);
begin
  if ( FActiveScene.AsValue = AValue ) then
    Exit;

  FActiveScene.AsValue:= AValue;

  DoCallNotifyHandler( mihtActiveSceneChange, ASender );
end;

procedure TP3DMainIntf.AddSetActiveMeshHandler(const AHandler: TNotifyEvent; AsLast: Boolean);
begin
  AddHandler( mihtActiveMeshChange, TMethod( AHandler ), AsLast );
end;

procedure TP3DMainIntf.RemoveSetActiveMeshHandler(const AHandler: TNotifyEvent);
begin
  RemoveHandler( mihtActiveMeshChange, TMethod( AHandler ));
end;

procedure TP3DMainIntf.SetActiveMesh(ASender: TObject; AValue: TP3DStreamable);
begin
  if ( FActiveMesh.AsValue = AValue ) then
    Exit;

  FActiveMesh.AsValue:= AValue;

  DoCallNotifyHandler( mihtActiveMeshChange, ASender );
end;

procedure TP3DMainIntf.AddSetActiveLibraryHandler(const AHandler: TNotifyEvent; AsLast: Boolean);
begin
  AddHandler( mihtActiveLibraryChange, TMethod( AHandler ), AsLast );
end;

procedure TP3DMainIntf.RemoveSetActiveLibraryHandler(const AHandler: TNotifyEvent);
begin
  RemoveHandler( mihtActiveLibraryChange, TMethod( AHandler ));
end;

procedure TP3DMainIntf.SetActiveLibrary(ASender: TObject; AValue: TP3DStreamable);
begin
  if ( FActiveLibrary.AsValue = AValue ) then
    Exit;

  FActiveLibrary.AsValue:= AValue;

  DoCallNotifyHandler( mihtActiveLibraryChange, ASender );
end;

procedure TP3DMainIntf.AddSetActiveMaterialHandler(const AHandler: TNotifyEvent; AsLast: Boolean);
begin
  AddHandler( mihtActiveMaterialChange, TMethod( AHandler ), AsLast );
end;

procedure TP3DMainIntf.RemoveSetActiveMaterialHandler(const AHandler: TNotifyEvent);
begin
  RemoveHandler( mihtActiveMaterialChange, TMethod( AHandler ));
end;

procedure TP3DMainIntf.SetActiveMaterial(ASender: TObject; AValue: TP3DStreamable);
begin
  if (( not Assigned( FActiveMaterial )) or ( FActiveMaterial.AsValue = AValue )) then
    Exit;

  FActiveMaterial.AsValue:= AValue;

  DoCallNotifyHandler( mihtActiveMaterialChange, ASender );
end;

procedure TP3DMainIntf.AddSetActiveTextureHandler(const AHandler: TNotifyEvent; AsLast: Boolean);
begin
  AddHandler( mihtActiveTextureChange, TMethod( AHandler ), AsLast );
end;

procedure TP3DMainIntf.RemoveSetActiveTextureHandler(const AHandler: TNotifyEvent);
begin
  RemoveHandler( mihtActiveTextureChange, TMethod( AHandler ));
end;

procedure TP3DMainIntf.SetActiveTexture(ASender: TObject; AValue: TP3DStreamable);
begin
  if (( not Assigned( FActiveTexture )) or ( FActiveTexture.AsValue = AValue )) then
    Exit;

  FActiveTexture.AsValue:= AValue;

  DoCallNotifyHandler( mihtActiveTextureChange, ASender );
end;

procedure TP3DMainIntf.AddSetActiveEffectHandler(const AHandler: TNotifyEvent; AsLast: Boolean);
begin
  AddHandler( mihtActiveEffectChange, TMethod( AHandler ), AsLast );
end;

procedure TP3DMainIntf.RemoveSetActiveEffectHandler(const AHandler: TNotifyEvent);
begin
  RemoveHandler( mihtActiveEffectChange, TMethod( AHandler ));
end;

procedure TP3DMainIntf.SetActiveEffect(ASender: TObject; AValue: TP3DStreamable);
begin
  if (( not Assigned( FActiveEffect )) or ( FActiveEffect.AsValue = AValue )) then
    Exit;

  FActiveEffect.AsValue:= AValue;

  DoCallNotifyHandler( mihtActiveEffectChange, ASender );
end;

procedure TP3DMainIntf.AddSetActiveEditorHandler(const AHandler: TNotifyEvent; AsLast: Boolean);
begin
  AddHandler( mihtActiveEditorChange, TMethod( AHandler ), AsLast );
end;

procedure TP3DMainIntf.RemoveSetActiveEditorHandler(const AHandler: TNotifyEvent);
begin
  RemoveHandler( mihtActiveEditorChange, TMethod( AHandler ));
end;

procedure TP3DMainIntf.SetActiveEditor(ASender: TObject; AValue: Integer);
begin
  if ( not InRange( AValue, 0, Editors.Count - 1 )) then
    exit;
  FActiveEditor:= AValue;
  DoCallNotifyHandler( mihtActiveEditorChange, ASender );
end;

procedure TP3DMainIntf.AddEditorChangeHandler(const AHandler: TNotifyEvent; AsLast: Boolean);
begin
  AddHandler( mihtEditorsChange, TMethod( AHandler ), AsLast );
end;

procedure TP3DMainIntf.RemoveEditorChangeHandler(const AHandler: TNotifyEvent);
begin
  RemoveHandler( mihtEditorsChange, TMethod( AHandler ));
end;

procedure TP3DMainIntf.AddFileOpenDlgHandler(const AHandler: TNotifyEvent; AsLast: Boolean);
begin
  AddHandler( mihtFileOpenDlg, TMethod( AHandler ), AsLast );
end;

procedure TP3DMainIntf.RemoveFileOpenDlgHandler(const AHandler: TNotifyEvent);
begin
  RemoveHandler( mihtFileOpenDlg, TMethod( AHandler ));
end;

procedure TP3DMainIntf.ShowFileOpenDlg(ASender: TObject );
begin
  DoCallNotifyHandler( mihtFileOpenDlg, ASender );
end;

procedure TP3DMainIntf.AddFileSaveDlgHandler(const AHandler: TNotifyEvent; AsLast: Boolean);
begin
  AddHandler( mihtFileSaveDlg, TMethod( AHandler ), AsLast );
end;

procedure TP3DMainIntf.RemoveFileSaveDlgHandler(const AHandler: TNotifyEvent);
begin
  RemoveHandler( mihtFileSaveDlg, TMethod( AHandler ));
end;

procedure TP3DMainIntf.ShowFileSaveDlg(ASender: TObject);
begin
  DoCallNotifyHandler( mihtFileSaveDlg, ASender );
end;

procedure TP3DMainIntf.AddMaterialNewHandler(const AHandler: TNotifyEvent; AsLast: Boolean);
begin
  AddHandler( mihtMaterialNew, TMethod( AHandler ), AsLast );
end;

procedure TP3DMainIntf.RemoveMaterialNewHandler(const AHandler: TNotifyEvent);
begin
  RemoveHandler( mihtMaterialNew, TMethod( AHandler ));
end;

procedure TP3DMainIntf.NewMaterial(ASender: TObject);
var
  Mat: TP3DDataBlock;
begin
  DoCallNotifyHandler( mihtMaterialNew, ASender );
  if ( Assigned( ActiveLibrary )) then begin
    Mat:= ActiveLibrary.CreateNew( TP3DMaterialNode );
    TP3DMaterialNode( Mat ).MakeRootNode( 'output' );
    SetSelection( Self, Mat );
    SetActiveMaterial( Self, Mat );
  end;
end;

procedure TP3DMainIntf.AddMaterialDeleteHandler(const AHandler: TNotifyEvent; AsLast: Boolean);
begin
  AddHandler( mihtMaterialDelete, TMethod( AHandler ), AsLast );
end;

procedure TP3DMainIntf.RemoveMaterialDeleteHandler(const AHandler: TNotifyEvent);
begin
  RemoveHandler( mihtMaterialDelete, TMethod( AHandler ));
end;

procedure TP3DMainIntf.DeleteMaterial(ASender: TObject);
begin
  DoCallNotifyHandler( mihtMaterialDelete, ASender );
end;

procedure TP3DMainIntf.AddMaterialMapNewHandler(const AHandler: TNotifyEvent; AsLast: Boolean);
begin
  AddHandler( mihtMaterialMapNew, TMethod( AHandler ), AsLast );
end;

procedure TP3DMainIntf.RemoveMaterialMapNewHandler(const AHandler: TNotifyEvent);
begin
  RemoveHandler( mihtMaterialMapNew, TMethod( AHandler ));
end;

procedure TP3DMainIntf.NewMaterialMap(ASender: TObject);
var
  map: TP3DMaterialMap;
begin
  DoCallNotifyHandler( mihtMaterialMapNew, ASender );
  if ( Assigned( ActiveTexture ) and Assigned( ActiveMaterial )) then begin
    map:= ActiveMaterial.Maps.AddCopy( 'texture2D' );
    map.Map:= ActiveTexture;
  end;
end;


procedure TP3DMainIntf.AddMeshAssignMaterialHandler(const AHandler: TNotifyEvent; AsLast: Boolean);
begin
  AddHandler( mihtMeshAssignMaterial, TMethod( AHandler ), AsLast );
end;

procedure TP3DMainIntf.RemoveMeshAssignMaterialHandler(const AHandler: TNotifyEvent);
begin
  RemoveHandler( mihtMeshAssignMaterial, TMethod( AHandler ));
end;

procedure TP3DMainIntf.AssignMeshMaterial(ASender: TObject);
begin
  DoCallNotifyHandler( mihtMeshAssignMaterial, ASender );
  if ( Assigned( ActiveMesh )) then
    ActiveMesh.Material:= ActiveMaterial;
end;

procedure TP3DMainIntf.AddTextureNewHandler(const AHandler: TNotifyEvent; AsLast: Boolean);
begin

end;

procedure TP3DMainIntf.RemoveTextureNewHandler(const AHandler: TNotifyEvent);
begin

end;

procedure TP3DMainIntf.NewTexture(ASender: TObject);
begin

end;

procedure TP3DMainIntf.AddEffectNewHandler(const AHandler: TNotifyEvent; AsLast: Boolean);
begin
  AddHandler( mihtEffectNew, TMethod( AHandler ), AsLast );
end;

procedure TP3DMainIntf.RemoveEffectNewHandler(const AHandler: TNotifyEvent);
begin
  RemoveHandler( mihtEffectNew, TMethod( AHandler ));
end;

procedure TP3DMainIntf.NewEffect(ASender: TObject; AEffect: TP3DEffect);
var
  Effect: TP3DDataBlock;
begin
  DoCallNotifyHandler( mihtEffectNew, ASender );
  if ( Assigned( ActiveLibrary )) then begin
    SetSelection( Self, AEffect );
    SetActiveEffect( Self, AEffect );
  end;
end;

procedure TP3DMainIntf.AddEffectDeleteHandler(const AHandler: TNotifyEvent; AsLast: Boolean);
begin
  AddHandler( mihtEffectDelete, TMethod( AHandler ), AsLast );
end;

procedure TP3DMainIntf.RemoveEffectDeleteHandler(const AHandler: TNotifyEvent);
begin
  RemoveHandler( mihtEffectDelete, TMethod( AHandler ));
end;

procedure TP3DMainIntf.DeleteEffect(ASender: TObject);
begin
  DoCallNotifyHandler( mihtEffectDelete, ASender );
  if ( Assigned( ActiveEffect )) then
    ActiveEffect.Free;
end;

procedure TP3DMainIntf.RegisterPopup(AClass: TClass; APopup: TP3DPopupMenu);
begin
  Popups.Add( AClass.ClassName, APopup );
end;

procedure TP3DMainIntf.UnregisterPopup(AClass: TClass);
begin
  Popups.Remove( AClass.ClassName );
end;



function TP3DMainIntf.GetSelection: TP3DStreamable;
begin
  Result:= FSelection.AsValue;
end;

function TP3DMainIntf.GetActiveLibrary: TP3DLibrary;
begin
  Result:= TP3DLibrary( FActiveLibrary.AsValue );
end;

function TP3DMainIntf.GetActiveMesh: TP3DMesh;
begin
  Result:= TP3DMesh( FActiveMesh.AsValue );
end;

function TP3DMainIntf.GetActiveScene: TP3DScene;
begin
  Result:= TP3DScene( FActiveScene.AsValue );
end;

function TP3DMainIntf.GetActiveMaterial: TP3DMaterial;
begin
  if ( Assigned( FActiveMaterial )) then
    Result:= TP3DMaterial( FActiveMaterial.AsValue )
  else
    Result:= nil;
end;

function TP3DMainIntf.GetActiveTexture: TP3DTexture;
begin
  if ( Assigned( FActiveTexture )) then
    Result:= TP3DTexture( FActiveTexture.AsValue )
  else
    Result:= nil;
end;

function TP3DMainIntf.GetActiveEffect: TP3DEffect;
begin
  if ( Assigned( FActiveEffect )) then
    Result:= TP3DEffect( FActiveEffect.AsValue )
  else
    Result:= nil;
end;

procedure TP3DMainIntf.AddHandler(HandlerType: TP3DMainIntfHandlerType; const AMethod: TMethod; AsLast: Boolean);
begin
  if ( FHandlers[ HandlerType ] = nil ) then
    FHandlers[ HandlerType ]:= TMethodList.Create;
  FHandlers[ HandlerType ].Add( AMethod, AsLast );
end;

procedure TP3DMainIntf.RemoveHandler( HandlerType: TP3DMainIntfHandlerType; const AMethod: TMethod );
begin
  FHandlers[ HandlerType ].Remove( AMethod );
end;

procedure TP3DMainIntf.DoCallNotifyHandler( HandlerType: TP3DMainIntfHandlerType; ASender: TObject );
begin
  FHandlers[ HandlerType ].CallNotifyEvents( ASender );
end;

procedure TP3DMainIntf.EditorChange(ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction);
begin
  DoCallNotifyHandler( mihtEditorsChange, AItem );
end;

constructor TP3DMainIntf.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FEditors:= TP3DEditorContainerList.Create( 'Editors' );
  Properties.Add( FEditors );
  FEditors.OnChange:= @EditorChange;
  FSelection:= TP3DStreamablePointer.Create( 'Selection', TP3DStreamable );
  FActiveScene:= TP3DStreamablePointer.Create( 'ActiveScene', TP3DScene );
  FActiveMesh:= TP3DStreamablePointer.Create( 'ActiveMesh', TP3DMesh );
  FActiveMaterial:= TP3DStreamablePointer.Create( 'ActiveMaterial', TP3DMaterial );
  FActiveTexture:= TP3DStreamablePointer.Create( 'ActiveTexture', TP3DTexture );
  FActiveLibrary:= TP3DStreamablePointer.Create( 'ActiveLibrary', TP3DLibrary );
  FActiveEffect:= TP3DStreamablePointer.Create( 'ActiveEffect', TP3DEffect );
  FActiveEditor:= 0;
  FPopups:= TP3DPopupDictionary.Create;
  FPopups.Sorted:= True;
  Properties.Add([ FSelection, FActiveScene, FActiveMesh, FActiveMaterial, FActiveTexture, FActiveLibrary, FActiveEffect ]);
end;

destructor TP3DMainIntf.Destroy;
var
  h: TP3DMainIntfHandlerType;
begin
  FPopups.Free;
  for h:= Low( FHandlers ) to High( FHandlers ) do
    FreeAndNil( FHandlers[ h ]);
  inherited Destroy;
end;

procedure TP3DMainIntf.AddSetSelectionHandler(const AHandler: TNotifyEvent; AsLast: Boolean);
begin
  AddHandler( mihtSelectionChange, TMethod( AHandler ), AsLast );
end;

procedure TP3DMainIntf.RemoveSetSelectionHandler(const AHandler: TNotifyEvent);
begin
  RemoveHandler( mihtSelectionChange, TMethod( AHandler ));
end;

procedure TP3DEditor.ToolbarBtnClick(AButtonIdx: Integer; AButtonCaption: String);
begin

end;

{ gP3DEditor }

function gP3DEditor.GetPopup: TP3DPopupMenu;
begin
  Result:= TP3DPopupMenu( FPopup.AsValue );
end;

function gP3DEditor.GetPopupList: TP3DPopupMenu;
begin
  Result:= TP3DPopupMenu( FPopupList.AsValue );
end;

procedure gP3DEditor.PopupClick(Sender: TP3DMenuItem);
begin

end;

procedure gP3DEditor.PopupListClick(Sender: TP3DMenuItem);
begin

end;

constructor gP3DEditor.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);

  FPopup:= TP3DStreamablePointer.Create( 'Popup', TP3DPopupMenu );
  Properties.Add( FPopup );
  FPopup.AsValue:= TP3DPopupMenu.Create();
  Popup.OnItemClick:= @PopupClick;
  Popup.AutoWidth:= True;

  FPopupList:= TP3DStreamablePointer.Create( 'PopupList', TP3DPopupMenu );
  Properties.Add( FPopupList );
  FPopupList.AsValue:= TP3DPopupMenu.Create();
  PopupList.AutoWidth:= True;
  PopupList.OnItemClick:= @PopupListClick;
end;

destructor gP3DEditor.Destroy;
begin
  inherited Destroy;
end;
{$ENDIF}

