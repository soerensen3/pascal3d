//p3dgui_objinspector.inc
//part of p3dgui.pas

{$IFDEF INTERFACE}
type

  { TP3DGUIOIEditor }

  { TP3DOIEditor }

  TP3DOIEditor = class ( TP3DGroupBox )
    private
      FObj: TInterfacedPersistent;

      procedure SetObj(AValue: TInterfacedPersistent); virtual;

    public
      procedure Update; virtual; abstract;
      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; virtual;

    published
      property Obj: TInterfacedPersistent read FObj write SetObj;
  end;

  { TP3DOIEditorObject }

  TP3DOIEditorObject = class ( TP3DOIEditor )
    private
      FNameEdit: TP3DEdit;

    public
      procedure Update; override;
      constructor Create(const AOwner: TP3DObjectList=nil;const AParent: TP3DGraphicControl=nil);
      destructor Destroy; override;
      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; override;

    published
      property NameEdit: TP3DEdit read FNameEdit write FNameEdit;
  end;

  { TP3DOIEditorTransform }

  TP3DOIEditorTransform = class ( TP3DOIEditor )
    private
      FPositionEdit: TP3DVectorEdit;
      FRotationEdit: TP3DVectorEdit;
      FScaleEdit: TP3DVectorEdit;

      function EdGetValue( Sender: TP3DVectorEdit ): TVec4;
      procedure EdSetValue( Sender: TP3DVectorEdit; AValue: TVec4 );

    public
      procedure Update; override;

      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; override;

      constructor Create(const AOwner: TP3DObjectList=nil;const AParent: TP3DGraphicControl=nil);
      destructor Destroy; override;

    published
      property PositionEdit: TP3DVectorEdit read FPositionEdit write FPositionEdit;
      property RotationEdit: TP3DVectorEdit read FRotationEdit write FRotationEdit;
      property ScaleEdit: TP3DVectorEdit read FScaleEdit write FScaleEdit;
  end;

  { TP3DOIEditorDataLight }

  TP3DOIEditorDataLight = class ( TP3DOIEditor )
    private
      FLightType: TP3DComboBox;

      procedure ComboChange( Sender: TP3DGraphicControl; var AValue: String );

    public
      procedure Update; override;

      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; override;

      constructor Create(const AOwner: TP3DObjectList=nil;const AParent: TP3DGraphicControl=nil);
      destructor Destroy; override;

    published
      property LightTypeCombo: TP3DComboBox read FLightType write FLightType;

  end;

  TP3DOIEditorList = specialize gP3DCustomObjectList < TP3DOIEditor >;

  { TP3DObjectInspector }

  TP3DObjectInspector = class( TP3DGraphicControl )
    private
      FEditors: TP3DOIEditorList;
      FObj: TInterfacedPersistent;

      procedure SetObj(AValue: TInterfacedPersistent);

    public
      constructor Create(const AOwner: TP3DObjectList=nil; const AParent: TP3DGraphicControl=nil);
      destructor Destroy; override;
      procedure AddEditor( Editor: TP3DOIEditor );

      procedure Update;

    published
      property Obj: TInterfacedPersistent read FObj write SetObj;
      property Editors: TP3DOIEditorList read FEditors write FEditors;
  end;
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DOIEditor }

procedure TP3DOIEditor.SetObj(AValue: TInterfacedPersistent);
begin
  if FObj=AValue then Exit;
  FObj:=AValue;
  Update;
end;

class function TP3DOIEditor.IsCompatible(AObj: TInterfacedPersistent): Boolean;
begin
  Result:= False;
end;

{ TP3DOIEditorObject }

procedure TP3DOIEditorObject.Update;
begin
  if ( Assigned( Obj )) then
    with ( Obj as IP3DObject ) do
      begin
        NameEdit.Text:= Name;
      end;
end;

constructor TP3DOIEditorObject.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
var
  Container: TP3DGraphicControl;
  Lbl: TP3DLabel;
begin
  inherited;
  Caption:= 'Object';
  Height:= 60;
  Container:= TP3DGraphicControl.Create( nil, Self );
  Container.Align:= alTop;
  Container.Height:= 20;
  Lbl:= TP3DLabel.Create( nil, Container );
  Lbl.Align:= alLeft;
  Lbl.Caption:= 'Name ';
  Lbl.AutoSize:= True;
  NameEdit:= TP3DEdit.Create( nil, Container );
  NameEdit.Align:= alClient;
  AutoSize:= True;
end;

destructor TP3DOIEditorObject.Destroy;
begin
  Controls.Clear( True );
  inherited Destroy;
end;

class function TP3DOIEditorObject.IsCompatible(AObj: TInterfacedPersistent
  ): Boolean;
begin
  Result:= AObj is IP3DObject;
end;

{ TP3DOIEditorTransform }

function TP3DOIEditorTransform.EdGetValue(Sender: TP3DVectorEdit): TVec4;
begin
  if ( Assigned( Obj )) then
    with ( Obj as IP3DTransform ) do
      begin
        if ( Sender = PositionEdit ) then
          Result:= vec4( Position, 0 )
        else if ( Sender = RotationEdit ) then
          Result:= vec4( Rotation, 0 )
        else if ( Sender = ScaleEdit ) then
          Result:= vec4( Scale, 0 );
      end;
end;

procedure TP3DOIEditorTransform.EdSetValue(Sender: TP3DVectorEdit; AValue: TVec4 );
begin
  if ( Assigned( Obj )) then
    with ( Obj as IP3DTransform ) do
      begin
        if ( Sender = PositionEdit ) then
          Position:= AValue.xyz
        else if ( Sender = RotationEdit ) then
          Rotation:= AValue.xyz
        else if ( Sender = ScaleEdit ) then
          Scale:= AValue.xyz;
      end;
end;

procedure TP3DOIEditorTransform.Update;
begin

end;

class function TP3DOIEditorTransform.IsCompatible( AObj: TInterfacedPersistent ): Boolean;
begin
  Result:= AObj is IP3DTransform;
end;

constructor TP3DOIEditorTransform.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
var
  Container: TP3DGraphicControl;
  Lbl: TP3DLabel;
begin
  inherited;
  Caption:= 'Transform';
  Height:= 200;

  Container:= TP3DGraphicControl.Create( nil, Self );
  Container.Align:= alTop;
  Container.Height:= 20;
  Lbl:= TP3DLabel.Create( nil, Container );
  Lbl.Align:= alLeft;
  Lbl.Caption:= 'Position ';
  Lbl.AutoSize:= True;
  PositionEdit:= TP3DVectorEdit.Create( nil, Container );
  PositionEdit.Align:= alClient;
  PositionEdit.GetValueEvent:= @EdGetValue;
  PositionEdit.SetValueEvent:= @EdSetValue;

  Container:= TP3DGraphicControl.Create( nil, Self );
  Container.Align:= alTop;
  Container.Height:= 20;
  Lbl:= TP3DLabel.Create( nil, Container );
  Lbl.Align:= alLeft;
  Lbl.Caption:= 'Rotation ';
  Lbl.AutoSize:= True;
  RotationEdit:= TP3DVectorEdit.Create( nil, Container );
  RotationEdit.Align:= alClient;
  RotationEdit.GetValueEvent:= @EdGetValue;
  RotationEdit.SetValueEvent:= @EdSetValue;
  RotationEdit.Transmission:= 5.0;

  Container:= TP3DGraphicControl.Create( nil, Self );
  Container.Align:= alTop;
  Container.Height:= 20;
  Lbl:= TP3DLabel.Create( nil, Container );
  Lbl.Align:= alLeft;
  Lbl.Caption:= 'Scale ';
  Lbl.AutoSize:= True;
  ScaleEdit:= TP3DVectorEdit.Create( nil, Container );
  ScaleEdit.Align:= alClient;
  ScaleEdit.GetValueEvent:= @EdGetValue;
  ScaleEdit.SetValueEvent:= @EdSetValue;
  AutoSize:= True;
end;

destructor TP3DOIEditorTransform.Destroy;
begin
  Controls.Clear( True );
  inherited Destroy;
end;



{ TP3DObjectInspector }

procedure TP3DObjectInspector.SetObj(AValue: TInterfacedPersistent);
begin
  if FObj=AValue then Exit;
  FObj:=AValue;
  Update;
end;

constructor TP3DObjectInspector.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
begin
  inherited;
  Editors:= TP3DOIEditorList.Create;
  AddEditor( TP3DOIEditorObject.Create( nil, Self ));
  AddEditor( TP3DOIEditorTransform.Create( nil, Self ));
  AddEditor( TP3DOIEditorDataLight.Create( nil, Self ));
end;

destructor TP3DObjectInspector.Destroy;
begin
  Editors.Clear( True );
  Editors.Free;
  inherited Destroy;
end;

procedure TP3DObjectInspector.AddEditor(Editor: TP3DOIEditor);
begin
  Editor.Align:= alTop;
  Editor.Obj:= Obj;
  Editor.Visible:= False;
  Editors.Add( Editor );
end;

procedure TP3DObjectInspector.Update;
var
  i: Integer;
begin
  if ( Assigned( Obj )) then
    begin
      for i:= 0 to Editors.Count - 1 do
        begin
          if ( Editors[ i ].IsCompatible( Obj )) then
            begin
              Editors[ i ].Visible:= True;
              Editors[ i ].Obj:= Obj;
            end
          else
            begin
              Editors[ i ].Visible:= False;
              Editors[ i ].Obj:= nil;
            end;
        end;
    end
  else
    for i:= 0 to Editors.Count - 1 do
      begin
        Editors[ i ].Obj:= nil;
        Editors[ i ].Visible:= False;
      end;
end;


{ TP3DOIEditorDataLight }

procedure TP3DOIEditorDataLight.ComboChange(Sender: TP3DGraphicControl; var AValue: String);
var
  L: TP3DLight;
begin
  if (( Obj is TP3DActor ) and ( TP3DActor( Obj ).Data is TP3DLight )) then
    L:= TP3DLight( TP3DActor( Obj ).Data )
  else if ( Obj is TP3DLight ) then
    L:= TP3DLight( Obj )
  else
    L:= nil;

  if ( Assigned( L )) then
    case AValue of
      'Point': L.LightType := p3dltPoint;
      'Spot': L.LightType := p3dltSpot;
      'Sun': L.LightType := p3dltSun;
    end;
end;

procedure TP3DOIEditorDataLight.Update;
var
  L: TP3DLight;
begin
  if (( Obj is TP3DActor ) and ( TP3DActor( Obj ).Data is TP3DLight )) then
    L:= TP3DLight( TP3DActor( Obj ).Data )
  else if ( Obj is TP3DLight ) then
    L:= TP3DLight( Obj )
  else
    L:= nil;

  if ( Assigned( L )) then
    begin
      case L.LightType of
        p3dltPoint: LightTypeCombo.Caption:= 'Point';
        p3dltSpot: LightTypeCombo.Caption:= 'Spot';
        p3dltSun: LightTypeCombo.Caption:= 'Sun';
      end;
    end;
end;

class function TP3DOIEditorDataLight.IsCompatible( AObj: TInterfacedPersistent ): Boolean;
begin
  Result:= (( AObj is TP3DActor ) and ( TP3DActor( AObj ).Data is TP3DLight )) or ( AObj is TP3DLight );
end;

constructor TP3DOIEditorDataLight.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
var
  Container: TP3DGraphicControl;
  Lbl: TP3DLabel;
begin
  inherited Create( AOwner, AParent );
  Caption:= 'Light';
  Container:= TP3DGraphicControl.Create( nil, Self );
  Container.Align:= alTop;
  Container.Height:= 20;
  Lbl:= TP3DLabel.Create( nil, Container );
  Lbl.Align:= alLeft;
  Lbl.Caption:= 'Light type ';
  Lbl.AutoSize:= True;
  LightTypeCombo:= TP3DComboBox.Create( nil, Container );
  LightTypeCombo.Align:= alClient;
  LightTypeCombo.Items.Add( 'Sun' );
  LightTypeCombo.Items.Add( 'Point' );
  LightTypeCombo.Items.Add( 'Spot' );
  LightTypeCombo.OnChange:= @ComboChange;
  AutoSize:= True;
end;

destructor TP3DOIEditorDataLight.Destroy;
begin
  inherited Destroy;
end;


{$ENDIF}
