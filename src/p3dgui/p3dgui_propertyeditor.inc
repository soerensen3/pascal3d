{ <description>

  Copyright (c) <2016> <Johannes Rosleff Sörensen>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}

{$IFDEF INTERFACE}
type
  { IP3DPropertyEditor }

  IP3DPropertyEditor = interface ( IP3DBaseInterface )
    ['{A6524E5C-6F92-4CF2-BBF4-FB68F1436A32}']
    function GetObjInsp: TP3DObjectInspector;
    function GetProp: TP3DPropertyAccess;
    procedure SetObjInsp(AValue: TP3DObjectInspector);
    procedure SetProp(AValue: TP3DPropertyAccess);
    property Prop: TP3DPropertyAccess read GetProp write SetProp;
    property ObjInsp: TP3DObjectInspector read GetObjInsp write SetObjInsp;
  end;

{  IP3DPropertyEditorList = interface ( IP3DBaseInterface )
    ['{0E1AE98B-41B5-4ACB-9F41-021C7DDC5490}']
    function GetProp: TP3DPropertyAccessList;
    procedure SetProp(AValue: TP3DPropertyAccessList);
    property Prop: TP3DPropertyAccess read GetProp write SetProp;
  end;}

  { gP3DPropertyEditorVector }

  generic gP3DPropertyEditorVector < TVecType > = class ( TP3DVectorEdit, IP3DPropertyEditor )
    private
      FProp: TP3DPropertyAccess;
      FLabel: TP3DTextBmp;
      FObjInsp: TP3DObjectInspector;

      function GetObjInsp: TP3DObjectInspector;
      procedure SetObjInsp(AValue: TP3DObjectInspector);
      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetProp: TP3DPropertyAccess;
      function GetValue: TVec4; override;
      procedure SetValue( AValue: TVec4 ); override;

      property GetValueEvent;
      property SetValueEvent;

    public
      constructor CreateBlock(const AData: TP3DLibrary=nil; const AParent: TP3DGraphicControl=nil); override;
      destructor Destroy; override;

      procedure Draw; override;

    published
      property Prop: TP3DPropertyAccess read GetProp write SetProp;
      property ObjInsp: TP3DObjectInspector read GetObjInsp write SetObjInsp;
  end;

  TP3DPropertyEditorVec2 = specialize gP3DPropertyEditorVector < TVec2 >;
  TP3DPropertyEditorVec3 = specialize gP3DPropertyEditorVector < TVec3 >;
  TP3DPropertyEditorVec4 = specialize gP3DPropertyEditorVector < TVec4 >;

  { TP3DCustomPropertyEditorColor }

  TP3DCustomPropertyEditorColor = class ( TP3DColorRGBAComboBox, IP3DPropertyEditor )
    protected
      FProp: TP3DPropertyAccess;
      FLabel: TP3DTextBmp;
      FObjInsp: TP3DObjectInspector;

      function GetObjInsp: TP3DObjectInspector;
      procedure SetObjInsp(AValue: TP3DObjectInspector);
      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetProp: TP3DPropertyAccess;
      function GetColorValue( Sender: TP3DColorRGBAPicker ): TP3DColorRGBA; virtual; abstract;
      procedure SetColorValue( Sender: TP3DColorRGBAPicker; AValue: TP3DColorRGBA ); virtual; abstract;

    public
      constructor CreateBlock(const AData: TP3DLibrary=nil; const AParent: TP3DGraphicControl=nil); override;
      destructor Destroy; override;

      procedure Draw; override;

    published
      property ObjInsp: TP3DObjectInspector read GetObjInsp write SetObjInsp;
      property Prop: TP3DPropertyAccess read GetProp write SetProp;
  end;

  { TP3DPropertyEditorColorRGB }

  TP3DPropertyEditorColorRGB = class ( TP3DCustomPropertyEditorColor )
    protected
      function GetColorValue( Sender: TP3DColorRGBAPicker ): TP3DColorRGBA; override;
      procedure SetColorValue( Sender: TP3DColorRGBAPicker; AValue: TP3DColorRGBA ); override;
  end;

  { TP3DPropertyEditorColorRGBA }

  TP3DPropertyEditorColorRGBA = class ( TP3DCustomPropertyEditorColor )
    protected
      function GetColorValue( Sender: TP3DColorRGBAPicker ): TP3DColorRGBA; override;
      procedure SetColorValue( Sender: TP3DColorRGBAPicker; AValue: TP3DColorRGBA ); override;
  end;


  { TP3DPropertyEditorString }

  TP3DPropertyEditorString = class ( TP3DEdit, IP3DPropertyEditor )
    protected
      FProp: TP3DPropertyAccess;
      FLabel: TP3DTextBmp;
      FObjInsp: TP3DObjectInspector;

      function GetObjInsp: TP3DObjectInspector;
      procedure SetObjInsp(AValue: TP3DObjectInspector);
      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetProp: TP3DPropertyAccess;
      procedure SetFocused( AValue: Boolean ); override;
      procedure ReadValue;
      procedure ApplyValue;

    public
      constructor CreateBlock(const AData: TP3DLibrary=nil; const AParent: TP3DGraphicControl=nil); override;
      destructor Destroy; override;

      procedure Draw; override;
      procedure KeyboardAction; override;

    published
      property ObjInsp: TP3DObjectInspector read GetObjInsp write SetObjInsp;
      property Prop: TP3DPropertyAccess read GetProp write SetProp;
  end;

  { TP3DPropertyEditorFloat }

  TP3DPropertyEditorFloat = class ( TP3DCustomValueEdit, IP3DPropertyEditor )
    protected
      FProp: TP3DPropertyAccess;
      FObjInsp: TP3DObjectInspector;
      function GetObjInsp: TP3DObjectInspector;
      procedure SetObjInsp(AValue: TP3DObjectInspector);
      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetProp: TP3DPropertyAccess;
      function GetValue: Single; override;
      procedure SetValue(AValue: Single); override;
      function GetValueName: String; override;

    public
      constructor CreateBlock(const AData: TP3DLibrary=nil; const AParent: TP3DGraphicControl=nil); override;

    published
      property Prop: TP3DPropertyAccess read GetProp write SetProp;
      property ObjInsp: TP3DObjectInspector read GetObjInsp write SetObjInsp;
  end;

  { TP3DPropertyEditorInt }

  TP3DPropertyEditorInt = class ( TP3DCustomValueEdit, IP3DPropertyEditor )
    protected
      FProp: TP3DPropertyAccess;
      FObjInsp: TP3DObjectInspector;
      function GetObjInsp: TP3DObjectInspector;
      procedure SetObjInsp(AValue: TP3DObjectInspector);
      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetProp: TP3DPropertyAccess;
      function GetValue: Single; override;
      procedure SetValue( AValue: Single ); override;
      function GetValueName: String; override;

    public
      constructor CreateBlock( const AData: TP3DLibrary=nil; const AParent: TP3DGraphicControl=nil ); override;

    published
      property Prop: TP3DPropertyAccess read GetProp write SetProp;
      property ObjInsp: TP3DObjectInspector read GetObjInsp write SetObjInsp;
  end;

  { TP3DPropertyEditorEnum }

  TP3DPropertyEditorEnum = class ( TP3DComboBox, IP3DPropertyEditor )
    protected
      FProp: TP3DPropertyAccess;
      FLabel: TP3DTextBmp;
      FObjInsp: TP3DObjectInspector;
      function GetObjInsp: TP3DObjectInspector;
      procedure SetObjInsp(AValue: TP3DObjectInspector);
      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetProp: TP3DPropertyAccess;
      procedure SetCaption(AValue: String); override;

    public
      constructor CreateBlock( const AData: TP3DLibrary=nil; const AParent: TP3DGraphicControl=nil ); override;
      destructor Destroy; override;

      procedure Draw; override;

    published
      property Prop: TP3DPropertyAccess read GetProp write SetProp;
      property ObjInsp: TP3DObjectInspector read GetObjInsp write SetObjInsp;
  end;

  { TP3DPropertyEditorBoolean }

  TP3DPropertyEditorBoolean = class ( TP3DCheckBox, IP3DPropertyEditor )
    protected
      FProp: TP3DPropertyAccess;
      FLabel: TP3DTextBmp;
      FObjInsp: TP3DObjectInspector;

      function GetObjInsp: TP3DObjectInspector;
      procedure SetObjInsp(AValue: TP3DObjectInspector);
      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetProp: TP3DPropertyAccess;
      procedure SetChecked(AValue: Boolean); override;
      function GetChecked: Boolean; override;
      function GetCaption: String; override;

    public
      constructor CreateBlock( const AData: TP3DLibrary=nil; const AParent: TP3DGraphicControl=nil ); override;
      destructor Destroy; override;
      procedure Draw; override;

    published
      property Prop: TP3DPropertyAccess read GetProp write SetProp;
      property ObjInsp: TP3DObjectInspector read GetObjInsp write SetObjInsp;
  end;

   { TP3DPropertyEditorInterfacedPersistent }

   TP3DPropertyEditorInterfacedPersistent = class ( TP3DGraphicControl, IP3DPropertyEditor )
     private
       FPopup: TP3DPopupMenu;
       FObjInsp: TP3DObjectInspector;
       function GetObjInsp: TP3DObjectInspector;
       procedure SetObjInsp(AValue: TP3DObjectInspector);

     protected
       FButton: TP3DButton;
       FEdit: TP3DPropertyEditorString;

       procedure SetProp( AValue: TP3DPropertyAccess );
       function GetProp: TP3DPropertyAccess;

     public
       constructor CreateBlock(const AData: TP3DLibrary=nil; const AParent: TP3DGraphicControl=nil); override;
       destructor Destroy; override;
       procedure ButtonClick( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
       procedure PopupClick( Sender: TP3DMenuItem );

     published
       property Prop: TP3DPropertyAccess read GetProp write SetProp;
       property ObjInsp: TP3DObjectInspector read GetObjInsp write SetObjInsp;
       property Edit: TP3DPropertyEditorString read FEdit write FEdit;
       property Button: TP3DButton read FButton write FButton;
       property Popup: TP3DPopupMenu read FPopup write FPopup;
   end;

  { gP3DPropertyEditorList }

  generic gP3DPropertyEditorList < TListProp > = class ( TP3DListView, IP3DPropertyEditor )
    private
      FNameProp: String;

      function GetObjInsp: TP3DObjectInspector;
      procedure SetNameProp(AValue: String);
      procedure SetObjInsp(AValue: TP3DObjectInspector);

    protected
      FProp: TListProp;
      FLabel: TP3DTextBmp;
      FDeleteSymbol: TP3DTextBmp;
      FClearSymbol: TP3DTextBmp;
      FPlusSymbol: TP3DTextBmp;
      FOptionsSymbol: TP3DTextBmp;
      FDrawInsertion: Single;
      FObjInsp: TP3DObjectInspector;

      procedure SetLProp( AValue: TListProp );
      function GetLProp: TListProp;
      function GetProp: TP3DPropertyAccess;
      procedure SetProp(AValue: TP3DPropertyAccess);
      procedure Update;

    public
      procedure MouseUp(mb1, mb2, mb3: Boolean; X, Y: Integer); override;
      function DrawListItem(p: TVec2; Node: TP3DListItem): TVec2; override;
      procedure Draw; override;
      procedure MouseMove(X, Y: Integer); override;

      constructor CreateBlock(const AData: TP3DLibrary=nil; const AParent: TP3DGraphicControl=nil); override;
      destructor Destroy; override;

      property LProp: TListProp read GetLProp write SetLProp;

    published
      property ObjInsp: TP3DObjectInspector read GetObjInsp write SetObjInsp;
      property Prop: TP3DPropertyAccess read GetProp write SetProp;
      property NameProp: String read FNameProp write SetNameProp;
  end;

  { gP3DPropertyEditorInterfacedPersistentList }

  generic gP3DPropertyEditorInterfacedPersistentList < TListProp > = class ( specialize gP3DPropertyEditorList < TListProp> )
    private
      FOptionsPopup: TP3DPopupMenu;

    public
      constructor CreateBlock(const AData: TP3DLibrary=nil; const AParent: TP3DGraphicControl=nil); override;
      destructor Destroy; override;

      procedure MouseUp(mb1, mb2, mb3: Boolean; X, Y: Integer); override;
      procedure PopupClick( Sender: TP3DMenuItem );
      property OptionsPopup: TP3DPopupMenu read FOptionsPopup write FOptionsPopup;
  end;

  TP3DPropertyEditorLibraryList = specialize gP3DPropertyEditorInterfacedPersistentList < TP3DPropAccessLibraryList >;
  TP3DPropertyEditorDataBlockList = specialize gP3DPropertyEditorInterfacedPersistentList < TP3DPropAccessDataBlockList >;
  TP3DPropertyEditorCameraList = specialize gP3DPropertyEditorInterfacedPersistentList < TP3DPropAccessCameraList >;
//  TP3DPropertyEditorJointList = specialize gP3DPropertyEditorInterfacedPersistentList < TP3DPropAccessPoseJointList >;
  TP3DPropertyEditorLightList = specialize gP3DPropertyEditorInterfacedPersistentList < TP3DPropAccessLightList >;
  TP3DPropertyEditorMaterialList = specialize gP3DPropertyEditorInterfacedPersistentList < TP3DPropAccessMaterialList >;
  TP3DPropertyEditorMaterialMapList = specialize gP3DPropertyEditorInterfacedPersistentList < TP3DPropAccessMaterialMapList >;
  TP3DPropertyEditorMeshList = specialize gP3DPropertyEditorInterfacedPersistentList < TP3DPropAccessMeshList >;
  TP3DPropertyEditorSceneList = specialize gP3DPropertyEditorInterfacedPersistentList < TP3DPropAccessSceneList >;
  TP3DPropertyEditorShaderList = specialize gP3DPropertyEditorInterfacedPersistentList < TP3DPropAccessShaderList >;
  TP3DPropertyEditorTextureList = specialize gP3DPropertyEditorInterfacedPersistentList < TP3DPropAccessTextureList >;
  TP3DPropertyEditorArmatureList = specialize gP3DPropertyEditorInterfacedPersistentList < TP3DPropAccessArmatureList >;
  TP3DPropertyEditorFontBmpList = specialize gP3DPropertyEditorInterfacedPersistentList < TP3DPropAccessFontBmpList >;
  TP3DPropertyEditorObjectModifierList = specialize gP3DPropertyEditorInterfacedPersistentList < TP3DPropAccessObjectModifierList >;
  TP3DPropertyEditorGraphicControlList = specialize gP3DPropertyEditorInterfacedPersistentList < TP3DPropAccessGraphicControlList >;
  TP3DPropertyEditorObjectList = specialize gP3DPropertyEditorInterfacedPersistentList < TP3DPropAccessObjectList >;
  TP3DPropertyEditorActionList = specialize gP3DPropertyEditorInterfacedPersistentList < TP3DPropAccessActionList >;
  TP3DPropertyEditorActionChannelList = specialize gP3DPropertyEditorInterfacedPersistentList < TP3DPropAccessActionChannelList >;
  TP3DPropertyEditorActionKeyList = specialize gP3DPropertyEditorInterfacedPersistentList < TP3DPropAccessActionKeyList >;
  TP3DPropertyEditorVec2List = specialize gP3DPropertyEditorList < TP3DPropAccessVec2List >;
  TP3DPropertyEditorVec3List = specialize gP3DPropertyEditorList < TP3DPropAccessVec3List >;
  TP3DPropertyEditorVec4List = specialize gP3DPropertyEditorList < TP3DPropAccessVec4List >;
  TP3DPropertyEditorQuatList = specialize gP3DPropertyEditorList < TP3DPropAccessQuatList >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ gP3DPropertyEditorVector }

procedure gP3DPropertyEditorVector.SetProp( AValue: TP3DPropertyAccess );
begin
  if ( FProp = AValue ) then Exit;
  FProp:= AValue;
  if ( Assigned( Prop )) then
    Canvas.GetValidText( Prop.Name + ':', FLabel )
  else
    Canvas.GetValidText( '<noprop>', FLabel );

  Margin.X:= FLabel.Width + 4;
  Update;
end;

function gP3DPropertyEditorVector.GetObjInsp: TP3DObjectInspector;
begin
  Result:= FObjInsp;
end;

procedure gP3DPropertyEditorVector.SetObjInsp(AValue: TP3DObjectInspector);
begin
  FObjInsp:= AValue;
end;

function gP3DPropertyEditorVector.GetProp: TP3DPropertyAccess;
begin
  Result:= FProp;
end;


function gP3DPropertyEditorVector.GetValue: TVec4;
var
  Val: TVecType;
begin
  Val.FromString( Prop.AsString );
  case Length( TVecType.FCoord ) of
    2: Result:= vec4( Val[ 0 ], Val[ 1 ], 0, 0 );
    3: Result:= vec4( Val[ 0 ], Val[ 1 ], Val[ 2 ], 0 );
    4: Result:= vec4( Val[ 0 ], Val[ 1 ], Val[ 2 ], Val[ 3 ]);
  end;
end;

procedure gP3DPropertyEditorVector.SetValue( AValue: TVec4 );
begin
  case Length( TVecType.FCoord ) of
    2: Prop.AsString:= AValue.XY.ToString();
    3: Prop.AsString:= AValue.XYZ.ToString();
    4: Prop.AsString:= AValue.ToString();
  end;
end;

constructor gP3DPropertyEditorVector.CreateBlock(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited CreateBlock(AData, AParent);
  Height:= 20;
  EdCount:= Length( TVecType.FCoord );
end;

destructor gP3DPropertyEditorVector.Destroy;
begin
  FreeAndNil( FLabel );
  inherited Destroy;
end;

procedure gP3DPropertyEditorVector.Draw;
begin
  inherited Draw;
  Canvas.RenderRect( vec2( 0 ), vec2( Width, Height ) - 1, Accent50, Accent50, Accent50, Accent50 );
  Canvas.RenderLineRect( vec2( 0 ), vec2( Width, Height ) - 1, Accent900, Accent900, Accent900, Accent900 );
  Canvas.RenderText( FLabel, vec2( 0, 0 ));
end;


{ TP3DPropertyEditorString }

function TP3DPropertyEditorString.GetObjInsp: TP3DObjectInspector;
begin
  Result:= FObjInsp;
end;

procedure TP3DPropertyEditorString.SetObjInsp(AValue: TP3DObjectInspector);
begin
  FObjInsp:= AValue;
end;

procedure TP3DPropertyEditorString.SetProp(AValue: TP3DPropertyAccess);
begin
  if FProp=AValue then Exit;
  FProp:=AValue;

  Canvas.Font.Name:= Font.Name;
  Canvas.Font.Size:= Font.Size;
  if ( Assigned( Prop )) then
    begin
      ReadValue;
      Canvas.GetValidText( Prop.Name + ':', FLabel );
    end
  else
    Canvas.GetValidText( '<noprop>', FLabel );
end;

function TP3DPropertyEditorString.GetProp: TP3DPropertyAccess;
begin
  Result:= FProp;
end;

procedure TP3DPropertyEditorString.SetFocused(AValue: Boolean);
begin
  {if ( AValue ) then
    ReadValue()
  else
    ApplyValue();}
  ReadValue;
  inherited SetFocused(AValue);
end;

procedure TP3DPropertyEditorString.ReadValue;
begin
  if ( Assigned( Prop )) then
    Caption:= Prop.AsString
  else
    Caption:= 'No property assigned';
end;

procedure TP3DPropertyEditorString.ApplyValue;
begin
  if ( Assigned( Prop )) then
    Prop.AsString:= Caption;
end;


procedure TP3DPropertyEditorString.KeyboardAction;
begin
  inherited KeyboardAction;

  if ( Focused ) then
    begin
      if ( P3DInput.Keyboard.DKeysLayout[ P3DK_RETURN ] = ksPressed ) then
        begin
          ApplyValue;
          Focused:= False;
        end;
      if ( P3DInput.Keyboard.DKeysLayout[ P3DK_ESCAPE ] = ksPressed ) then
        Focused:= False;
    end;
end;

constructor TP3DPropertyEditorString.CreateBlock(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited CreateBlock(AData, AParent);
  HAlignment:= haRight;
  Height:= 20;
end;

destructor TP3DPropertyEditorString.Destroy;
begin
  FreeAndNil( FLabel );
  inherited Destroy;
end;

procedure TP3DPropertyEditorString.Draw;
begin
  inherited Draw;
  if (( not Focused ) and ( Assigned( FLabel ))) then
    begin
      Canvas.RenderRect( vec2( 0 ), vec2( FLabel.Width, Height ), Primary200 );
      Canvas.RenderRect( vec2( FLabel.Width, 0 ), vec2( FLabel.Width + 10, Height ), Primary200, vec4( Primary200.XYZ, 0 ), vec4( Primary200.XYZ, 0 ), Primary200 );
      Canvas.RenderText( FLabel, vec2( 0 ));
    end;
end;

{ TP3DPropertyEditorInt }

function TP3DPropertyEditorInt.GetObjInsp: TP3DObjectInspector;
begin
  Result:= FObjInsp;
end;

procedure TP3DPropertyEditorInt.SetObjInsp(AValue: TP3DObjectInspector);
begin
  FObjInsp:= AValue;
end;

procedure TP3DPropertyEditorInt.SetProp(AValue: TP3DPropertyAccess);
begin
  if ( Prop = AValue ) then
    exit;

  FProp:= AValue;
  UpdateValueText( GetValue );
  UpdateNameText( GetValueName );
end;

function TP3DPropertyEditorInt.GetProp: TP3DPropertyAccess;
begin
  Result:= FProp;
end;

function TP3DPropertyEditorInt.GetValue: Single;
begin
  if ( Assigned( Prop )) then
    Result:= StrToIntDef( Prop.AsString, 0 );
end;

procedure TP3DPropertyEditorInt.SetValue(AValue: Single);
begin
  if ( Assigned( Prop )) then
    Prop.AsString:= IntToStr( round( AValue ));
end;

function TP3DPropertyEditorInt.GetValueName: String;
begin
  if ( Assigned( Prop )) then
    Result:= Prop.Name;
end;

constructor TP3DPropertyEditorInt.CreateBlock(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited CreateBlock( AData, AParent );
  Height:= 20;
  Precision:= 0;
  FixedDigits:= 0;
  HAlignment:= haRight;
end;

{ TP3DPropertyEditorFloat }

function TP3DPropertyEditorFloat.GetProp: TP3DPropertyAccess;
begin
  Result:= FProp;
end;

function TP3DPropertyEditorFloat.GetObjInsp: TP3DObjectInspector;
begin
  Result:= FObjInsp;
end;

procedure TP3DPropertyEditorFloat.SetObjInsp(AValue: TP3DObjectInspector);
begin
  FObjInsp:= AValue;
end;

procedure TP3DPropertyEditorFloat.SetProp( AValue: TP3DPropertyAccess );
begin
  if ( Prop = AValue ) then
    exit;

  FProp:= AValue;
  UpdateValueText( GetValue );
  UpdateNameText( GetValueName );
end;

function TP3DPropertyEditorFloat.GetValue: Single;
begin
  if ( Assigned( Prop )) then
    Result:= StrToFloat( Prop.AsString );
end;

procedure TP3DPropertyEditorFloat.SetValue(AValue: Single);
begin
  if ( Assigned( Prop )) then
    Prop.AsString:= FloatToStr( AValue );
end;

function TP3DPropertyEditorFloat.GetValueName: String;
begin
  if ( Assigned( Prop )) then
    Result:= Prop.Name;
end;

constructor TP3DPropertyEditorFloat.CreateBlock(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited CreateBlock(AData, AParent);
  Height:= 20;
  HAlignment:= haRight;
end;

{ TP3DPropertyEditorDataBlockList }

constructor gP3DPropertyEditorList.CreateBlock(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited CreateBlock(AData, AParent);
  Font.Size:= 20;
  Height:= 60;
  ScrollBarH.Visible:= True;
  NameProp:= 'Name';
  SetProp( nil );
end;

destructor gP3DPropertyEditorList.Destroy;
begin
  FClearSymbol.Free;
  FDeleteSymbol.Free;
  FOptionsSymbol.Free;
  FPlusSymbol.Free;
  FLabel.Free;
  inherited Destroy;
end;

function gP3DPropertyEditorList.GetLProp: TListProp;
begin
  Result:= FProp;
end;

function gP3DPropertyEditorList.GetObjInsp: TP3DObjectInspector;
begin
  Result:= FObjInsp;
end;

procedure gP3DPropertyEditorList.SetNameProp(AValue: String);
begin
  if FNameProp=AValue then Exit;
  FNameProp:=AValue;
  Update;
end;

procedure gP3DPropertyEditorList.SetObjInsp(AValue: TP3DObjectInspector);
begin
  FObjInsp:= AValue;
end;

procedure gP3DPropertyEditorList.SetLProp( AValue: TListProp );
begin
  Prop:= AValue;
end;

function gP3DPropertyEditorList.GetProp: TP3DPropertyAccess;
begin
  Result:= FProp;
end;

procedure gP3DPropertyEditorList.SetProp( AValue: TP3DPropertyAccess );
begin
  if ( AValue = Prop ) then
    exit;


  //if ( AValue is TListProp ) then // ClassName required because this doesn't work
  if (( Assigned( AValue )) and ( AValue.ClassName = TListProp.ClassName )) then
    begin
      FProp:= TListProp( AValue );
      Canvas.GetValidText( Prop.Name + ':', FLabel );
    end
  else
    begin
      FProp:= nil;
      Canvas.GetValidText( '<noprop>', FLabel );
    end;

  Update;
end;

procedure gP3DPropertyEditorList.Update;
var
  i: Integer;
  Text: String;
  NProp: TP3DPropertyAccess;
  Item: TP3DPropertyAccess;
begin
  Children.Clear();
  if ( Assigned( LProp )) then
    for i:= 0 to LProp.Count- 1 do
      begin
        Item:= LProp.GetArrayItemAsProp( i );
        //WriteLn( LProp.ClassName, '=', Item.AsString );
        if ( Assigned( Item )) then
          begin
            {NProp:= Item.Value.Properties.GetPropByName( NameProp );
            if ( Assigned( Prop )) then
              Text:= NProp.AsString
            else
              Text:= '<' + Item.ClassName + '>';
            }
            Children.Add( Item.AsString, Pointer( i ))
          end
        else
          Children.Add( '<None>', Item );
      end;
end;

procedure gP3DPropertyEditorList.MouseUp(mb1, mb2, mb3: Boolean; X, Y: Integer);
  {function GetIndex( AValue: Single ): Integer;
  var
    i: Integer;
  begin
    Result:= 0;
    for i:= 0 to Children.Count do
      begin
        Result:= i;
        if (( i < Children.Count ) and ( AValue <= Children[ i ].FRenderPos.Y )) then
          break;
      end;
  end;}

begin
  (*Canvas.GetValidText( P3DSymbolPlus, FPlusSymbol, P3DFontManager.Fonts[ 'Pascal3D-Symbols', Canvas.Font.Size ]);
  Canvas.GetValidText( P3DSymbolClose, FDeleteSymbol, P3DFontManager.Fonts[ 'Pascal3D-Symbols', Canvas.Font.Size ]);
  Canvas.GetValidText( P3DSymbolClear, FClearSymbol, P3DFontManager.Fonts[ 'Pascal3D-Symbols', Canvas.Font.Size ]);


  {if ( FDrawInsertion > -1 ) then
    begin
      Prop.Value.Insert( GetIndex( FDrawInsertion ), nil );
      FDrawInsertion:= -1;
      Update;
    end
  else} if ( Assigned( Selection )) then
    begin
      {if ( P3DRect( ClientRect.Right - FDeleteSymbol.Width - ScrollBarV.Width, Selection.FRenderPos.Y,
                    FDeleteSymbol.Width, FDeleteSymbol.Height ).PtInRect( vec2( X, Y + Scroll.Y ))) then
        begin // Delete
          LProp.Value.Delete( Children.IndexOf( Selection ));
          Children.Remove( Selection );
          FreeAndNil( FSelection );
        end
      else if ( P3DRect( ClientRect.Right - FDeleteSymbol.Width - FClearSymbol.Width - ScrollBarV.Width, Selection.FRenderPos.Y,
                    FClearSymbol.Width, FClearSymbol.Height ).PtInRect( vec2( X, Y + Scroll.Y ))) then
        begin // Clear
          LProp.Values[ Children.IndexOf( Selection )]:= nil;
          Selection.Data:= nil;
          Selection.Caption:= '<None>';
        end}
      if ( P3DRect( Canvas.Width - FOptionsSymbol.Width - ScrollBarV.Width, Selection.FRenderPos.Y - ScrollBarV.Position,
                          FOptionsSymbol.Width, FOptionsSymbol.Height ).PtInRect( vec2( X, Y ))) then
        begin // Options
          OptionsPopup.Width:= Canvas.Width;
          OptionsPopup.PopUp( Canvas.Left, round( Canvas.Top + Selection.FRenderPos.Y - ScrollBarV.Position + FOptionsSymbol.Height ));
        end
      else
        inherited MouseUp(mb1, mb2, mb3, X, Y);
    end
  else*)
    inherited MouseUp(mb1, mb2, mb3, X, Y);
end;

function gP3DPropertyEditorList.DrawListItem(p: TVec2; Node: TP3DListItem): TVec2;
begin
  Result:= inherited DrawListItem( p, Node );
  if ( Node = Selection ) then
    begin
      Canvas.Font.Color:= Red800;
//      Canvas.GetValidText( P3DSymbolClose, FDeleteSymbol, P3DFontManager.Fonts[ 'Pascal3D-Symbols', Canvas.Font.Size ]);
//      Canvas.RenderText( FDeleteSymbol, vec2( ClientRect.Right - FDeleteSymbol.Width - ScrollBarV.Width, p.Y ));
      Canvas.Font.Color:= Orange800;
//      Canvas.GetValidText( P3DSymbolClear, FClearSymbol, P3DFontManager.Fonts[ 'Pascal3D-Symbols', Canvas.Font.Size ]);
//      Canvas.RenderText( FClearSymbol, vec2( ClientRect.Right - FDeleteSymbol.Width - FClearSymbol.Width - ScrollBarV.Width, p.Y ));
      Canvas.GetValidText( P3DSymbolOptions, FOptionsSymbol, P3DFontManager.Fonts[ 'Pascal3D-Symbols', Canvas.Font.Size ]);
      Canvas.RenderText( FOptionsSymbol, vec2( Canvas.Rect.Width - FOptionsSymbol.Width - ScrollBarV.Width, p.Y - ScrollBarV.Position ));
    end;
end;

procedure gP3DPropertyEditorList.Draw;
begin
  inherited Draw;
  if ( FDrawInsertion > -1 ) then
    begin
      Canvas.Pen.Width:= 3;
      Canvas.RenderLine( vec2( ClientRect.Left, FDrawInsertion ), vec2( ClientRect.Right, FDrawInsertion ), Accent500 );
      Canvas.GetValidText( P3DSymbolPlus, FPlusSymbol, P3DFontManager.Fonts[ 'Pascal3D-Symbols', Canvas.Font.Size ]);
      Canvas.RenderCircle( vec2( Width / 2, FDrawInsertion ), FPlusSymbol.Width / 2, 8, Accent500 );
      Canvas.Font.Color:= vec4( 0 );
      Canvas.RenderText( FPlusSymbol, vec2(( Width - FPlusSymbol.Width ) / 2, FDrawInsertion - FPlusSymbol.Height / 2 ));
    end;
  if ( not Focused and Assigned( FLabel )) then
    begin
      Canvas.RenderRectShadow( vec2( 0 ), vec2( FLabel.Width - 5, FLabel.Height - 5 ), 10, Primary200 );
      Canvas.RenderText( FLabel, vec2( 0 ));
    end;
end;

procedure gP3DPropertyEditorList.MouseMove(X, Y: Integer);
var
  itm: TP3DListItem;
begin
  inherited MouseMove(X, Y);
  itm:= ItemAtPos( vec2( X + ScrollBarH.Position, Y + ScrollBarV.Position ));
  FDrawInsertion:= -1;
  if ( Assigned( itm )) then
    begin
      if ( Y <= itm.FRenderPos.Y + 1 ) then
        FDrawInsertion:= itm.FRenderPos.y
      else if ( Y >= itm.FRenderPos.Y + itm.Text.Height - 1 ) then
        FDrawInsertion:= itm.FRenderPos.y + itm.Text.Height;
    end;
end;


{ gP3DPropertyEditorInterfacedPersistentList }

constructor gP3DPropertyEditorInterfacedPersistentList.CreateBlock(const AData: TP3DLibrary;
  const AParent: TP3DGraphicControl);
begin
  inherited CreateBlock(AData, AParent);
  OptionsPopup:= TP3DPopupMenu.CreateBlock();
  OptionsPopup.Items.Add( 'Select' );
  OptionsPopup.Items.Add( 'Delete' );
  OptionsPopup.Items.Add( 'Clear' );
  OptionsPopup.OnItemClick:= @PopupClick;
end;

destructor gP3DPropertyEditorInterfacedPersistentList.Destroy;
begin
  if ( P3DDataBlockCache.IsValid( OptionsPopup )) then
    OptionsPopup.Free;

  inherited Destroy;
end;

procedure gP3DPropertyEditorInterfacedPersistentList.MouseUp(mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  if (( Assigned( Selection ) and ( P3DRect( Canvas.Width - FOptionsSymbol.Width - ScrollBarV.Width, Selection.FRenderPos.Y - ScrollBarV.Position,
                          FOptionsSymbol.Width, FOptionsSymbol.Height ).PtInRect( vec2( X, Y ))))) then
    begin // Options
      OptionsPopup.Width:= Canvas.Width;
      OptionsPopup.PopUp( Canvas.Left, round( Canvas.Top + Selection.FRenderPos.Y - ScrollBarV.Position + FOptionsSymbol.Height ));
    end
  else
    inherited MouseUp(mb1, mb2, mb3, X, Y);
end;

procedure gP3DPropertyEditorInterfacedPersistentList.PopupClick(Sender: TP3DMenuItem);
begin
  OptionsPopup.Hide;
  if (( Sender.Caption = 'Select' ) and ( Assigned( ObjInsp ))) then
    begin
      //WriteLn( Prop.Name, ' = ', TP3DPropAccessDataBlock( Prop ).Value.Name );
      ObjInsp.Obj:= LProp.AsValues[ Integer( Selection.Data )];
    end
  else if ( Sender.Caption = 'Browse' ) then
    ;
end;


{ TP3DPropertyEditorEnum }

function TP3DPropertyEditorEnum.GetObjInsp: TP3DObjectInspector;
begin
  Result:= FObjInsp;
end;

procedure TP3DPropertyEditorEnum.SetObjInsp(AValue: TP3DObjectInspector);
begin
  FObjInsp:= AValue;
end;

procedure TP3DPropertyEditorEnum.SetProp(AValue: TP3DPropertyAccess);
var
  Names: TStringList;
  i: Integer;
begin
  if ( AValue = Prop ) then
    exit;

  Items.Clear( True );

  FProp:= AValue;
  Canvas.Font.Name:= Font.Name;
  Canvas.Font.Size:= Font.Size;

  if ( Assigned( Prop ))  then
    begin
      Canvas.GetValidText( Prop.Name + ':', FLabel );
      Names:= Prop.GetNames;
      inherited SetCaption( Prop.AsString );
      if ( Assigned( Names )) then
        begin
          for i:= 0 to Names.Count - 1 do
            Items.Add( Names[ i ]);
          Names.Free;
        end;
    end
  else
    begin
      inherited SetCaption( '<None>' );
      Canvas.GetValidText( '<noprop>', FLabel );
    end;
end;

function TP3DPropertyEditorEnum.GetProp: TP3DPropertyAccess;
begin
  Result:= FProp;
end;

procedure TP3DPropertyEditorEnum.SetCaption(AValue: String);
begin
  inherited SetCaption(AValue);
  if ( Assigned( Prop )) then
    Prop.AsString:= AValue;
end;

constructor TP3DPropertyEditorEnum.CreateBlock(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited CreateBlock(AData, AParent);
  HAlignment:= haRight;
  Height:= 20;
end;

destructor TP3DPropertyEditorEnum.Destroy;
begin
  FLabel.Free;
  inherited Destroy;
end;

procedure TP3DPropertyEditorEnum.Draw;
begin
  inherited Draw;
  if ( not Focused ) then
    begin
      Canvas.RenderRect( vec2( 0 ), vec2( FLabel.Width - 5, Height ), Primary200 );
      Canvas.RenderRect( vec2( FLabel.Width - 5, 0 ), vec2( FLabel.Width + 5, Height ), Primary200, vec4( Primary200.XYZ, 0 ), vec4( Primary200.XYZ, 0 ), Primary200 );
      Canvas.RenderText( FLabel, vec2( 0 ));
    end;
end;

{ TP3DPropertyEditorDataBlock }

function TP3DPropertyEditorInterfacedPersistent.GetObjInsp: TP3DObjectInspector;
begin
  Result:= FObjInsp;
end;

procedure TP3DPropertyEditorInterfacedPersistent.SetObjInsp(AValue: TP3DObjectInspector);
begin
  FObjInsp:= AValue;
end;

procedure TP3DPropertyEditorInterfacedPersistent.SetProp(AValue: TP3DPropertyAccess);
begin
  Edit.Prop:= AValue;
end;

function TP3DPropertyEditorInterfacedPersistent.GetProp: TP3DPropertyAccess;
begin
  Result:= Edit.Prop;
end;


constructor TP3DPropertyEditorInterfacedPersistent.CreateBlock(const AData: TP3DLibrary;
  const AParent: TP3DGraphicControl);
begin
  inherited CreateBlock(AData, AParent);
  Height:= 20;
  Edit:= TP3DPropertyEditorString.CreateBlock( AData, Self );
  Edit.Align:= alClient;
  Button:= TP3DButton.CreateBlock( AData, Self );
  Button.Width:= 20;
  Button.Caption:= '...';
  Button.Align:= alRight;
  Button.OnMouseClick:= @ButtonClick;
  Popup:= TP3DPopupMenu.CreateBlock();
  Popup.Items.Add( 'Select' );
  Popup.Items.Add( 'Browse' );
  Popup.Items.Add( 'Clear' );
  Popup.OnItemClick:= @PopupClick;
end;

destructor TP3DPropertyEditorInterfacedPersistent.Destroy;
begin
  if ( P3DDataBlockCache.IsValid( Edit )) then
    Edit.Free;
  if ( P3DDataBlockCache.IsValid( Button )) then
    Button.Free;
  if ( P3DDataBlockCache.IsValid( Popup )) then
    Popup.Free;
  inherited Destroy;
end;

procedure TP3DPropertyEditorInterfacedPersistent.ButtonClick(Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  Popup.Width:= Canvas.Width;
  Popup.PopUp( round( Canvas.Left ), round( Canvas.Top + Canvas.Height ));
end;

procedure TP3DPropertyEditorInterfacedPersistent.PopupClick(Sender: TP3DMenuItem);
begin
  Popup.Hide;
  if (( Sender.Caption = 'Select' ) and ( Assigned( ObjInsp ))) then
    begin
      ObjInsp.Obj:= TP3DPropAccessStreamable( Prop ).AsValue
    end
  else if ( Sender.Caption = 'Browse' ) then

  else if ( Sender.Caption = 'Clear' ) then
    TP3DPropAccessStreamable( Prop ).AsValue:= nil;
end;


{ TP3DCustomPropertyEditorColor }

function TP3DCustomPropertyEditorColor.GetObjInsp: TP3DObjectInspector;
begin
  Result:= FObjInsp;
end;

procedure TP3DCustomPropertyEditorColor.SetObjInsp(AValue: TP3DObjectInspector);
begin
  FObjInsp:= AValue;
end;

procedure TP3DCustomPropertyEditorColor.SetProp(AValue: TP3DPropertyAccess);
begin
  FProp:= AValue;

  if ( Assigned( Prop )) then
    Canvas.GetValidText( Prop.Name + ':', FLabel )
  else
    Canvas.GetValidText( '<noprop>', FLabel );
end;

function TP3DCustomPropertyEditorColor.GetProp: TP3DPropertyAccess;
begin
  Result:= FProp;
end;

constructor TP3DCustomPropertyEditorColor.CreateBlock(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited CreateBlock(AData, AParent);
  Height:= 20;
  GetValueEvent:= @GetColorValue;
  SetValueEvent:= @SetColorValue;
end;

destructor TP3DCustomPropertyEditorColor.Destroy;
begin
  FLabel.Free;
  inherited Destroy;
end;

procedure TP3DCustomPropertyEditorColor.Draw;
begin
  inherited Draw;
  if (( not Focused ) and ( Assigned( FLabel ))) then
    begin
      Canvas.RenderRect( vec2( 0 ), vec2( FLabel.Width, Height ), Primary200 );
      Canvas.RenderRect( vec2( FLabel.Width, 0 ), vec2( FLabel.Width + 10, Height ), Primary200, vec4( Primary200.XYZ, 0 ), vec4( Primary200.XYZ, 0 ), Primary200 );
      Canvas.RenderText( FLabel, vec2( 0 ));
    end;
end;

function TP3DPropertyEditorColorRGB.GetColorValue(Sender: TP3DColorRGBAPicker): TP3DColorRGBA;
var
  Color: TP3DColorRGB;
begin
  if ( not Assigned( Prop )) then
    exit;
  Color.FromString( Prop.AsString );
  Result:= vec4( Color, 1 );
end;

procedure TP3DPropertyEditorColorRGB.SetColorValue(Sender: TP3DColorRGBAPicker; AValue: TP3DColorRGBA);
begin
  if ( not Assigned( Prop )) then
    exit;
  Prop.AsString:= AValue.XYZ.ToString();
end;


function TP3DPropertyEditorColorRGBA.GetColorValue(Sender: TP3DColorRGBAPicker): TP3DColorRGBA;
var
  Color: TP3DColorRGBA;
begin
  if ( not Assigned( Prop )) then
    exit;
  Color.FromString( Prop.AsString );
  Result:= Color;
end;

procedure TP3DPropertyEditorColorRGBA.SetColorValue(Sender: TP3DColorRGBAPicker; AValue: TP3DColorRGBA);
begin
  if ( not Assigned( Prop )) then
    exit;
  Prop.AsString:= AValue.ToString();
end;


{$ENDIF}
