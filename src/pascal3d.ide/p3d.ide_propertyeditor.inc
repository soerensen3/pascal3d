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
    function GetProp: TP3DPropertyAccess;
    procedure SetProp(AValue: TP3DPropertyAccess);
    property Prop: TP3DPropertyAccess read GetProp write SetProp;
  end;

{  IP3DPropertyEditorList = interface ( IP3DBaseInterface )
    ['{0E1AE98B-41B5-4ACB-9F41-021C7DDC5490}']
    function GetProp: TP3DPropertyAccessList;
    procedure SetProp(AValue: TP3DPropertyAccessList);
    property Prop: TP3DPropertyAccess read GetProp write SetProp;
  end;}

  { gP3DPropertyEditorVector }

  generic gP3DPropertyEditorVector < TVecType > = class ( TP3DButton, IP3DPropertyEditor )
    private
      FProp: TP3DPropertyPointer;
      FLabel: TP3DTextBmp;
      FDropDown: TP3DStreamablePointer;
      FVecEdit: TP3DStreamablePointer;

      function GetDropDown: TP3DCustomPopupMenu;
      function GetVecEdit: TP3DVectorEdit;
      procedure SetDropDown(AValue: TP3DCustomPopupMenu);
      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetProp: TP3DPropertyAccess;
      function GetValue( Sender: TP3DVectorEdit ): TVec4;
      procedure SetValue( Sender: TP3DVectorEdit; AValue: TVec4 );
      procedure SetVecEdit(AValue: TP3DVectorEdit);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure Draw; override;
      procedure MouseClick(mb1, mb2, mb3: Boolean; X, Y: Float); override;

    published
      property Prop: TP3DPropertyAccess read GetProp write SetProp;
      property DropDown: TP3DCustomPopupMenu read GetDropDown write SetDropDown;
      property VecEdit: TP3DVectorEdit read GetVecEdit write SetVecEdit;
  end;

  TP3DPropertyEditorVec2 = specialize gP3DPropertyEditorVector < TVec2 >;
  TP3DPropertyEditorVec3 = specialize gP3DPropertyEditorVector < TVec3 >;
  TP3DPropertyEditorVec4 = specialize gP3DPropertyEditorVector < TVec4 >;

  { TP3DCustomPropertyEditorColor }

  TP3DCustomPropertyEditorColor = class ( TP3DColorRGBAComboBox, IP3DPropertyEditor )
    protected
      FProp: TP3DPropertyPointer;
      FLabel: TP3DTextBmp;

      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetProp: TP3DPropertyAccess;
      function GetColorValue( Sender: TP3DColorRGBAPicker ): TP3DColorRGBA; virtual; abstract;
      procedure SetColorValue( Sender: TP3DColorRGBAPicker; AValue: TP3DColorRGBA ); virtual; abstract;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure Draw; override;

    published
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

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
  end;


  { TP3DPropertyEditorString }

  TP3DPropertyEditorString = class ( TP3DEdit, IP3DPropertyEditor )
    protected
      FProp: TP3DPropertyPointer;
      FLabel: TP3DTextBmp;

      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetProp: TP3DPropertyAccess;
      procedure SetFocused( AValue: Boolean ); override;
      procedure ReadValue;
      procedure ApplyValue;
      procedure PropOnChange( Sender: TObject );

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure Draw; override;
      procedure KeyboardAction; override;

    published
      property Prop: TP3DPropertyAccess read GetProp write SetProp;
  end;

  { TP3DPropertyEditorFloat }

  TP3DPropertyEditorFloat = class ( TP3DCustomValueEdit, IP3DPropertyEditor )
    protected
      FProp: TP3DPropertyPointer;
      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetProp: TP3DPropertyAccess;
      function GetValue: Single; override;
      procedure SetValue(AValue: Single); override;
      function GetValueName: String; override;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

    published
      property Prop: TP3DPropertyAccess read GetProp write SetProp;
  end;

  { TP3DPropertyEditorInt }

  TP3DPropertyEditorInt = class ( TP3DCustomValueEdit, IP3DPropertyEditor )
    protected
      FProp: TP3DPropertyPointer;
      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetProp: TP3DPropertyAccess;
      function GetValue: Single; override;
      procedure SetValue( AValue: Single ); override;
      function GetValueName: String; override;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

    published
      property Prop: TP3DPropertyAccess read GetProp write SetProp;
  end;

  { TP3DPropertyEditorEnum }

  TP3DPropertyEditorEnum = class ( TP3DComboBox, IP3DPropertyEditor )
    protected
      FProp: TP3DPropertyPointer;
      FLabel: TP3DTextBmp;
      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetProp: TP3DPropertyAccess;
      procedure SetCaption(AValue: String); override;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure Draw; override;

    published
      property Prop: TP3DPropertyAccess read GetProp write SetProp;
  end;

  { TP3DPropertyEditorBoolean }

  TP3DPropertyEditorBoolean = class ( TP3DCheckBox, IP3DPropertyEditor )
    protected
      FProp: TP3DPropertyPointer;
      FLabel: TP3DTextBmp;

      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetProp: TP3DPropertyAccess;
      procedure SetChecked(AValue: Boolean); override;
      function GetChecked: Boolean; override;
      function GetCaption: String; override;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;
      procedure Draw; override;

    published
      property Prop: TP3DPropertyAccess read GetProp write SetProp;
  end;

   { TP3DPropertyEditorStreamable }

   TP3DPropertyEditorStreamable = class ( TP3DGraphicControl, IP3DPropertyEditor )
     private
       FPopup: TP3DStreamablePointer;
       FDialogAdd: TP3DStreamablePointer;
       function GetDialogAdd: TP3DModalWindow;
       procedure SetDialogAdd(AValue: TP3DModalWindow);
       function GetButton: TP3DButton;
       function GetEdit: TP3DPropertyEditorString;
       function GetPopup: TP3DPopupMenu;
       procedure SetButton(AValue: TP3DButton);
       procedure SetEdit(AValue: TP3DPropertyEditorString);
       procedure SetPopup(AValue: TP3DPopupMenu);

     protected
       FButton: TP3DStreamablePointer;
       FEdit: TP3DStreamablePointer;

       procedure SetProp( AValue: TP3DPropertyAccess ); virtual;
       function GetProp: TP3DPropertyAccess; virtual;

     public
       constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
       destructor Destroy; override;
       procedure ButtonClick( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Float );
       procedure PopupClick( Sender: TP3DMenuItem );
       procedure ModalCallBack( Sender: TP3DModalWindow; Result: TP3DModalResult );

     published
       property Prop: TP3DPropertyAccess read GetProp write SetProp;
       property Edit: TP3DPropertyEditorString read GetEdit write SetEdit;
       property Button: TP3DButton read GetButton write SetButton;
       property Popup: TP3DPopupMenu read GetPopup write SetPopup;
       property DialogAdd: TP3DModalWindow read GetDialogAdd write SetDialogAdd;
   end;

   { TP3DPropertyEditorStreamableProperties }

   TP3DPropertyEditorStreamableProperties = class ( TP3DGroupBox, IP3DPropertyEditor )
     protected
       FProp: TP3DPropertyAccess;

       function GetProp: TP3DPropertyAccess;
       procedure SetProp(AValue: TP3DPropertyAccess);

     public
       procedure Update;

     published
       property Prop: TP3DPropertyAccess read GetProp write SetProp;
   end;


   { TP3DPropertyEditorTexture }

   TP3DPropertyEditorTexture = class( TP3DPropertyEditorStreamable )
     protected
       FPreview: TP3DStreamablePointer;

       function GetPreview: TP3DImage;
       procedure SetProp(AValue: TP3DPropertyAccess); override;

     public
       constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

       property Preview: TP3DImage read GetPreview;
   end;

  { gP3DPropertyEditorList }

  generic gP3DPropertyEditorList < TListProp > = class ( TP3DListView, IP3DPropertyEditor )
    private
      FMaxDisplay: Integer;
      FNameProp: String;

      procedure SetMaxDisplay(AValue: Integer);
      procedure SetNameProp(AValue: String);

    protected
      FProp: TP3DPropertyPointer;
      FLabel: TP3DTextBmp;
      FDeleteSymbol: TP3DTextBmp;
      FClearSymbol: TP3DTextBmp;
      FPlusSymbol: TP3DTextBmp;
      FOptionsSymbol: TP3DTextBmp;
      FDrawInsertion: Single;

      procedure SetLProp( AValue: TListProp );
      function GetLProp: TListProp;
      function GetProp: TP3DPropertyAccess;
      procedure SetProp(AValue: TP3DPropertyAccess);

    public
      procedure MouseUp(mb1, mb2, mb3: Boolean; X, Y: Float); override;
      function DrawListItem(p: TVec2; Node: TP3DListItem): TVec2; override;
      procedure Draw; override;
      procedure MouseMove(X, Y: Float); override;
      procedure Update; override;

      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      property LProp: TListProp read GetLProp write SetLProp;

    published
      property Prop: TP3DPropertyAccess read GetProp write SetProp;
      property NameProp: String read FNameProp write SetNameProp;
      property MaxDisplay: Integer read FMaxDisplay write SetMaxDisplay;
  end;

  { gP3DPropertyEditorStreamableList }

  generic gP3DPropertyEditorStreamableList < TListProp > = class ( specialize gP3DPropertyEditorList < TListProp> )
    protected
      FOptionsPopup: TP3DStreamablePointer;
      FDialogAdd: TP3DStreamablePointer;

      function GetDialogAdd: TP3DModalWindow;
      function GetOptionsPopup: TP3DPopupMenu;
      procedure SetDialogAdd(AValue: TP3DModalWindow);
      procedure SetOptionsPopup(AValue: TP3DPopupMenu);
      procedure ModalCallBack( Sender: TP3DModalWindow; Result: TP3DModalResult );

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure Update; override;
      procedure MouseUp(mb1, mb2, mb3: Boolean; X, Y: Float); override;
      procedure PopupClick( Sender: TP3DMenuItem );
      property OptionsPopup: TP3DPopupMenu read GetOptionsPopup write SetOptionsPopup;
      property DialogAdd: TP3DModalWindow read GetDialogAdd write SetDialogAdd;
  end;

  TP3DPropertyEditorStreamableList = specialize gP3DPropertyEditorStreamableList < TP3DStreamableListBase >;
  {TP3DPropertyEditorLibraryList = specialize gP3DPropertyEditorStreamableList < TP3DLibraryContainerList >;
  TP3DPropertyEditorDataBlockList = specialize gP3DPropertyEditorStreamableList < TP3DDataBlock >;
  TP3DPropertyEditorCameraList = specialize gP3DPropertyEditorStreamableList < TP3DCameraContainerList >;
  TP3DPropertyEditorLightList = specialize gP3DPropertyEditorStreamableList < TP3DLightContainerList >;
  TP3DPropertyEditorMaterialList = specialize gP3DPropertyEditorStreamableList < TP3DMaterialContainerList >;
  TP3DPropertyEditorMaterialMapList = specialize gP3DPropertyEditorStreamableList < TP3DMaterialMapContainerList >;
  TP3DPropertyEditorMeshList = specialize gP3DPropertyEditorStreamableList < TP3DPropAccessMeshList >;
  TP3DPropertyEditorSceneList = specialize gP3DPropertyEditorStreamableList < TP3DPropAccessSceneList >;
  TP3DPropertyEditorShaderList = specialize gP3DPropertyEditorStreamableList < TP3DPropAccessShaderList >;
  TP3DPropertyEditorTextureList = specialize gP3DPropertyEditorStreamableList < TP3DPropAccessTextureList >;
  TP3DPropertyEditorArmatureList = specialize gP3DPropertyEditorStreamableList < TP3DPropAccessArmatureList >;
  TP3DPropertyEditorFontBmpList = specialize gP3DPropertyEditorStreamableList < TP3DPropAccessFontBmpList >;
  TP3DPropertyEditorObjectModifierList = specialize gP3DPropertyEditorStreamableList < TP3DPropAccessObjectModifierList >;
  TP3DPropertyEditorGraphicControlList = specialize gP3DPropertyEditorStreamableList < TP3DPropAccessGraphicControlList >;
  TP3DPropertyEditorObjectList = specialize gP3DPropertyEditorStreamableList < TP3DPropAccessObjectList >;
  TP3DPropertyEditorActionList = specialize gP3DPropertyEditorStreamableList < TP3DPropAccessActionList >;
  TP3DPropertyEditorActionChannelList = specialize gP3DPropertyEditorStreamableList < TP3DPropAccessActionChannelList >;
  TP3DPropertyEditorActionKeyList = specialize gP3DPropertyEditorStreamableList < TP3DPropAccessActionKeyList >;}
  TP3DPropertyEditorVec2List = specialize gP3DPropertyEditorList < TP3DPropAccessVec2List >;
  TP3DPropertyEditorVec3List = specialize gP3DPropertyEditorList < TP3DPropAccessVec3List >;
  TP3DPropertyEditorVec4List = specialize gP3DPropertyEditorList < TP3DPropAccessVec4List >;
  TP3DPropertyEditorQuatList = specialize gP3DPropertyEditorList < TP3DPropAccessQuatList >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ gP3DPropertyEditorVector }

procedure gP3DPropertyEditorVector.SetProp(AValue: TP3DPropertyAccess);
begin
  if ( FProp.Prop = AValue ) then Exit;
  FProp.Prop:= AValue;

  //VecEdit.Margin.X:= FLabel.Width + 4;
  VecEdit.Update;
end;

function gP3DPropertyEditorVector.GetDropDown: TP3DCustomPopupMenu;
begin
  Result:= TP3DCustomPopupMenu( FDropDown.AsValue );
end;

function gP3DPropertyEditorVector.GetVecEdit: TP3DVectorEdit;
begin
  Result:= TP3DVectorEdit( FVecEdit.AsValue );
end;

procedure gP3DPropertyEditorVector.SetDropDown(AValue: TP3DCustomPopupMenu);
begin
  FDropDown.AsValue:= AValue;
end;

function gP3DPropertyEditorVector.GetProp: TP3DPropertyAccess;
begin
  Result:= FProp.Prop;
end;


function gP3DPropertyEditorVector.GetValue(Sender: TP3DVectorEdit): TVec4;
var
  Val: TVecType;
  s: String;
begin
  if ( not Assigned( Prop )) then
    exit( vec4( 0 ));

  s:= Prop.AsString;
  Val.FromString( s );
  case Length( TVecType.FCoord ) of
    2: Result:= vec4( Val[ 0 ], Val[ 1 ], 0, 0 );
    3: Result:= vec4( Val[ 0 ], Val[ 1 ], Val[ 2 ], 0 );
    4: Result:= vec4( Val[ 0 ], Val[ 1 ], Val[ 2 ], Val[ 3 ]);
  end;
end;

procedure gP3DPropertyEditorVector.SetValue(Sender: TP3DVectorEdit; AValue: TVec4);
begin
  if ( not Assigned( Prop )) then
    exit;
  case Length( TVecType.FCoord ) of
    2: Prop.AsString:= AValue.XY.ToString();
    3: Prop.AsString:= AValue.XYZ.ToString();
    4: Prop.AsString:= AValue.ToString();
  end;
end;

procedure gP3DPropertyEditorVector.SetVecEdit(AValue: TP3DVectorEdit);
begin
  FVecEdit.AsValue:= AValue;
end;

constructor gP3DPropertyEditorVector.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  Height:= 20;
  FDropDown:= TP3DStreamablePointer.Create( 'DropDown', TP3DCustomPopupMenu );
  Properties.Add( FDropDown );
  DropDown:= TP3DCustomPopupMenu.Create();
  DropDown.Visible:= False;
  DropDown.PageModeY:= pmExpand;
  FVecEdit:= TP3DStreamablePointer.Create( 'VecEdit', TP3DVectorEdit );
  Properties.Add( FVecEdit );
  VecEdit:= TP3DVectorEdit.Create( DropDown.Controls );
  VecEdit.Align:= alTop;
  VecEdit.GetValueEvent:= @GetValue;
  VecEdit.SetValueEvent:= @SetValue;
  FProp:= TP3DPropertyPointer.Create;

  VecEdit.EdCount:= Length( TVecType.FCoord );
end;

destructor gP3DPropertyEditorVector.Destroy;
begin
  FreeAndNil( FProp );
  FreeAndNil( FLabel );
  inherited Destroy;
end;

procedure gP3DPropertyEditorVector.Draw;
begin
  inherited Draw;
  with ( P3DThemeDefault ) do
    begin
      if ( Assigned( Prop )) then
        Canvas.GetValidText( Prop.Name + ': ' + Prop.AsString, FLabel )
      else
        Canvas.GetValidText( '<noprop>', FLabel );

      Canvas.RenderRect( vec2( 0 ), vec2( Width, Height ) - 1, Accent[ cp50 ]);
      Canvas.RenderLineRect( vec2( 0 ), vec2( Width, Height ) - 1, Accent[ cp900 ]);
      Canvas.RenderText( FLabel, vec2( 0, 0 ));
    end;
end;

procedure gP3DPropertyEditorVector.MouseClick(mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  inherited MouseClick(mb1, mb2, mb3, X, Y);
  DropDown.Width:= CanvasRect.Width;
  DropDown.PopUp( CanvasRect.Left, CanvasRect.Top + CanvasRect.Height );
end;


{ TP3DPropertyEditorString }

procedure TP3DPropertyEditorString.SetProp(AValue: TP3DPropertyAccess);
begin
  if FProp.Prop=AValue then Exit;

  if ( Assigned( Prop )) then
    Prop.OnChange:= nil;

  FProp.Prop:=AValue;

  Canvas.Font.Name:= Font.Name;
  Canvas.Font.Size:= Font.Size;
  if ( Assigned( Prop )) then
    begin
      Prop.OnChange:= @PropOnChange;
      ReadValue;
      Canvas.GetValidText( Prop.Name + ':', FLabel );
    end
  else
    Canvas.GetValidText( '<noprop>', FLabel );
end;

function TP3DPropertyEditorString.GetProp: TP3DPropertyAccess;
begin
  Result:= FProp.Prop;
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

procedure TP3DPropertyEditorString.PropOnChange(Sender: TObject);
begin
  ReadValue;
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

constructor TP3DPropertyEditorString.Create(const AOwner: IP3DStreamableContainer);
begin
  FProp:= TP3DPropertyPointer.Create;
  inherited Create( AOwner );

  HAlignment:= haRight;
  Height:= 20;
end;

destructor TP3DPropertyEditorString.Destroy;
begin
  if ( Assigned( Prop )) then
    Prop.OnChange:= nil;
  FreeAndNil( FProp );
  FreeAndNil( FLabel );
  inherited Destroy;
end;

procedure TP3DPropertyEditorString.Draw;
begin
  inherited Draw;
  if (( not Focused ) and ( Assigned( FLabel ))) then
    begin
      Canvas.RenderRect( vec2( 0 ), vec2( FLabel.Width, Height ), P3DThemeDefault.Primary[ cp200 ]);
      Canvas.RenderRect( vec2( FLabel.Width, 0 ), vec2( FLabel.Width + 10, Height ), P3DThemeDefault.Primary[ cp200 ], vec4( P3DThemeDefault.Primary[ cp200 ].XYZ, 0 ), vec4( P3DThemeDefault.Primary[ cp200 ].XYZ, 0 ), P3DThemeDefault.Primary[ cp200 ]);
      Canvas.RenderText( FLabel, vec2( 0 ));
    end;
end;

{ TP3DPropertyEditorInt }

procedure TP3DPropertyEditorInt.SetProp(AValue: TP3DPropertyAccess);
begin
  if ( Prop = AValue ) then
    exit;

  FProp.Prop:= AValue;
  UpdateValueText( GetValue );
  UpdateNameText( GetValueName );
end;

function TP3DPropertyEditorInt.GetProp: TP3DPropertyAccess;
begin
  Result:= FProp.Prop;
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

constructor TP3DPropertyEditorInt.Create(const AOwner: IP3DStreamableContainer);
begin
  FProp:= TP3DPropertyPointer.Create;
  inherited Create( AOwner );
  Height:= 20;
  Precision:= 0;
  FixedDigits:= 0;
  HAlignment:= haRight;
end;

destructor TP3DPropertyEditorInt.Destroy;
begin
  FreeAndNil( FProp );
  inherited Destroy;
end;

{ TP3DPropertyEditorFloat }

function TP3DPropertyEditorFloat.GetProp: TP3DPropertyAccess;
begin
  Result:= FProp.Prop;
end;

procedure TP3DPropertyEditorFloat.SetProp( AValue: TP3DPropertyAccess );
begin
  if ( Prop = AValue ) then
    exit;

  FProp.Prop:= AValue;
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

constructor TP3DPropertyEditorFloat.Create(const AOwner: IP3DStreamableContainer);
begin
  FProp:= TP3DPropertyPointer.Create;
  inherited Create( AOwner );
  Height:= 20;
  HAlignment:= haRight;
end;

destructor TP3DPropertyEditorFloat.Destroy;
begin
  FreeAndNil( FProp );
  inherited Destroy;
end;




{ TP3DPropertyEditorDataBlockList }

constructor gP3DPropertyEditorList.Create(const AOwner: IP3DStreamableContainer);
begin
  FProp:= TP3DPropertyPointer.Create;
  inherited Create( AOwner );

  Font.Size:= 20;
  Height:= 60;
  ScrollBarH.Visible:= True;
  NameProp:= 'Name';
  SetProp( nil );
  FMaxDisplay:= 10;
end;

destructor gP3DPropertyEditorList.Destroy;
begin
  FreeAndNil( FProp );
  FClearSymbol.Free;
  FDeleteSymbol.Free;
  FOptionsSymbol.Free;
  FPlusSymbol.Free;
  FLabel.Free;
  inherited Destroy;
end;

function gP3DPropertyEditorList.GetLProp: TListProp;
begin
  Result:= TListProp( FProp.Prop );
end;

procedure gP3DPropertyEditorList.SetNameProp(AValue: String);
begin
  if FNameProp=AValue then Exit;
  FNameProp:=AValue;
  Update;
end;

procedure gP3DPropertyEditorList.SetMaxDisplay(AValue: Integer);
begin
  if FMaxDisplay=AValue then Exit;
  FMaxDisplay:=AValue;
  Update;
end;

procedure gP3DPropertyEditorList.SetLProp( AValue: TListProp );
begin
  Prop:= AValue;
end;

function gP3DPropertyEditorList.GetProp: TP3DPropertyAccess;
begin
  Result:= FProp.Prop;
end;

procedure gP3DPropertyEditorList.SetProp( AValue: TP3DPropertyAccess );
begin
  if ( AValue = Prop ) then
    exit;
  //if ( AValue is TListProp ) then // ClassName required because this doesn't work
  if (( Assigned( AValue )) and ( AValue.InheritsFrom( TListProp ))) then
    begin
      FProp.Prop:= TListProp( AValue );
      Canvas.GetValidText( Prop.Name + ':', FLabel );
    end
  else
    begin
      FProp.Prop:= nil;
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
    for i:= 0 to Min( MaxDisplay, LProp.Count ) - 1 do
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
  inherited Update;
end;

procedure gP3DPropertyEditorList.MouseUp(mb1, mb2, mb3: Boolean; X, Y: Float);
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
      Canvas.RenderText( FOptionsSymbol, vec2( Controls.PageSize.X - FOptionsSymbol.Width - ScrollBarV.Width, p.Y - ScrollBarV.Position ));
    end;
end;

procedure gP3DPropertyEditorList.Draw;
begin
  inherited Draw;
  if ( FDrawInsertion > -1 ) then
    begin
      Canvas.Pen.Width:= 3;
      Canvas.RenderLine( vec2( ClientRect.Left, FDrawInsertion ), vec2( ClientRect.Right, FDrawInsertion ), P3DThemeDefault.Accent[ cp500 ]);
      Canvas.GetValidText( P3DSymbolPlus, FPlusSymbol, P3DFontManager.Fonts[ 'Pascal3D-Symbols', Canvas.Font.Size ]);
      Canvas.RenderCircle( vec2( Width / 2, FDrawInsertion ), FPlusSymbol.Width / 2, 8, P3DThemeDefault.Accent[ cp500 ]);
      Canvas.Font.Color:= vec4( 0 );
      Canvas.RenderText( FPlusSymbol, vec2(( Width - FPlusSymbol.Width ) / 2, FDrawInsertion - FPlusSymbol.Height / 2 ));
    end;
  if ( Selection = nil ) then
    begin
      Canvas.GetValidText( P3DSymbolOptions, FOptionsSymbol, P3DFontManager.Fonts[ 'Pascal3D-Symbols', Canvas.Font.Size ]);
      Canvas.RenderText( FOptionsSymbol, vec2( Controls.PageSize.X - FOptionsSymbol.Width - ScrollBarV.Width, 0 - ScrollBarV.Position ));
    end;

  if ( not Focused and Assigned( FLabel )) then
    begin
      Canvas.RenderRectShadow( vec2( 0 ), vec2( FLabel.Width - 5, FLabel.Height - 5 ), 10, P3DThemeDefault.Primary[ cp200 ]);
      Canvas.RenderText( FLabel, vec2( 0 ));
    end;
end;

procedure gP3DPropertyEditorList.MouseMove(X, Y: Float);
var
  itm: TP3DListItem;
begin
  inherited MouseMove(X, Y);
  itm:= ItemAtPos( vec2( X + ScrollBarH.Position, Y + ScrollBarV.Position ));
  FDrawInsertion:= -1;
  if ( Assigned( itm )) then
    begin
      if ( Y <= itm.RenderPos.Y + 1 ) then
        FDrawInsertion:= itm.RenderPos.y
      else if ( Y >= itm.RenderPos.Y + itm.Text.Height - 1 ) then
        FDrawInsertion:= itm.RenderPos.y + itm.Text.Height;
    end;
end;


{ gP3DPropertyEditorStreamableList }

function gP3DPropertyEditorStreamableList.GetOptionsPopup: TP3DPopupMenu;
begin
  Result:= TP3DPopupMenu( FOptionsPopup.AsValue );
end;

function gP3DPropertyEditorStreamableList.GetDialogAdd: TP3DModalWindow;
begin
  Result:= TP3DDialogStreamableClass( FDialogAdd.AsValue );
end;

procedure gP3DPropertyEditorStreamableList.SetDialogAdd(AValue: TP3DModalWindow);
begin
  FDialogAdd.AsValue:= AValue;
end;

procedure gP3DPropertyEditorStreamableList.SetOptionsPopup(AValue: TP3DPopupMenu);
begin
  FOptionsPopup.AsValue:= AValue;
end;

procedure gP3DPropertyEditorStreamableList.Update;
begin
  inherited Update;
  if ( Assigned( LProp )) then
    if ( LProp.InheritsFrom( TP3DStreamableContainerListBase )) then
      begin
        DialogAdd.Free;
        DialogAdd:= TP3DDialogStreamableClass.Create();
        DialogAdd.WidthHeight:= vec2( 600, 400 );
        TP3DDialogStreamableClass( DialogAdd ).ClassFactory.Factory:= P3DClassFactory;

        TP3DDialogStreamableClass( DialogAdd ).ClassFactory.BaseClass:= LProp.ItemClass;
      end
    else
      begin
        DialogAdd.Free;
        DialogAdd:= TP3DDialogStreamableBrowseInstances.Create();
        DialogAdd.WidthHeight:= vec2( 600, 400 );

        TP3DDialogStreamableBrowseInstances( DialogAdd ).Objects.Obj:= P3DData;
      end;
end;

procedure gP3DPropertyEditorStreamableList.ModalCallBack(Sender: TP3DModalWindow; Result: TP3DModalResult);
var
  obj: IP3DStreamableContainer;
begin
  if ( Result = mrOk ) then
    begin
      if ( LProp.InheritsFrom( TP3DStreamableContainerListBase )) then
        TP3DStreamableClass( TP3DDialogStreamableClass( DialogAdd ).ClassFactory.Selection.Data ).Create( TP3DStreamableContainerListBase( LProp ))
      else if ( LProp.InheritsFrom( TP3DStreamableList )) then
        TP3DStreamableList( LProp ).Add( TP3DStreamable( TP3DDialogStreamableBrowseInstances( DialogAdd ).Objects.Selection.Data ));
      Update;
    end;
end;

constructor gP3DPropertyEditorStreamableList.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );

  FOptionsPopup:= TP3DStreamablePointer.Create( 'OptionsPopup', TP3DPopupMenu );
  Properties.Add( FOptionsPopup );
  FDialogAdd:= TP3DStreamablePointer.Create( 'DialogAdd', TP3DModalWindow );
  Properties.Add( FDialogAdd );
  OptionsPopup:= TP3DPopupMenu.Create();
  OptionsPopup.Items.Add( 'Select' );
  OptionsPopup.Items.Add( 'Delete' );
  OptionsPopup.Items.Add( 'Clear' );
  OptionsPopup.Items.Add( 'Add' );
  OptionsPopup.OnItemClick:= @PopupClick;
end;

destructor gP3DPropertyEditorStreamableList.Destroy;
begin
  OptionsPopup.Free;
  DialogAdd.Free;

  inherited Destroy;
end;

procedure gP3DPropertyEditorStreamableList.MouseUp(mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  if (( Assigned( Selection ) and ( P3DRect( CanvasRect.Width - FOptionsSymbol.Width - ScrollBarV.Width, Selection.RenderPos.Y - ScrollBarV.Position,
                          FOptionsSymbol.Width, FOptionsSymbol.Height ).PtInRect( vec2( X, Y ))))) then
    begin // Options
      OptionsPopup.Width:= CanvasRect.Width;
      OptionsPopup.PopUp( CanvasRect.Left, CanvasRect.Top + Selection.RenderPos.Y - ScrollBarV.Position + FOptionsSymbol.Height );
    end
  else if ( P3DRect( CanvasRect.Width - FOptionsSymbol.Width, 0,
                     FOptionsSymbol.Width, FOptionsSymbol.Height ).PtInRect( vec2( X, Y ))) then
    begin // Options
      OptionsPopup.Width:= CanvasRect.Width;
      OptionsPopup.PopUp( CanvasRect.Left, CanvasRect.Top + FOptionsSymbol.Height );
    end
  else
    inherited MouseUp(mb1, mb2, mb3, X, Y);
end;

procedure gP3DPropertyEditorStreamableList.PopupClick(Sender: TP3DMenuItem);
begin
  OptionsPopup.Hide;
  if (( Sender.Caption = 'Select' ) and ( Assigned( P3DMainIntf ))) then
    begin
      //WriteLn( Prop.Name, ' = ', TP3DPropAccessDataBlock( Prop ).Value.Name );
      if ( Assigned( Selection )) then
        P3DMainIntf.SetSelection( Self, LProp[ Integer( Selection.Data )])
//        P3DMainIntf.SetSelection( Self, TP3DStreamable( Selection.Data ))
      else
        P3DMainIntf.SetSelection( Self, nil );
    end
  else if ( Sender.Caption = 'Add' ) then
    DialogAdd.ShowModal( @ModalCallBack );
end;


{ TP3DPropertyEditorEnum }

procedure TP3DPropertyEditorEnum.SetProp(AValue: TP3DPropertyAccess);
var
  Names: TStringList;
  i: Integer;
begin
  if ( AValue = Prop ) then
    exit;

  Items.Clear;

  FProp.Prop:= AValue;
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
  Result:= FProp.Prop;
end;

procedure TP3DPropertyEditorEnum.SetCaption(AValue: String);
begin
  inherited SetCaption(AValue);
  if ( Assigned( Prop )) then
    Prop.AsString:= AValue;
end;

constructor TP3DPropertyEditorEnum.Create(const AOwner: IP3DStreamableContainer);
begin
  FProp:= TP3DPropertyPointer.Create;
  inherited Create( AOwner );
  HAlignment:= haRight;
  Height:= 20;
end;

destructor TP3DPropertyEditorEnum.Destroy;
begin
  FreeAndNil( FProp );
  FLabel.Free;
  inherited Destroy;
end;

procedure TP3DPropertyEditorEnum.Draw;
begin
  inherited Draw;
  if ( not Focused ) then
    begin
      if ( Assigned( Prop )) then
        Canvas.GetValidText( Prop.Name + ':', FLabel )
      else
        Canvas.GetValidText( 'not assigned!', FLabel );
      Canvas.RenderRect( vec2( 0 ), vec2( FLabel.Width - 5, Height ), P3DThemeDefault.Primary[ cp200 ]);
      Canvas.RenderRect( vec2( FLabel.Width - 5, 0 ), vec2( FLabel.Width + 5, Height ), P3DThemeDefault.Primary[ cp200 ], vec4( P3DThemeDefault.Primary[ cp200 ].XYZ, 0 ), vec4( P3DThemeDefault.Primary[ cp200 ].XYZ, 0 ), P3DThemeDefault.Primary[ cp200 ]);
      Canvas.RenderText( FLabel, vec2( 0 ));
    end;
end;

{ TP3DPropertyEditorBoolean }

procedure TP3DPropertyEditorBoolean.SetProp(AValue: TP3DPropertyAccess);
begin
  if ( AValue = Prop ) then
    exit;

  FProp.Prop:= AValue;

  if ( Assigned( AValue )) then
    begin
      FChecked:= Boolean( StrToInt( AValue.AsString ));
      Canvas.GetValidText( Prop.Name + ':', FLabel );
    end
  else
    Canvas.GetValidText( '<noprop>', FLabel );
end;

function TP3DPropertyEditorBoolean.GetProp: TP3DPropertyAccess;
begin
  Result:= FProp.Prop;
end;

procedure TP3DPropertyEditorBoolean.SetChecked(AValue: Boolean);
begin
  if ( Assigned( Prop )) then
    Prop.AsString:= IntToStr( Integer( AValue ));
end;

function TP3DPropertyEditorBoolean.GetChecked: Boolean;
begin
  if ( Assigned( Prop )) then
    Result:= Boolean( StrToInt( Prop.AsString ))
  else
    Result:= False;
end;

function TP3DPropertyEditorBoolean.GetCaption: String;
begin
  {if ( Assigned( Prop )) then
    Result:= Prop.Name
  else
    Result:= '<None>';}
  Result:= ''; //We want the caption to align left
end;

constructor TP3DPropertyEditorBoolean.Create(const AOwner: IP3DStreamableContainer);
begin
  FProp:= TP3DPropertyPointer.Create;
  inherited Create( AOwner );
  Height:= 20;
  HAlignment:= haRight;
end;

destructor TP3DPropertyEditorBoolean.Destroy;
begin
  FreeAndNil( FProp );
  FreeAndNil( FLabel );
  inherited Destroy;
end;

procedure TP3DPropertyEditorBoolean.Draw;
begin
  inherited Draw;
  begin
    Canvas.RenderRect( vec2( 0 ), vec2( FLabel.Width - 5, Height ), P3DThemeDefault.Primary[ cp200 ]);
    Canvas.RenderRect( vec2( FLabel.Width - 5, 0 ), vec2( FLabel.Width + 5, Height ), P3DThemeDefault.Primary[ cp200 ], vec4( P3DThemeDefault.Primary[ cp200 ].XYZ, 0 ), vec4( P3DThemeDefault.Primary[ cp200 ].XYZ, 0 ), P3DThemeDefault.Primary[ cp200 ] );
    Canvas.RenderText( FLabel, vec2( 0 ));
  end;
end;


{ TP3DPropertyEditorDataBlock }

function TP3DPropertyEditorStreamable.GetDialogAdd: TP3DModalWindow;
begin
  Result:= TP3DModalWindow( FDialogAdd.AsValue );
end;

procedure TP3DPropertyEditorStreamable.SetDialogAdd(AValue: TP3DModalWindow);
begin
  FDialogAdd.AsValue:= AValue;
end;

function TP3DPropertyEditorStreamable.GetButton: TP3DButton;
begin
  Result:= TP3DButton( FButton.AsValue );
end;

function TP3DPropertyEditorStreamable.GetEdit: TP3DPropertyEditorString;
begin
  Result:= TP3DPropertyEditorString( FEdit.AsValue );
end;

function TP3DPropertyEditorStreamable.GetPopup: TP3DPopupMenu;
begin
  Result:= TP3DPopupMenu( FPopup.AsValue );
end;

procedure TP3DPropertyEditorStreamable.SetButton(AValue: TP3DButton);
begin
  FButton.AsValue:= AValue;
end;

procedure TP3DPropertyEditorStreamable.SetEdit(AValue: TP3DPropertyEditorString);
begin
  FEdit.AsValue:= AValue;
end;

procedure TP3DPropertyEditorStreamable.SetPopup(AValue: TP3DPopupMenu);
begin
  FPopup.AsValue:= AValue;
end;

procedure TP3DPropertyEditorStreamable.SetProp(AValue: TP3DPropertyAccess);
begin
  Edit.Prop:= AValue;
  if ( Assigned( AValue )) then
    if ( AValue.InheritsFrom( TP3DStreamableContainer )) then
      begin
        DialogAdd.Free;
        DialogAdd:= TP3DDialogStreamableClass.Create();
        DialogAdd.WidthHeight:= vec2( 600, 400 );
        TP3DDialogStreamableClass( DialogAdd ).ClassFactory.Factory:= P3DClassFactory;

        TP3DDialogStreamableClass( DialogAdd ).ClassFactory.BaseClass:= TP3DStreamableContainer( Prop ).ItemClass;
      end
    else
      begin
        DialogAdd.Free;
        DialogAdd:= TP3DDialogStreamableBrowseInstances.Create();
        DialogAdd.WidthHeight:= vec2( 600, 400 );

        TP3DDialogStreamableBrowseInstances( DialogAdd ).Objects.Obj:= P3DData;
      end;
end;

function TP3DPropertyEditorStreamable.GetProp: TP3DPropertyAccess;
begin
  Result:= Edit.Prop;
end;


constructor TP3DPropertyEditorStreamable.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  Height:= 20;
  FEdit:= TP3DStreamablePointer.Create( 'Edit', TP3DPropertyEditorString );
  FButton:= TP3DStreamablePointer.Create( 'Button', TP3DButton );
  FPopup:= TP3DStreamablePointer.Create( 'Popup', TP3DPopupMenu );
  Properties.Add([ FEdit, FButton, FPopup ]);
  Edit:= TP3DPropertyEditorString.Create( Self.Controls );
  Edit.Align:= alClient;
  Button:= TP3DButton.Create( Self.Controls );
  Button.Width:= 20;
  Button.Caption:= '...';
  Button.Align:= alRight;
  Button.OnMouseClick:= @ButtonClick;
  Popup:= TP3DPopupMenu.Create();
  Popup.AutoWidth:= False;
  Popup.Alignment:= haRight;
  Popup.Items.Add( 'Select' );
  Popup.Items.Add( 'Browse' );
  Popup.Items.Add( 'Clear' );
  Popup.OnItemClick:= @PopupClick;
  FDialogAdd:= TP3DStreamablePointer.Create( 'DialogAdd', TP3DModalWindow );
  Properties.Add( FDialogAdd );
end;

destructor TP3DPropertyEditorStreamable.Destroy;
begin
//  if ( P3DDataBlockCache.IsValid( Edit )) then
//    Edit.Free;
//  if ( P3DDataBlockCache.IsValid( Button )) then
//    Button.Free;
//  if ( P3DDataBlockCache.IsValid( Popup )) then
    Popup.Free;
  inherited Destroy;
end;

procedure TP3DPropertyEditorStreamable.ButtonClick(Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  Popup.Width:= CanvasRect.Width;
  Popup.PopUp( CanvasRect.Left, CanvasRect.Top + CanvasRect.Height );
end;

procedure TP3DPropertyEditorStreamable.PopupClick(Sender: TP3DMenuItem);
begin
  Popup.Hide;
  if (( Sender.Caption = 'Select' ) and ( Assigned( P3DMainIntf ))) then
    P3DMainIntf.SetSelection( Self, TP3DPropAccessStreamable( Prop ).AsValue )
  else if ( Sender.Caption = 'Browse' ) then
    DialogAdd.ShowModal( @ModalCallBack )
  else if ( Sender.Caption = 'Clear' ) then
    TP3DPropAccessStreamable( Prop ).AsValue:= nil;
end;

procedure TP3DPropertyEditorStreamable.ModalCallBack(Sender: TP3DModalWindow; Result: TP3DModalResult);
begin
  if ( Result = mrOk ) then
    begin
      if ( Prop.InheritsFrom( TP3DStreamableContainer )) then
        TP3DStreamableClass( TP3DDialogStreamableClass( DialogAdd ).ClassFactory.Selection.Data ).Create( TP3DStreamableContainer( Prop ))
      else if ( Prop.InheritsFrom( TP3DStreamablePointer )) then
        TP3DStreamablePointer( Prop ).AsValue:= TP3DStreamable( TP3DDialogStreamableBrowseInstances( DialogAdd ).Objects.Selection.Data );
    end;
end;


{ TP3DPropertyEditorStreamableProperties }

function TP3DPropertyEditorStreamableProperties.GetProp: TP3DPropertyAccess;
begin
  Result:= FProp;
end;

procedure TP3DPropertyEditorStreamableProperties.SetProp(AValue: TP3DPropertyAccess);
begin
  if FProp=AValue then Exit;
  FProp:=AValue;
  Update;
end;

procedure TP3DPropertyEditorStreamableProperties.Update;
  function AddCategory( Prop: TP3DPropertyAccess; var Cat: TP3DOIEditorGroup ):TP3DOIEditorGroup;
  begin
    if ( not Assigned( Cat ) or ( Cat.Caption <> Prop.Category )) then
      begin
        Cat:= TP3DOIEditorGroup.Create( Self.Controls );
        Cat.Align:= alTop;
        Cat.Caption:= Prop.Category;
        Cat.PageModeY:= pmExpand;
      end;
    Result:= Cat;
  end;

var
  SProp: IP3DPropAccessStreamable;
  Cat: TP3DOIEditorGroup = nil;
  _Prop: TP3DPropertyAccess;
  Obj: TP3DStreamable;

begin
  Controls.Clear;
  PageModeY:= pmExpand;
  if ( Assigned( Prop )) then begin
    if ( Prop is IP3DPropAccessStreamable ) then begin
      SProp:= Prop as IP3DPropAccessStreamable;
      Obj:= SProp.GetValueAsStreamable;
      if ( Assigned( Obj )) then begin
        Caption:= Prop.Name;
        for _Prop in Obj.Properties do
          AddCategory( _Prop, Cat ).AddPropertyEditor( _Prop );
      end else
        Caption:= Prop.Name + ' = None';
    end else
      Caption:= Prop.Name + ' is not of type TP3DStreamable!';

  end else
    Caption:= '<no prop assigned!>';
end;



{ TP3DCustomPropertyEditorColor }

procedure TP3DCustomPropertyEditorColor.SetProp(AValue: TP3DPropertyAccess);
begin
  FProp.Prop:= AValue;

  if ( Assigned( Prop )) then
    Canvas.GetValidText( Prop.Name + ':', FLabel )
  else
    Canvas.GetValidText( '<noprop>', FLabel );
end;

function TP3DCustomPropertyEditorColor.GetProp: TP3DPropertyAccess;
begin
  Result:= FProp.Prop;
end;

constructor TP3DCustomPropertyEditorColor.Create(const AOwner: IP3DStreamableContainer);
begin
  FProp:= TP3DPropertyPointer.Create;
  inherited Create( AOwner );

  Height:= 20;
  GetValueEvent:= @GetColorValue;
  SetValueEvent:= @SetColorValue;
end;

destructor TP3DCustomPropertyEditorColor.Destroy;
begin
  FreeAndNil( FProp );
  FLabel.Free;
  inherited Destroy;
end;

procedure TP3DCustomPropertyEditorColor.Draw;
begin
  inherited Draw;
  if (( not Focused ) and ( Assigned( FLabel ))) then
    begin
      Canvas.RenderRect( vec2( 0 ), vec2( FLabel.Width, Height ), P3DThemeDefault.Primary[ cp200 ]);
      Canvas.RenderRect( vec2( FLabel.Width, 0 ), vec2( FLabel.Width + 10, Height ), P3DThemeDefault.Primary[ cp200 ], vec4( P3DThemeDefault.Primary[ cp200 ].XYZ, 0 ), vec4( P3DThemeDefault.Primary[ cp200 ].XYZ, 0 ), P3DThemeDefault.Primary[ cp200 ]);
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

constructor TP3DPropertyEditorColorRGBA.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  ColorPicker.EditCount:= 4;
end;


{ TP3DPropertyEditorTexture }

function TP3DPropertyEditorTexture.GetPreview: TP3DImage;
begin
  Result:= TP3DImage( FPreview.AsValue );
end;

procedure TP3DPropertyEditorTexture.SetProp(AValue: TP3DPropertyAccess);
begin
  inherited SetProp(AValue);
  if ( Prop is TP3DPropAccessStreamable and ( TP3DPropAccessStreamable( Prop ).ItemClass = TP3DTexture )) then
    Preview.Texture:= TP3DTexture( TP3DPropAccessStreamable( Prop ).AsValue )
  else
    Preview.Texture:= nil;
end;

constructor TP3DPropertyEditorTexture.Create(const AOwner: IP3DStreamableContainer);
var
  Container: TP3DGraphicControl;
begin
  inherited Create(AOwner);
  FPreview:= TP3DStreamablePointer.Create( 'Preview', TP3DImage );
  FPreview.AsValue:= TP3DImage.Create( Controls );
  with ( Preview ) do
    begin
      Align:= alTop;
      Height:= 100;
    end;
  Properties.Add( FPreview );
  Container:= TP3DGraphicControl.Create( Controls );
  Container.Align:= alTop;
  Container.Height:= 20;
  Button.Parent:= Container;
  Edit.Parent:= Container;

  PageModeY:= pmExpand;
end;



{$ENDIF}
