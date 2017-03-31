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

  IP3DPropertyEditor = interface
    ['{A6524E5C-6F92-4CF2-BBF4-FB68F1436A32}']
    function GetProp: TP3DPropertyAccess;
    procedure SetProp(AValue: TP3DPropertyAccess);
    property Prop: TP3DPropertyAccess read GetProp write SetProp;
  end;

{  IP3DPropertyEditorList = interface
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

      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetProp: TP3DPropertyAccess;
      function GetValue: TVec4; override;
      procedure SetValue( AValue: TVec4 ); override;

      property GetValueEvent;
      property SetValueEvent;

    public
      constructor Create(const AData: TP3DLibrary=nil; const AParent: TP3DGraphicControl=nil); override;
      destructor Destroy; override;

      procedure Draw; override;

    published
      property Prop: TP3DPropertyAccess read GetProp write SetProp;
  end;

  TP3DPropertyEditorVec2 = specialize gP3DPropertyEditorVector < TVec2 >;
  TP3DPropertyEditorVec3 = specialize gP3DPropertyEditorVector < TVec3 >;
  TP3DPropertyEditorVec4 = specialize gP3DPropertyEditorVector < TVec4 >;

  { TP3DPropertyEditorString }

  TP3DPropertyEditorString = class ( TP3DEdit, IP3DPropertyEditor )
    protected
      FProp: TP3DPropertyAccess;
      FLabel: TP3DTextBmp;

      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetProp: TP3DPropertyAccess;
      procedure SetFocused( AValue: Boolean ); override;
      procedure ReadValue;
      procedure ApplyValue;

    public
      constructor Create(const AData: TP3DLibrary=nil; const AParent: TP3DGraphicControl=nil); override;
      destructor Destroy; override;

      procedure Draw; override;
      procedure KeyboardAction; override;

    published
      property Prop: TP3DPropertyAccess read GetProp write SetProp;
  end;

  { TP3DPropertyEditorFloat }

  TP3DPropertyEditorFloat = class ( TP3DCustomValueEdit, IP3DPropertyEditor )
    protected
      FProp: TP3DPropertyAccess;
      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetProp: TP3DPropertyAccess;
      function GetValue: Single; override;
      procedure SetValue(AValue: Single); override;
      function GetValueName: String; override;

    public
      constructor Create(const AData: TP3DLibrary=nil; const AParent: TP3DGraphicControl=nil); override;

    published
      property Prop: TP3DPropertyAccess read GetProp write SetProp;
  end;

  { TP3DPropertyEditorInt }

  TP3DPropertyEditorInt = class ( TP3DCustomValueEdit, IP3DPropertyEditor )
    protected
      FProp: TP3DPropertyAccess;
      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetProp: TP3DPropertyAccess;
      function GetValue: Single; override;
      procedure SetValue( AValue: Single ); override;
      function GetValueName: String; override;

    public
      constructor Create( const AData: TP3DLibrary=nil; const AParent: TP3DGraphicControl=nil ); override;

    published
      property Prop: TP3DPropertyAccess read GetProp write SetProp;
  end;

  { TP3DPropertyEditorEnum }

  TP3DPropertyEditorEnum = class ( TP3DComboBox, IP3DPropertyEditor )
    protected
      FProp: TP3DPropertyAccess;
      FLabel: TP3DTextBmp;
      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetProp: TP3DPropertyAccess;
      procedure SetCaption(AValue: String); override;

    public
      constructor Create( const AData: TP3DLibrary=nil; const AParent: TP3DGraphicControl=nil ); override;
      procedure Draw; override;

    published
      property Prop: TP3DPropertyAccess read GetProp write SetProp;
  end;

  { TP3DPropertyEditorBoolean }

  TP3DPropertyEditorBoolean = class ( TP3DCheckBox, IP3DPropertyEditor )
    protected
      FProp: TP3DPropertyAccess;

      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetProp: TP3DPropertyAccess;
      procedure SetChecked(AValue: Boolean); override;
      function GetChecked: Boolean; override;
      function GetCaption: String; override;

    public
      constructor Create( const AData: TP3DLibrary=nil; const AParent: TP3DGraphicControl=nil ); override;

    published
      property Prop: TP3DPropertyAccess read GetProp write SetProp;
  end;


  { gP3DPropertyEditorList }

  generic gP3DPropertyEditorList < TList, TListData > = class ( TP3DListView, IP3DPropertyEditor )
    protected
      type TListProp = specialize gP3DListPropertyAccess < TList, TListData >;
    protected
      FProp: TListProp;
      FLabel: TP3DTextBmp;
      FDeleteSymbol: TP3DTextBmp;
      FClearSymbol: TP3DTextBmp;
      FPlusSymbol: TP3DTextBmp;
      FDrawInsertion: Single;

      procedure SetLProp( AValue: TListProp );
      function GetLProp: TListProp;
      function GetProp: TP3DPropertyAccess;
      procedure SetProp(AValue: TP3DPropertyAccess);
      procedure Update;

    public
      procedure MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer); override;
      function DrawListItem(p: TVec2; Node: TP3DListItem): TVec2; override;
      procedure Draw; override;
      procedure MouseMove(X, Y: Integer); override;

      constructor Create(const AData: TP3DLibrary=nil; const AParent: TP3DGraphicControl=nil); override;

    published
      property LProp: TListProp read GetLProp write SetLProp;
      property Prop: TP3DPropertyAccess read GetProp write SetProp;
  end;

  TP3DPropertyEditorDataBlockList = specialize gP3DPropertyEditorList < TP3DDataBlockList, TP3DDataBlock >;

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

  MarginTL.X:= FLabel.Width + 4;
  Update;
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

constructor gP3DPropertyEditorVector.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create(AData, AParent);
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

  Canvas.RenderText( FLabel, vec2( 0, 0 ));
end;


{ TP3DPropertyEditorString }

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

  if ( Assigned( Prop )) then
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

constructor TP3DPropertyEditorString.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create(AData, AParent);
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
  if ( not Focused ) then
    Canvas.RenderText( FLabel, vec2( 0 ));
end;

{ TP3DPropertyEditorInt }

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
    Result:= StrToInt( Prop.AsString );
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

constructor TP3DPropertyEditorInt.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
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

constructor TP3DPropertyEditorFloat.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create(AData, AParent);
  Height:= 20;
  HAlignment:= haRight;
end;

{ TP3DPropertyEditorDataBlockList }

constructor gP3DPropertyEditorList.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create(AData, AParent);
  Font.Size:= 20;
  Height:= 60;
  ScrollBarH.Visible:= True;
end;

function gP3DPropertyEditorList.GetLProp: TListProp;
begin
  Result:= FProp;
end;

procedure gP3DPropertyEditorList.SetLProp( AValue: TListProp );
begin
  FProp:= AValue;
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
  Item: TListData;
begin
  Children.Clear();
  if ( Assigned( LProp )) then
    for i:= 0 to LProp.Count - 1 do
      begin
        Item:= LProp.Values[ i ];
        if ( Assigned( Item )) then
          Children.Add( {Item.Owner.Name + '.' + } Item.Name, Item )
        else
          Children.Add( '<empty>', nil );
      end;
end;

procedure gP3DPropertyEditorList.MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer );
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
  Canvas.GetValidText( P3DSymbolPlus, FPlusSymbol, P3DFontManager.Fonts[ 'Pascal3D-Symbols', Canvas.Font.Size ]);
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
      if ( P3DRect( ClientRect.Right - FDeleteSymbol.Width - ScrollBarV.Width, Selection.FRenderPos.Y,
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
          Selection.Caption:= '<empty>';
        end
      else
        inherited MouseDown(mb1, mb2, mb3, X, Y);
    end
  else
    inherited MouseDown(mb1, mb2, mb3, X, Y);
end;

function gP3DPropertyEditorList.DrawListItem(p: TVec2; Node: TP3DListItem): TVec2;
begin
  Result:= inherited DrawListItem( p, Node );
  if ( Node = Selection ) then
    begin
      Canvas.Font.Color:= Red800;
      Canvas.GetValidText( P3DSymbolClose, FDeleteSymbol, P3DFontManager.Fonts[ 'Pascal3D-Symbols', Canvas.Font.Size ]);
      Canvas.RenderText( FDeleteSymbol, vec2( ClientRect.Right - FDeleteSymbol.Width - ScrollBarV.Width, p.Y ));
      Canvas.Font.Color:= Orange800;
      Canvas.GetValidText( P3DSymbolClear, FClearSymbol, P3DFontManager.Fonts[ 'Pascal3D-Symbols', Canvas.Font.Size ]);
      Canvas.RenderText( FClearSymbol, vec2( ClientRect.Right - FDeleteSymbol.Width - FClearSymbol.Width - ScrollBarV.Width, p.Y ));
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
  if ( not Focused ) then
    Canvas.RenderText( FLabel, vec2( 0 ));
end;

procedure gP3DPropertyEditorList.MouseMove(X, Y: Integer);
var
  itm: TP3DListItem;
begin
  inherited MouseMove(X, Y);
  itm:= ItemAtPos( vec2( X + Scroll.X, Y + Scroll.Y ));
  FDrawInsertion:= -1;
  if ( Assigned( itm )) then
    begin
      if ( Y <= itm.FRenderPos.Y + 1 ) then
        FDrawInsertion:= itm.FRenderPos.y
      else if ( Y >= itm.FRenderPos.Y + itm.Text.Height - 1 ) then
        FDrawInsertion:= itm.FRenderPos.y + itm.Text.Height;
    end;
end;

{ TP3DPropertyEditorEnum }

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
      inherited SetCaption( '<empty>' );
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

constructor TP3DPropertyEditorEnum.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create(AData, AParent);
  HAlignment:= haRight;
  Height:= 20;
end;

procedure TP3DPropertyEditorEnum.Draw;
begin
  inherited Draw;
  if ( not Focused ) then
    Canvas.RenderText( FLabel, vec2( 0 ));
end;




{$ENDIF}
