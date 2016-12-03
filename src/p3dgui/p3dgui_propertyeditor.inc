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
  { gP3DPropertyEditorVector }

  generic gP3DPropertyEditorVector < TVecType > = class ( TP3DVectorEdit )
    private
      FProp: TP3DPropertyAccess;

      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetValue: TVec4; override;
      procedure SetValue( AValue: TVec4 ); override;

      property GetValueEvent;
      property SetValueEvent;

    public
      constructor Create(const AData: TP3DData=nil; const AParent: TP3DGraphicControl=nil); override;

    published
      property Prop: TP3DPropertyAccess read FProp write SetProp;
  end;

  TP3DPropertyEditorVec2 = specialize gP3DPropertyEditorVector < TVec2 >;
  TP3DPropertyEditorVec3 = specialize gP3DPropertyEditorVector < TVec3 >;
  TP3DPropertyEditorVec4 = specialize gP3DPropertyEditorVector < TVec4 >;

  { TP3DPropertyEditorString }

  TP3DPropertyEditorString = class ( TP3DEdit )
    protected
      FProp: TP3DPropertyAccess;

      procedure SetProp( AValue: TP3DPropertyAccess );
      procedure SetFocused( AValue: Boolean ); override;
      procedure ReadValue;
      procedure ApplyValue;

    public
      procedure KeyboardAction; override;

    published
      property Prop: TP3DPropertyAccess read FProp write SetProp;
  end;

  { TP3DPropertyEditorFloat }

  TP3DPropertyEditorFloat = class ( TP3DCustomValueEdit )
    protected
      FProp: TP3DPropertyAccess;
      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetValue: Single; override;
      procedure SetValue(AValue: Single); override;
      function GetValueName: String; override;

    published
      property Prop: TP3DPropertyAccess read FProp write SetProp;
  end;

  { TP3DPropertyEditorInt }

  TP3DPropertyEditorInt = class ( TP3DCustomValueEdit )
    protected
      FProp: TP3DPropertyAccess;
      procedure SetProp( AValue: TP3DPropertyAccess );
      function GetValue: Single; override;
      procedure SetValue( AValue: Single ); override;
      function GetValueName: String; override;

    public
      constructor Create( const AData: TP3DData=nil; const AParent: TP3DGraphicControl=nil ); override;

    published
      property Prop: TP3DPropertyAccess read FProp write SetProp;
  end;

  { TP3DPropertyEditorDataBlockList }

  TP3DPropertyEditorDataBlockList = class ( TP3DListView )
    protected
      FProp: TP3DPropertyAccessDataBlockList;
      FDeleteSymbol: TP3DTextBmp;
      FClearSymbol: TP3DTextBmp;
      FPlusSymbol: TP3DTextBmp;
      FDrawInsertion: Single;

      procedure SetProp( AValue: TP3DPropertyAccessDataBlockList );
      procedure Update;

    public
      procedure MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer); override;
      function DrawListItem(p: TVec2; Node: TP3DListItem): TVec2; override;
      procedure Draw; override;
      procedure MouseMove(X, Y: Integer); override;

      constructor Create(const AData: TP3DData=nil; const AParent: TP3DGraphicControl=nil); override;

    published
      property Prop: TP3DPropertyAccessDataBlockList read FProp write SetProp;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ gP3DPropertyEditorVector }

procedure gP3DPropertyEditorVector.SetProp( AValue: TP3DPropertyAccess );
begin
  if ( FProp = AValue ) then Exit;
  FProp:= AValue;
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

constructor gP3DPropertyEditorVector.Create(const AData: TP3DData; const AParent: TP3DGraphicControl);
begin
  inherited Create(AData, AParent);
  EdCount:= Length( TVecType.FCoord );
end;


{ TP3DPropertyEditorString }

procedure TP3DPropertyEditorString.SetProp(AValue: TP3DPropertyAccess);
begin
  if FProp=AValue then Exit;
  FProp:=AValue;

  if ( Assigned( Prop )) then
    ReadValue;
end;

procedure TP3DPropertyEditorString.SetFocused(AValue: Boolean);
begin
  if ( AValue ) then
    ReadValue()
  else
    ApplyValue();
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
        ApplyValue;
      if ( P3DInput.Keyboard.DKeysLayout[ P3DK_ESCAPE ] = ksPressed ) then
        Focused:= False;
    end;
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

constructor TP3DPropertyEditorInt.Create(const AData: TP3DData; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  Precision:= 0;
  FixedDigits:= 0;
end;

{ TP3DPropertyEditorFloat }

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

{ TP3DPropertyEditorDataBlockList }

constructor TP3DPropertyEditorDataBlockList.Create(const AData: TP3DData; const AParent: TP3DGraphicControl);
begin
  inherited Create(AData, AParent);
  Font.Size:= 20;
  ScrollBarH.Visible:= True;
end;

procedure TP3DPropertyEditorDataBlockList.SetProp( AValue: TP3DPropertyAccessDataBlockList );
begin
  if ( AValue = Prop ) then
    exit;
  FProp:= AValue;
  Update;
end;

procedure TP3DPropertyEditorDataBlockList.Update;
var
  i: Integer;
  Item: TP3DDataBlock;
begin
  Children.Clear();
  if ( Assigned( Prop )) then
    for i:= 0 to Prop.Count - 1 do
      begin
        Item:= Prop.Values[ i ];
        if ( Assigned( Item )) then
          Children.Add( Item.Owner.Name + '.' + Item.Name, Item )
        else
          Children.Add( '<empty>', nil );
      end;
end;

procedure TP3DPropertyEditorDataBlockList.MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer );
  function GetIndex( AValue: Single ): Integer;
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
  end;

begin
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
          Prop.Value.Delete( Children.IndexOf( Selection ));
          Children.Remove( Selection );
          FreeAndNil( FSelection );
        end
      else if ( P3DRect( ClientRect.Right - FDeleteSymbol.Width - FClearSymbol.Width - ScrollBarV.Width, Selection.FRenderPos.Y,
                    FClearSymbol.Width, FClearSymbol.Height ).PtInRect( vec2( X, Y + Scroll.Y ))) then
        begin // Clear
          Prop.Values[ Children.IndexOf( Selection )]:= nil;
          Selection.Data:= nil;
          Selection.Caption:= '<empty>';
        end
      else
        inherited MouseDown(mb1, mb2, mb3, X, Y);
    end
  else
    inherited MouseDown(mb1, mb2, mb3, X, Y);
end;

function TP3DPropertyEditorDataBlockList.DrawListItem(p: TVec2; Node: TP3DListItem): TVec2;
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

procedure TP3DPropertyEditorDataBlockList.Draw;
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
end;

procedure TP3DPropertyEditorDataBlockList.MouseMove(X, Y: Integer);
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



{$ENDIF}
