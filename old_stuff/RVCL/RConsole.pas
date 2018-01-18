unit RConsole;

interface
  uses
    Core,
    RVCL,
    RForms,
    RStdCtrls,
//    GlobalVars,
    SysUtils,
    Input,
    Math,
    Timers,
    Classes;

  type
    TConsole = class( TRevForm )
      private
        FAlpha: Byte;
        FFader: String;

        procedure OnTimerFinish( Sender: TObject );

      protected
        FListBox: TRevListBox;
        FEdit: TRevEdit;
        Input: TStringList;
        Cursor: Integer;

        procedure _KeyDown( Sender: TObject );
        procedure _OnAutoComplete( Sender: TObject );
        procedure SetVisible( const Value: Boolean ); override;

      public
        constructor Create( AOwner: TBaseObject; AEngine: TEngine; _OwnerCtrlBuf: TControlBuffer; _Manager: TVCLManager );
        destructor Destroy; override;
    end;

implementation

{ TConsole }

constructor TConsole.Create( AOwner: TBaseObject; AEngine: TEngine; _OwnerCtrlBuf: TControlBuffer; _Manager: TVCLManager );
var
  I: Integer;
begin
  inherited;
  FEdit:= TRevEdit.Create( Self, AEngine, Controls, Manager );
  FListBox:= TRevListBox.Create( Self, AEngine, Controls, Manager );
  FListBox.Align:= alClient;
  FListBox.Color:= $40FFFFFF;
  FListBox.CanSelect:= False;
  FEdit.Height:= 25;
  FEdit.Align:= alBottom;
  FEdit.OnKeyDown:= _KeyDown;
  FEdit.OnAutoComplete:= _OnAutoComplete;
  FListBox.Items:= Engine.Log.ConsoleLog;
  Caption:= Name;
  Input:= TStringList.Create;
  FAlpha:= Alpha;
  FListBox.ScrollBarV.Percentage:= 1.0;
end;

destructor TConsole.Destroy;
begin
//  FEdit.Free;
//  FListBox.Free;
//  Input.Free;
  inherited;
end;

procedure TConsole.OnTimerFinish( Sender: TObject );
begin
  if ( Alpha = 0 ) then
    FVisible:= False;
end;

procedure TConsole.SetVisible( const Value: Boolean );
begin
  inherited;
{  if ( Value ) then
    begin
      TTimedFade.Create( nil, Engine, PByte( Integer( @FBaseColor ) + 3 ), 255, 100 );
      if ( FEdit <> nil ) then
        begin
          FEdit.HasFocus:= True;
          FEdit.Text:= '';
        end;

    end
  else
    begin
      FVisible:= True;
      TTimedFade.Create( nil, Engine, PByte( Integer( @FBaseColor ) + 3 ), 0, 100 ).OnFinish:= OnTimerFinish;
    end;}
end;

procedure TConsole._KeyDown( Sender: TObject );
begin
  if ( Manager.GameInput.Keys[ IK_UP ]) then
    begin
      Cursor:= Max( Cursor - 1, 0 );
      if ( Cursor < Input.Count ) then
        FEdit.Text:= Input[ Cursor ]
      else
        FEdit.Text:= '';
    end
  else if ( Manager.GameInput.Keys[ IK_DOWN ]) then
    begin
      Cursor:= Min( Cursor + 1, Input.Count );
      if ( Cursor < Input.Count ) then
        FEdit.Text:= Input[ Cursor ]
      else
        FEdit.Text:= '';
    end
  else if ( Manager.GameInput.Keys[ IK_RETURN ]) then
    if ( FEdit.Text <> '' ) then
      begin
//        if ( FEdit.
//        Cmd( FEdit.Text );
        Engine.CommandContainer.Call( FEdit.Text );
        Input.Add( FEdit.Text );
        Cursor:= Input.Count;
        FListBox.ScrollBarV.Position:= 1.0;
        FListBox.ScrollBarH.Position:= 0.0;        
        FEdit.Text:= '';
//        FListBox.ScrollBarV.Position:= 1;
      end;
end;

procedure TConsole._OnAutoComplete( Sender: TObject );
var
  S: TStringList;
  I: Integer;

begin
  S:= TStringList.Create;
  for I:= 0 to Engine.CommandContainer.Count - 1 do
    begin
      S.Add( Engine.CommandContainer[ I ].Name );
      if ( Pos( Engine.CommandContainer[ I ].Name + ' ', FEdit.Text ) > 0 ) then
        begin
          S.Clear;
//          if ( Assigned( Engine.CommandContainer[ I ].ParamAutoCompl )) then
//            Engine.CommandContainer[ I ].ParamAutoCompl( 0, S );
          break;
        end;
    end;

  FEdit.Autocomplete( S );
  S.Free;
end;

initialization
  Rev_RegisterClass( TConsole );

end.
