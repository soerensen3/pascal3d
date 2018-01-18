{$IFDEF INTERFACE}

  { TP3DActionEditor }

  { TP3DTimeLine }

  TP3DTimeLine = class( TP3DGraphicControl )
    private type
      TGetTimeFunc = function : Float of object;
      TSetTimeFunc = procedure ( AValue: Float ) of object;

    private
      FActionList: TP3DTreeView;
      FShift: Single;
      FZoom: Single;
      FOnGetTime: TGetTimeFunc;
      FOnSetTime: TSetTimeFunc;
      FTimeLbl: TP3DTextBmp;

      procedure SetShift(AValue: Single);
      procedure SetZoom(AValue: Single);
      function GetTime: Single;
      procedure SetTime(AValue: Single);

    public
      constructor CreateBlock(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;
      destructor Destroy; override;

      procedure Draw; override;
      procedure MouseWheel(deltax: Integer; deltay: Integer); override;
      procedure MouseMove( X, Y: Integer); override;

      property OnGetTime: TGetTimeFunc read FOnGetTime write FOnGetTime;
      property OnSetTime: TSetTimeFunc read FOnSetTime write FOnSetTime;

      property ActionList: TP3DTreeView read FActionList write FActionList;
      property Zoom: Single read FZoom write SetZoom;
      property Time: Single read GetTime write SetTime;
      property Shift: Single read FShift write SetShift;
  end;

  TP3DActionEditor = class ( TP3DGraphicControl )
    private
      FActionList: TP3DTreeView;
      FTimeLine: TP3DTimeLine;
      FToolBar: TP3DGroupBox;
      function GetEditAction: TP3DAction;
      procedure SetEditAction(AValue: TP3DAction);
      function GetTime: Float;
      procedure SetTime( AValue: Float );

    public
      procedure Draw; override;
      procedure UpdateList;
      constructor CreateBlock(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

      property ToolBar: TP3DGroupBox read FToolBar write FToolBar;
      property ActionList: TP3DTreeView read FActionList write FActionList;
      property TimeLine: TP3DTimeLine read FTimeLine write FTimeLine;
      property EditAction: TP3DAction read GetEditAction write SetEditAction;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DTimeLine }

procedure TP3DTimeLine.SetZoom(AValue: Single);
begin
  if FZoom=AValue then Exit;
  FZoom:= clamp( AValue, 0.1, 10 );
end;

procedure TP3DTimeLine.SetShift(AValue: Single);
begin
  if FShift=AValue then Exit;
  FShift:= min( 25, AValue );
end;

function TP3DTimeLine.GetTime: Single;
begin
  if ( Assigned( OnGetTime )) then
    Result:= OnGetTime()
  else
    Result:= 0;
end;

procedure TP3DTimeLine.SetTime(AValue: Single);
begin
  if ( Assigned( OnSetTime )) then
    OnSetTime( AValue );
end;

constructor TP3DTimeLine.CreateBlock(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited CreateBlock( AData, AParent );
  Zoom:= 5;
end;

destructor TP3DTimeLine.Destroy;
begin
  FTimeLbl.Free;
  inherited Destroy;
end;

procedure TP3DTimeLine.Draw;
  procedure DrawList( List: TP3DTreeNodeList );
  var
    i: Integer;
    p: TVec2;
    Data: TP3DStreamable;
    Channel: TP3DActionChannel absolute Data;
    Node: TP3DTreeNode;
  begin
    for Node in List do
      begin
        p:= Node.RenderPos - vec2( 0, ActionList.ScrollBarV.Position );
        if ( NOde = ActionList.Selection ) then
          Canvas.RenderRect( vec2( 0, p.Y ), vec2( Canvas.Width, p.Y + Node.Text.Height ), Accent900.Fade( 0.5 ), Accent400.Fade( 0.5 ), Accent600.Fade( 0.5 ), Accent900.Fade( 0.5 ));
        Data:= TP3DDataBlock( Node.Data );
        if ( Data is TP3DActionChannel ) then
          begin
            for i:= 0 to Channel.Keys.Count - 1 do
              begin
                Canvas.RenderLineCircle( vec2(( Shift + Channel.Keys[ i ].Time ) * Zoom, Node.RenderPos.Y + Node.Text.Height / 2 - ActionList.ScrollBarV.Position ), Node.Text.Height / 4, 16, Secondary900 );
                Canvas.RenderCircle( vec2(( Shift + Channel.Keys[ i ].Time ) * Zoom, Node.RenderPos.Y + Node.Text.Height / 2 - ActionList.ScrollBarV.Position ), Node.Text.Height / 4, 16, Secondary500 );
              end;
          end;
        if ( Node.Expanded ) then
          DrawList( Node.Children );
      end;
  end;
var
  i: Integer;
begin
  Canvas.RenderRect( vec2( 0 ), Canvas.WidthHeight, Accent200);
  for i:= 0 to Canvas.Width div ( 10 * max( 1, floor( Zoom ))) do
    Canvas.RenderLine( vec2( Shift mod 10 * Zoom + i * Zoom * 10, 0 ), vec2( Shift mod 10 * Zoom + i * Zoom * 10, Canvas.Height ), Primary300 );
//  Canvas.RenderText( );
  Canvas.RenderRect( vec2( 0, Canvas.Height - 20 ), Canvas.WidthHeight, Accent900.Fade( 0.5 ));
  if ( Assigned( ActionList )) then
    begin
      Canvas.Scroll:= ActionList.Canvas.Scroll;
      DrawList( ActionList.Children );
      Canvas.RenderLine( vec2(( Shift + Time ) * Zoom, 0 ), vec2(( Shift + Time ) * Zoom, Canvas.Height ), Secondary900 );
      Canvas.GetValidText( FloatToStr( Time ), FTimeLbl );
      Canvas.RenderRect( vec2(( Shift + Time ) * Zoom, Canvas.Height - FTimeLbl.Height ), vec2(( Shift + Time ) * Zoom + FTimeLbl.Width, Canvas.Height ), Secondary900 );
      Canvas.RenderText( FTimeLbl, vec2(( Shift + Time ) * Zoom, Canvas.Height - FTimeLbl.Height ));
    end;
end;

procedure TP3DTimeLine.MouseWheel(deltax: Integer; deltay: Integer);
begin
  if ( gcisMouseOver in InputState ) then
    Zoom:= Zoom + deltay * 0.1;
end;

procedure TP3DTimeLine.MouseMove(X, Y: Integer);
begin
  if ( gcisMouseBtn1Down in InputState ) then
    Time:= -Shift + X / Zoom;
  if ( gcisMouseBtn2Down in InputState ) then
    begin
      Shift:= Shift + P3DInput.Mouse.DX / Zoom;
      ActionList.ScrollBarV.Position:= ActionList.ScrollBarV.Position - P3DInput.Mouse.DY;
    end;
end;


function TP3DActionEditor.GetEditAction: TP3DAction;
begin
  if ( SceneMain.ActiveDataBlock is TP3DAction ) then
    Result:= TP3DAction( SceneMain.ActiveDataBlock )
  else if ( SceneMain.ActiveDataBlock is IP3DAnimatable ) then
    Result:= ( SceneMain.ActiveDataBlock as IP3DAnimatable ).Action
  else
    Result:= Nil;
end;


procedure TP3DActionEditor.SetEditAction(AValue: TP3DAction);
begin
  SceneMain.ActiveDataBlock:= AValue;
  UpdateList;
end;

function TP3DActionEditor.GetTime: Float;
begin
  if ( Assigned( EditAction )) then
    Result:= EditAction.OwnerLib.GlobalTime
  else
    Result:= 0;
end;

procedure TP3DActionEditor.SetTime(AValue: Float);
begin
  if ( Assigned( EditAction )) then
    EditAction.OwnerLib.GlobalTime:= AValue;
end;

procedure TP3DActionEditor.Draw;
begin
  inherited Draw;
  //Canvas.RenderRect( vec2( 0 ), Canvas.WidthHeight, Accent200 );
end;

procedure TP3DActionEditor.UpdateList;
var
  A: TP3DAction;
  i, n: Integer;
begin
  ActionList.Children.Clear( True );
  A:= EditAction;
  if ( Assigned( A )) then
    begin
      n:= ActionList.Children.Add( A.Name, A );

      for i:= 0 to A.Channels.Count - 1 do
        ActionList.Children[ n ].Children.Add( A.Channels[ i ].PropStr, A.Channels[ i ]);
    end;
end;

constructor TP3DActionEditor.CreateBlock(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited CreateBlock(AData, AParent);
  Height:= 100;
  //ToolBar:= TP3DGroupBox.Create( Owner, Self );
  //ToolBar.Align:= alBottom;
  ActionList:= TP3DTreeView.CreateBlock( OwnerLib, Self );
  ActionList.Align:= alLeft;
  ActionList.Width:= 200;
  with ( TP3DSplitter.CreateBlock( OwnerLib, Self )) do
    Align:= alLeft;
  TimeLine:= TP3DTimeLine.CreateBlock( OwnerLib, Self );
  TimeLine.Align:= alClient;
  TimeLine.ActionList:= ActionList;
  TimeLine.OnGetTime:= @GetTime;
  TimeLine.OnSetTime:= @SetTime;
end;

{$ENDIF}

