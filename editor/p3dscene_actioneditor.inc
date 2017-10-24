{$IFDEF INTERFACE}

  { TP3DActionEditor }

  { TP3DTimeLine }

  TP3DTimeLine = class( TP3DGraphicControl )
    private type
      TGetTimeFunc = function : Float of object;
      TSetTimeFunc = procedure ( AValue: Float ) of object;

    private
      FActionList: TP3DTreeView;
      FZoom: Single;
      FOnGetTime: TGetTimeFunc;
      FOnSetTime: TSetTimeFunc;
      FTimeLbl: TP3DTextBmp;

      procedure SetZoom(AValue: Single);
      function GetTime: Single;
      procedure SetTime(AValue: Single);

    public
      constructor Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl); override;
      destructor Destroy; override;

      procedure Draw; override;
      procedure MouseWheel(deltax: Integer; deltay: Integer); override;
      procedure MouseMove( X, Y: Integer); override;

      property OnGetTime: TGetTimeFunc read FOnGetTime write FOnGetTime;
      property OnSetTime: TSetTimeFunc read FOnSetTime write FOnSetTime;

      property ActionList: TP3DTreeView read FActionList write FActionList;
      property Zoom: Single read FZoom write SetZoom;
      property Time: Single read GetTime write SetTime;
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
      constructor Create(const AData: TP3DLibrary=nil; const AParent: TP3DGraphicControl=nil); override;

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

constructor TP3DTimeLine.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
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
    Data: TP3DInterfacedPersistent;
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
                Canvas.RenderLineCircle( vec2( Channel.Keys[ i ].Time * Zoom, Node.RenderPos.Y + Node.Text.Height / 2 - ActionList.ScrollBarV.Position ), Node.Text.Height / 4, 16, Secondary900 );
                Canvas.RenderCircle( vec2( Channel.Keys[ i ].Time * Zoom, Node.RenderPos.Y + Node.Text.Height / 2 - ActionList.ScrollBarV.Position ), Node.Text.Height / 4, 16, Secondary500 );
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
    Canvas.RenderLine( vec2( i * Zoom * 10, 0 ), vec2( i * Zoom * 10, Canvas.Height ), Primary300 );
//  Canvas.RenderText( );
  Canvas.RenderRect( vec2( 0, Canvas.Height - 20 ), Canvas.WidthHeight, Accent900.Fade( 0.5 ));
  if ( Assigned( ActionList )) then
    begin
      Canvas.Scroll:= ActionList.Canvas.Scroll;
      DrawList( ActionList.Children );
      Canvas.RenderLine( vec2( Time * Zoom, 0 ), vec2( Time * Zoom, Canvas.Height ), Secondary900 );
      Canvas.GetValidText( FloatToStr( Time ), FTimeLbl );
      Canvas.RenderRect( vec2( Time * Zoom, Canvas.Height - FTimeLbl.Height ), vec2( Time * Zoom + FTimeLbl.Width, Canvas.Height ), Secondary900 );
      Canvas.RenderText( FTimeLbl, vec2( Time * Zoom, Canvas.Height - FTimeLbl.Height ));
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
    Time:= X / Zoom;
end;


function TP3DActionEditor.GetEditAction: TP3DAction;
begin
  if ( SceneMain.ActiveDataBlock is TP3DAction ) then
    Result:= TP3DAction( SceneMain.ActiveDataBlock )
  else
    Result:= Nil;
end;


procedure TP3DActionEditor.SetEditAction(AValue: TP3DAction);
begin
  SceneMain.ActiveDataBlock:= AValue;
end;

function TP3DActionEditor.GetTime: Float;
begin
  if ( Assigned( EditAction )) then
    Result:= EditAction.Owner.GlobalTime
  else
    Result:= 0;
end;

procedure TP3DActionEditor.SetTime(AValue: Float);
begin
  if ( Assigned( EditAction )) then
    EditAction.Owner.GlobalTime:= AValue;
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

constructor TP3DActionEditor.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create(AData, AParent);
  Height:= 100;
  //ToolBar:= TP3DGroupBox.Create( Owner, Self );
  //ToolBar.Align:= alBottom;
  ActionList:= TP3DTreeView.Create( Owner, Self );
  ActionList.Align:= alLeft;
  ActionList.Width:= 200;
  with ( TP3DSplitter.Create( Owner, Self )) do
    Align:= alLeft;
  TimeLine:= TP3DTimeLine.Create( Owner, Self );
  TimeLine.Align:= alClient;
  TimeLine.ActionList:= ActionList;
  TimeLine.OnGetTime:= @GetTime;
  TimeLine.OnSetTime:= @SetTime;
end;

{$ENDIF}

