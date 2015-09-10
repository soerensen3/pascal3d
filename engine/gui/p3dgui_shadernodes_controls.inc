{$IFDEF INTERFACE}
  TP3DNodeSocketDirection = ( nsdInput, nsdOutput );

  { TP3DNodeSocketButton }

  TP3DNodeSocketButton = class ( TP3DButton )
    constructor Create(AOwner: TP3DObjectList; AManager: TGUIManager;
    const AParent: TP3DGraphicControl=nil);
    procedure Draw; override;
    function MouseRay(X, Y: Integer): Boolean; override;
  end;

  { TP3DNodeSocketControlSimple }

  TP3DNodeSocketControlSimple = class ( TP3DGraphicControl )
    private
      Editing: Boolean;
      FDirection: TP3DNodeSocketDirection;
      FLbl: TP3DLabel;
      FBtn: TP3DNodeSocketButton;
      FSocket: TP3DNodeSocket;

      procedure BtnMouseDown(Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer);
      procedure BtnDragDrop( Sender, Source: TP3DGraphicControl; X, Y: Integer );

    public
      constructor Create(AOwner: TP3DObjectList; AManager: TGUIManager;
        ASocket: TP3DNodeSocket; ADirection: TP3DNodeSocketDirection;
        const AParent: TP3DGraphicControl);

      procedure Draw; override;
      destructor Destroy; override;

      property Lbl: TP3DLabel read FLbl;
      property Btn: TP3DNodeSocketButton read FBtn;
      property Direction: TP3DNodeSocketDirection read FDirection;
      property Socket: TP3DNodeSocket read FSocket write FSocket;
  end;

  TP3DNodeSocketControlSimpleList = specialize TP3DCustomObjectList < TP3DNodeSocketControlSimple >;

{$ENDIF}


{$IFDEF IMPLEMENTATION}
{ TP3DGUINodeSocketSimple }

procedure TP3DNodeSocketControlSimple.BtnMouseDown(Sender: TP3DGraphicControl;
  mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  if (( mb1 ) and ( gcisMouseOver in Sender.InputState )) then
    begin
      BeginDrag();
      Editing:= True;
    end;
end;

procedure TP3DNodeSocketControlSimple.BtnDragDrop(Sender,
  Source: TP3DGraphicControl; X, Y: Integer);
begin
  Editing:= False;
  if ( Source is TP3DNodeSocketControlSimple ) then
    TP3DNodeSocketControlSimple( Source ).Editing:= False;
end;

constructor TP3DNodeSocketControlSimple.Create(AOwner: TP3DObjectList;
  AManager: TGUIManager; ASocket: TP3DNodeSocket;
  ADirection: TP3DNodeSocketDirection; const AParent: TP3DGraphicControl);
begin
  inherited Create( AOwner, AManager, AParent );
  Socket:= ASocket;
  Height:= 30;
  FLbl:= TP3DLabel.Create( ParentList, Manager, Self );
  Lbl.Caption:= Socket.Name;
  //Lbl.AutoSize:= True;
  Lbl.Align:= alClient;
  Lbl.VAlignment:= vaCenter;
  FDirection:= ADirection;
  FBtn:= TP3DNodeSocketButton.Create( ParentList, Manager, Self );
  Btn.OnDragDrop:= @BtnDragDrop;
  Btn.OnMouseDown:= @BtnMouseDown;
  Btn.Width:= 30;
  Btn.Height:= 30;
  if ( Direction = nsdOutput ) then
    Btn.Align:= alRight
  else
    Btn.Align:= alLeft;
  Btn.Caption:= '';
  if ( Direction = nsdOutput ) then
    Lbl.HAlignment:= haRight
  else
    Lbl.HAlignment:= haLeft;
end;

procedure TP3DNodeSocketControlSimple.Draw;
  procedure RenderConnector;
  var
    i: Integer;
    S: TVec2;
    E: TVec2;
    d: Single;
  begin
    S:= vec2( Canvas.Left + Canvas.Width + 5, Canvas.Top + Canvas.Height / 2 );

    if ( Editing ) then
      begin
        E:= vec2( InputManager.Mouse.X, InputManager.Mouse.Y );
        Manager.ScreenCanvas.Lock();

        d:= ( S - E ).GetDist();
        Manager.ScreenCanvas.Pen.Width:= 3;
        Manager.ScreenCanvas.RenderBezier( S, vec2( S.X + d / 3, S.Y ),
                             vec2( E.X - d / 3, E.Y ), E, round( sqrt( d )), vec4( 0, 0, 0, 1 ));
        Manager.ScreenCanvas.Pen.Width:= 1;
        Manager.ScreenCanvas.RenderBezier( S, vec2( S.X + d / 3, S.Y ),
                             vec2( E.X - d / 3, E.Y ), E, round( sqrt( d )), vec4( 1 ));
        Manager.ScreenCanvas.Unlock();
      end;

    {for i:= 0 to Socket.Fragments.Count - 1 do
      begin
        E:= vec2( Socket.Fragments[ i ].X, Socket.Fragments[ i ].Y );
        Manager.ScreenCanvas.Lock();

        d:= ( S - E ).GetDist();
        Manager.ScreenCanvas.Pen.Width:= 3;
        Manager.ScreenCanvas.RenderBezier( S, vec2( S.X + d / 3, S.Y ),
                             vec2( E.X - d / 3, E.Y ), E, round( sqrt( d )), vec4( 0, 0, 0, 1 ));
        Manager.ScreenCanvas.Pen.Width:= 1;
        Manager.ScreenCanvas.RenderBezier( S, vec2( S.X + d / 3, S.Y ),
                             vec2( E.X - d / 3, E.Y ), E, round( sqrt( d )), vec4( 1 ));
        Manager.ScreenCanvas.Unlock();
      end;}
  end;
begin
  inherited Draw;
  if ( Direction = nsdOutput ) then
    RenderConnector;
end;

destructor TP3DNodeSocketControlSimple.Destroy;
begin
  Lbl.Free;
  Btn.Free;
  inherited Destroy;
end;

{$ENDIF}
