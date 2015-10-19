{$IFDEF INTERFACE}

  { TP3DNodeSocketButton }

  TP3DNodeSocketButton = class ( TP3DButton )
    constructor Create(AOwner: TP3DObjectList; AManager: TP3DGUIManager;
      const AParent: TP3DGraphicControl=nil);
    procedure Draw; override;
    function MouseRay(X, Y: Integer): Boolean; override;
  end;

  { TP3DNodeSocketControlSimple }

  TP3DNodeSocketControlSimple = class ( TP3DGraphicControl )
    private
      Editing: Boolean;
      FLbl: TP3DLabel;
      FBtn: TP3DNodeSocketButton;
      FSocket: TP3DNodeSocket;

      procedure BtnMouseDown(Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer);
      procedure BtnDragDrop( Sender, Source: TP3DGraphicControl; X, Y: Integer; var Accept: Boolean );
      procedure BtnDragDropCancel( Sender: TP3DGraphicControl; X, Y: Integer );

    public
      constructor Create(AOwner: TP3DObjectList; AManager: TP3DGUIManager;
        ASocket: TP3DNodeSocket; ADirection: TP3DNodeSocketDirection;
        const AParent: TP3DGraphicControl);

      procedure Draw; override;
      destructor Destroy; override;

      property Lbl: TP3DLabel read FLbl;
      property Btn: TP3DNodeSocketButton read FBtn;
      property Socket: TP3DNodeSocket read FSocket write FSocket;
  end;

  TP3DNodeSocketControlSimpleList = specialize TP3DCustomObjectList < TP3DNodeSocketControlSimple >;

  { TP3DNodeSocketVectorEdit }

  TP3DNodeSocketVectorEdit = class ( TP3DButton )
    private
      FDropDown: TP3DCustomPopupMenu;
      FSocket: TP3DShaderNodeSocketVector;
      FValueEdits: array of TP3DEventValueEdit;
      procedure SetSocket(AValue: TP3DShaderNodeSocketVector);
      function GetValue( Sender: TP3DEventValueEdit ): Single;
      procedure SetValue( Sender: TP3DEventValueEdit; AValue: Single );
      function GetValueName( Sender: TP3DEventValueEdit ): String;

    public
      constructor Create(AOwner: TP3DObjectList; AManager: TP3DGUIManager; ASocket: TP3DShaderNodeSocketVector;
        const AParent: TP3DGraphicControl=nil);
      destructor Destroy; override;
      procedure Update;
      procedure MouseClick(mb1, mb2, mb3: Boolean; X, Y: Integer); override;
      property Socket: TP3DShaderNodeSocketVector read FSocket write SetSocket;
  end;

	{ TP3DNodeSocketControlVector }

  TP3DNodeSocketControlVector = class ( TP3DNodeSocketControlSimple )
    private
      FCanEdit: Boolean;
      FVecEdit: TP3DNodeSocketVectorEdit;

      procedure SetCanEdit(AValue: Boolean);
      property CanEdit: Boolean read FCanEdit write SetCanEdit;

    published
      property VecEdit: TP3DNodeSocketVectorEdit read FVecEdit write FVecEdit;
      procedure Draw; override;
      constructor Create(AOwner: TP3DObjectList; AManager: TP3DGUIManager; ASocket: TP3DNodeSocket; ADirection: TP3DNodeSocketDirection; const AParent: TP3DGraphicControl);
	end;

{$ENDIF}


{$IFDEF IMPLEMENTATION}
{ TP3DGUINodeSocketSimple }

procedure TP3DNodeSocketControlSimple.BtnMouseDown(Sender: TP3DGraphicControl;
  mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  if (( mb1 ) and ( gcisMouseOver in Sender.InputState )) then
    begin
      Sender.BeginDrag();
      Editing:= True;
    end;
end;

procedure TP3DNodeSocketControlSimple.BtnDragDrop(Sender,
  Source: TP3DGraphicControl; X, Y: Integer; var Accept: Boolean);
var
  ConnectTo: TP3DNodeSocketControlSimple;
begin
  Editing:= False;
  Accept:= False;
  if ( Source is TP3DNodeSocketControlSimple ) then
    ConnectTo:= TP3DNodeSocketControlSimple( Source )
  else if ( Source.Parent is TP3DNodeSocketControlSimple ) then
    ConnectTo:= TP3DNodeSocketControlSimple( Source.Parent )
  else
    ConnectTo:= nil;
  if ( Assigned( ConnectTo )) then
    begin
      ConnectTo.Editing:= False;
      if ( ConnectTo.Socket.Node = Socket.Node ) then
        exit;
      if (( ConnectTo.Socket.Direction <> Socket.Direction ){ and ( ConnectTo.Socket <> Socket )}) then
        if ( ConnectTo.Socket.Direction = nsdInput ) then
          ConnectTo.Socket.Connected:= Socket
        else
          Socket.Connected:= ConnectTo.Socket;
      Accept:= True;
    end;
end;

procedure TP3DNodeSocketControlSimple.BtnDragDropCancel(
  Sender: TP3DGraphicControl; X, Y: Integer);
begin
  Editing:= False;
  Socket.Connected:= nil;
end;

constructor TP3DNodeSocketControlSimple.Create(AOwner: TP3DObjectList;
  AManager: TP3DGUIManager; ASocket: TP3DNodeSocket;
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
  //FDirection:= ADirection;
  Lbl.OnDragDrop:= @BtnDragDrop;
  Lbl.OnMouseDown:= @BtnMouseDown;
  Lbl.OnDragDropCancel:= @BtnDragDropCancel;
  FBtn:= TP3DNodeSocketButton.Create( ParentList, Manager, Self );
  Btn.OnDragDrop:= @BtnDragDrop;
  Btn.OnMouseDown:= @BtnMouseDown;
  Btn.OnDragDropCancel:= @BtnDragDropCancel;
  Btn.Width:= 30;
  Btn.Height:= 30;
  Btn.Cursor:= curNodeEdit;
  Socket.UserData:= Self;
  if ( Socket.Direction = nsdOutput ) then
    Btn.Align:= alRight
  else
    Btn.Align:= alLeft;
  Btn.Caption:= '';
  if ( Socket.Direction = nsdOutput ) then
    Lbl.HAlignment:= haRight
  else
    Lbl.HAlignment:= haLeft;
end;

procedure TP3DNodeSocketControlSimple.Draw;
  procedure RenderConnector;
  var
    S, E, M1, M2: TVec2;
    d: Single;
    Connected: TP3DNodeSocketControlSimple;
    cl: TVec4;
  begin
    if ( Socket.Direction = nsdOutput ) then
      S:= vec2( Canvas.Left + Canvas.Width + 5, Canvas.Top + Canvas.Height / 2 )
    else
      S:= vec2( Canvas.Left - 5, Canvas.Top + Canvas.Height / 2 );

    if ( Editing ) then
      begin
        E:= vec2( InputManager.Mouse.X, InputManager.Mouse.Y );
        d:= ( S - E ).GetDist();
        cl:= vec4( 0.6, 0.3, 0.5, 1 );
        if ( Socket.Direction = nsdOutput ) then
          begin
            M1:= vec2( S.X + d / 3, S.Y );
            M2:= vec2( E.X - d / 3, E.Y );
          end
        else
          begin
            M1:= vec2( S.X - d / 3, S.Y );
            M2:= vec2( E.X + d / 3, E.Y );
          end;
      end
    else
      if ( Assigned( Socket.Connected ) and ( Socket.Direction = nsdInput )) then
        begin
          cl:= vec4( 1 );

          Connected:= TP3DNodeSocketControlSimple( Socket.Connected.UserData );
          E:= vec2( Connected.Canvas.Left + Connected.Canvas.Width + 5, Connected.Canvas.Top + Connected.Canvas.Height / 2 );
          d:= ( S - E ).GetDist();
          M1:= vec2( S.X - d / 3, S.Y );
          M2:= vec2( E.X + d / 3, E.Y );
        end
    else
      exit;

    Manager.ScreenCanvas.Lock();
    Manager.ScreenCanvas.Pen.Width:= 3;
    Manager.ScreenCanvas.RenderBezier( S, M1, M2, E, round( sqrt( d )), vec4( 0, 0, 0, 1 ));
    Manager.ScreenCanvas.Pen.Width:= 1;
    Manager.ScreenCanvas.RenderBezier( S, M1, M2, E, round( sqrt( d )), cl );
    Manager.ScreenCanvas.Unlock();
  end;
begin
  inherited Draw;
  if (( gcisMouseOver in InputState ) and ( DragDropSrc <> nil )) then
    Canvas.RenderLineRect( vec2( 0 ), vec2( Width, Height ), vec4( 1, 0, 0, 1 ));
  RenderConnector;
end;

destructor TP3DNodeSocketControlSimple.Destroy;
begin
  Lbl.Free;
  Btn.Free;
  inherited Destroy;
end;

{$ENDIF}
