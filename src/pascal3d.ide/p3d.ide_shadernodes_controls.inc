{$IFDEF INTERFACE}

  { TP3DNodeSocketPropEdit }

  TP3DNodeSocketPropEdit = class ( TP3DGraphicControl )
    private
      FPropEdit: IP3DPropertyEditor;
      //FDropDown: TP3DStreamablePointer;
      FSocket: TP3DStreamablePointer;

      //function GetDropDown: TP3DCustomPopupMenu;
      //procedure SetDropDown(AValue: TP3DCustomPopupMenu);
      function GetSocket: TP3DNodeSocketClone;
      procedure SetSocket(AValue: TP3DNodeSocketClone);

      //property DropDown: TP3DCustomPopupMenu read GetDropDown write SetDropDown;
      property PropEdit: IP3DPropertyEditor read FPropEdit;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      procedure Update; override;
      procedure MouseClick(mb1, mb2, mb3: Boolean; X, Y: Float); override;
      property Socket: TP3DNodeSocketClone read GetSocket write SetSocket;
  end;

  { TP3DNodeSocketControl }

  TP3DNodeSocketControl = class ( TP3DButton )
    private
      Editing: Boolean;
      FBtn: TP3DStreamablePointer;
      FSocket: TP3DStreamablePointer;
      FPropEd: TP3DStreamablePointer;

      procedure BtnMouseDown(Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Float);
      procedure BtnDragDrop( Sender, Source: TP3DGraphicControl; X, Y: Float; var Accept: Boolean );
      procedure BtnDragDropCancel( Sender: TP3DGraphicControl; X, Y: Float );
      procedure BtnDraw( Sender: TP3DGraphicControl; OffSetX, OffSetY, _Width, _Height: Float );
      function GetBtn: TP3DGraphicControl;
      function GetPropEd: TP3DNodeSocketPropEdit;
      function GetSocket: TP3DNodeSocketClone;
      procedure SetSocket(AValue: TP3DNodeSocketClone); virtual;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      procedure Draw; override;

      property Btn: TP3DGraphicControl read GetBtn;
      property Socket: TP3DNodeSocketClone read GetSocket write SetSocket;
      property PropEd: TP3DNodeSocketPropEdit read GetPropEd;
  end;

  TP3DNodeSocketControlPointerList = specialize gP3DStreamablePointerList < TP3DNodeSocketControl >;

{$ENDIF}


{$IFDEF IMPLEMENTATION}
{ TP3DGUINodeSocketSimple }

procedure TP3DNodeSocketControl.BtnMouseDown(Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  if (( mb1 ) and ( gcisMouseOver in Sender.InputState )) then
    begin
      Sender.BeginDrag();
      Editing:= True;
    end;
end;

procedure TP3DNodeSocketControl.BtnDragDrop(Sender, Source: TP3DGraphicControl; X, Y: Float; var Accept: Boolean);
var
  ConnectTo: TP3DNodeSocketControl;
begin
  Editing:= False;
  Accept:= False;
  if ( Source is TP3DNodeSocketControl ) then
    ConnectTo:= TP3DNodeSocketControl( Source )
  else if ( Source.Parent is TP3DNodeSocketControl ) then
    ConnectTo:= TP3DNodeSocketControl( Source.Parent )
  else
    ConnectTo:= nil;
  if ( Assigned( ConnectTo ) and Assigned( ConnectTo.Socket )) then
    begin
      ConnectTo.Editing:= False;
      if ( ConnectTo.Socket.Node = Socket.Node ) then
        exit;
      if (( ConnectTo.Socket.CloneOf.Direction <> Socket.CloneOf.Direction ){ and ( ConnectTo.Socket <> Socket )}) then
        if ( ConnectTo.Socket.CloneOf.Direction = nsdInput ) then
          ConnectTo.Socket.Connected:= Socket
        else
          Socket.Connected:= ConnectTo.Socket;
      Accept:= True;
    end;
end;

procedure TP3DNodeSocketControl.BtnDragDropCancel(Sender: TP3DGraphicControl; X, Y: Float);
begin
  Editing:= False;
  if ( Assigned( Socket )) then
    Socket.Connected:= nil;
end;

procedure TP3DNodeSocketControl.BtnDraw(Sender: TP3DGraphicControl; OffSetX, OffSetY, _Width, _Height: Float);
var
  cl: TP3DColorRGBA;
begin
  if ( Assigned( Socket ) and Assigned( Socket.CloneOf )) then
    case Socket.CloneOf.SocketType of
      'vec4': cl:= Green900;
      'vec3': cl:= Green600;
      'vec2': cl:= Green300;
      'float': cl:= Green100;
      'color4': cl:= Red300;
      'text': cl:= Blue300;
      'int': cl:= Yellow100;
      'sampler1D': cl:= Orange300;
      'sampler2D': cl:= Orange600;
      'sampler3D': cl:= Orange900;
      else
        cl:= White;
    end;
  //if ( not Assigned( Socket.Connected )) then
  //  cl:= cl.fade( 0.37 );
  Btn.Canvas.RenderCircle( Btn.ClippedRect.WidthHeight / 2 + 1, 5, 16, Black.fade( 0.05 ) );
  Btn.Canvas.RenderCircle( Btn.ClippedRect.WidthHeight / 2, 5, 16, Grey200 );
  Btn.Canvas.RenderCircle( Btn.ClippedRect.WidthHeight / 2, 3, 16, cl );
end;

function TP3DNodeSocketControl.GetBtn: TP3DGraphicControl;
begin
  Result:= TP3DButton( FBtn.AsValue );
end;

function TP3DNodeSocketControl.GetPropEd: TP3DNodeSocketPropEdit;
begin
  Result:= TP3DNodeSocketPropEdit( FPropEd.AsValue );
end;

function TP3DNodeSocketControl.GetSocket: TP3DNodeSocketClone;
begin
  Result:= TP3DNodeSocketClone( FSocket.AsValue );
end;

procedure TP3DNodeSocketControl.SetSocket(AValue: TP3DNodeSocketClone);
begin
  FSocket.AsValue:= AValue;
  if ( Assigned( Socket )) then
    begin
      Socket.UserData:= Self;
      Caption:= Socket.CloneOf.Name + ': ' + Socket.CloneOf.SocketType;
      if ( Socket.CloneOf.Direction = nsdOutput ) then
        begin
          HAlignment:= haLeft;
          Btn.Align:= alRight;
        end
      else
        begin
          HAlignment:= haRight;
          Btn.Align:= alLeft;
        end;
      if ( Socket.CloneOf.Direction = nsdInput ) then begin
        FPropEd.AsValue:= TP3DNodeSocketPropEdit.Create( Controls );
        with ( PropEd ) do
          begin
            Align:= alClient;
            Socket:= Self.Socket;
            Visible:= not Assigned( Self.Socket.Connected );
          end;
      end else
        FPropEd.AsValue.Free;
    end;
end;

constructor TP3DNodeSocketControl.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  Font.Size:= 12;
  Height:= 12;
  FBtn:= TP3DStreamablePointer.Create( 'Btn', TP3DGraphicControl );
  Properties.Add( FBtn );
  FBtn.AsValue:= TP3DGraphicControl.Create( Self.Controls );
  Btn.OnDragDrop:= @BtnDragDrop;
  Btn.OnMouseDown:= @BtnMouseDown;
  Btn.OnDragDropCancel:= @BtnDragDropCancel;
  Btn.OnDraw:= @BtnDraw;
  Btn.Width:= 12;
  Btn.Height:= 12;
  Btn.Cursor:= curNodeEdit;

  {Btn.PresetNormal.Color:= Blue;
  Btn.PresetHover.Color:= Blue;
  Btn.PresetDown.Color:= Blue;}
  FPropEd:= TP3DStreamablePointer.Create( 'PropEd', TP3DNodeSocketPropEdit );
  Properties.Add( FPropEd );
  Margin:= vec4( 0 );
  OnDragDrop:= @BtnDragDrop;
  OnMouseDown:= @BtnMouseDown;
  OnDragDropCancel:= @BtnDragDropCancel;
  FSocket:= TP3DStreamablePointer.Create( 'Socket', TP3DNodeSocketClone );
  Properties.Add( FSocket );
  //Btn.Caption:= '';
end;

procedure TP3DNodeSocketControl.Draw;
  procedure RenderConnector;
  var
    S, E, M1, M2: TVec2;
    d: Single;
    Connected: TP3DNodeSocketControl;
    cl: TVec4;
  begin
    if ( not Assigned( Socket.CloneOf )) then
      exit;
    if ( Socket.CloneOf.Direction = nsdOutput ) then
      S:= vec2( ClippedRect.Left + ClippedRect.Width + 5, ClippedRect.Top + ClippedRect.Height / 2 )
    else
      S:= vec2( ClippedRect.Left - 5, ClippedRect.Top + ClippedRect.Height / 2 );

    if ( Editing ) then
      begin
        E:= vec2( UIViewer.MouseX, UIViewer.MouseY );
        d:= ( S - E ).length();
        cl:= vec4( 0.6, 0.3, 0.5, 1 );
        if ( Socket.CloneOf.Direction = nsdOutput ) then
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
      if ( Assigned( Socket.Connected ) and ( Socket.CloneOf.Direction = nsdInput )) then
        begin
          cl:= vec4( 1 );

          Connected:= TP3DNodeSocketControl( Socket.Connected.UserData );
          if ( not Assigned( Connected )) then
            exit;
          E:= vec2( Connected.ClippedRect.Left + Connected.ClippedRect.Width + 5, Connected.ClippedRect.Top + Connected.ClippedRect.Height / 2 );
          d:= ( S - E ).length();
          M1:= vec2( S.X - d / 3, S.Y );
          M2:= vec2( E.X + d / 3, E.Y );
        end
    else
      exit;

    with ( UIViewer ) do
      begin
        ScreenCanvas.Lock();
        ScreenCanvas.Pen.Width:= 3;
        ScreenCanvas.RenderBezier( S, M1, M2, E, round( sqrt( d )), vec4( 0, 0, 0, 1 ));
        ScreenCanvas.Pen.Width:= 1;
        ScreenCanvas.RenderBezier( S, M1, M2, E, round( sqrt( d )), cl );
        ScreenCanvas.Unlock();
      end;
  end;
begin
  inherited Draw;

  if (( gcisMouseOver in InputState ) and ( P3DUI.DragDropSrc <> nil )) then
    Canvas.RenderRectShadowInner( vec2( 0 ), vec2( Width, Height ), 2, 5, vec4( 0, 0, 0, 0.5 ));
  if ( Assigned( Socket )) then
    RenderConnector;
end;

{ TP3DNodeSocketPropEdit }

procedure TP3DNodeSocketPropEdit.SetSocket( AValue: TP3DNodeSocketClone );
begin
  FSocket.AsValue:= AValue;
  {if ( Assigned( AValue )) then
    Caption:= AValue.CloneOf.Name + ': ' + AValue.CloneOf.SocketType
  else
    Caption:= 'not assigned!';}
  Update;
end;

{function TP3DNodeSocketPropEdit.GetDropDown: TP3DCustomPopupMenu;
begin
  Result:= TP3DCustomPopupMenu( FDropDown.AsValue );
end;

procedure TP3DNodeSocketPropEdit.SetDropDown(AValue: TP3DCustomPopupMenu);
begin
  FDropDown.AsValue:= AValue;
end;
}
function TP3DNodeSocketPropEdit.GetSocket: TP3DNodeSocketClone;
begin
  Result:= TP3DNodeSocketClone( FSocket.AsValue );
end;

constructor TP3DNodeSocketPropEdit.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  {FDropDown:= TP3DStreamablePointer.Create( 'DropDown', TP3DCustomPopupMenu );
  Properties.Add( FDropDown );
  DropDown:= TP3DCustomPopupMenu.Create();
  DropDown.Visible:= False;
  DropDown.PageModeY:= pmExpand;}
  FSocket:= TP3DStreamablePointer.Create( 'Socket', TP3DNodeSocketClone );
  Properties.Add( FSocket );
  Update;
end;

procedure TP3DNodeSocketPropEdit.Update;
var
  i: Integer;
  Ed: TP3DGraphicControl;
begin
  Controls.Clear;
  //DropDown.Width:= Width;
  if ( Assigned( Socket )) then
    begin
      Ed:= GetPropEdForProp( Socket.ValueProp ).Create( {DropDown.}Controls );
      if ( Ed is TP3DButton ) then
        TP3DButton( Ed ).Font.Size:= 12;
      Ed.Canvas.Font.Size:= 12;
      FPropEdit:= Ed as IP3DPropertyEditor;
      PropEdit.Prop:= Socket.ValueProp;
      Ed.Align:= alClient;
    end;
  inherited Update;
end;

procedure TP3DNodeSocketPropEdit.MouseClick(mb1, mb2, mb3: Boolean; X,
  Y: Float);
begin
  inherited MouseClick(mb1, mb2, mb3, X, Y);
  {DropDown.Width:= ClippedRect.Width;
  DropDown.PopUp( ClippedRect.Left, ClippedRect.Top + ClippedRect.Height );}
end;


{$ENDIF}
