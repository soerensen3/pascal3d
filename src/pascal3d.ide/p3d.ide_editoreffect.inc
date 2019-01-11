{ p3d.ide_editoreffect.inc }

{$IFDEF INTERFACE}
type

  { TP3DEditorEffect }

  TP3DEditorEffect = class ( specialize gP3DEditor < TP3DEffect >)
    protected
      procedure PopupClick(Sender: TP3DMenuItem); override;
      procedure PopupListClick(Sender: TP3DMenuItem); override;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;
  end;

  { TP3DEditorRenderCommand }

  {TP3DEditorRenderCommand = class ( specialize gP3DEditor < TP3DRenderCommand >)
    protected
      procedure PopupClick(Sender: TP3DMenuItem); override;
      procedure PopupListClick(Sender: TP3DMenuItem); override;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;
  end;}


{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DEditorEffect }

procedure TP3DEditorEffect.PopupClick(Sender: TP3DMenuItem);
begin
  inherited PopupClick(Sender);
  if ( Sender.Caption = 'Delete' ) then
    P3DMainIntf.DeleteEffect( Self );
  Popup.Hide;
end;

procedure TP3DEditorEffect.PopupListClick(Sender: TP3DMenuItem);
begin
  inherited PopupListClick(Sender);
  if ( Sender.Caption = 'Add effect' ) then
    P3DMainIntf.NewEffect( Self, TP3DEffect.CreateBlock( P3DMainIntf.ActiveLibrary ))
  else if ( Sender.Caption = 'Add effect with framebuffer' ) then
    P3DMainIntf.NewEffect( Self, TP3DEffectFramebuffer.CreateBlock( P3DMainIntf.ActiveLibrary ));

  PopupList.Hide;
end;

constructor TP3DEditorEffect.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  Popup.Items.Add( 'Delete' );
  PopupList.Items.Add( 'Add effect' );
  PopupList.Items.Add( 'Add effect with framebuffer' );
  if ( Assigned( P3DMainIntf )) then begin
    P3DMainIntf.RegisterPopup( TP3DEffect, Popup );
    P3DMainIntf.RegisterPopup( TP3DEffectContainerList, PopupList );
  end;
end;

destructor TP3DEditorEffect.Destroy;
begin
  if ( Assigned( P3DMainIntf )) then begin
    P3DMainIntf.UnregisterPopup( TP3DEffect );
    P3DMainIntf.UnregisterPopup( TP3DEffectContainerList );
  end;
  inherited Destroy;
end;


{ TP3DEditorRenderCommand }

procedure TP3DEditorRenderCommand.PopupClick(Sender: TP3DMenuItem);
begin
  inherited PopupClick(Sender);
  Popup.Hide;
end;

procedure TP3DEditorRenderCommand.PopupListClick(Sender: TP3DMenuItem);
begin
  inherited PopupListClick(Sender);

  case Sender.Caption of
  'Renderlist': TP3DRenderCommandRenderList.Create( P3DMainIntf.ActiveEffect.CommandList );
  'Clear': TP3DRenderCommandClear.Create( P3DMainIntf.ActiveEffect.CommandList );
  'Framebuffer': TP3DRenderCommandFramebuffer.Create( P3DMainIntf.ActiveEffect.CommandList );
  'Framebuffer (Target)': TP3DRenderCommandFramebufferTarget.Create( P3DMainIntf.ActiveEffect.CommandList );
  'Framebuffer (Unbind)': TP3DRenderCommandFramebufferUnbind.Create( P3DMainIntf.ActiveEffect.CommandList );
  'Push Viewport': TP3DRenderCommandPushVP.Create( P3DMainIntf.ActiveEffect.CommandList );
  'Pop Viewport': TP3DRenderCommandPopVP.Create( P3DMainIntf.ActiveEffect.CommandList );
  'Set Active Material': TP3DRenderCommandSetActiveMat.Create( P3DMainIntf.ActiveEffect.CommandList );
  end;
  PopupList.Hide;
end;

constructor TP3DEditorRenderCommand.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  Popup.Items.Add( 'Delete' );
  PopupList.Items.Add( 'Renderlist' );
  PopupList.Items.Add( 'Clear' );
  PopupList.Items.Add( 'Framebuffer' );
  PopupList.Items.Add( 'Framebuffer (Target)' );
  PopupList.Items.Add( 'Framebuffer (Unbind)' );
  PopupList.Items.Add( 'Push Viewport' );
  PopupList.Items.Add( 'Pop Viewport' );
  PopupList.Items.Add( 'Set Active Material' );
  if ( Assigned( P3DMainIntf )) then begin
    P3DMainIntf.RegisterPopup( TP3DRenderCommand, Popup );
    P3DMainIntf.RegisterPopup( TP3DRenderCommandContainerList, PopupList );
  end;
end;

destructor TP3DEditorRenderCommand.Destroy;
begin
  if ( Assigned( P3DMainIntf )) then begin
    P3DMainIntf.UnregisterPopup( TP3DRenderCommand );
    P3DMainIntf.UnregisterPopup( TP3DRenderCommandContainerList );
  end;
  inherited Destroy;
end;

{$ENDIF}

