{ p3d.core.effect.inc }

{$IFDEF INTERFACE}

type

  { TP3DEffect }

  TP3DEffect = class ( TP3DDataBlock )
    protected
      FCommandList: TP3DRenderCommandContainerList;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      procedure Render; virtual;

      class function GetContainer(ALib: TP3DLibrary): TP3DPropertyAccess; override;

      property CommandList: TP3DRenderCommandContainerList read FCommandList;
  end;

  { TP3DEffectCustom }

  TP3DEffectCustom = class ( TP3DEffect )
    protected
      FLib: TP3DStreamablePointer;

      function GetLib: TP3DLibrary;

      procedure CreateLib; virtual;
      procedure PopulateCommandList; virtual; abstract;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      property Lib: TP3DLibrary read GetLib;
  end;


  { TP3DEffectFramebuffer }

  TP3DEffectFramebuffer = class ( TP3DEffect )
    protected
      FFramebuffer: TP3DStreamableContainer;

      function GetFramebuffer: TP3DFrameBuffer;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      procedure Render; override;

      property Framebuffer: TP3DFrameBuffer read GetFramebuffer;
  end;

  TP3DEffectContainerList = specialize gP3DNamedStreamableContainerList < TP3DEffect >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DEffect }

class function TP3DEffect.GetContainer(ALib: TP3DLibrary): TP3DPropertyAccess;
begin
  Result:= ALib.Properties.GetPropByName( 'Effects' );
end;

constructor TP3DEffect.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FCommandList:= TP3DRenderCommandContainerList.Create( 'CommandList', smText );
  Properties.Add( FCommandList );
end;

procedure TP3DEffect.Render;
begin
  CommandList.Execute;
end;

function TP3DEffectCustom.GetLib: TP3DLibrary;
begin
  Result:= TP3DLibrary( FLib.AsValue );
end;

procedure TP3DEffectCustom.CreateLib;
begin
  FLib.AsValue:= TP3DLibrary.Create();
end;

constructor TP3DEffectCustom.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FLib:= TP3DStreamablePointer.Create( 'Lib', TP3DLibrary );
  Properties.Add( FLib );
end;

destructor TP3DEffectCustom.Destroy;
begin
  if ( Assigned( FLib ) and Assigned( Lib )) then
    Lib.Free;
  inherited Destroy;
end;


{ TP3DEffectFramebuffer }

function TP3DEffectFramebuffer.GetFramebuffer: TP3DFrameBuffer;
begin
  Result:= TP3DFrameBuffer( FFramebuffer.AsValue );
end;

constructor TP3DEffectFramebuffer.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FFramebuffer:= TP3DStreamableContainer.Create( 'Framebuffer', TP3DFrameBuffer, smText );
  Properties.Add( FFramebuffer );
  TP3DFrameBuffer.Create( FFramebuffer );
end;

procedure TP3DEffectFramebuffer.Render;
begin
  Framebuffer.BindAll();
  inherited Render;
  Framebuffer.Unbind();
end;

{$ENDIF}

