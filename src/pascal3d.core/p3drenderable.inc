//p3drenderable.inc

{$IFDEF INTERFACE}
  type
    TP3DObject = class; TP3DScene = class;

    IP3DRenderable = interface
      ['{D1EC4419-688C-44DD-8EFD-B7CD43F10C06}']

      procedure Render( world: TMat4; Scene: TP3DScene; RenderObject: TP3DObject );
      procedure AddToRenderList( ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags );
    end;

    {generic gP3DRenderable < Tp: TP3DDataBlock > = class( Tp, IP3DRenderable )
      public
        procedure Render( world: TMat4; Scene: TP3DScene; RenderObject: TP3DObject ); virtual; abstract;
        procedure AddToRenderList( ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags );  virtual; abstract;
    end;}

    //TP3DRenderable = specialize gP3DRenderable < TP3DDataBlock >;
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{$ENDIF}

