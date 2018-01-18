unit p3dlclceditorscene;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Math, p3dlcleditorfile, OpenGLContext, Controls, dglOpenGL,
  p3dscene, p3dmodel, p3dMath, p3dshaders, p3dinput, p3dSDLApplication, LCLIntf, SDL2;

type
  { TP3DEditorFileScene }

  TP3DEditorFileScene = class( TP3DEditorFile )
    private
      FRenderScene: tScene;
      FScene: TP3DScene;
      Fworld: TMat4;
      FXRot: Single;
      FYRot: Single;
      FZoom: Single;

      Viewport: TOpenGLControl;
    public
      constructor Create(AFileName: String; TabSheet: TTabSheet); override;
      destructor Destroy; override;

      procedure Paint( Sender: TObject );
      procedure Input;
      procedure FileRender; override;
      procedure RenderObjects( AScene: tScene );
      property Scene: TP3DScene read FScene write FScene;
      property RenderScene: tScene read FRenderScene write FRenderScene;

      property XRot: Single read FXRot write FXRot;
      property YRot: Single read FYRot write FYRot;
      property Zoom: Single read FZoom write FZoom;
      property world: TMat4 read Fworld write Fworld;
  end;


  procedure RegExt;

  var
    cube_rotationx, cube_rotationy, cube_rotationz: Single;


implementation


procedure RegExt;
begin
  RegisterExtension( '.model', TP3DEditorFileScene );
end;


{ TP3DEditorFileScene }

constructor TP3DEditorFileScene.Create(AFileName: String; TabSheet: TTabSheet);
begin
  inherited Create( AFileName, TabSheet );

  {Viewport:= TOpenGLControl.Create( Page );
  Viewport.Parent:= Page;
  Viewport.Align:= alClient;
  Viewport.AutoResizeViewport:= True;}

  RenderScene:= tScene.Create;
  SetCurrentDir( '../pascal3d/engine_runtime/shaders/' ); //TODO: Change this
  RenderScene.Shader:= CreateVertexAndFragmentShader( LoadShaderToText( 'default3d.vert' ), LoadShaderToText( 'default3d.frag' ));
  Scene:= TP3DScene.Create( AFileName );
  RenderScene.Cam:= tCamera.Create;
  RenderScene.Cam.Position:= vec3( 0, 0, 10 );

  //Viewport.OnPaint:= @Paint;
  RenderScene.DrawObjectsObj:= @RenderObjects;

  FZoom:= 1;
  world:= mat4rotate( vec3_Axis_PX, deg2rad * FXRot ) * mat4rotate( vec3_Axis_PY, deg2rad * FYRot ) * mat4scale( vec4( Zoom )) * mat4translate( vec4( 0, 0, -1, 1 ));
end;

destructor TP3DEditorFileScene.Destroy;
begin
  RenderScene.Free;
  Scene.Free;
  Viewport.Free;
  inherited Destroy;
end;

procedure TP3DEditorFileScene.Paint(Sender: TObject);
begin
  Input;
  glClearColor($06 / 255, $2C / 255, $29 / 255, 1.0);                      // Set the Background colour of or scene
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);   // Clear the colour buffer
  RenderScene.Render;
  //SDL_GL_MakeCurrent( @P3DApplication.MainWindow, P3DApplication.MainWindow. );
  SDL_GL_SwapWindow( P3DApplication.MainWindow.Window );
  //Viewport.SwapBuffers;
end;

procedure TP3DEditorFileScene.Input;
var
  clrect: Classes.TRect;
begin
//  clrect:= Rect( Page.ClientOrigin.X, Page.ClientOrigin.Y,
//                 Page.ClientOrigin.X + Page.ClientWidth, Page.ClientOrigin.X + Page.ClientHeight );
  clrect:= Rect( 0, 0, P3DApplication.MainWindow.Width, P3DApplication.MainWindow.Height );
  if ( InputManager.Mouse.Buttons[ 0 ]) then
    if ( PtInRect( clrect, Point( InputManager.Mouse.X, InputManager.Mouse.Y ))) then
      begin
        if ( InputManager.Keyboard.Keys[ P3DK_LCTRL ]) then
          FZoom:= Max( 0.05, FZoom - InputManager.Mouse.DY / 200 )
        else
          begin
           FXRot+= InputManager.Mouse.DY / 5;
           FYRot+= InputManager.Mouse.DX / 5;
          end;
        world:= mat4rotate( vec3_Axis_PX, deg2rad * FXRot ) * mat4rotate( vec3_Axis_PY, deg2rad * FYRot ) * mat4scale( vec4( Zoom )) * mat4translate( vec4( 0, 0, -1, 1 ));
      end;
end;

procedure TP3DEditorFileScene.FileRender;
begin
  Input;
  //Viewport.Invalidate;
  Paint(nil);
end;

procedure TP3DEditorFileScene.RenderObjects(AScene: tScene);
begin
  if ( Assigned( Scene )) then
    Scene.Render( world );
end;

initialization
  RegExt;

end.

