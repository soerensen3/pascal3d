unit p3dgui_sceneviewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  p3dgui_stdctrls, p3dshaders, p3dscene, dglOpenGL, p3dMath, p3dbuffers, p3dviewport;

type

{ TP3DGUIShaderPreview }

  { TP3DGUISceneViewer }

  TP3DGUISceneViewer = class ( TP3DGroupBox )
    private
      FScene: tScene;

    public
      procedure Render(BaseColor: TVec4; ScrollAcc: TVec2); override;
      property Scene: tScene read FScene write FScene;
  end;

  procedure InitTriangle;

  var
    TriangleScene: tScene;
    TriangleCam: tCamera;
    TrianglePoints: TP3DVec3BufferGL;
    TriangleColors: TP3DVec4BufferGL;

implementation

procedure InitTriangle;
begin
  TrianglePoints:= TP3DVec3BufferGL.Create( False );
  TrianglePoints.Add([ vec3( -1.0, -1.0, 0.0 ),
               vec3(  1.0, -1.0, 0.0 ),
               vec3(  0.0,  1.0, 0.0 )]);
  TrianglePoints.PushData;
  TriangleColors:= TP3DVec4BufferGL.Create( False );
  TriangleColors.Add([ vec4( 1.0, 0.0, 0.0, 1.0 ),
               vec4( 0.0, 1.0, 0.0, 1.0 ),
               vec4( 0.0, 0.0, 1.0, 1.0 )]);
  TriangleColors.PushData;
end;


procedure RenderTriangle(Scene: tScene);
begin
  if ( not Assigned( TrianglePoints )) then
    InitTriangle;
  TrianglePoints.SetAttribArray( P3DAttribPosition );
  TriangleColors.SetAttribArray( P3DAttribColor );

  Scene.Shader.Uniforms[ Scene.Shader.Uniforms.FindByName( 'world' )].AsMat4:= Mat4Identity;

  glDrawArrays( GL_TRIANGLES, 0, 3 );

  TrianglePoints.UnsetAttribArray();
  TriangleColors.UnsetAttribArray();
end;


{ TP3DGUISceneViewer }

procedure TP3DGUISceneViewer.Render(BaseColor: TVec4; ScrollAcc: TVec2);
begin
  inherited Render(BaseColor, ScrollAcc);
  //Canvas.Lock;
  P3DViewports.Push( Canvas.Left + ClientRect.Left, Canvas.Top + ClientRect.Top,
                     ClientRect.Right, ClientRect.Bottom );
  if ( Assigned( Scene )) then
    Scene.Render
  else
    TriangleScene.Render;
  P3DViewports.Pop;
  //Canvas.Unlock();
end;

initialization
  TriangleCam:= tCamera.Create;
  TriangleCam.Position:= vec3( 0, 0, 2 );
  TriangleScene:= tScene.Create;
  TriangleScene.Cam:= TriangleCam;
  TriangleScene.DrawObjects:= @RenderTriangle;

finalization
  TriangleScene.Free;
  TrianglePoints.Free;
  TriangleColors.Free;


end.

