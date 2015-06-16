{$IFDEF INTERFACE}
procedure DrawObjects( Scene: tScene ); forward;
{$UNDEF INTERFACE}
{$ELSE}

procedure CheckFileChanges; //inline;
begin
  if ( WatchTime < GetTickCount ) then
    WatchTime:= GetTickCount + 500
  else
    Exit;
  if ( Assigned( Watch_simpleshader_F ) AND Assigned( Watch_simpleshader_V )) then
    if ( Watch_simpleshader_F.CheckForChange or Watch_simpleshader_V.CheckForChange ) then
      begin
        UnloadShader;
        LoadShader;
      end;
  if ( Assigned( Watch_Mdl )) then
    if ( Watch_Mdl.CheckForChange ) then
      begin
        UnloadModels;
        LoadModels;
      end;
end;

procedure Render( Sender: TSDLWindow );
var
  p: TVec4;
begin
  CheckFileChanges;

  glClearColor($06 / 255, $2C / 255, $29 / 255, 1.0);                      // Set the Background colour of or scene
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);   // Clear the colour buffer

  mainscene.Render;
end;

procedure DrawObjects( Scene: tScene );
var
  i: Integer;
  cell: Integer;
  mdlview: TMat4;
  Timing: Integer;
  TimeGone: Integer;
  cube: TPhysicObject;
  sphere: TPhysicObject;
  PlayerMat: TMat4;
begin
  Timing:= GetTickCount;
  TimeGone:= Timing - LastTime;
  LastTime:= Timing;

  Physics.Process( TimeGone / 1000 );
  PlayerMat:= character.Body.GetMatrix();
  character.ApplyToCamera( maincamera );
//  maincamera.Position:= vec3( -PlayerMat._30, -PlayerMat._31, -PlayerMat._32 - 2 );
//  maincamera.UpdateMatrices;
  Scene.UpdateMatrices;
  if ( not Assigned( ActShad )) then
    exit;

  mainwnd.Title:= Format( 'Camera: Position = [ %f, %f, %f ] YawPitchRoll = [ %f, %f, %f ]',
                  [ maincamera.Position.X, maincamera.Position.Y, maincamera.Position.Z,
                    maincamera.YawPitchRoll.X, maincamera.YawPitchRoll.Y, maincamera.YawPitchRoll.Z]);
//  plane.Render();
  for cube in cubes do
    cube.Render();
  for sphere in spheres do
    sphere.Render();
  terrain.Render();// Mat4Identity, [ rfDebugShowLocation, rfDebugShowBoundingBox ]); //terrain.Body.GetMatrix());
  //Setup3D( maincamera.view * maincamera.proj );
  //RenderLines3D([ vec3( 0 ), vec3( 1 )], vec4( 1, 0, 0, 1 ));
end;

procedure ResizeWnd( Sender: TSDLWindow );
begin

end;

{$ENDIF}
