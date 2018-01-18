{$IFDEF IMPLEMENTATION}

procedure __appclassname__.SetCatchMouse(AValue: Boolean);
begin
  if ( CatchMouse = AValue ) then Exit;
  FCatchMouse:= AValue;
  SDL_SetRelativeMouseMode( TSDL_Bool( CatchMouse )); // Catch the cursor. Useful for fly cams.
end;

procedure __appclassname__.SetClearColor(AValue: TVec4);
begin
  if ( FClearColor = AValue ) then Exit;
  FClearColor:= AValue;
  glClearColor( ClearColor.r, ClearColor.g, ClearColor.b, 1.0 );    // Set the background color
end;

{$IFDEF GUI}
procedure __appclassname__.TreeViewSelectionChange(var NewSelection: TP3DTreeNode);
begin
  //Change the selected object of the object inspector according to the treeview selection
  if ( Assigned( NewSelection )) then
    OI.Obj:= TInterfacedPersistent( NewSelection.Data )
  else
    OI.Obj:= nil;
end;
{$ENDIF}

procedure __appclassname__.Initialize;
begin
  inherited Initialize;

  try
    //Do some OpenGL initializations
    InitGL;

    ClearColor:= Blue500; //Set the background color of the window to blue

    {Load different objects and append to scenes.
     We make one scene for each set of objects that should be rendered individually.
     We could also search the libraries for the scenes like this:
     MainScene:= P3DData.Libraries[ P3DData.OpenLibrary( 'colorwheel.p3d' )].Scenes.GetByName( 'Scene' );
     If we append the scenes as instances to the main scene we are able to move or rotate the scenes individually.
    }

    // create the mainscenes a skybox and the camobjects
    MainScene:= TP3DScene.Create;
    MainScene.AppendFile( 'colorwheel.p3d' );
    MainScene.MakeRenderList( nil, Mat4Identity, FRenderList );

    CamObj:= P3DData.Objects[ P3DData.CreateNew( TP3DCamera )];
    CamObj.Position:= vec3( -1.5, -3, 1 );
    CamObj.Rotation:= vec3( 70, 0, -20 );
    // Main camera

    SkyScene:= TP3DScene.Create;
    SkyScene.AppendFile( 'skybox.p3d' );
    SkyScene.MakeRenderList( nil, Mat4Identity, FSkyRenderList );

    SkyCamObj:= P3DData.Objects[ P3DData.CreateNew( TP3DCamera )];
    // Make a separate sky cam that is only rotated but stays in the center.

    // Draw arrows to display camera rotation.
    ArrowScene:= TP3DScene.Create;
    ArrowScene.AppendFile( 'EditorArrows.p3d' );
    ArrowScene.MakeRenderList( nil, Mat4Identity, FArrowRenderList );

    //Create renderlists of the
    MainScene.MakeRenderList( nil, Mat4Identity, FRenderList );
    SkyScene.MakeRenderList( nil, Mat4Identity, FSkyRenderList );
    ArrowScene.MakeRenderList( nil, Mat4Identity, FArrowRenderList );

    {$IFDEF GUI}
    //GUI
    {Create a TreeView and object inspector to debug the scene.
    We set the mainscene as root.}
    P3DGUIInit;
    with ( TP3DTreeViewActors.Create()) do
      begin
        Align:= alLeft;
        Width:= 200;
        Obj:= nil;
        Update;
        ShowSymbols:= True;
        OnSelectionChange:= @TreeViewSelectionChange;
      end;
    OI:= TP3DObjectInspector.Create();
    with OI do
      begin
        Align:= alRight;
        Width:= 300;
        Obj:= MainScene;
      end;
    {$ENDIF}
  except
    on E: Exception do
      begin
        P3DLog.LogException( Self, E.Message ); // Log and display error messages, if an asset was not found
        SDL_ShowSimpleMessageBox( SDL_MESSAGEBOX_ERROR, PChar( 'Error while initializing application' ), PChar( E.Message ), Nil );
      end;
  end;
end;

procedure __appclassname__.InitGL;
begin
  glEnable( GL_BLEND );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
end;

procedure __appclassname__.Finalize;
begin
  {$IFDEF GUI}
  P3DGUIFinish;
  {$ENDIF}
  inherited Finalize;
  mainscene.Free;
  camobj.Free;
end;



{$ENDIF}
