procedure TP3DSceneApplication.SetClearColor(AValue: TVec4);
begin

end;

constructor TP3DSceneApplication.Create;
begin
  inherited Create;
  CreateTestSurface;
end;

procedure TP3DSceneApplication.MouseMotion(Event: TSDL_MouseMotionEvent);
begin
  inherited MouseMotion(Event);
end;

procedure TP3DSceneApplication.Keyboard(Event: TSDL_KeyboardEvent);
  procedure UpdateMaterials( Data: TP3DLibrary );
  var
    i: Integer;
  begin
    for i:= 0 to Data.Materials.Count - 1 do
      if ( Data.Materials[ i ] is TP3DMaterialBase ) then
        ( Data.Materials[ i ] as TP3DMaterialBase ).BuildShader();
  end;

var
  i: Integer;
  Tex: TP3DTexture;
  FrameBuffer: TP3DFrameBuffer;
  S: TFileStream;
  P, json: TJSONParser;
  D, json_str: TJSONData;
  lib: TP3DLibrary;
  Loader: TP3DJSONLoader;

begin
  inherited Keyboard(Event);

  if ( Event.keysym.scancode = SDL_SCANCODE_F2 ) then
    begin
      {with ( P3DData ) do
        begin
          CreateDOMFile;
          SaveToDOM();
          SaveDOMFile( 'P3DData.p3d' );
          CloseDOMFile;
        end;

      for i:= 0 to P3DData.Libraries.Count - 1 do
        begin
          P3DData.Libraries[ i ].CreateDOMFile;
          P3DData.Libraries[ i ].SaveToDOM();
          P3DData.Libraries[ i ].SaveDOMFile( P3DData.Libraries[ i ].Name );
          P3DData.Libraries[ i ].CloseDOMFile;
        end;}
      if ( Assigned( SceneMain.ActiveLibrary )) then
        with ( SceneMain.ActiveLibrary ) do
          begin
            SetCurrentDir( P3DSearchPaths.BaseDir.FileName ); //SceneMain.ActiveLibrary.FileWatch.FileName );
            SaveToFile();
          end;
    end;
  {  begin
      FrameBuffer:= TP3DFrameBuffer.Create();
      //FrameBuffer.Bind( 0 );
      glBindFramebuffer( GL_FRAMEBUFFER, FrameBuffer.FBO );
      glGenTextures( 1, @i );
      glBindTexture(GL_TEXTURE_2D, i);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_BASE_LEVEL, 0);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 0);
      //NULL means reserve texture memory, but texels are undefined
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, P3DGUIManager.ScreenCanvas.Width, P3DGUIManager.ScreenCanvas.Height, 0, GL_BGRA, GL_UNSIGNED_BYTE, nil );
      Tex:= TP3DTexture.CreateFromGLTexture( i );
      //Tex:= TP3DTexture.CreateFromEmpty( P3DGUIManager.ScreenCanvas.Width, P3DGUIManager.ScreenCanvas.Height, 3, dtByte );
      Tex.MipMap:= False;
      Tex.Filtering:= tfNearest;
      Tex.FilteringMipMap:= tfNearest;

      FrameBuffer.AttachColor( Tex );
      Tex.Unbind();
      try
        FrameBuffer.CheckFBO;
        P3DViewports.Push( 0, 0, Tex.Width, Tex.Height );
        glClearColor( 0.5, 1.0, 1.0, 1.0 );
        FrameBuffer.Clear();
        //P3DGUIManager.Render;
        {glBegin(GL_TRIANGLES);
          glColor3f(1, 0, 0); glVertex3f(-1,-1, 0);
          glColor3f(0, 0, 1); glVertex3f( 1,-1, 0);
          glColor3f(0, 1, 0); glVertex3f( 0, 1, 0);
        glEnd;}
        FrameBuffer.Unbind();
        P3DViewports.Pop;
      except
        on E: Exception do
          SDL_ShowSimpleMessageBox( SDL_MESSAGEBOX_ERROR, PChar( 'Error rendering to FB' ), PChar( E.Message ), Nil );
      end;
      FrameBuffer.Free;
      DataView.Update;
      DataView.Children.ExpandAll();
    end;}
  if (( Event.keysym.scancode = SDL_SCANCODE_F3 ) and ( Event._repeat = 0 ) and ( Event.type_ = SDL_KEYDOWN )) then
    //if ( FileExists( '/tmp/frog.p3d' )) then
      begin
        try
          if ( Assigned( SceneMain.ActiveDataBlock )) then
            with ( TStringList.Create ) do
              begin
                Text:= SceneMain.ActiveDataBlock.Serialize();
                SaveToFile( '/tmp/tmp.json' );
                Free;
              end;
          //SetCurrentDir( '/tmp/' );
          //SceneMain.ActiveLibrary.SaveToFile( 'test.p3d' );
          {with TStringList.Create do
            begin
              Text:= SceneMain.ActiveLibrary.Serialize;
              json:= TJSONParser.Create( Text );
              json_str:= json.Parse;
              Text:= json_str.FormatJSON();
              json_str.Free;
              json.Free;
              SaveToFile( 'test.p3d' );
              Free;
            end;}
          {Loader:= TP3DJSONLoader.Create( '/tmp/frog.p3d' );
          lib:= SceneMain.NewLibrary( '/tmp/frog.p3d' );
          Loader.Root:= lib;
          lib.LoadFromJSONContext( P3DJSONContext( Loader, Loader.JSON, lib, nil ));}

        finally
//          Loader.Free;
        end;
{        S:= TFileStream.Create( '/tmp/frog.p3d', fmOpenRead );
        try
          P:= TJSONParser.Create( S );
          P.Strict:= False;
          D:= P.Parse;
          lib:= SceneMain.NewLibrary( '/tmp/frog.p3d' );
          lib.LoadFromJSONContext( P3DJSONContext( D, lib, nil ));
        finally
          P.Free;
        end;
        S.Free;}
      end;
  if (( Event.keysym.scancode = SDL_SCANCODE_F4 ) and ( Event._repeat = 0 ) and ( Event.type_ = SDL_KEYDOWN )) then
    with ( TP3DModalWindow.Create()) do
      begin
        Width:= 300;
        Height:= 150;
        ShowModal;
      end;
  if ( Event.keysym.scancode = SDL_SCANCODE_F5 ) then
    begin DataView.Viewer.Update; DataView.Viewer.Children.ExpandAll(); end;
  if ( Event.keysym.scancode = SDL_SCANCODE_F8 ) then
    begin
      P3DShaderNodeLib.Nodes.Clear( True );

      SetCurrentDir( ExtractFileDir( ParamStr( 0 )));
      UpdateMaterials( P3DData );
      for i:= 0 to P3DData.Libraries.Count - 1 do
        UpdateMaterials( P3DData.Libraries[ i ]);
    end;
  if ( Event.keysym.scancode = SDL_SCANCODE_LALT ) then
    begin
      OIPanel.ObjectInspector.Obj:= P3DGUIManager.Controls.MouseRay( P3DInput.Mouse.X, P3DInput.Mouse.Y, True );
    end;

end;


procedure TP3DSceneApplication.Events;
begin
  inherited Events;
  P3DGUIManager.Input;
end;

