procedure OnMouseButton( Sender: TP3DApplication; Event: TSDL_MouseButtonEvent );
begin

end;

procedure OnMouseMotion( Sender: TP3DApplication; Event: TSDL_MouseMotionEvent );
begin

end;

procedure OnMouseWheel( Sender: TP3DApplication; Event: TSDL_MouseWheelEvent );
begin

end;

procedure OnKey( Sender: TP3DApplication; Event: TSDL_KeyboardEvent );
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
begin
  if ( Event.keysym.scancode = SDL_SCANCODE_F2 ) then
    begin
      SetCurrentDir( P3DSearchPaths.BaseDir );
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
            CreateDOMFile;
            SaveToDOM();
            SaveDOMFile();
            CloseDOMFile;
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
  if ( Event.keysym.scancode = SDL_SCANCODE_F5 ) then
    begin DataView.Viewer.Update; DataView.Viewer.Children.ExpandAll(); end;
  if ( Event.keysym.scancode = SDL_SCANCODE_F8 ) then
    begin
      P3DShaderNodeLib.Nodes.Clear( True );
      P3DShaderNodeLib.LoadLibraryPath( P3DSearchPaths.BaseDir + 'shaders/nodes/core/', '*.pmd' );;
      UpdateMaterials( P3DData );
      for i:= 0 to P3DData.Libraries.Count - 1 do
        UpdateMaterials( P3DData.Libraries[ i ]);
    end;
end;

procedure OnInput( Sender: TP3DApplication );
begin
  P3DGUIManager.Input;
end;
