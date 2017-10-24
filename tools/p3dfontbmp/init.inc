{$IFDEF IMPLEMENTATION}

procedure ProcessFile( FileName: String );
var
  FN: String;
  n: Integer;
  loader: TP3DJSONLoader;
  fnt: TP3DFontBmp;
  FN2: String;
begin
  FN:= P3DSearchPaths.FindFile( FileName );
  FN2:= ChangeFileExt( FN, '.p3d' );
  if ( FileExists( FN2 )) then
    DeleteFile( FN2 );
  fnt:= P3DFontManagerBmp.FontsByP3DFont[ TP3DFont.Create( FN, round( P3DFontManagerBmp.DefaultTextSize ))];
  fnt.Owner.SaveToFile();
  //fnt.Owner.Free;
  {loader:= TP3DJSONLoader.Create( ChangeFileExt( FN, '.p3d' ), fnt );
  loader.WriteFile;
  loader.Free;}
end;

procedure ParseParams;
var
  i: Integer;
  tmpStr: String;
begin
  i:= 1;
  while i <= ParamCount do
    begin
      case ParamStr( i ) of
        '--font-size':
          begin
            P3DFontManagerBmp.DefaultTextSize:= StrToFloat( ParamStr( i + 1 ));
            Inc( i, 1 );
          end;
        '--tex-size':
          begin
            P3DFontManagerBmp.DefaultTexWidth:= StrToInt( ParamStr( i + 1 ));
            P3DFontManagerBmp.DefaultTexHeight:= StrToInt( ParamStr( i + 2 ));
            Inc( i, 2 );
          end;
        '--text-margin':
          begin
            P3DFontManagerBmp.DefaultTextMargin:= StrToInt( ParamStr( i + 1 ));
            Inc( i, 1 );
          end;
        '--from-file':
          begin
            tmpStr:= ParamStr( i + 1 );
            ProcessFile( tmpStr );
            //WriteLn( 'from file: ', tmpStr );
            Inc( i );
          end;
        '--from-path':
          begin
            tmpStr:= ParamStr( i + 1 );
            WriteLn( 'from path: ', tmpStr );
            Inc( i );
          end;
        else
          WriteLn( 'Unknown param: ', ParamStr( i ));
      end;
      Inc( i );
    end;

end;

procedure TP3DFontBitmap.Initialize;
begin
  inherited Initialize;
  Zoom:= 1;

  try
    //Do some OpenGL initializations
    InitGL;
    ParseParams;
    ActiveFont:= P3DFontManagerBmp.Items[ 0 ];
    Canvas:= TP3DCanvas2D.Create( Self );
    //Canvas.Material:= P3DFontManager.Material;
    Canvas.WidthHeight:= vec2( P3DViewports.Screen.Width, P3DViewports.Screen.Height );

  except
    on E: Exception do
      begin
        P3DLog.LogException( Self, E.Message ); // Log and display error messages, if an asset was not found
        SDL_ShowSimpleMessageBox( SDL_MESSAGEBOX_ERROR, PChar( 'Error while initializing application' ), PChar( E.Message ), Nil );
      end;
  end;
end;

procedure TP3DFontBitmap.InitGL;
begin
  glEnable( GL_BLEND );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
end;

procedure TP3DFontBitmap.Finalize;
begin
  Canvas.Free;
  {$IFDEF GUI}
  P3DGUIFinish;
  {$ENDIF}
  inherited Finalize;
end;



{$ENDIF}
