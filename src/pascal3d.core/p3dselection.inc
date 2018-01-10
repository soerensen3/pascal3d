//p3dselection.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}

procedure P3DSelectStart( ARect: TP3DRect );
procedure P3DSelectPush( Value: Pointer );
procedure P3DSelectEnd();

var
  P3DSelection: array [ 0..2047 ] of Pointer;
  P3DSelectionSize: LongWord;

{$ENDIF}

{$IFDEF IMPLEMENTATION}
var
  PickMaterial: TP3DMaterialBase;

  P3DSelRect: TP3DRect;
  P3DPushStack: array [ 0..2047 ] of Pointer;
  P3DPushStackSize: LongWord;


procedure P3DSelectStart( ARect: TP3DRect );
begin
  glScissor( ARect.LeftI, P3DViewports.Screen.Height - ARect.TopI, ARect.WidthI, ARect.HeightI ); cle( nil );
  glEnable( GL_SCISSOR_TEST ); cle( nil );
  if ( not Assigned( PickMaterial )) then
    begin
      PickMaterial:= TP3DMaterialBase.CreateBlock();
      PickMaterial.Name:= 'PickMaterial';
      PickMaterial.Unlit:= True;
    end;
  PickMaterial.PassToShader();
  P3DPushStackSize:= 0;
  glClearColor( 1, 1, 1, 1 );
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );
  P3DSelRect:= ARect;
end;

procedure P3DSelectPush( Value: Pointer );
var
  iv: array [0..3] of byte absolute P3DPushStackSize;
  Diff: TVec3;
begin
  if ( P3DPushStackSize = Length( P3DPushStack )) then
    raise Exception.Create( 'Too many objects in push stack! Maximum 2048 allowed!' );
  P3DPushStack[ P3DPushStackSize ]:= Value;
  Inc( P3DPushStackSize );

  Diff:= vec3( iv[ 0 ], iv[ 1 ], iv[ 2 ]) / 255;
  glVertexAttrib4f( P3DAttribColor,
    Diff.R, Diff.G, Diff.B, 1.0 ); cle( nil );
end;

procedure P3DSelectEnd();
var
  PixelData: array of longword;
  i, idx: Integer;

  function AlreadyInSelection( idx: LongWord ): Boolean; inline;
  var
    n: Integer;
  begin
    Result:= False;
    for n:= 0 to P3DSelectionSize - 1 do
      if ( P3DSelection[ n ] = P3DPushStack[ idx ]) then
        begin
          Result:= True;
          break;
        end;
  end;

begin
  glDisable( GL_SCISSOR_TEST ); cle( nil );
  PickMaterial.Shader.Disable;

  SetLength( PixelData, P3DSelRect.WidthI * P3DSelRect.HeightI );
  glReadPixels( P3DSelRect.LeftI, P3DViewports.Screen.Height - P3DSelRect.TopI, P3DSelRect.WidthI, P3DSelRect.HeightI, GL_RGBA, GL_UNSIGNED_BYTE, @PixelData[ 0 ]); cle( nil, 'glReadPixels' );

  P3DSelectionSize := 0;

  for i := 0 to High( PixelData ) do
    begin
      idx:= PixelData[ i ] and $FFFFFF - 1;
      //P3DApplication.MainWindow.Title:= hexStr( idx, 8 );
      if (( idx >= 0 ) and ( idx < P3DPushStackSize ) and ( not AlreadyInSelection( idx ))) then
        begin
          P3DSelection[ P3DSelectionSize ]:= P3DPushStack[ idx ];
          Inc( P3DSelectionSize );
        end;
    end;
end;

{$ENDIF}
