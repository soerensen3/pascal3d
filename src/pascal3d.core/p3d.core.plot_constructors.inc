{ p3d.core.plot_constructors.inc }

{$IFDEF INTERFACE}

function layer( ALayer: TP3DPlotCommandLayer ): TP3DPlotListLayer;
function layers( ALayers: array of TP3DPlotCommandLayer ): TP3DPlotListLayer;

function layers_join( lists: array of TP3DPlotListLayer ): TP3DPlotListLayer;

function layer_default( Settings: TP3DPlotListSetting ): TP3DPlotCommandLayer;

function settings( ASetting: TP3DPlotCommandSetting ): TP3DPlotListSetting;
function settings( ASettings: array of TP3DPlotCommandSetting ): TP3DPlotListSetting;

function settings_join( lists: array of TP3DPlotListSetting ): TP3DPlotListSetting;

function geom_lines( APoints: TVec3Array; ASettings: TP3DPlotListSetting; const PrimitiveMode: TP3DPrimitiveMode = pmLineStrip ): TP3DPlotCommandLayer;
function geom_lines( APoints: TP3DVec3BufferGL; ASettings: TP3DPlotListSetting; const PrimitiveMode: TP3DPrimitiveMode = pmLineStrip ): TP3DPlotCommandLayer;
function geom_lines( APoints: TP3DVec3BufferGL; AIndices: TP3DIntBufferGL; ASettings: TP3DPlotListSetting; const PrimitiveMode: TP3DPrimitiveMode = pmLines ): TP3DPlotCommandLayer;

function geom_points( APoints: TVec3Array; ASettings: TP3DPlotListSetting ): TP3DPlotCommandLayer;
function geom_points( APoints: TP3DVec3BufferGL; ASettings: TP3DPlotListSetting): TP3DPlotCommandLayer;

function geom_polygon(APoints: TVec3Array; ASettings: TP3DPlotListSetting; const PrimitiveMode: TP3DPrimitiveMode = pmTriangles ): TP3DPlotCommandLayer;
function geom_polygon(APoints: TVec3Array; AIndices: TP3DIntBufferGL; ASettings: TP3DPlotListSetting; const PrimitiveMode: TP3DPrimitiveMode = pmTriangles ): TP3DPlotCommandLayer;
function geom_polygon(APoints: TP3DVec3BufferGL; ASettings: TP3DPlotListSetting; const PrimitiveMode: TP3DPrimitiveMode = pmTriangles ): TP3DPlotCommandLayer;
function geom_polygon(APoints: TP3DVec3BufferGL; AIndices: TP3DIntBufferGL; ASettings: TP3DPlotListSetting; const PrimitiveMode: TP3DPrimitiveMode = pmTriangles ): TP3DPlotCommandLayer;

function geom_polygon(APoints: TVec2Array; ASettings: TP3DPlotListSetting; const PrimitiveMode: TP3DPrimitiveMode = pmTriangles ): TP3DPlotCommandLayer;
function geom_polygon(APoints: TVec2Array; AIndices: TP3DIntBufferGL; ASettings: TP3DPlotListSetting; const PrimitiveMode: TP3DPrimitiveMode = pmTriangles ): TP3DPlotCommandLayer;
function geom_polygon(APoints: TP3DVec2BufferGL; ASettings: TP3DPlotListSetting; const PrimitiveMode: TP3DPrimitiveMode = pmTriangles ): TP3DPlotCommandLayer;
function geom_polygon(APoints: TP3DVec2BufferGL; AIndices: TP3DIntBufferGL; ASettings: TP3DPlotListSetting; const PrimitiveMode: TP3DPrimitiveMode = pmTriangles ): TP3DPlotCommandLayer;

function command_clear( AClearFlags: TP3DPlotCommandClearFlags; AClearColor: TP3DColorRGBA ): TP3DPlotCommandLayer;
function command_render_to_texture( ATexture: TP3DTexture; ALayers: TP3DPlotListLayer ): TP3DPlotCommandLayer;
function command_restore_fb(): TP3DPlotCommandLayer;
function extensions( AExtensions: array of String ): TP3DPlotCommandSetting;

function attrib( ALocation: Integer; AAttrib: Float ): TP3DPlotCommandSetting;
function attrib( AName: String; AAttrib: Float ): TP3DPlotCommandSetting;
function attrib( ALocation: Integer; AAttrib: TVec2 ): TP3DPlotCommandSetting;
function attrib( AName: String; AAttrib: TVec2 ): TP3DPlotCommandSetting;
function attrib( ALocation: Integer; AAttrib: TVec3 ): TP3DPlotCommandSetting;
function attrib( AName: String; AAttrib: TVec3 ): TP3DPlotCommandSetting;
function attrib( ALocation: Integer; AAttrib: TVec4 ): TP3DPlotCommandSetting;
function attrib( AName: String; AAttrib: TVec4 ): TP3DPlotCommandSetting;
function attrib( ALocation: Integer; AAttrib: TVec2Array ): TP3DPlotCommandSetting;
function attrib( ALocation: Integer; AAttrib: TP3DVec2BufferGL ): TP3DPlotCommandSetting;
function attrib( ALocation: Integer; AAttrib: TVec3Array ): TP3DPlotCommandSetting;
function attrib( ALocation: Integer; AAttrib: TP3DVec3BufferGL ): TP3DPlotCommandSetting;
function attrib( ALocation: Integer; AAttrib: TVec4Array ): TP3DPlotCommandSetting;
function attrib( ALocation: Integer; AAttrib: TP3DVec4BufferGL ): TP3DPlotCommandSetting;
function attrib( AName: String; AAttrib: TP3DVec2BufferGL ): TP3DPlotCommandSetting;
function attrib( AName: String; AAttrib: TVec2Array ): TP3DPlotCommandSetting;
function attrib( AName: String; AAttrib: TP3DVec3BufferGL ): TP3DPlotCommandSetting;
function attrib( AName: String; AAttrib: TVec3Array ): TP3DPlotCommandSetting;
function attrib( AName: String; AAttrib: TP3DVec4BufferGL ): TP3DPlotCommandSetting;
function attrib( AName: String; AAttrib: TVec4Array ): TP3DPlotCommandSetting;

function uniform_material( AMaterial: TP3DMaterial ): TP3DPlotCommandSetting;
function uniform_light( ALightObj: TP3DObject ): TP3DPlotCommandSetting;
function uniform( AName: String; ATexture: TP3DTexture ): TP3DPlotCommandSetting;
function uniform( ALocation: GLint; ATexture: TP3DTexture ): TP3DPlotCommandSetting;
function uniform( ALocation: GLint; ATransform: TP3DTransform ): TP3DPlotCommandSetting;
function uniform( AName: String; ATransform: TP3DTransform ): TP3DPlotCommandSetting;
function uniform( AName: String; AValue: Integer ): TP3DPlotCommandSetting;
function uniform( ALocation: GLint; AValue: Integer ): TP3DPlotCommandSetting;
function uniform( AName: String; AValue: Float ): TP3DPlotCommandSetting;
function uniform( ALocation: GLint; AValue: TVec2 ): TP3DPlotCommandSetting;
function uniform( AName: String; AValue: TVec2 ): TP3DPlotCommandSetting;
function uniform( ALocation: GLint; AValue: TVec3 ): TP3DPlotCommandSetting;
function uniform( AName: String; AValue: TVec3 ): TP3DPlotCommandSetting;
function uniform( ALocation: GLint; AValue: TVec4 ): TP3DPlotCommandSetting;
function uniform( AName: String; AValue: TVec4 ): TP3DPlotCommandSetting;
function uniform( ALocation: GLint; AValue: TMat2 ): TP3DPlotCommandSetting;
function uniform( AName: String; AValue: TMat2 ): TP3DPlotCommandSetting;
function uniform( ALocation: GLint; AValue: TMat3 ): TP3DPlotCommandSetting;
function uniform( AName: String; AValue: TMat3 ): TP3DPlotCommandSetting;
function uniform( ALocation: GLint; AValue: TMat4 ): TP3DPlotCommandSetting;
function uniform( AName: String; AValue: TMat4 ): TP3DPlotCommandSetting;

function sample( AName: String; ATexture: TP3DTexture ): TP3DPlotCommandSetting;

function texcoord( AName: String; AAttrib: TP3DVec2BufferGL ): TP3DPlotCommandSetting;
function texcoord( AName: String; AAttrib: TVec2Array ): TP3DPlotCommandSetting;
function texcoord( AName: String; AAttrib: TP3DVec3BufferGL ): TP3DPlotCommandSetting;
function texcoord( AName: String; AAttrib: TVec3Array ): TP3DPlotCommandSetting;
function texcoord( AName: String; AAttrib: TP3DVec4BufferGL ): TP3DPlotCommandSetting;
function texcoord( AName: String; AAttrib: TVec4Array ): TP3DPlotCommandSetting;
function normal( AName: String; AAttrib: TP3DVec2BufferGL ): TP3DPlotCommandSetting;
function normal( AName: String; AAttrib: TVec2Array ): TP3DPlotCommandSetting;
function normal( AName: String; AAttrib: TP3DVec3BufferGL ): TP3DPlotCommandSetting;
function normal( AName: String; AAttrib: TVec3Array ): TP3DPlotCommandSetting;
function sample_normal( AName: String; ATexture: TP3DTexture ): TP3DPlotCommandSetting;
{$ENDIF}

{$IFDEF IMPLEMENTATION}
function layer(ALayer: TP3DPlotCommandLayer): TP3DPlotListLayer;
begin
  //initialize( Result );
  Result:= TP3DPlotListLayer.Create( False );
  Result.FItems:= nil;
  Result.Append( ALayer );
end;

function layers(ALayers: array of TP3DPlotCommandLayer): TP3DPlotListLayer;
var
  i: Integer;
begin
  Result:= TP3DPlotListLayer.Create( False );
  for i:= low( ALayers ) to high( ALayers ) do
    Result.Append( ALayers[ i ]);
end;

function layers_join(lists: array of TP3DPlotListLayer): TP3DPlotListLayer;
var
  i: Integer;
begin
  Result:= TP3DPlotListLayer.Create( False );
  for i:= low( lists ) to high( lists ) do begin
    Result.AppendList( lists[ i ]);
    lists[ i ].CheckDestroy;
  end;
end;

function settings( ASettings: array of TP3DPlotCommandSetting ): TP3DPlotListSetting;
var
  i: Integer;
begin
  Result:= TP3DPlotListSetting.Create( False );
  for i:= low( ASettings ) to high( ASettings ) do
    Result.Append( ASettings[ i ]);
end;


function settings(ASetting: TP3DPlotCommandSetting): TP3DPlotListSetting;
begin
  Result:= TP3DPlotListSetting.Create( False );
  //Result:= default( TP3DPlotListSetting );
  //initialize( Result );
  Result.Append( ASetting );
end;

function settings_join(lists: array of TP3DPlotListSetting): TP3DPlotListSetting;
var
  i: Integer;
begin
  Result:= TP3DPlotListSetting.Create( False );
  for i:= low( lists ) to high( lists ) do
    if ( Assigned( lists[ i ])) then begin
      Result.AppendList( Result.TPlotList( lists[ i ]));
      lists[ i ].CheckDestroy;
    end;
end;

function geom_polygon(APoints: TVec3Array; ASettings: TP3DPlotListSetting; const PrimitiveMode: TP3DPrimitiveMode): TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandPrimitive.Create( PrimitiveMode, Length( APoints ),
              settings_join([
                settings( TP3DPlotCommandAttribVec3Array.CreateArray( P3DAttribPosition, APoints )),
                ASettings ]));
end;

function geom_polygon(APoints: TVec3Array; AIndices: TP3DIntBufferGL; ASettings: TP3DPlotListSetting;
  const PrimitiveMode: TP3DPrimitiveMode): TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandPrimitiveIndexed.Create( PrimitiveMode, AIndices,
              settings_join([
                settings( TP3DPlotCommandAttribVec3Array.CreateArray( P3DAttribPosition, APoints )),
                ASettings ]));
end;

function geom_polygon(APoints: TP3DVec3BufferGL; ASettings: TP3DPlotListSetting; const PrimitiveMode: TP3DPrimitiveMode
  ): TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandPrimitive.Create( PrimitiveMode, APoints.Count,
              settings_join([
                settings( TP3DPlotCommandAttribVec3Array.Create( P3DAttribPosition, APoints )),
                ASettings ]));
end;

function geom_polygon(APoints: TP3DVec3BufferGL; AIndices: TP3DIntBufferGL; ASettings: TP3DPlotListSetting;
  const PrimitiveMode: TP3DPrimitiveMode): TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandPrimitiveIndexed.Create( PrimitiveMode, AIndices,
              settings_join([
                settings( TP3DPlotCommandAttribVec3Array.Create( P3DAttribPosition, APoints )),
                ASettings ]));
end;

function geom_polygon(APoints: TVec2Array; ASettings: TP3DPlotListSetting; const PrimitiveMode: TP3DPrimitiveMode): TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandPrimitive.Create( PrimitiveMode, Length( APoints ),
              settings_join([
                settings( TP3DPlotCommandAttribVec2Array.CreateArray( P3DAttribPosition, APoints )),
                ASettings ]));
end;

function geom_polygon(APoints: TVec2Array; AIndices: TP3DIntBufferGL; ASettings: TP3DPlotListSetting; const PrimitiveMode: TP3DPrimitiveMode
  ): TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandPrimitiveIndexed.Create( PrimitiveMode, AIndices,
              settings_join([
                settings( TP3DPlotCommandAttribVec2Array.CreateArray( P3DAttribPosition, APoints )),
                ASettings ]));
end;

function geom_polygon(APoints: TP3DVec2BufferGL; ASettings: TP3DPlotListSetting; const PrimitiveMode: TP3DPrimitiveMode
  ): TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandPrimitive.Create( PrimitiveMode, APoints.Count,
              settings_join([
                settings( TP3DPlotCommandAttribVec2Array.Create( P3DAttribPosition, APoints )),
                ASettings ]));
end;

function geom_polygon(APoints: TP3DVec2BufferGL; AIndices: TP3DIntBufferGL; ASettings: TP3DPlotListSetting;
  const PrimitiveMode: TP3DPrimitiveMode): TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandPrimitiveIndexed.Create( PrimitiveMode, AIndices,
              settings_join([
                settings( TP3DPlotCommandAttribVec2Array.Create( P3DAttribPosition, APoints )),
                ASettings ]));
end;

function command_clear(AClearFlags: TP3DPlotCommandClearFlags; AClearColor: TP3DColorRGBA): TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandClear.Create( AClearFlags, AClearColor );
end;

function command_render_to_texture(ATexture: TP3DTexture; ALayers: TP3DPlotListLayer): TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandRenderToTexture.Create( ATexture, ALayers );
end;

function command_restore_fb(): TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandRestoreFB.Create();
end;

function extensions(AExtensions: array of String): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandExtensions.Create( AExtensions );
end;

function layer_default(Settings: TP3DPlotListSetting): TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandDefault.Create( Settings );
end;

function geom_lines(APoints: TVec3Array; ASettings: TP3DPlotListSetting; const PrimitiveMode: TP3DPrimitiveMode): TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandPrimitive.Create( PrimitiveMode, Length( APoints ),
              settings_join([
                settings( TP3DPlotCommandAttribVec3Array.CreateArray( P3DAttribPosition, APoints )),
                ASettings ]));
end;

function geom_lines(APoints: TP3DVec3BufferGL; ASettings: TP3DPlotListSetting; const PrimitiveMode: TP3DPrimitiveMode ): TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandPrimitive.Create( PrimitiveMode, APoints.Count,
              settings_join([
                settings( TP3DPlotCommandAttribVec3Array.Create( P3DAttribPosition, APoints )),
                ASettings ]));
end;

function geom_lines(APoints: TP3DVec3BufferGL; AIndices: TP3DIntBufferGL; ASettings: TP3DPlotListSetting; const PrimitiveMode: TP3DPrimitiveMode): TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandPrimitiveIndexed.Create( PrimitiveMode, AIndices,
    settings_join([
      settings( TP3DPlotCommandAttribVec3Array.Create( P3DAttribPosition, APoints )),
      ASettings ]));
end;

function geom_points(APoints: TVec3Array; ASettings: TP3DPlotListSetting): TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandPrimitive.Create( pmPoints, Length( APoints ),
              settings_join([
                settings( TP3DPlotCommandAttribVec3Array.CreateArray( P3DAttribPosition, APoints )),
                ASettings ]));
end;

function geom_points(APoints: TP3DVec3BufferGL; ASettings: TP3DPlotListSetting): TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandPrimitive.Create( pmPoints, APoints.Count,
              settings_join([
                settings( TP3DPlotCommandAttribVec3Array.Create( P3DAttribPosition, APoints )),
                ASettings ]));
end;

function attrib(ALocation: Integer; AAttrib: Float): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribFloat.Create( ALocation, AAttrib );
end;

function attrib(AName: String; AAttrib: Float): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribFloat.Create( AName, AAttrib );
end;

function attrib(ALocation: Integer; AAttrib: TVec2): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribVec2.Create( ALocation, AAttrib );
end;

function attrib(AName: String; AAttrib: TVec2): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribVec2.Create( AName, AAttrib );
end;

function attrib(ALocation: Integer; AAttrib: TVec3): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribVec3.Create( ALocation, AAttrib );
end;

function attrib(AName: String; AAttrib: TVec3): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribVec3.Create( AName, AAttrib );
end;

function attrib(ALocation: Integer; AAttrib: TVec4): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribVec4.Create( ALocation, AAttrib );
end;

function attrib(AName: String; AAttrib: TVec4): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribVec4.Create( AName, AAttrib );
end;

function attrib(ALocation: Integer; AAttrib: TVec2Array): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribVec2Array.CreateArray( ALocation, AAttrib );
end;

function attrib(ALocation: Integer; AAttrib: TP3DVec2BufferGL): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribVec2Array.Create( ALocation, AAttrib );
end;

function attrib(ALocation: Integer; AAttrib: TVec3Array): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribVec3Array.CreateArray( ALocation, AAttrib );
end;

function attrib(ALocation: Integer; AAttrib: TP3DVec3BufferGL): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribVec3Array.Create( ALocation, AAttrib );
end;

function attrib(ALocation: Integer; AAttrib: TVec4Array): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribVec4Array.CreateArray( ALocation, AAttrib );
end;

function attrib(ALocation: Integer; AAttrib: TP3DVec4BufferGL): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribVec4Array.Create( ALocation, AAttrib );
end;

function attrib(AName: String; AAttrib: TP3DVec2BufferGL): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribVec2Array.Create( AName, AAttrib );
end;

function attrib(AName: String; AAttrib: TVec2Array): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribVec2Array.CreateArray( AName, AAttrib );
end;

function attrib(AName: String; AAttrib: TP3DVec3BufferGL): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribVec3Array.Create( AName, AAttrib );
end;

function attrib(AName: String; AAttrib: TVec3Array): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribVec3Array.CreateArray( AName, AAttrib );
end;

function attrib(AName: String; AAttrib: TP3DVec4BufferGL): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribVec4Array.Create( AName, AAttrib );
end;

function attrib(AName: String; AAttrib: TVec4Array): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribVec4Array.CreateArray( AName, AAttrib );
end;

function uniform_material(AMaterial: TP3DMaterial): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandMaterial.Create( AMaterial );
end;

function uniform_light(ALightObj: TP3DObject): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformLight.Create( -1, ALightObj );
end;

function uniform(AName: String; ATexture: TP3DTexture): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformTexture.Create( AName, ATexture );
end;

function uniform(ALocation: GLint; ATexture: TP3DTexture): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformTexture.Create( ALocation, ATexture );
end;

function uniform(ALocation: GLint; ATransform: TP3DTransform): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformTransform.Create( ALocation, ATransform );
end;

function uniform(AName: String; ATransform: TP3DTransform): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformTransform.Create( AName, ATransform );
end;

function uniform(AName: String; AValue: Integer): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformInt.Create( AName, AValue );
end;

function uniform(ALocation: GLint; AValue: Integer): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformInt.Create( ALocation, AValue );
end;

function uniform(AName: String; AValue: Float): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformFloat.Create( AName, AValue );
end;

function uniform(ALocation: GLint; AValue: TVec2): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformVec2.Create( ALocation, AValue );
end;

function uniform(AName: String; AValue: TVec2): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformVec2.Create( AName, AValue );
end;

function uniform(ALocation: GLint; AValue: TVec3): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformVec3.Create( ALocation, AValue );
end;

function uniform(AName: String; AValue: TVec3): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformVec3.Create( AName, AValue );
end;

function uniform(ALocation: GLint; AValue: TVec4): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformVec4.Create( ALocation, AValue );
end;

function uniform(AName: String; AValue: TVec4): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformVec4.Create( AName, AValue );
end;

function uniform(ALocation: GLint; AValue: TMat2): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformMat2.Create( ALocation, AValue );
end;

function uniform(AName: String; AValue: TMat2): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformMat2.Create( AName, AValue );
end;

function uniform(ALocation: GLint; AValue: TMat3): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformMat3.Create( ALocation, AValue );
end;

function uniform(AName: String; AValue: TMat3): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformMat3.Create( AName, AValue );
end;

function uniform(ALocation: GLint; AValue: TMat4): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformMat4.Create( ALocation, AValue );
end;

function uniform(AName: String; AValue: TMat4): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformMat4.Create( AName, AValue );
end;

function sample(AName: String; ATexture: TP3DTexture): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformSampleTexture.Create( AName, ATexture );
end;

function texcoord(AName: String; AAttrib: TP3DVec2BufferGL): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandTexCoordVec2.Create( AName, AAttrib );
  Result.FCategory:= 'texcoord';
end;

function texcoord(AName: String; AAttrib: TVec2Array): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandTexCoordVec2.CreateArray( AName, AAttrib );
  Result.FCategory:= 'texcoord';
end;

function texcoord(AName: String; AAttrib: TP3DVec3BufferGL): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandTexCoordVec3.Create( AName, AAttrib );
  Result.FCategory:= 'texcoord';
end;

function texcoord(AName: String; AAttrib: TVec3Array): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandTexCoordVec3.CreateArray( AName, AAttrib );
  Result.FCategory:= 'texcoord';
end;

function texcoord(AName: String; AAttrib: TP3DVec4BufferGL): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandTexCoordVec4.Create( AName, AAttrib );
  Result.FCategory:= 'texcoord';
end;

function texcoord(AName: String; AAttrib: TVec4Array): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandTexCoordVec4.CreateArray( AName, AAttrib );
  Result.FCategory:= 'texcoord';
end;

function normal(AName: String; AAttrib: TP3DVec2BufferGL): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandTexCoordVec2.Create( AName, AAttrib );
  Result.FCategory:= 'normals';
end;

function normal(AName: String; AAttrib: TVec2Array): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandTexCoordVec2.CreateArray( AName, AAttrib );
  Result.FCategory:= 'normals';
end;

function normal(AName: String; AAttrib: TP3DVec3BufferGL): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandTexCoordVec3.Create( AName, AAttrib );
  Result.FCategory:= 'normals';
end;

function normal(AName: String; AAttrib: TVec3Array): TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandTexCoordVec3.CreateArray( AName, AAttrib );
  Result.FCategory:= 'normals';
end;

function sample_normal(AName: String; ATexture: TP3DTexture): TP3DPlotCommandSetting;
begin
  Result:= sample( AName, ATexture );
  TP3DPlotCommandUniformSampleTexture( Result ).NoConnectToColor:= True;
  Result.FCategory:= 'normal_map';
end;

{$ENDIF}

