{ p3dplot_primitives.inc }

{$IFDEF INTERFACE}
type
  TP3DPrimitiveMode = ( pmPoints = GL_POINTS,
                        pmLines = GL_LINES,
                        pmLineLoop = GL_LINE_LOOP,
                        pmLineStrip = GL_LINE_STRIP,
                        pmTriangles = GL_TRIANGLES,
                        pmTriangleStrip = GL_TRIANGLE_STRIP,
                        pmTriangleFan = GL_TRIANGLE_FAN );

  { TP3DPlotCommandPrimitive }

  TP3DPlotCommandPrimitive = class ( TP3DPlotCommandLayer )
    private
      FCount: Integer;
      FPrimitiveMode: TP3DPrimitiveMode;

    public
      constructor Create( APrimitiveMode: TP3DPrimitiveMode; ACount: Integer; ASettings: TP3DPlotListSetting );

      procedure Execute( AContext: TP3DPlotCommandContext ); override;
      function Clone: TP3DPlotCommandLayer; override;

      property PrimitiveMode: TP3DPrimitiveMode read FPrimitiveMode write FPrimitiveMode;
      property Count: Integer read FCount write FCount;
  end;

  { TP3DPlotCommandPrimitiveIndexed }

  TP3DPlotCommandPrimitiveIndexed = class ( TP3DPlotCommandLayer )
    private
      FIndices: TP3DIntBufferGL;
      FPrimitiveMode: TP3DPrimitiveMode;

    public
      constructor Create( APrimitiveMode: TP3DPrimitiveMode; AIndices: TP3DIntBufferGL; ASettings: TP3DPlotListSetting );
      destructor Destroy; override;

      procedure Execute( AContext: TP3DPlotCommandContext ); override;
      function Clone: TP3DPlotCommandLayer; override;

      property PrimitiveMode: TP3DPrimitiveMode read FPrimitiveMode write FPrimitiveMode;
      property Indices: TP3DIntBufferGL read FIndices write FIndices;
  end;


{$ENDIF}

{$IFDEF IMPLEMENTATION}



{ TP3DPlotCommandPrimitiveIndexed }

constructor TP3DPlotCommandPrimitiveIndexed.Create(APrimitiveMode: TP3DPrimitiveMode; AIndices: TP3DIntBufferGL;
  ASettings: TP3DPlotListSetting);
begin
  inherited Create;
  FIndices:= TP3DIntBufferGL.Create;
  if ( Assigned( AIndices )) then begin
    AIndices.AssignTo( FIndices );
    FIndices.PushData;
  end;
  FPrimitiveMode:= APrimitiveMode;
  ShaderSettings:= ASettings;
end;

destructor TP3DPlotCommandPrimitiveIndexed.Destroy;
begin
  Indices.Free;
  inherited Destroy;
end;

procedure TP3DPlotCommandPrimitiveIndexed.Execute(AContext: TP3DPlotCommandContext);
begin
  inherited Execute( AContext );
  Indices.Bind( GL_ELEMENT_ARRAY_BUFFER );
  glDrawElements( LongWord( FPrimitiveMode ), Indices.Count, GL_UNSIGNED_INT, Pointer( 0 ));
end;

function TP3DPlotCommandPrimitiveIndexed.Clone: TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandPrimitiveIndexed.Create( FPrimitiveMode, FIndices, ShaderSettings );
end;


{ TP3DPlotCommandPrimitive }

constructor TP3DPlotCommandPrimitive.Create(APrimitiveMode: TP3DPrimitiveMode; ACount: Integer; ASettings: TP3DPlotListSetting);
begin
  inherited Create;
  FPrimitiveMode:= APrimitiveMode;
  FCount:= ACount;
  ShaderSettings:= ASettings;
end;

procedure TP3DPlotCommandPrimitive.Execute( AContext: TP3DPlotCommandContext );
begin
  inherited Execute( AContext );
  glDrawArrays( LongWord( FPrimitiveMode ), 0, Count );
end;

function TP3DPlotCommandPrimitive.Clone: TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandPrimitive.Create( PrimitiveMode, Count, ShaderSettings );
end;
{$ENDIF}

