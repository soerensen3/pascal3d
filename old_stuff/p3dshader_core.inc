//p3dshader_core.inc

{$IFDEF INTERFACE}

  { TP3DShaderNodeSocketShader }

  TP3DShaderNodeSocketShader = class ( TP3DShaderNodeSocket )
    private
      class function GetSocketType: String; static; override;

    public
      function GetStringOutput( Buffer: TP3DShaderBuffer ): String;
  end;

  { TP3DShaderNodeSocketInt }

  TP3DShaderNodeSocketInt = class ( TP3DShaderNodeSocket )
    private
      FValue: Integer;

      class function GetSocketType: String; static; override;

      procedure SetValue(AValue: Integer);

    public
      constructor Create( ANode: TP3DNode; ADirecton: TP3DNodeSocketDirection ); override;
      destructor Destroy; override;

      function GetOutput( Buffer: TP3DShaderBuffer ): String; override;

      property Value: Integer read FValue write SetValue;
  end;

  { TP3DShaderNodeSocketFloat }

  TP3DShaderNodeSocketFloat = class ( TP3DShaderNodeSocket )
    private
      FValue: Single;

      class function GetSocketType: String; static; override;

      procedure SetValue(AValue: Single);

    public
      constructor Create( ANode: TP3DNode; ADirecton: TP3DNodeSocketDirection ); override;
      destructor Destroy; override;

      function GetOutput( Buffer: TP3DShaderBuffer ): String; override;

      property Value: Single read FValue write SetValue;
  end;


  { TP3DShaderNodeSocketVector }

  TP3DShaderNodeSocketVector = class ( TP3DShaderNodeSocket )
    private
      FNumFields: Integer;
      FValue: TVec4;

      class function GetSocketType: String; static; override;

      procedure SetValue(AValue: TVec4);

    public
      constructor Create( ANode: TP3DNode; ADirecton: TP3DNodeSocketDirection ); override;
      destructor Destroy; override;

      function GetOutput( Buffer: TP3DShaderBuffer ): String; override;

      property Value: TVec4 read FValue write SetValue;
      property NumFields: Integer read FNumFields write FNumFields;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}


{ TP3DShaderNodeVector }

class function TP3DShaderNodeSocketVector.GetSocketType: String;
begin
  Result:= 'vec4';
end;

procedure TP3DShaderNodeSocketVector.SetValue(AValue: TVec4);
begin
  if FValue=AValue then Exit;
  FValue:=AValue;
end;

constructor TP3DShaderNodeSocketVector.Create(ANode: TP3DNode;
			ADirecton: TP3DNodeSocketDirection);
begin
  inherited;
  NumFields:= 4;
  Value:= vec4( 0, 0, 0, 1 );
end;

destructor TP3DShaderNodeSocketVector.Destroy;
begin
  inherited Destroy;
end;

function TP3DShaderNodeSocketVector.GetOutput(Buffer: TP3DShaderBuffer): String;
begin
  if (( Direction = nsdOutput ) or Assigned( Connected )) then
    Result:= inherited GetOutput(Buffer)
  else
    Result:= 'vec4( ' + Value.ToString(-1) + ')';
end;

{ TP3DShaderNodeSocketFloat }

class function TP3DShaderNodeSocketFloat.GetSocketType: String;
begin
  Result:= 'float';
end;

procedure TP3DShaderNodeSocketFloat.SetValue(AValue: Single);
begin
  if FValue=AValue then Exit;
  FValue:=AValue;
end;

constructor TP3DShaderNodeSocketFloat.Create(ANode: TP3DNode;
  ADirecton: TP3DNodeSocketDirection);
begin
  inherited Create(ANode, ADirecton);
  Value:= 0;
end;

destructor TP3DShaderNodeSocketFloat.Destroy;
begin
  inherited Destroy;
end;

function TP3DShaderNodeSocketFloat.GetOutput(Buffer: TP3DShaderBuffer): String;
begin
  if (( Direction = nsdOutput ) or Assigned( Connected )) then
    Result:= inherited GetOutput(Buffer)
  else
    Result:= FloatToStr( Value );
end;

{ TP3DShaderNodeSocketInt }

class function TP3DShaderNodeSocketInt.GetSocketType: String;
begin
  Result:= 'int'
end;

procedure TP3DShaderNodeSocketInt.SetValue(AValue: Integer);
begin
  if FValue=AValue then Exit;
  FValue:=AValue;
end;

constructor TP3DShaderNodeSocketInt.Create(ANode: TP3DNode;
  ADirecton: TP3DNodeSocketDirection);
begin
  inherited Create(ANode, ADirecton);
  Value:= 0;
end;

destructor TP3DShaderNodeSocketInt.Destroy;
begin
  inherited Destroy;
end;

function TP3DShaderNodeSocketInt.GetOutput(Buffer: TP3DShaderBuffer): String;
begin
  if (( Direction = nsdOutput ) or Assigned( Connected )) then
    Result:= inherited GetOutput(Buffer)
  else
    Result:= IntToStr( Value );
end;


{$ENDIF}
