//p3dshadernodes_core.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}

type

  { TP3DShaderNodeSocketShader }

  TP3DShaderNodeSocketString = class ( TP3DNodeSocketClone )
    protected
      FValue: String;

      class function GetSocketType: String; static; override;

    public
      function GetOutput( Buffer: TP3DShaderBuffer ): String; override;

      property Value: String read FValue write FValue;
  end;

  { TP3DShaderNodeSocketInt }

  TP3DNodeSocketInt = class ( TP3DNodeSocketClone )
    protected
      FValue: Integer;

      class function GetSocketType: String; static; override;

      procedure SetValue(AValue: Integer);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      function GetOutput( Buffer: TP3DShaderBuffer ): String; override;

      property Value: Integer read FValue write SetValue;
  end;

  { TP3DShaderNodeSocketFloat }

  TP3DShaderNodeSocketFloat = class ( TP3DNodeSocketClone )
    protected
      FValue: Single;

      class function GetSocketType: String; static; override;

      procedure SetValue(AValue: Single);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      function GetOutput( Buffer: TP3DShaderBuffer ): String; override;

      property Value: Single read FValue write SetValue;
  end;


  { TP3DShaderNodeSocketVector }

  TP3DShaderNodeSocketVector = class ( TP3DNodeSocketClone )
    protected
      FNumFields: Integer;
      FValue: TVec4;

      procedure SetValue(AValue: TVec4);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      class function GetSocketType: String; static; override;
      function GetOutput( Buffer: TP3DShaderBuffer ): String; override;

      property Value: TVec4 read FValue write SetValue;
      property NumFields: Integer read FNumFields write FNumFields;
  end;

  { TP3DShaderNodeSocketMatrix }

  TP3DShaderNodeSocketMatrix = class ( TP3DNodeSocketClone )
    protected
      FNumFields: Integer;
      FValue: TMat4;

      procedure SetValue(AValue: TMat4);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      class function GetSocketType: String; static; override;

      function GetOutput( Buffer: TP3DShaderBuffer ): String; override;

      property Value: TMat4 read FValue write SetValue;
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

constructor TP3DShaderNodeSocketVector.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  NumFields:= 4;
  Value:= vec4( 0, 0, 0, 1 );
end;

function TP3DShaderNodeSocketVector.GetOutput(Buffer: TP3DShaderBuffer): String;
var
  n: Integer;
begin
  if (( Direction = nsdOutput ) or Assigned( CloneSocket.Connected )) then
    Result:= inherited GetOutput( Buffer, CloneSocket )
  else if ( Buffer.Defines.Find( 'assign', n ) or Buffer.Defines.Find( 'main', n )) then
    Result:= 'vec4( ' + Value.ToString( 4 ) + ')'
  else
    Result:= '';
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

constructor TP3DShaderNodeSocketFloat.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  Value:= 0;
end;

function TP3DShaderNodeSocketFloat.GetOutput(Buffer: TP3DShaderBuffer): String;
var
  n: Integer;
begin
  if (( Direction = nsdOutput ) or Assigned( CloneSocket.Connected )) then
    Result:= inherited GetOutput( Buffer, CloneSocket )
  else
    Result:= FloatToStr( Value ) + 'f';
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

constructor TP3DShaderNodeSocketInt.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  Value:= 0;
end;

function TP3DShaderNodeSocketInt.GetOutput(Buffer: TP3DShaderBuffer): String;
var
  n: Integer;
begin
  if ( Buffer.Defines.Find( 'assign', n ) or Buffer.Defines.Find( 'main', n )) then
    if (( Direction = nsdOutput ) or Assigned( CloneSocket.Connected )) then
      Result:= inherited GetOutput( Buffer, CloneSocket )
    else
      Result:= IntToStr( Value )
  else
    Result:= '';
end;

{ TP3DShaderNodeSocketMatrix }

class function TP3DShaderNodeSocketMatrix.GetSocketType: String;
begin
  Result:= 'mat4';
end;

procedure TP3DShaderNodeSocketMatrix.SetValue(AValue: TMat4);
begin
  if FValue=AValue then Exit;
  FValue:=AValue;
end;

constructor TP3DShaderNodeSocketMatrix.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  NumFields:= 4;
  Value:= Mat4Identity;
end;

function TP3DShaderNodeSocketMatrix.GetOutput(Buffer: TP3DShaderBuffer): String;
var
  n: Integer;
begin
  if ( Buffer.Defines.Find( 'assign', n ) or Buffer.Defines.Find( 'main', n )) then
    if (( Direction = nsdOutput ) or Assigned( CloneSocket.Connected )) then
      Result:= inherited GetOutput( Buffer, CloneSocket )
    else
      Result:= 'mat4( ' + Value.ToString(False, 8) + ')'
  else
    Result:= '';
end;



{$ENDIF}
