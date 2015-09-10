//p3dshader_core.inc

{$IFDEF INTERFACE}
  { TP3DShaderNodeCustom }

  TP3DShaderNodeCustom = class
    //function GetStringOutput(InputCallback: TP3DInputCallback;
    //  Context: TP3DShaderCompiled): String; virtual;
  end;

  { TP3DShaderNodeVector }

  TP3DShaderNodeVector = class ( TP3DShaderNodeCustom )
    private
      FNumFields: Integer;
      FValue: TVec4;
      Module: TP3DShaderModule;
      Text: TP3DShaderNodeFragmentInline;
      procedure SetValue(AValue: TVec4);

    public
      constructor Create;
      destructor Destroy; override;

      //function GetStringOutput(InputCallback: TP3DInputCallback;
      //  Context: TP3DShaderCompiled): String; override;
      property Value: TVec4 read FValue write SetValue;
      property NumFields: Integer read FNumFields write FNumFields;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DShaderNodeCustom }

function TP3DShaderNodeCustom.GetStringOutput(InputCallback: TP3DInputCallback;
  Context: TP3DShaderCompiled): String;
begin

end;


{ TP3DShaderNodeVector }

procedure TP3DShaderNodeVector.SetValue(AValue: TVec4);
begin
  if FValue=AValue then Exit;
  FValue:=AValue;
end;

constructor TP3DShaderNodeVector.Create;
var
  ThisNode: TP3DShaderNode;
begin
  inherited;
  Module:= TP3DShaderModule.Create( Self );
  ThisNode:= Module.Nodes[ Module.Nodes.Add( TP3DShaderNode.Create )];
  ThisNode.Name:= 'vshader';
  ThisNode.Fragments;
  NumFields:= 4;
  Fragments.Add( FragmentShader );
end;

destructor TP3DShaderNodeVector.Destroy;
begin
  inherited Destroy;
end;

function TP3DShaderNodeVector.GetStringOutput(InputCallback: TP3DInputCallback;
  Context: TP3DShaderCompiled): String;
begin
  //Result:= inherited GetStringOutput(InputCallback, Context);

end;

{$ENDIF}
