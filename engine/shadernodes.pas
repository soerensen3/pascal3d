unit shadernodes;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, Math3d;

type
  TShaderVarType = ( svtDefault, svtIn, svtOut, svtUniform );
  TShaderVar = object
    Name: String;
    VarType: TShaderVarType;
  end;

{  {$MACRO ON}
  {$DEFINE TCustomList:= TShaderVarList}
  {$DEFINE TCustomItem:= TShaderVar}
  {$DEFINE INTERFACE}
  {$INCLUDE custom_list.inc}}

  TShaderVarFloat = object( TShaderVar )
    Value: Float;
  end;

  TShaderVarVec2 = object( TShaderVar )
    Value: TVec2;
  end;

  TShaderVarVec3 = object( TShaderVar )
    Value: TVec3;
  end;

  TShaderVarVec4 = object( TShaderVar )
    Value: TVec4;
  end;

  TShaderVarInt = object( TShaderVar )
    Value: Integer;
  end;


  { TShaderNode }

  TShaderNode = class abstract
    private
//      FShaderVars: TShaderVarList;
      function GetVars( index: Integer ): TShaderVar;
      procedure SetVars( index: Integer ; AValue: TShaderVar);

    public
      function GetFragmentHeader: String; virtual;
      function GetFragmentCode: String; virtual;
      function GetVertexHeader: String; virtual;
      function GetVertexCode: String; virtual;
      property Vars[ index: Integer ]: TShaderVar read GetVars;
  end;

const
  {$DEFINE DEFAULTSHADER}
  {$DEFINE FRAGMENTHEADER}
  DefaultShaderFragmentHeaderCode = {$INCLUDE shadernodes_constants.inc};
  {$DEFINE DEFAULTSHADER}
  {$DEFINE FRAGMENTCODE}
  DefaultShaderFragmentCode = {$INCLUDE shadernodes_constants.inc};

  {$DEFINE DEFAULTSHADER}
  {$DEFINE VERTEXHEADER}
  DefaultShaderVertexHeaderCode = {$INCLUDE shadernodes_constants.inc};
  {$DEFINE DEFAULTSHADER}
  {$DEFINE VERTEXCODE}
  DefaultShaderVertexCode = {$INCLUDE shadernodes_constants.inc};

type
  TDefaultShader = class( TShaderNode )

  end;

implementation

{ TShaderNode }

function TShaderNode.GetVars( index: Integer ): TShaderVar;
begin
//  Result:= TShaderVar( FShaderVars[ index ]);
end;

procedure TShaderNode.SetVars(index: Integer; AValue: TShaderVar);
begin

end;

function TShaderNode.GetFragmentHeader: String;
begin
  Result:= DefaultShaderFragmentHeaderCode;
end;

function TShaderNode.GetFragmentCode: String;
begin
  Result:= DefaultShaderFragmentCode;
end;

function TShaderNode.GetVertexHeader: String;
begin
  Result:= DefaultShaderVertexHeaderCode;
end;

function TShaderNode.GetVertexCode: String;
begin
  Result:= DefaultShaderVertexCode;
end;

{
{$MACRO ON}
{$DEFINE TCustomList:= TShaderVarList}
{$DEFINE TCustomItem:= TShaderVar}
{$DEFINE IMPLEMENTATION}
{$INCLUDE custom_list.inc}
}

end.

