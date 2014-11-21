unit p3dshadernodes;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, p3dMath;

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
  {$DEFINE BASESHADER}
  {$DEFINE FRAGMENTHEADER}
  BaseShaderFragmentHeaderCode = {$INCLUDE p3dshadernodes_constants.inc};
  {$DEFINE BASESHADER}
  {$DEFINE FRAGMENTCODE}
  BaseShaderFragmentCode = {$INCLUDE p3dshadernodes_constants.inc};

  {$DEFINE BASESHADER}
  {$DEFINE VERTEXHEADER}
  BaseShaderVertexHeaderCode = {$INCLUDE p3dshadernodes_constants.inc};
  {$DEFINE BASESHADER}
  {$DEFINE VERTEXCODE}
  BaseShaderVertexCode = {$INCLUDE p3dshadernodes_constants.inc};

type
  TBaseShader = class( TShaderNode )

  end;

  TSimpleVShader = class( TShaderNode )

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
  Result:= BaseShaderFragmentHeaderCode;
end;

function TShaderNode.GetFragmentCode: String;
begin
  Result:= BaseShaderFragmentCode;
end;

function TShaderNode.GetVertexHeader: String;
begin
  Result:= BaseShaderVertexHeaderCode;
end;

function TShaderNode.GetVertexCode: String;
begin
  Result:= BaseShaderVertexCode;
end;

{
{$MACRO ON}
{$DEFINE TCustomList:= TShaderVarList}
{$DEFINE TCustomItem:= TShaderVar}
{$DEFINE IMPLEMENTATION}
{$INCLUDE custom_list.inc}
}

end.
