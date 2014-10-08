unit Img;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TImgObj }

  TImgObj = class
  private
    FFileName: String;
    FTags: TStringList;

  public
    constructor Create;
    destructor Destroy; override;

  published
    property FileName: String read FFileName write FFileName;
    property Tags: TStringList read FTags write FTags;
  end;


implementation

{ TImgObj }

constructor TImgObj.Create;
begin
  inherited;
  Tags:= TStringList.Create;
end;

destructor TImgObj.Destroy;
begin
  Tags.Free;
  inherited Destroy;
end;

end.

