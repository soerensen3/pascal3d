program math3d_debug;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, Math3d
  { you can add units after this };

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  private
    FMatrixConstructors: Boolean;
    FSwizzling: Boolean;
    FVectorConstructors: Boolean;
    procedure SetMatrixConstructors(AValue: Boolean);
    procedure SetSwizzling(AValue: Boolean);
    procedure SetVectorConstructors(AValue: Boolean);
  protected
    procedure DoRun; override;
    procedure StartTest;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;

    property Swizzling: Boolean read FSwizzling write SetSwizzling;
    property VectorConstructors: Boolean read FVectorConstructors write SetVectorConstructors;
    property MatrixConstructors: Boolean read FMatrixConstructors write SetMatrixConstructors;
  end;

{ TMyApplication }

procedure TMyApplication.SetSwizzling(AValue: Boolean);
begin
  if FSwizzling=AValue then Exit;
  FSwizzling:=AValue;
end;

procedure TMyApplication.SetMatrixConstructors(AValue: Boolean);
begin
  if FMatrixConstructors=AValue then Exit;
  FMatrixConstructors:=AValue;
end;

procedure TMyApplication.SetVectorConstructors(AValue: Boolean);
begin
  if FVectorConstructors=AValue then Exit;
  FVectorConstructors:=AValue;
end;

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h',['help','vec-swizzling', 'vec-constructors', 'mat-constructors' ]);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  Swizzling:= HasOption('vec-swizzling');

  VectorConstructors:= HasOption('vec-constructors');

  MatrixConstructors:= HasOption('mat-constructors');
  { add your program here }
  StartTest;

  // stop program loop
  Terminate;
end;

procedure TMyApplication.StartTest;
  procedure OutPutVec2( v: TVec2 );
  begin
    WriteLn( ' -> vec2=<', FloatToStr( v.x ), ', ', FloatToStr( v.y ), '>' );
  end;

  procedure OutPutVec3( v: TVec3 );
  begin
    WriteLn( ' -> vec3=<', FloatToStr( v.x ), ', ', FloatToStr( v.y ), ', ', FloatToStr( v.z ), '>' );
  end;

  procedure OutPutVec4( v: TVec4 );
  begin
    WriteLn( ' -> vec4=<', FloatToStr( v.x ), ', ', FloatToStr( v.y ), ', ', FloatToStr( v.z ), ', ', FloatToStr( v.w ), '>' );
  end;


  {$INCLUDE VectorConstructorTest.inc}
  {$INCLUDE MatrixConstructorTest.inc}
  {$INCLUDE VectorSwizzlingTest.inc}

begin
  if ( Swizzling ) then
    SwizzlingTestVector;
  if ( VectorConstructors ) then
    ConstructorTestVec;
  if ( MatrixConstructors ) then
    ConstructorTestMat;

//  ReadLn;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.
