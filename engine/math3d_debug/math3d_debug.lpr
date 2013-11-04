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
  protected
    procedure DoRun; override;
    procedure StartTest;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
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

  { add your program here }
  StartTest;

  // stop program loop
  Terminate;
end;

procedure TMyApplication.StartTest;
  procedure OutPutVec2( v: TVec2 );
  begin
    WriteLn( 'vec2=<', FloatToStr( v.x ), ', ', FloatToStr( v.y ), '>' );
  end;

  procedure OutPutVec3( v: TVec3 );
  begin
    WriteLn( 'vec3=<', FloatToStr( v.x ), ', ', FloatToStr( v.y ), ', ', FloatToStr( v.z ), '>' );
  end;

  procedure OutPutVec4( v: TVec4 );
  begin
    WriteLn( 'vec4=<', FloatToStr( v.x ), ', ', FloatToStr( v.y ), ', ', FloatToStr( v.z ), ', ', FloatToStr( v.w ), '>' );
  end;

begin
  WriteLn( 'function vec2( x: Float; y: Float ); TVec2;' );
  OutPutVec2( vec2( 1.0, 2.0 ));
  WriteLn( 'function vec2( xy: Float ); TVec2;' );
  OutPutVec2( vec2( 0.5 ));
  WriteLn( 'function vec2( xy: TVec3 ); TVec2;' );
  OutPutVec2( vec2( vec3( 1.0, 2.0, 3.0 )));
  WriteLn( 'function vec2( xy: TVec4 ); TVec2;' );
  OutPutVec2( vec2( vec4( 1.0, 2.0, 3.0, 4.0 )));
  WriteLn( 'function vec3( x: Float; y: Float; z: Float ); TVec3;' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ));
  WriteLn( 'function vec3( xyz: Float ); TVec3;' );
  OutPutVec3( vec3( 0.5 ));
  WriteLn( 'function vec3( xy: TVec2; z: Float ); TVec3;' );
  OutPutVec3( vec3( vec2( 1.0, 2.0 ), 3.0 ));
  WriteLn( 'function vec3( x: Float; yz: TVec2 ); TVec3;' );
  OutPutVec3( vec3( 1.0, vec2( 2.0, 3.0 )));
  WriteLn( 'function vec3( xyz: TVec4 ); TVec3;' );
  OutPutVec3( vec3( vec4( 1.0, 2.0, 3.0, 4.0 )));
  WriteLn( 'function vec4( x: Float; y: Float; z: Float; w: Float ); TVec4;' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ));
  WriteLn( 'function vec4( xyzw: Float ); TVec4;' );
  OutPutVec4( vec4( 0.5 ));
  WriteLn( 'function vec4( xy: TVec2; z: Float; w: Float ); TVec4;' );
  OutPutVec4( vec4( vec2( 1.0, 2.0 ), 3.0, 4.0 ));
  WriteLn( 'function vec4( x: Float; yz: TVec2; w: Float ); TVec4;' );
  OutPutVec4( vec4( 1.0, vec2( 2.0, 3.0 ), 4.0 ));
  WriteLn( 'function vec4( x: Float; y: Float; zw: TVec2 ); TVec4;' );
  OutPutVec4( vec4( 1.0, 2.0, vec2( 3.0, 4.0 )));
  WriteLn( 'function vec4( xy: TVec2; zw: TVec2 ); TVec4;' );
  OutPutVec4( vec4( vec2( 1.0, 2.0 ), vec2( 3.0, 4.0 )));

  ReadLn;
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

