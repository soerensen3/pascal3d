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

begin
  WriteLn( 'Constructor Test' );
  WriteLn( '--------------------------------------------------------------------' );
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

  WriteLn( 'Swizzeling Test' );
  WriteLn( '--------------------------------------------------------------------' );
  Write( 'vec2( 1.0, 2.0 ).xx' );
  OutPutVec2( vec2( 1.0, 2.0 ).XX );
  Write( 'vec2( 1.0, 2.0 ).xy' );
  OutPutVec2( vec2( 1.0, 2.0 ).XY );
  Write( 'vec3( 1.0, 2.0, 3.0 ).xz' );
  OutPutVec2( vec3( 1.0, 2.0, 3.0 ).XZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xw' );
  OutPutVec2( vec4( 1.0, 2.0, 3.0, 4.0 ).XW );

  Write( 'vec2( 1.0, 2.0 ).yx' );
  OutPutVec2( vec2( 1.0, 2.0 ).YX );
  Write( 'vec2( 1.0, 2.0 ).yy = ' );
  OutPutVec2( vec2( 1.0, 2.0 ).YY );
  Write( 'vec3( 1.0, 2.0, 3.0 ).yz' );
  OutPutVec2( vec3( 1.0, 2.0, 3.0 ).YZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yw' );
  OutPutVec2( vec4( 1.0, 2.0, 3.0, 4.0 ).YW );

  Write( 'vec3( 1.0, 2.0, 3.0 ).zx' );
  OutPutVec2( vec3( 1.0, 2.0, 3.0 ).ZX );
  Write( 'vec3( 1.0, 2.0, 3.0 ).zy' );
  OutPutVec2( vec3( 1.0, 2.0, 3.0 ).ZY );
  Write( 'vec3( 1.0, 2.0, 3.0 ).zz' );
  OutPutVec2( vec3( 1.0, 2.0, 3.0 ).ZZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zw' );
  OutPutVec2( vec4( 1.0, 2.0, 3.0, 4.0 ).ZW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wx' );
  OutPutVec2( vec4( 1.0, 2.0, 3.0, 4.0 ).WX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wy' );
  OutPutVec2( vec4( 1.0, 2.0, 3.0, 4.0 ).WY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wz' );
  OutPutVec2( vec4( 1.0, 2.0, 3.0, 4.0 ).WZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).ww' );
  OutPutVec2( vec4( 1.0, 2.0, 3.0, 4.0 ).WW );

  Write( 'vec3( 1.0, 2.0, 3.0 ).xxx' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).XXX );
  Write( 'vec3( 1.0, 2.0, 3.0 ).xxy' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).XXY );
  Write( 'vec3( 1.0, 2.0, 3.0 ).xxz' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).XXZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxw' );
  OutPutVec3( vec4( 1.0, 2.0, 3.0, 4.0 ).XXW );

  Write( 'vec3( 1.0, 2.0, 3.0 ).xyx' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).XYX );
  Write( 'vec3( 1.0, 2.0, 3.0 ).xyy' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).XYY );
  Write( 'vec3( 1.0, 2.0, 3.0 ).xyz' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).XYZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xyw' );
  OutPutVec3( vec4( 1.0, 2.0, 3.0, 4.0 ).XYW );


  Write( 'vec3( 1.0, 2.0, 3.0 ).xzx' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).XZX );
  Write( 'vec3( 1.0, 2.0, 3.0 ).xzy' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).XZY );
  Write( 'vec3( 1.0, 2.0, 3.0 ).xzz' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).XZZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xzw' );
  OutPutVec3( vec4( 1.0, 2.0, 3.0, 4.0 ).XZW );


  Write( 'vec3( 1.0, 2.0, 3.0 ).yxx' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).YXX );
  Write( 'vec3( 1.0, 2.0, 3.0 ).yxy' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).YXY );
  Write( 'vec3( 1.0, 2.0, 3.0 ).yxz' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).YXZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yxw' );
  OutPutVec3( vec4( 1.0, 2.0, 3.0, 4.0 ).YXW );

  Write( 'vec3( 1.0, 2.0, 3.0 ).yyx' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).YYX );
  Write( 'vec3( 1.0, 2.0, 3.0 ).yyy' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).YYY );
  Write( 'vec3( 1.0, 2.0, 3.0 ).yyz' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).YYZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yyw' );
  OutPutVec3( vec4( 1.0, 2.0, 3.0, 4.0 ).YYW );


  Write( 'vec3( 1.0, 2.0, 3.0 ).yzx' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).YZX );
  Write( 'vec3( 1.0, 2.0, 3.0 ).yzy' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).YZY );
  Write( 'vec3( 1.0, 2.0, 3.0 ).yzz' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).YZZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yzw' );
  OutPutVec3( vec4( 1.0, 2.0, 3.0, 4.0 ).YZW );


  Write( 'vec3( 1.0, 2.0, 3.0 ).zxx' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).ZXX );
  Write( 'vec3( 1.0, 2.0, 3.0 ).zxy' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).ZXY );
  Write( 'vec3( 1.0, 2.0, 3.0 ).zxz' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).ZXZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zxw' );
  OutPutVec3( vec4( 1.0, 2.0, 3.0, 4.0 ).ZXW );


  Write( 'vec3( 1.0, 2.0, 3.0 ).zyx' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).ZYX );
  Write( 'vec3( 1.0, 2.0, 3.0 ).zyy' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).ZYY );
  Write( 'vec3( 1.0, 2.0, 3.0 ).zyz' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).ZYZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zyw' );
  OutPutVec3( vec4( 1.0, 2.0, 3.0, 4.0 ).ZYW );


  Write( 'vec3( 1.0, 2.0, 3.0 ).xzx' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).ZZX );
  Write( 'vec3( 1.0, 2.0, 3.0 ).zzy' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).ZZY );
  Write( 'vec3( 1.0, 2.0, 3.0 ).zzz' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ).ZZZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zzw' );
  OutPutVec3( vec4( 1.0, 2.0, 3.0, 4.0 ).ZZW );


  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxxx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXXX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxxy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXXY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxxz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXXZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxxw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXXW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxyx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXYX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxyy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXYY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxyz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXYZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxyw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXYW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxzx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXZX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxzy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXZY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxzz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXZZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxzw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXZW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxwx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXWX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxwy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXWY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxwz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXWZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxww' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXWW );


  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xyxx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xyXX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xyxy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xyXY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xyxz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xyXZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xyxw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xyXW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xyyx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xyYX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xyyy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xyYY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xyyz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xyYZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xyyw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xyYW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xyzx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xyZX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xyzy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xyZY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xyzz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xyZZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xyzw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xyZW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xywx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xyWX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xywy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xyWY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xywz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xyWZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xyww' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xyWW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xzxx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xzXX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xzxy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xzXY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xzxz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xzXZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xzxw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xzXW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xzyx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xzYX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xzyy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xzYY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xzyz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xzYZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xzyw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xzYW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xzzx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xzZX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xzzy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xzZY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xzzz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xzZZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xzzw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xzZW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xzwx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xzWX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xzwy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xzWY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xzwz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xzWZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xzww' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xzWW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xwxx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xwXX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xwxy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xwXY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xwxz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xwXZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xwxw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xwXW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xwyx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xwYX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xwyy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xwYY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xwyz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xwYZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xwyw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xwYW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xwzx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xwZX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xwzy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xwZY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xwzz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xwZZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xwzw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xwZW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xwwx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xwWX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xwwy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xwWY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xwwz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xwWZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xwww' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).xwWW );


  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxxx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXXX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxxy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXXY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxxz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXXZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxxw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXXW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxyx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXYX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxyy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXYY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxyz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXYZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxyw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXYW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxzx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXZX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxzy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXZY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxzz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXZZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxzw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXZW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxwx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXWX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxwy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXWY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxwz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXWZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).xxww' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).XXWW );


  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yyxx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yyXX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yyxy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yyXY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yyxz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yyXZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yyxw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yyXW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yyyx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yyYX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yyyy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yyYY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yyyz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yyYZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yyyw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yyYW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yyzx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yyZX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yyzy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yyZY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yyzz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yyZZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yyzw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yyZW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yywx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yyWX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yywy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yyWY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yywz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yyWZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yyww' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yyWW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yzxx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yzXX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yzxy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yzXY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yzxz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yzXZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yzxw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yzXW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yzyx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yzYX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yzyy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yzYY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yzyz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yzYZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yzyw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yzYW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yzzx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yzZX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yzzy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yzZY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yzzz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yzZZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yzzw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yzZW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yzwx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yzWX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yzwy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yzWY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yzwz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yzWZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).yzww' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).yzWW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).ywxx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).ywXX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).ywxy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).ywXY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).ywxz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).ywXZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).ywxw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).ywXW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).ywyx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).ywYX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).ywyy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).ywYY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).ywyz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).ywYZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).ywyw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).ywYW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).ywzx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).ywZX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).ywzy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).ywZY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).ywzz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).ywZZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).ywzw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).ywZW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).ywwx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).ywWX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).ywwy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).ywWY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).ywwz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).ywWZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).ywww' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).ywWW );



  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zxxx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zXXX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zxxy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zXXY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zxxz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zXXZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zxxw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zXXW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zxyx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zXYX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zxyy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zXYY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zxyz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zXYZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zxyw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zXYW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zxzx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zXZX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zxzy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zXZY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zxzz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zXZZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zxzw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zXZW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zxwx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zXWX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zxwy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zXWY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zxwz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zXWZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zxww' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zXWW );


  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zyxx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zyXX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zyxy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zyXY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zyxz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zyXZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zyxw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zyXW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zyyx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zyYX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zyyy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zyYY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zyyz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zyYZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zyyw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zyYW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zyzx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zyZX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zyzy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zyZY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zyzz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zyZZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zyzw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zyZW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zywx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zyWX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zywy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zyWY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zywz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zyWZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zyww' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zyWW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zzxx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zzXX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zzxy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zzXY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zzxz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zzXZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zzxw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zzXW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zzyx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zzYX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zzyy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zzYY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zzyz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zzYZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zzyw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zzYW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zzzx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zzZX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zzzy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zzZY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zzzz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zzZZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zzzw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zzZW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zzwx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zzWX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zzwy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zzWY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zzwz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zzWZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zzww' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zzWW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zwxx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zwXX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zwxy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zwXY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zwxz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zwXZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zwxw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zwXW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zwyx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zwYX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zwyy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zwYY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zwyz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zwYZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zwyw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zwYW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zwzx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zwZX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zwzy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zwZY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zwzz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zwZZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zwzw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zwZW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zwwx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zwWX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zwwy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zwWY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zwwz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zwWZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).zwww' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).zwWW );




  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wxxx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wXXX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wxxy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wXXY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wxxz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wXXZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wxxw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wXXW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wxyx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wXYX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wxyy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wXYY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wxyz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wXYZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wxyw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wXYW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wxzx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wXZX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wxzy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wXZY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wxzz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wXZZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wxzw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wXZW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wxwx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wXWX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wxwy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wXWY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wxwz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wXWZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wxww' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wXWW );


  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wyxx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wyXX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wyxy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wyXY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wyxz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wyXZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wyxw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wyXW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wyyx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wyYX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wyyy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wyYY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wyyz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wyYZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wyyw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wyYW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wyzx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wyZX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wyzy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wyZY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wyzz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wyZZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wyzw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wyZW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wywx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wyWX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wywy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wyWY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wywz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wyWZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wyww' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wyWW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wzxx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wzXX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wzxy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wzXY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wzxz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wzXZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wzxw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wzXW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wzyx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wzYX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wzyy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wzYY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wzyz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wzYZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wzyw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wzYW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wzzx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wzZX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wzzy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wzZY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wzzz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wzZZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wzzw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wzZW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wzwx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wzWX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wzwy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wzWY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wzwz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wzWZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wzww' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wzWW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wwxx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wwXX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wwxy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wwXY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wwxz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wwXZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wwxw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wwXW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wwyx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wwYX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wwyy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wwYY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wwyz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wwYZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wwyw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wwYW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wwzx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wwZX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wwzy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wwZY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wwzz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wwZZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wwzw' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wwZW );

  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wwwx' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wwWX );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wwwy' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wwWY );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wwwz' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wwWZ );
  Write( 'vec4( 1.0, 2.0, 3.0, 4.0 ).wwww' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ).wwWW );
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
