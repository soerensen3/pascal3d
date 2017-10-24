{
The MIT License (MIT)

Copyright (c) [2017] [Johannes Rosleff Soerensen]

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
}

{$IFDEF INTERFACE}
type

  { TP3DTransform }

  TP3DTransform = class( TP3DInterfacedPersistent )
    protected
      FPosition: TVec3;
      FQuaternion: TQuaternion;
      FRotation: TVec3;
      FScale: TVec3;
      FMatrix: TMat4;
      FRotationOrder: TRotationOrder;

      function GetDirection: TVec3;
      function GetMatrix: TMat4;
      function GetPosition: TVec3;
      function GetQuaternion: TQuaternion;
      function GetRotation: TVec3;
      function GetRotationOrder: TRotationOrder;
      function GetScale: TVec3;
      procedure SetMatrix( AValue: TMat4 );
      procedure SetPosition( AValue: TVec3 );
      procedure SetQuaternion( AValue: TQuaternion );
      procedure SetRotation( AValue: TVec3 );
      procedure SetRotationOrder( AValue: TRotationOrder );
      procedure SetScale( AValue: TVec3 );
      procedure UpdateMatrix;
      procedure UpdateEuler;

    public
      constructor Create;
      procedure MoveAlongGlobalAxis( AAxis: TVec3; AValue: Float );
      procedure MoveAlongLocalAxis( AAxis: TVec3; AValue: Float );
      procedure RotateAroundGlobalAxis( AAxis: TVec3; AValue: Float );
      procedure RotateAroundLocalAxis( AAxis: TVec3; AValue: Float );

      property Position: TVec3 read FPosition write SetPosition;
      property Direction: TVec3 read GetDirection;
      property Rotation: TVec3 read FRotation write SetRotation;
      property Scale: TVec3 read FScale write SetScale;
      property Matrix: TMat4 read FMatrix write SetMatrix;
      property Quaternion: TQuaternion read FQuaternion write SetQuaternion;

    published
      property RotationOrder: TRotationOrder read GetRotationOrder write SetRotationOrder;
  end;

  { IP3DTransform }

  {IP3DTransform = interface
    ['{DC16F6B0-37DD-4AD3-A093-E49DC022D6AA}']
    function GetTransform: TP3DTransform;
    procedure SetTransform( AValue: TP3DTransform );

    property Transform: TP3DTransform read GetTransform write SetTransform;
  end;}


{$ENDIF}

{$IFDEF IMPLEMENTATION}
procedure TP3DTransform.UpdateMatrix;
begin
  FMatrix:= mat4translate( vec4( Position, 1 )) * mat4( mat3( Quaternion )) * mat4scale( vec4( Scale, 1 ));
end;

procedure TP3DTransform.UpdateEuler;
begin
  FRotation:= QuatToEuler( Quaternion, deg2rad * Rotation, RotationOrder ) / deg2rad;
end;

constructor TP3DTransform.Create;
begin
  inherited Create;
  Properties.Add([ TP3DPropertyAccessVec3.CreateGetSet( 'Position', @GetPosition, @SetPosition, smAttribute ),
                   TP3DPropertyAccessVec3.CreateGetSet( 'Rotation', @GetRotation, @SetRotation ),
                   TP3DPropertyAccessRTTI.Create( Self, 'RotationOrder', smAttribute ),
                   TP3DPropertyAccessQuat.CreateGetSet( 'Quaternion', @GetQuaternion, @SetQuaternion, smAttribute ),
                   TP3DPropertyAccessVec3.CreateGetSet( 'Direction', @GetDirection, nil ),
                   TP3DPropertyAccessVec3.CreateGetSet( 'Scale', @GetScale, @SetScale, smAttribute )],
                   'Transform' );
end;


procedure TP3DTransform.MoveAlongGlobalAxis( AAxis: TVec3; AValue: Float );
begin
  Position:= Position + AAxis * AValue;
end;

procedure TP3DTransform.MoveAlongLocalAxis( AAxis: TVec3; AValue: Float );
begin
  Position:= Position + ( Quaternion * AAxis ) * AValue;
end;

procedure TP3DTransform.RotateAroundGlobalAxis( AAxis: TVec3; AValue: Float );
begin
  Quaternion:= Quaternion * quatRotation( AAxis, AValue );
end;

procedure TP3DTransform.RotateAroundLocalAxis( AAxis: TVec3; AValue: Float );
begin
  //TODO: TP3DTransform.RotateAroundLocalAxis
end;

procedure TP3DTransform.SetMatrix(AValue: TMat4);
begin
  if FMatrix= AValue then Exit;
  FMatrix:= AValue;
  FPosition:= vec3( Matrix._30, Matrix._31, Matrix._32 );
  FRotation:= MatToEuler( mat3( AValue ), deg2rad * Rotation, RotationOrder ) / deg2rad;
  FQuaternion:= EulerToQuat( Rotation * deg2rad, RotationOrder );
  FScale:= mat3toScale( mat3( FMatrix ));
end;

function TP3DTransform.GetDirection: TVec3;
begin
  Result:= Matrix.Row[ 2 ].xyz;
end;

function TP3DTransform.GetMatrix: TMat4;
begin
  Result:= FMatrix;
end;

function TP3DTransform.GetPosition: TVec3;
begin
  Result:= FPosition;
end;

function TP3DTransform.GetQuaternion: TQuaternion;
begin
  Result:= FQuaternion;
end;

function TP3DTransform.GetRotation: TVec3;
begin
  Result:= FRotation;
end;

function TP3DTransform.GetRotationOrder: TRotationOrder;
begin
  Result:= FRotationOrder;
end;

function TP3DTransform.GetScale: TVec3;
begin
  Result:= FScale;
end;

procedure TP3DTransform.SetPosition(AValue: TVec3);
begin
  if FPosition=AValue then Exit;
  FPosition:= AValue;
  UpdateMatrix;
end;

procedure TP3DTransform.SetQuaternion(AValue: TQuaternion);
begin
  if ( FQuaternion = AValue ) then Exit;
  FQuaternion:= AValue;
  UpdateMatrix;
  UpdateEuler;
end;

procedure TP3DTransform.SetRotation(AValue: TVec3);
begin
  if FRotation=AValue then Exit;
  FRotation:=AValue;
  FQuaternion:= EulerToQuat( AValue * deg2rad, RotationOrder );
  UpdateMatrix;
end;

procedure TP3DTransform.SetRotationOrder(AValue: TRotationOrder);
begin
  FRotationOrder:= AValue;
  FQuaternion:= EulerToQuat( Rotation * deg2rad, RotationOrder );
  UpdateMatrix;
end;

procedure TP3DTransform.SetScale(AValue: TVec3);
begin
  if ( FScale = AValue ) then Exit;
  FScale:= AValue;
  UpdateMatrix;
end;

{$ENDIF}

