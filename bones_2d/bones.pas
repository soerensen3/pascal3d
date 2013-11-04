unit bones;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, compo, GL;

type

  { TVertex }

  TVertex = object
    type TVertNotifyEvent = procedure ( Sender: TVertex ) of object;

    private
      FOnChange: TVertNotifyEvent;
      Fx: Real;
      Fy: Real;
      procedure SetX(AValue: Real);
      procedure SetY(AValue: Real);
    public
      property x: Real read Fx write SetX;
      property y: Real read Fy write SetY;
      property OnChange: TVertNotifyEvent read FOnChange write FOnChange;
  end;

  TMatrix = array [0..3, 0..3 ] of Real;

  TBone = class;
  TBoneList = specialize TFPGList <TBone>;

  { TBone }

  TBone = class ( TPersistent )
    private
      FBoneLen: Real;
      FChilds: TBoneList;
      FGraphic: TImage;
      FPos: TVertex;
      FRot: Real;

      procedure SetLen(AValue: Real);
      procedure SetPos(AValue: TVertex);
      procedure SetRot(AValue: Real);

    public
      property Pos: TVertex read FPos write SetPos;
      property Rot: Real read FRot write SetRot;
      property BoneLen: Real read FBoneLen write SetLen;
      property Childs: TBoneList read FChilds write FChilds;

      property Graphic: TImage read FGraphic write FGraphic;

      constructor Create;
      destructor Destroy; override;

      procedure UpdateImg;
      procedure Clear;
  end;

  { TBonePainter }

  TBonePainter = class ( TGraphicControl )
  private
    FBones: TBoneList;
    FDrawLines: Boolean;
  public
    procedure Draw; override;
  published
    property Bones: TBoneList read FBones write FBones;
    property DrawLines: Boolean read FDrawLines write FDrawLines;
  end;

implementation

{ TBonePainter }

procedure TBonePainter.Draw;
  procedure DrawBoneList( root: TBone; List: TBoneList; mat: TMatrix );
  var
    i: Integer;
    m: TMatrix;
  begin
    for i:= 0 to List.Count - 1 do
      begin
        glLoadMatrixf( @mat );
        glRotated( List[ i ].Rot, 0, 0, 1 );
        glTranslatef( List[ i ].Pos.x, List[ i ].Pos.y, 0 );
        if ( DrawLines ) then
          begin
            glColor3b( 0, 255, 0 );
            glBegin( GL_LINE_LOOP );
              glVertex2f( -List[ i ].BoneLen / 5, -List[ i ].BoneLen / 5 );
              glVertex2f(  List[ i ].BoneLen / 5, -List[ i ].BoneLen / 5 );
              glVertex2f(  List[ i ].BoneLen / 5,  List[ i ].BoneLen / 5 );
              glVertex2f( -List[ i ].BoneLen / 5,  List[ i ].BoneLen / 5 );
            glEnd;
            glBegin( GL_LINE );
              glVertex2f( 0, 0 );
              glVertex2f( 0, List[ i ].BoneLen );
            glEnd;
          end;
        glGetFloatv( GL_MODELVIEW_MATRIX, @m );
        DrawBoneList( List[ i ], List[ i ].Childs, m);
      end;
  end;
var
  m: TMatrix;
begin
  inherited Draw;
  glPushMatrix;
  glGetFloatv( GL_MODELVIEW_MATRIX, @m );
  if ( Assigned( Bones )) then
    DrawBoneList( nil, Bones, m );
  glPopMatrix;
end;

{ TVertex }

procedure TVertex.SetX(AValue: Real);
begin
  if Fx=AValue then Exit;
  Fx:=AValue;
  if ( Assigned( FOnChange )) then
    FOnChange( Self );
end;

procedure TVertex.SetY(AValue: Real);
begin
  if Fy=AValue then Exit;
  Fy:=AValue;
  if ( Assigned( FOnChange )) then
    FOnChange( Self );
end;


{ TBone }

procedure TBone.SetRot(AValue: Real);
begin
  if FRot=AValue then Exit;
  FRot:=AValue;
  UpdateImg;
end;

procedure TBone.SetPos(AValue: TVertex);
begin
  if @FPos=@AValue then Exit;
  FPos:=AValue;
  UpdateImg;
end;

procedure TBone.SetLen(AValue: Real);
begin
  if ( FBoneLen = AValue ) then Exit;
  FBoneLen:=AValue;
  UpdateImg;
end;

constructor TBone.Create;
begin
  inherited;
  Childs:= TBoneList.Create;
end;

destructor TBone.Destroy;
begin
  Childs.Free;
  Clear;
  inherited Destroy;
end;

procedure TBone.UpdateImg;
begin
  if ( Assigned( Graphic )) then
    begin
      Graphic.X:= Round( Pos.x );
      Graphic.Y:= Round( Pos.y );
      Graphic.Rotation:= Rot;
    end;
end;

procedure TBone.Clear;
var
  i: Integer;
begin
  for i:= 0 to Childs.Count - 1 do
    Childs[ i ].Free;
end;

end.

