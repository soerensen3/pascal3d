{ p3drect.inc }

{$IFDEF INTERFACE}
type
  { TP3DRect }

  TP3DRect = packed object
    private
      FLeft: Float;
      FTop: Float;
      FWidth: Float;
      FHeight: Float;

      function GetBottom: Float;
      function GetBottomI: Integer;
      function GetBottomRight: TVec2;
      function GetHeightI: Integer;
      function GetLeftI: Integer;
      function GetRight: Float;
      function GetRightI: Integer;
      function GetTopI: Integer;
      function GetTopLeft: TVec2;
      function GetWidthHeight: TVec2;
      function GetWidthI: Integer;
      procedure SetBottom(AValue: Float);
      procedure SetBottomRight(AValue: TVec2);
      procedure SetRight(AValue: Float);
      procedure SetTopLeft(AValue: TVec2);
      procedure SetWidthHeight(AValue: TVec2);

    public
      function PtInRect( P: TVec2 ): Boolean;
      function RectCollision( R: TP3DRect ): Boolean;
      function Clip( R: TP3DRect ): TP3DRect;
      function Inset( ATopLeft, ABottomRight: TVec2 ): TP3DRect;
      function ToString: String;
      function Transform( Matrix: TMat4 ): TP3DRect;

      property TopLeft: TVec2 read GetTopLeft write SetTopLeft;
      property BottomRight: TVec2 read GetBottomRight write SetBottomRight;
      property WidthHeight: TVec2 read GetWidthHeight write SetWidthHeight;
      property Left: Float read FLeft write FLeft;
      property Top: Float read FTop write FTop;
      property Width: Float read FWidth write FWidth;
      property Height: Float read FHeight write FHeight;
      property Bottom: Float read GetBottom write SetBottom;
      property Right: Float read GetRight write SetRight;
      property LeftI: Integer read GetLeftI;
      property TopI: Integer read GetTopI;
      property WidthI: Integer read GetWidthI;
      property HeightI: Integer read GetHeightI;
      property RightI: Integer read GetRightI;
      property BottomI: Integer read GetBottomI;
    end;

function P3DRect( Left, Top, Width, Height: Float ): TP3DRect;
function P3DRect(TopLeft, WidthHeight: TVec2): TP3DRect;
function P3DRectEx(Left, Top, Right, Bottom: Float): TP3DRect;
function P3DRectEx( TopLeft, BottomRight: TVec2 ): TP3DRect;

operator + ( R: TP3DRect; v: TVec2 ): TP3DRect;
operator - ( R: TP3DRect; v: TVec2 ): TP3DRect;
operator * ( m: TMat4; R: TP3DRect ): TP3DRect;
operator * ( R: TP3DRect; S: Float ): TP3DRect;
operator / ( R: TP3DRect; S: Float ): TP3DRect;
operator = ( R1, R2: TP3DRect ): Boolean;


{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DRect }

function TP3DRect.GetBottomRight: TVec2;
begin
  Result:= TopLeft + vec2( Width, Height );
end;

function TP3DRect.GetHeightI: Integer;
begin
  Result:= round( Height );
end;

function TP3DRect.GetLeftI: Integer;
begin
  Result:= round( Left );
end;

function TP3DRect.GetBottom: Float;
begin
  Result:= TopLeft.y + Height;
end;

function TP3DRect.GetBottomI: Integer;
begin
  Result:= round( Bottom );
end;

function TP3DRect.GetRight: Float;
begin
  Result:= TopLeft.x + Width;
end;

function TP3DRect.GetRightI: Integer;
begin
  Result:= round( Right );
end;

function TP3DRect.GetTopI: Integer;
begin
  Result:= round( Top );
end;

function TP3DRect.GetTopLeft: TVec2;
begin
  Result:= vec2( Left, Top );
end;

function TP3DRect.GetWidthHeight: TVec2;
begin
  Result:= vec2( Width, Height );
end;

function TP3DRect.GetWidthI: Integer;
begin
  Result:= round( Width );
end;

procedure TP3DRect.SetBottom(AValue: Float);
begin
  Height:= Max( 0, AValue - Top );
end;


procedure TP3DRect.SetBottomRight(AValue: TVec2);
begin
  Width:= Max( 0, AValue.x - Left );
  Height:= Max( 0, AValue.y - Top );
end;


procedure TP3DRect.SetRight(AValue: Float);
begin
  Width:= Max( 0, AValue - Left );
end;


procedure TP3DRect.SetTopLeft(AValue: TVec2);
begin
  Left:= AValue.X;
  Top:= AValue.Y;
end;

procedure TP3DRect.SetWidthHeight(AValue: TVec2);
begin
  Width:= AValue.X;
  Height:= AValue.Y;
end;


function TP3DRect.PtInRect(P: TVec2): Boolean;
begin
  Result:= ( P.x >= Left ) and ( P.x < Right )
       and ( P.Y >= Top )  and ( P.y < Bottom );
end;

function TP3DRect.RectCollision(R: TP3DRect): Boolean;
begin
  Result:= ( Left <= R.Right ) and
           ( Right >= R.Left ) and
           ( Top <= R.Bottom ) and
           ( Bottom >= R.Top );
end;

function TP3DRect.Clip(R: TP3DRect): TP3DRect;
begin
  Result.TopLeft:= Max( R.TopLeft, TopLeft );
  Result.BottomRight:= Max( Result.TopLeft, Min( R.BottomRight, BottomRight ));
end;

function TP3DRect.Inset(ATopLeft, ABottomRight: TVec2): TP3DRect;
begin
  Result.TopLeft:= TopLeft + ATopLeft;
  Result.BottomRight:= BottomRight - ABottomRight;
end;

function TP3DRect.ToString: String;
begin
  Result:= TopLeft.ToString() + ', ' + BottomRight.ToString();
end;

function TP3DRect.Transform(Matrix: TMat4): TP3DRect;
var
  _TopLeft, _TopRight, _BottomLeft, _BottomRight: TVec2;
begin
  _TopLeft:= ( Matrix * vec4( TopLeft, 0, 1 )).xy;
  _TopRight:= ( Matrix * vec4( Right, Top, 0, 1 )).xy;
  _BottomLeft:= ( Matrix * vec4( Left, Bottom, 0, 1 )).xy;
  _BottomRight:= ( Matrix * vec4( BottomRight, 0, 1 )).xy;
  Result.TopLeft:= min( _TopLeft, min( _TopRight, min( _BottomLeft, _BottomRight )));
  Result.BottomRight:= max( _TopLeft, max( _TopRight, max( _BottomLeft, _BottomRight )));
end;

operator+(R: TP3DRect; v: TVec2): TP3DRect;
begin
  Result:= P3DRect( R.TopLeft + v, R.WidthHeight );
end;

operator-(R: TP3DRect; v: TVec2): TP3DRect;
begin
  Result:= P3DRect( R.TopLeft - v, R.WidthHeight );
end;

operator*(m: TMat4; R: TP3DRect): TP3DRect;
begin
  Result:= R.Transform( m );
end;

operator*(R: TP3DRect; S: Float): TP3DRect;
begin
  Result.TopLeft:= R.TopLeft * S;
  Result.BottomRight:= R.BottomRight * S;
end;

operator/(R: TP3DRect; S: Float): TP3DRect;
begin
  Result.TopLeft:= R.TopLeft / S;
  Result.BottomRight:= R.BottomRight / S;
end;

operator=(R1, R2: TP3DRect): Boolean;
begin
  Result:= ( R1.TopLeft = R2.TopLeft ) and ( R1.WidthHeight = R2.WidthHeight );
end;

function P3DRect( Left, Top, Width, Height: Float ): TP3DRect;
begin
  Result.Left:= Left;
  Result.Top:= Top;
  Result.Width:= Max( 0, Width );
  Result.Height:= Max( 0, Height );
end;

function P3DRect(TopLeft, WidthHeight: TVec2): TP3DRect;
begin
  Result.TopLeft:= TopLeft;
  Result.WidthHeight:= WidthHeight;
end;

function P3DRectEx(Left, Top, Right, Bottom: Float): TP3DRect;
begin
  Result.Left:= Left;
  Result.Top:= Top;
  Result.Right:= Right;
  Result.Bottom:= Bottom;
end;

function P3DRectEx(TopLeft, BottomRight: TVec2): TP3DRect;
begin
  Result.TopLeft:= TopLeft;
  Result.BottomRight:= BottomRight;
end;

{$ENDIF}

