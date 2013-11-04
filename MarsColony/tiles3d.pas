unit tiles3d;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, strutils, Math,
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_mouse,
  zgl_keyboard,
  zgl_joystick,
  zgl_primitives_2d,
  zgl_font,
  zgl_text,
  zgl_textures,
  zgl_sprite_2d,
  zgl_textures_png,
  zgl_math_2d,
  zgl_collision_2d,
  zgl_utils,
  zgl_render,
  LCLIntf,
  zgl_opengl,
  dglOpenGL,

  //tmp
  shaders,

  wavefront;
type

  { TTile }

  TTile = class
    Name: String;
    Model: TModel;

    procedure Render( X, Y, RotD: Real );
  end;

  { TTileSet }

  TTileSet = class( TPersistent )
    private
      FFName: String;
      FHeight: Integer;
      FTiles: TList;
      FWidth: Integer;

      function GetCount: Integer;
      function GetTile( Index: Integer ): TTile;
      procedure SetCount(AValue: Integer);
      procedure SetTile( Index: Integer ; AValue: TTile);

    public
      constructor Create;
      constructor CreateFromFile( FName: String );
      destructor Destroy; override;

      procedure SaveToFile( FName: String );

      procedure Clear;
      function Add( ATile: TTile ): Integer;
      procedure Delete( Index: Integer );

      property Tiles[ Index: Integer ]: TTile read GetTile write SetTile; default;
      property Width: Integer read FWidth write FWidth;
      property Height: Integer read FHeight write FHeight;
      property Count: Integer read GetCount write SetCount;
      property FileName: String read FFName write FFName;
  end;

  TMapTile = record
    Index: Integer;
    RotD: Real;
  end;

  { TTileMap }

  TTileMap = class( TPersistent )
    private
      FHeight: Integer;
      FTileHeight: Integer;
      FTileSet: TTileSet;
      FTileWidth: Integer;
      FWidth: Integer;
      FMap: Array of TMapTile;

      function GetMap( X, Y: Integer ): TMapTile;
      function GetMapA( n: Integer ): TMapTile;
      procedure SetHeight(AValue: Integer);
      procedure SetMap( X, Y: Integer ; AValue: TMapTile);
      procedure SetMapA( n: Integer ; AValue: TMapTile);
      procedure SetWidth(AValue: Integer);

      procedure UpdateSize;

    public
      constructor Create;
      destructor Destroy; override;

      procedure LoadFromFile( FName: String );
      procedure Draw;

      property Map[ X, Y: Integer ]: TMapTile read GetMap write SetMap;
      property MapArray[ n: Integer ]: TMapTile read GetMapA write SetMapA;

    published
      property Width: Integer read FWidth write SetWidth;
      property Height: Integer read FHeight write SetHeight;
      property TileSet: TTileSet read FTileSet write FTileSet;
  end;

  function Between( n, a, b: Integer ): Boolean; inline;

implementation

uses
  vars;


function Between( n, a, b: Integer ): Boolean; inline;
begin
  Result:= ( n >= a ) AND ( n <= b );
end;

{ TTileMap }

procedure TTileMap.SetHeight(AValue: Integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
  UpdateSize;
end;

function TTileMap.GetMap( X, Y: Integer ): TMapTile;
begin
  if (( Between( X, 0, Width - 1 )) AND ( Between( Y, 0, Height - 1 ))) then
    Result:= FMap[ Y * Width + X ]
  else
    Write( 'TTileMap.GetMap: Out of range! [' + IntToStr( x ) + ', ' + IntToStr( y ) + ']' );
end;

function TTileMap.GetMapA( n: Integer ): TMapTile;
begin
  if ( Between( n, 0, High( FMap ))) then
    Result:= FMap[ n ]
  else
    Write( 'TTileMap.GetMapA: Out of range! [' + IntToStr( n ) + ']' );
end;

procedure TTileMap.SetMap( X, Y: Integer ; AValue: TMapTile);
begin
  if (( Between( X, 0, Width - 1 )) AND ( Between( X, 0, Width - 1 ))) then
    FMap[ Y * Width + X ]:= AValue
  else
    Write( 'TTileMap.SetMap: Out of range! [' + IntToStr( x ) + ', ' + IntToStr( y ) + ']' );
end;

procedure TTileMap.SetMapA( n: Integer ; AValue: TMapTile);
begin
  if ( Between( n, 0, Width * Height - 1 )) then
    FMap[ n ]:= AValue
  else
    Write( 'TTileMap.SetMapA: Out of range! [' + IntToStr( n ) + ']' );
end;

procedure TTileMap.SetWidth(AValue: Integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
  UpdateSize;
end;

procedure TTileMap.UpdateSize;
begin
  SetLength( FMap, Width * Height );
end;

constructor TTileMap.Create;
begin
  inherited;
  Width:= 0;
  Height:= 0;
  UpdateSize;
end;

destructor TTileMap.Destroy;
begin
  SetLength( FMap, 0 );
  inherited Destroy;
end;

procedure TTileMap.LoadFromFile(FName: String);
  procedure LoadMapFile; inline;
  var
    F: TStringList;
    i: Integer;
    C, P, tex: String;

    procedure SetCP;
    begin
      C:= LowerCase( Trim( F.Names[ i ]));
      P:= Trim( F.ValueFromIndex[ i ]);
      if ( C = '' ) then
        C:= P;
    end;

    procedure LoadTiles;
      function GetNumber( var OffSet: Integer; S: String ): Integer;
      var
        _s: String;
      begin
        _s:= '';
        while (( OffSet <= Length( S )) AND ( S[ OffSet ] <> ' ' )) do
          begin
            _s:= _s + S[ OffSet ];
            Inc( OffSet );
          end;
        Inc( OffSet );
        if ( OffSet > Length( S )) then
          OffSet:= 0;
        Result:= StrToInt( _s );
      end;

    var
      j, k, o: Integer;
      chunk: String;
      Tile: TMapTile;
    begin
      Inc( i );

      k:= 0;

      while ( i < F.Count ) do
        begin
          SetCP;
          if ( c = 'endmap' ) then
            exit;

          j:= 0;
          for o:= 1 to WordCount( F[ i ], [ ' ' ]) do
            begin
              chunk:= ExtractWord( o, F[ i ], [ ' ' ]);
              Tile.Index:= StrToInt( ExtractDelimited( 1, chunk, [ '/' ]));
              Tile.RotD:= StrToFloat( ExtractDelimited( 2, chunk, [ '/' ]));
              Map[ j, k ]:= Tile;
              Inc( j );
            end;
          Inc( k );
          Inc( i );
        end;
    end;

    procedure LoadDimensions;
    begin
      Inc( i );

      while i < F.Count do
        begin
          SetCP;
          if ( c = 'enddimensions' ) then
            exit
          else if ( c = 'height' ) then
            Height:= StrToInt( P )
          else if ( c = 'width' ) then
            Width:= StrToInt( P )
          else
            WriteLn( 'Note: Line ' + IntToStr( i + 1 ) + ' ignored: P = ' + P + ' C = ' + c );
          Inc( i );
        end;
    end;

  begin
    if ( FileExists( FName )) then
      begin
        F:= TStringList.Create;
        F.LoadFromFile( FName );
        F.Delimiter:= '=';
        while i < F.Count do
          begin
            SetCP;
            if ( c = 'dimensions' ) then
              begin
                LoadDimensions;
                Inc( i );
              end
            else if ( c = 'map' ) then
              begin
                LoadTiles;
                Inc( i );
              end
            else
              begin
                WriteLn( 'Note: Line ' + IntToStr( i + 1 ) + ' ignored: P = ' + P + ' C = ' + c );
                Inc( i );
              end;
          end;

        F.Free;
      end
    else
      write( 'The specified file "' + FName + '" does not exist!' );
  end;

var
  i: Integer;
begin
  LoadMapFile;
end;

procedure TTileMap.Draw;
var
  i: Integer;
  MapTile: TMapTile;
begin
  for i:= 0 to High( FMap ) do
    begin
      MapTile:= MapArray[ i ];
      TileSet[ MapTile.Index ].Render( i mod Width, i div Width, MapTile.RotD );
    end;
end;

{ TTile }

procedure TTile.Render( X, Y, RotD: Real );
var
  WorldMatrix: TMatrix4f;
begin
  glPushMatrix;
  glTranslatef( X, 0, Y );
  glRotated( RotD, 0, 1, 0 );


//  glGetFloatv(GL_MODELVIEW_MATRIX, @worldMatrix);
//  ShaderSetParameter4fv( Shad_lightning2, 'WorldMatrix', WorldMatrix );

  Model.Draw;
  glPopMatrix();
end;

{ TTileSet }

function TTileSet.GetCount: Integer;
begin
  Result:= FTiles.Count;
end;

function TTileSet.GetTile( Index: Integer ): TTile;
begin
  Result:= TTile( FTiles[ Index ]);
end;

procedure TTileSet.SetCount(AValue: Integer);
begin
  FTiles.Count:= AValue;
end;

procedure TTileSet.SetTile( Index: Integer ; AValue: TTile);
begin
  FTiles[ Index ]:= AValue;
end;

constructor TTileSet.Create;
begin
  inherited;
  FTiles:= TList.Create;
end;

constructor TTileSet.CreateFromFile(FName: String);
{  procedure LoadTileSet; inline;
  var
    F: TStringList;
    i: Integer;
    C, P, tex: String;

    procedure SetCP;
    begin
      C:= LowerCase( Trim( F.Names[ i ]));
      P:= Trim( F.ValueFromIndex[ i ]);
      if ( C = '' ) then
        C:= P;
    end;

    function LoadTile( ): TTile;
    begin
      Result:= TTile.Create;
      Result.Name:= P;
      Inc( i );
      while i < F.Count do
        begin
          SetCP;
          if ( c = 'endtile' ) then
            exit
          else if ( c = 'left' ) then
            Result.Left:= StrToInt( P )
          else if ( c = 'top' ) then
            Result.Top:= StrToInt( P )
          else if ( c = 'height' ) then
            Result.Height:= StrToInt( P )
          else if ( c = 'width' ) then
            Result.Width:= StrToInt( P )
          else if ( c = 'offx' ) then
            Result.offx:= StrToInt( P )
          else if ( c = 'offy' ) then
            Result.offy:= StrToInt( P )
          else
            WriteLn( 'Note: Line ' + IntToStr( i + 1 ) + ' ignored: P = ' + P + ' C = ' + c );
          Inc( i );
        end;
    end;

    procedure LoadTiles;
    begin
//      tex:= P;
      TexName:= P;
      Inc( i );

      while ( i < F.Count ) do
        begin
          SetCP;
          if ( c = 'endfile' ) then
            exit
          else if ( c = 'tile' ) then
            begin
              Add( LoadTile());
              Inc( i );
            end
          else
            begin
              WriteLn( 'Note: Line ' + IntToStr( i + 1 ) + ' ignored: P = ' + P + ' C = ' + c );
              Inc( i );
            end;
        end;
    end;

    procedure LoadDimensions;
    begin
      Inc( i );

      while i < F.Count do
        begin
          SetCP;
          if ( c = 'enddimensions' ) then
            exit
          else if ( c = 'height' ) then
            Height:= StrToInt( P )
          else if ( c = 'width' ) then
            Width:= StrToInt( P )
          else
            WriteLn( 'Note: Line ' + IntToStr( i + 1 ) + ' ignored: P = ' + P + ' C = ' + c );
          Inc( i );
        end;
    end;

  begin
    if ( FileExists( FName )) then
      begin
        F:= TStringList.Create;
        F.LoadFromFile( FName );
        F.Delimiter:= '=';
        while i < F.Count do
          begin
            SetCP;
            if ( c = 'dimensions' ) then
              begin
                LoadDimensions;
                Inc( i );
              end
            else if ( c = 'file' ) then
              begin
                LoadTiles;
                Inc( i );
              end
            else
              begin
                WriteLn( 'Note: Line ' + IntToStr( i + 1 ) + ' ignored: P = ' + P + ' C = ' + c );
                Inc( i );
              end;
          end;

        F.Free;
      end;
  end;

var
  i: Integer;}
begin
  inherited Create;
  FFName:= FName;
  FTiles:= TList.Create;

{  LoadTileSet;

  for i:= 0 to FTiles.Count -1 do
    begin
      WriteLn( 'Tile: ' + Tiles[ i ].Name );
      WriteLn( '  left=' + IntToStr( Tiles[ i ].Left ));
      WriteLn( '  top=' + IntToStr( Tiles[ i ].Top ));
      WriteLn( '  width=' + IntToStr( Tiles[ i ].Width ));
      WriteLn( '  height=' + IntToStr( Tiles[ i ].Height ));
      WriteLn( '######################################################' );
   end;}
end;

destructor TTileSet.Destroy;
begin
  Clear;
  FTiles.Free;
  inherited Destroy;
end;

procedure TTileSet.SaveToFile( FName: String );
{var
  F: TStringList;

  procedure SaveDimensions;
  begin
    F.Add( 'dimensions' );
    F.Add( 'width=' + IntToStr( Width ));
    F.Add( 'height=' + IntToStr( Height ));
    F.Add( 'enddimensions' );
    F.Add( '' );
  end;

  procedure SaveTiles;
    procedure SaveTile( Index: Integer );
    begin
      with ( Tiles[ Index ]) do
        begin
          F.Add( 'tile=' + Name );
          F.Add( 'left=' + IntToStr( Left ));
          F.Add( 'top=' + IntToStr( Top ));
          F.Add( 'width=' + IntToStr( Width ));
          F.Add( 'height=' + IntToStr( Height ));
          F.Add( 'offx=' + IntToStr( offx ));
          F.Add( 'offy=' + IntToStr( offy ));
          F.Add( 'endtile' );
          F.Add( '' );
        end;
    end;
  var
    i: Integer;
  begin
    for i:= 0 to Count - 1 do
      SaveTile( i );
  end;
}
begin
{  F:= TStringList.Create;

  SaveDimensions;
  F.Add( 'file=' + FileName );
  SaveTiles;

  F.SaveToFile( FName );
  F.Free;}
end;

procedure TTileSet.Clear;
var
  i: Integer;
begin
  for i:= 0 to FTiles.Count - 1 do
    TTile( FTiles[ i ]).Free;
  FTiles.Clear;
end;

function TTileSet.Add(ATile: TTile): Integer;
begin
  Result:= FTiles.Add( ATile );
end;

procedure TTileSet.Delete(Index: Integer);
begin
  TTile( FTiles[ Index ]).Free;
  FTiles.Delete( Index );
end;



end.

