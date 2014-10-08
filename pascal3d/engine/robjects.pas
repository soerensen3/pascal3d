unit RObjects;

{$mode objfpc}{$H+}

interface
  uses
    Classes, SysUtils;

  const
    ELogUnitName = '(RObjects.pas)';
  type
    TRevFileWriter = class;
    TRevFileReader = class;

    TBaseObject = class;
    //restricted to TBaseObject and above -> see create for this

    { TObjectList }

{    generic gObjectList <T> = class( TList )
      private
        FEngine: TREngine;

      protected
        function Get( Index: Integer ): T;
        procedure Put( Index: Integer; Item: T );

      public
        constructor Create( AEngine: TREngine );
        destructor Destroy; override;

        function GetByName( Name: String ): T;
        procedure KillAll;
        procedure Kill( Item: T );
        procedure DeleteItm( Item: T );
        procedure MoveTo( Index: Integer; Dest: gObjectList );
        function AddItm( Item: T ): Integer;
        function FindUniqueName( BaseStr: String ): String;

        procedure SaveToStream( Writer: TRevFileWriter );
        procedure LoadFromStream( Reader: TRevFileReader );
        function SaveToFile( FileName: String ): Boolean;

        property Items[ Index: Integer ]: T read Get write Put; default;
    end;}


//    TObjectBuffer = specialize gObjectList <TBaseObject>;

    //TODO: Implement Rendering
    //TODO: Implement File and Stream Read/Write         +
    //TODO: Write RegisterClass

    {$MACRO ON}
    {$DEFINE TCustomList:= TCustomObjectList}
    {$DEFINE TCustomListEnumerator:= TObjectEnumerator}
    {$DEFINE TCustomItem:= TBaseObject}
    {$DEFINE INTERFACE}
    {$INCLUDE custom_list.inc}

    TObjectList = class( TCustomObjectList )
      procedure Clear; override;
      function FindUniqueName( BaseStr: String ): String;
      function FindByName( AName: String ): Integer;
    end;

    { TBaseObject }

    TBaseObjectClass = class of TBaseObject;

      TObjectHeader = record
        ObjName: String;
        ObjClass: String;
        _Start,
        _End: Integer;
      end;

    TBaseObject = class( TPersistent )
      private
        FName: TComponentName;
        FParentList: TObjectList;
//        FVisible: Boolean;

        procedure SetName( const NewName: TComponentName );

      public
        constructor Create( AParentList: TObjectList ); //TODO: Replace AParentList Param by something else
        destructor Destroy; override;

//        procedure Render; virtual;
//        procedure Process; virtual;

        procedure SaveToStream( Writer: TRevFileWriter );
        procedure LoadFromStream( Reader: TRevFileReader );
        function SaveToFile( FileName: String ): Boolean;
        procedure OnLoad( Reader: TRevFileReader ); virtual;
        procedure OnSave( Writer: TRevFileWriter ); virtual;
        procedure OnAutoCreate; virtual;
        procedure AfterLoad; virtual;

//        class procedure RegisterClass;

      published
        property Name: TComponentName read FName write SetName stored False;
        property ParentList: TObjectList read FParentList;
//        property Visible: Boolean read FVisible write FVisible;
    end;

    { TRevFileWriter }

    TRevFileWriter = class( TPersistent )
      private
        HeaderPointer: Integer;
//        Engine: TEngine;

      public
        Header: array [ 0..2047 ] of TObjectHeader;
        HeaderCount: Integer;

        S: TFileStream;

        constructor Create( {AEngine: TEngine; }FileName: String );
        destructor Destroy; override;

        function SaveObjectToFile( Obj: TBaseObject ): Boolean;
        function SaveIntToFile( Val: Integer ): Boolean;
        function SaveFloatToFile( Val: Single ): Boolean;
        function SaveByteToFile( Val: Byte ): Boolean;
        function SaveBoolToFile( Val: Boolean ): Boolean;
        function SaveStringToFile( Val: String ): Boolean;
    end;

    { TRevFileReader }

    TRevFileReader = class( TPersistent )
      private
        HeaderPointer: Integer;

      public
        Header: array [ 0..2047 ] of TObjectHeader;
        HeaderCount: Integer;
        S: TFileStream;

        constructor Create( FileName: String );
        destructor Destroy; override;

        function CreateHeaderObjects(): Boolean;
        function LoadObjectFromFile( Obj: TBaseObject ): Boolean;
        function LoadIntFromFile( out Val: Integer ): Boolean;
        function LoadFloatFromFile( out Val: Single ): Boolean;
        function LoadByteFromFile( out Val: Byte ): Boolean;
        function LoadBoolFromFile( out Val: Boolean ): Boolean;
        function LoadStringFromFile( out Val: String ): Boolean;
    end;

implementation

{ TObjectList }

procedure TObjectList.Clear;
var
  Item: TBaseObject;
begin
  for Item in Self do
    Item.Free;
  inherited Clear;
end;

function TObjectList.FindUniqueName( BaseStr: String ): String;
var
  I: Integer;
begin
  I:= 0;
  repeat
    Inc( I );
    Result:= BaseStr + IntToStr( I );
  until ( FindByName( Result ) = -1 );
end;

function TObjectList.FindByName(AName: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( Items[ i ].Name = AName ) then
      begin
        Result:= i;
        break;
      end;
end;

{ TRevFileReader }

constructor TRevFileReader.Create( FileName: String );
begin
  inherited Create;
  S:= TFileStream.Create( FileName, fmOpenRead );
end;

destructor TRevFileReader.Destroy;
begin
  S.Free;
  inherited;
end;

function TRevFileReader.CreateHeaderObjects(): Boolean;
var
  i: Integer;
begin
  LoadIntFromFile( HeaderPointer );
  S.Position:= HeaderPointer;

  LoadIntFromFile( HeaderCount );
  for i:= 0 to HeaderCount - 1 do
    begin
//      S.Read( Header[ I ], SizeOf( TObjectHeader ));
      LoadStringFromFile( Header[ i ].ObjName );
      LoadStringFromFile( Header[ i ].ObjClass );
      LoadIntFromFile( Header[ i ]._Start );
      LoadIntFromFile( Header[ i ]._End );

      if ( GetClass( Header[ I ].ObjClass ) = nil ) then
        begin
          raise Exception.Create( 'Failed to load object from file! No such class registered: "' + Header[ I ].ObjClass + '"'{,
            ELogUnitName + '.' + ClassName + '.CreateHeaderObjects'} );
          exit;
        end;
      with ( TBaseObjectClass( GetClass( Header[ I ].ObjClass )).Create( nil )) do
        begin
          Name:= Header[ I ].ObjName;
          OnAutoCreate;
        end;
    end;
  S.Position:= Header[ 0 ]._Start;
end;

function TRevFileReader.LoadObjectFromFile(Obj: TBaseObject): Boolean;
begin

end;

function TRevFileReader.LoadIntFromFile(out Val: Integer): Boolean;
begin

end;

function TRevFileReader.LoadFloatFromFile(out Val: Single): Boolean;
begin

end;

function TRevFileReader.LoadByteFromFile(out Val: Byte): Boolean;
begin

end;

function TRevFileReader.LoadBoolFromFile(out Val: Boolean): Boolean;
begin

end;

function TRevFileReader.LoadStringFromFile(out Val: String): Boolean;
begin

end;

{ TRevFileWriter }

constructor TRevFileWriter.Create( FileName: String );
begin
  inherited Create;

  S:= TFileStream.Create( FileName, fmCreate );
  HeaderPointer:= S.Position;
  SaveIntToFile( 0 ); // Reserve 1 Int for Headerpointer
end;

destructor TRevFileWriter.Destroy;
var
  i, SPos: Integer;
begin
  //Writing Header to file
  SPos:= S.Position; //Save Headerposition
  SaveIntToFile( HeaderCount ); //Number of header entries

  for i:= 0 to HeaderCount - 1 do //Writing one header for each object
    begin
      SaveStringToFile( Header[ i ].ObjName );
      SaveStringToFile( Header[ i ].ObjClass );
      SaveIntToFile( Header[ i ]._Start );
      SaveIntToFile( Header[ i ]._End );
    end;

  S.Position:= HeaderPointer; //Writing down pointer to header
  SaveIntToFile( SPos );

  S.Free;
  inherited;
end;

function TRevFileWriter.SaveObjectToFile(Obj: TBaseObject): Boolean;
begin
   with( Header[ HeaderCount ]) do
    begin
      ObjName:= Obj.Name;
      ObjClass:= Obj.ClassName;
      _Start:= S.Position;
    end;
  try
    Obj.SaveToStream( Self );
  except
    on E: Exception do
      begin
        raise Exception.Create( 'Failed to save object "' + Obj.Name + '" to file: ' + E.Message{,
          ELogUnitName + '.' + ClassName + '.SaveObjectToFile'} );
        S.Position:= Header[ HeaderCount ]._Start;
        S.Size:= S.Position;
        Result:= False;
        exit;
      end;
  end;
  Header[ HeaderCount ]._End:= S.Position - 1;
  Inc( HeaderCount );
end;

function TRevFileWriter.SaveIntToFile(Val: Integer): Boolean;
begin
  S.WriteBuffer( Val, SizeOf( Val ));
  Result:= True;
end;

function TRevFileWriter.SaveFloatToFile(Val: Single): Boolean;
begin
  S.WriteBuffer( Val, SizeOf( Val ));
  Result:= True;
end;

function TRevFileWriter.SaveByteToFile(Val: Byte): Boolean;
begin
  S.WriteBuffer( Val, SizeOf( Val ));
  Result:= True;
end;

function TRevFileWriter.SaveBoolToFile(Val: Boolean): Boolean;
begin
  S.WriteBuffer( Val, SizeOf( Val ));
  Result:= True;
end;

function TRevFileWriter.SaveStringToFile(Val: String): Boolean;
var
  Len,
  i: Integer;
begin
  Len:= Length( Val );
  S.WriteBuffer( Len, SizeOf( Len ));
  for i:= 1 to Len do
    S.WriteBuffer( Val[ I ], 1 );
  Result:= True;
end;

{ TBaseObject }

procedure TBaseObject.SetName(const NewName: TComponentName);
begin

end;

constructor TBaseObject.Create( AParentList: TObjectList );
var
  BaseName: String;
begin
  inherited Create;
//  FEngine:= AEngine;
  FParentList:= AParentList;

  if ( ClassName[ 1 ] = 'T' ) then
    BaseName:= Copy( ClassName, 2, Length( ClassName ) - 1 )
  else
    BaseName:= ClassName;

  FName:= ParentList.FindUniqueName( BaseName );
//  Visible:= True;
//  SaveChilds:= True;
end;

destructor TBaseObject.Destroy;
begin
  inherited Destroy;
end;

procedure TBaseObject.SaveToStream(Writer: TRevFileWriter);
begin

end;

procedure TBaseObject.LoadFromStream(Reader: TRevFileReader);
begin

end;

function TBaseObject.SaveToFile(FileName: String): Boolean;
begin

end;

procedure TBaseObject.OnLoad(Reader: TRevFileReader);
begin

end;

procedure TBaseObject.OnSave(Writer: TRevFileWriter);
begin

end;

procedure TBaseObject.OnAutoCreate;
begin

end;

procedure TBaseObject.AfterLoad;
begin

end;

{ TObjectList }
{
function gObjectList.Get(Index: Integer): T;
begin
  inherited Get( Index );
end;

procedure gObjectList.Put(Index: Integer; Item: T);
begin
  inherited Put( Index, Item );
end;

constructor gObjectList.Create( AEngine: TREngine );
begin
  if ( T <> TBaseObject ) then
    raise Exception.Create( 'Only TBaseObject and above classes supported!' ); //TODO: Implement better mechanism for catching unwanted types
  FEngine:= AEngine;
  inherited Create;
end;

destructor gObjectList.Destroy;
begin
  inherited Destroy;
end;

function gObjectList.GetByName( Name: String ): T;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to Count - 1 do
    if ( TBaseObject( Items[ i ]).Name = Name ) then
      begin
        Result:= TBaseObject( Items[ i ]);
        break;
      end;
end;

procedure gObjectList.KillAll;
var
  i: Integer;
begin
  for i:= Count - 1 downto 0 do
    TBaseObject( Get( i )).Free;
  Count:= 0;
end;

procedure gObjectList.Kill( Item: T );
begin
  DeleteItm( Item );
  TBaseObject( Item ).Free;
end;


procedure gObjectList.DeleteItm(Item: T);
begin
  Delete( IndexOf( Item ));
end;

procedure gObjectList.MoveTo( Index: Integer; Dest: gObjectList );
begin
  Dest.Add( Get( Index ));
  Delete( Index );
end;

function gObjectList.AddItm( Item: T ): Integer;
begin
  Result:= inherited Add( Item );
  if ( T = TBaseObject ) then
    TBaseObject( Item ).FParentList:= TObjectBuffer( Self ); //TODO: Add Notifiers instead of typecasting
end;

function gObjectList.FindUniqueName( BaseStr: String ): String;
var
  I: Integer;
begin
  I:= 0;
  repeat
    Inc( I );
    Result:= BaseStr + IntToStr( I );
  until ( GetByName( Result ) = nil );
end;

procedure gObjectList.SaveToStream( Writer: TRevFileWriter );
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do;
    Writer.SaveObjectToFile( Items[ i ]);
end;

procedure gObjectList.LoadFromStream(Reader: TRevFileReader);
begin

end;

function gObjectList.SaveToFile(FileName: String): Boolean;
var
  i: Integer;
  Writer: TRevFileWriter;
begin
  Writer:= TRevFileWriter.Create( FileName );

  for i:= 0 to Count - 1 do
    Writer.SaveObjectToFile( Items[ i ]);

  Writer.Free;
  Result:= True;
end;
}
{$DEFINE TCustomList:= TCustomObjectList}
{$DEFINE TCustomListEnumerator:= TObjectEnumerator}
{$DEFINE TCustomItem:= TBaseObject}
{$DEFINE IMPLEMENTATION}
{$INCLUDE custom_list.inc}

end.
