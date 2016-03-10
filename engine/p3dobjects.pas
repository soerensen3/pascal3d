unit p3dobjects;

{$mode objfpc}{$H+}

interface
  uses
    Classes, SysUtils, p3dgenerics;

  type
    TP3DFileWriter = class;
    TP3DFileReader = class;

    TP3DObject = class;

    //TODO: Implement Rendering
    //TODO: Implement File and Stream Read/Write         +
    //TODO: Write RegisterClass


    { TP3DObjectList }
    TP3DCustomObjectList = specialize gP3DCustomObjectList < TP3DObject >;
    TP3DObjectList = class( TP3DCustomObjectList )
      function FindUniqueName( BaseStr: String ): String;
      function FindByName( AName: String ): Integer;
      procedure Clear(const FreeObjects: Boolean = True ); override;
    end;

    TP3DObjectClass = class of TP3DObject;

    TP3DObjectHeader = record
      ObjName: String;
      ObjClass: String;
      _Start,
      _End: Integer;
    end;

    TP3DObject = class( TPersistent )
      private
        FName: TComponentName;
        FParentList: TP3DObjectList;

        procedure SetName( const NewName: TComponentName );

      public
        constructor Create( AParentList: TP3DObjectList ); //TODO: Replace AParentList Param by something else
        destructor Destroy; override;

//        procedure Render; virtual;
//        procedure Process; virtual;

        procedure SaveToStream( Writer: TP3DFileWriter );
        procedure LoadFromStream( Reader: TP3DFileReader );
        function SaveToFile( FileName: String ): Boolean;
        procedure OnLoad( Reader: TP3DFileReader ); virtual;
        procedure OnSave( Writer: TP3DFileWriter ); virtual;
        procedure OnAutoCreate; virtual;
        procedure AfterLoad; virtual;

//        class procedure RegisterClass;

      published
        property Name: TComponentName read FName write SetName stored False;
        property ParentList: TP3DObjectList read FParentList;
//        property Visible: Boolean read FVisible write FVisible;
    end;

    { TP3DFileWriter }

    TP3DFileWriter = class( TPersistent )
      private
        HeaderPointer: Integer;

      public
        Header: array [ 0..2047 ] of TP3DObjectHeader;
        HeaderCount: Integer;

        S: TFileStream;

        constructor Create( FileName: String );
        destructor Destroy; override;

        function SaveObjectToFile( Obj: TP3DObject ): Boolean;
        function SaveIntToFile( Val: Integer ): Boolean;
        function SaveFloatToFile( Val: Single ): Boolean;
        function SaveByteToFile( Val: Byte ): Boolean;
        function SaveBoolToFile( Val: Boolean ): Boolean;
        function SaveStringToFile( Val: String ): Boolean;
    end;

    { TP3DFileReader }

    TP3DFileReader = class( TPersistent )
      private
        HeaderPointer: Integer;

      public
        Header: array [ 0..2047 ] of TP3DObjectHeader;
        HeaderCount: Integer;
        S: TFileStream;

        constructor Create( FileName: String );
        destructor Destroy; override;

        function CreateHeaderObjects(): Boolean;
        function LoadObjectFromFile( Obj: TP3DObject ): Boolean;
        function LoadIntFromFile( out Val: Integer ): Boolean;
        function LoadFloatFromFile( out Val: Single ): Boolean;
        function LoadByteFromFile( out Val: Byte ): Boolean;
        function LoadBoolFromFile( out Val: Boolean ): Boolean;
        function LoadStringFromFile( out Val: String ): Boolean;
    end;

implementation

{ TP3DObjectList }

function TP3DObjectList.FindUniqueName( BaseStr: String ): String;
var
  I: Integer;
begin
  I:= 0;
  repeat
    Inc( I );
    Result:= BaseStr + IntToStr( I );
  until ( FindByName( Result ) = -1 );
end;

function TP3DObjectList.FindByName(AName: String): Integer;
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

procedure TP3DObjectList.Clear(const FreeObjects: Boolean);
var
  i: Integer;
begin
  if ( FreeObjects ) then
    for i:= Count - 1 downto 0 do
      Items[ i ].Free;
  Count:= 0;
end;

{ TP3DFileReader }

constructor TP3DFileReader.Create( FileName: String );
begin
  inherited Create;
  S:= TFileStream.Create( FileName, fmOpenRead );
end;

destructor TP3DFileReader.Destroy;
begin
  S.Free;
  inherited;
end;

function TP3DFileReader.CreateHeaderObjects(): Boolean;
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
      with ( TP3DObjectClass( GetClass( Header[ I ].ObjClass )).Create( nil )) do
        begin
          Name:= Header[ I ].ObjName;
          OnAutoCreate;
        end;
    end;
  S.Position:= Header[ 0 ]._Start;
end;

function TP3DFileReader.LoadObjectFromFile(Obj: TP3DObject): Boolean;
begin

end;

function TP3DFileReader.LoadIntFromFile(out Val: Integer): Boolean;
begin

end;

function TP3DFileReader.LoadFloatFromFile(out Val: Single): Boolean;
begin

end;

function TP3DFileReader.LoadByteFromFile(out Val: Byte): Boolean;
begin

end;

function TP3DFileReader.LoadBoolFromFile(out Val: Boolean): Boolean;
begin

end;

function TP3DFileReader.LoadStringFromFile(out Val: String): Boolean;
begin

end;

{ TP3DFileWriter }

constructor TP3DFileWriter.Create( FileName: String );
begin
  inherited Create;

  S:= TFileStream.Create( FileName, fmCreate );
  HeaderPointer:= S.Position;
  SaveIntToFile( 0 ); // Reserve 1 Int for Headerpointer
end;

destructor TP3DFileWriter.Destroy;
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

function TP3DFileWriter.SaveObjectToFile(Obj: TP3DObject): Boolean;
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

function TP3DFileWriter.SaveIntToFile(Val: Integer): Boolean;
begin
  S.WriteBuffer( Val, SizeOf( Val ));
  Result:= True;
end;

function TP3DFileWriter.SaveFloatToFile(Val: Single): Boolean;
begin
  S.WriteBuffer( Val, SizeOf( Val ));
  Result:= True;
end;

function TP3DFileWriter.SaveByteToFile(Val: Byte): Boolean;
begin
  S.WriteBuffer( Val, SizeOf( Val ));
  Result:= True;
end;

function TP3DFileWriter.SaveBoolToFile(Val: Boolean): Boolean;
begin
  S.WriteBuffer( Val, SizeOf( Val ));
  Result:= True;
end;

function TP3DFileWriter.SaveStringToFile(Val: String): Boolean;
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

{ TP3DObject }

procedure TP3DObject.SetName(const NewName: TComponentName);
begin
  if ( Name = NewName ) then
    exit;
  if ( ParentList.FindByName( NewName ) < 0 ) then
    FName:= NewName
  else
    raise Exception.Create( Format('Error in TP3DObject.SetName(''%s''). An object with that name already exists!', [NewName ]));
end;

constructor TP3DObject.Create( AParentList: TP3DObjectList );
var
  BaseName: String;
begin
  if ( AParentList = nil ) then
    raise Exception.Create( 'Error in TP3DObject.Create(nil). AParentList cant be nil!' );
  inherited Create;
//  FEngine:= AEngine;
  FParentList:= AParentList;

  if ( ClassName[ 1 ] = 'T' ) then
    BaseName:= Copy( ClassName, 2, Length( ClassName ) - 1 )
  else
    BaseName:= ClassName;

  FName:= ParentList.FindUniqueName( BaseName );
  ParentList.Add( Self );
//  Visible:= True;
//  SaveChilds:= True;
end;

destructor TP3DObject.Destroy;
begin
  ParentList.Delete( ParentList.IndexOf( Self ));
  inherited Destroy;
end;

procedure TP3DObject.SaveToStream(Writer: TP3DFileWriter);
begin

end;

procedure TP3DObject.LoadFromStream(Reader: TP3DFileReader);
begin

end;

function TP3DObject.SaveToFile(FileName: String): Boolean;
begin

end;

procedure TP3DObject.OnLoad(Reader: TP3DFileReader);
begin

end;

procedure TP3DObject.OnSave(Writer: TP3DFileWriter);
begin

end;

procedure TP3DObject.OnAutoCreate;
begin

end;

procedure TP3DObject.AfterLoad;
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
  if ( T <> TP3DObject ) then
    raise Exception.Create( 'Only TP3DObject and above classes supported!' ); //TODO: Implement better mechanism for catching unwanted types
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
    if ( TP3DObject( Items[ i ]).Name = Name ) then
      begin
        Result:= TP3DObject( Items[ i ]);
        break;
      end;
end;

procedure gObjectList.KillAll;
var
  i: Integer;
begin
  for i:= Count - 1 downto 0 do
    TP3DObject( Get( i )).Free;
  Count:= 0;
end;

procedure gObjectList.Kill( Item: T );
begin
  DeleteItm( Item );
  TP3DObject( Item ).Free;
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
  if ( T = TP3DObject ) then
    TP3DObject( Item ).FParentList:= TObjectBuffer( Self ); //TODO: Add Notifiers instead of typecasting
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

procedure gObjectList.SaveToStream( Writer: TP3DFileWriter );
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do;
    Writer.SaveObjectToFile( Items[ i ]);
end;

procedure gObjectList.LoadFromStream(Reader: TP3DFileReader);
begin

end;

function gObjectList.SaveToFile(FileName: String): Boolean;
var
  i: Integer;
  Writer: TP3DFileWriter;
begin
  Writer:= TP3DFileWriter.Create( FileName );

  for i:= 0 to Count - 1 do
    Writer.SaveObjectToFile( Items[ i ]);

  Writer.Free;
  Result:= True;
end;
}

end.

