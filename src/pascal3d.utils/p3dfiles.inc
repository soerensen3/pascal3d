{ <description>

  Copyright (c) 2017 Johannes Rosleff Sörensen

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the
  "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish,
  distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to
  the following conditions:

  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
  ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH
  THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

  p3dfiles.inc
  purpose: provides the file structures used in the engine as well as all loader classes.
}

{$IFDEF INTERFACE}
type
  { TP3DBinaryFile

    combines a file pointer with a stream. This way it includes change detection.
  }

  TP3DBinaryFile = class ( TP3DFilePointer )
    private
      FStream: TStream;

    public
      constructor Create( AFileName: String; AUserPointer: Pointer=nil; FileMode: Word = fmOpenRead );
      destructor Destroy; override;

    published
      property Stream: TStream read FStream write FStream;
  end;

  TP3DInterfacedPersistent = class;
  TP3DPropertyList = class;

  { TP3DJSONLoader

    Streaming support for any object that extends TP3DInterfacedPersistent by loading it's properties from a json or a binary file.
    The binary file is generally expected to have the same file name as the json but a ".p3dbin" extension. Property means a descendant of
    TP3DPropertyAccess. All properties in the Properties field of the object are saved or loaded.
  }

  TP3DJSONLoader = class ( TP3DFilePointer )
    private
      FBinaryFile: TP3DBinaryFile;
      FJSON: TJSONData;
      FPostponedLoading: TP3DPropertyList;
      FRoot: TP3DInterfacedPersistent;

      procedure OpenFiles; // Open a JSON File if the filepointer exists or create a new file in memory. If the file is found a corresponding
                          // binary file will be opened for read as well. If not found an empty binary file will be created.
      procedure CloseFiles; // Closes the JSON File and the corresponding binary file. All changes that are not written to the filesystem
                            // are lost. Call WriteFile before to store the changes.

      function DoFindPath(ARoot: TJSONData; APath: String; out ARestPath: String): TJSONData;
      function DoFindPathArray( ARoot: TJSONArray; APath: String; out ARestPath: String ): TJSONData;
      function DoFindPathObject( ARoot: TJSONObject; APath: String; out ARestPath: String ): TJSONData;

    public
      constructor Create(AFileName: String; AUserPointer: Pointer=nil); // Create a new JSON Loader with the given file name. The file does
                                                                        // not need to exist, but the path should be valid when saved. You
                                                                        // can pass an optional user pointer.

      function FindPath( APath: String ): TJSONData; // Utility function to find a path in JSON. The difference to TJSONData.FindPath is that
                                                     // supports string indices on arrays (E.g. `Objects[ "Object1" ]` for finding the
                                                     // object with the name "Object1" in the array Objects). String indexes guaranty that the
                                                     // Paths stay valid even if the Objects array is changed afterwards (unless the object
                                                     // itself is deleted from the array).
      destructor Destroy; override;

      procedure ReadFile; // Starts reading a object's properties from a JSON File by traversing the JSON's tree
      procedure WriteFile; // Starts writing a object's properties to a JSON File by traversing the object's tree

      procedure Reload; override;  // Tries to reopen the JSON file and checks if a binary file exists and is newer than the possibly loaded one.
      //function LoadFromPath( ):

    published
      property JSON: TJSONData read FJSON write FJSON; // A pointer to the opened JSON file. May be nil if OpenFiles was not called.
      property BinaryFile: TP3DBinaryFile read FBinaryFile write FBinaryFile; // A pointer to the opened Binary file. May be nil if OpenFiles was not called.
      property Root: TP3DInterfacedPersistent read FRoot write FRoot; // This is the root object to be loaded and saved. From there on a JSON
                                                                      // tree with the properties and subproperties is created on save or parsed on load.
  end;

  { TP3DJSONContext

    A JSON Context is passed when a property is loaded by a JSON Loader. It stores the JSON Loader and the current JSON node (Data) and it's
    parent.
  }

  TP3DJSONContext = record
    JSONLoader: TP3DJSONLoader;
    Data: TJSONData;
    Owner: TPersistent;
    Parent: TJSONData;
  end;

  {IP3DJSONStreamable = interface
    [ '{5ED472BA-80BE-48E6-A86A-D33B74A75DF7}' ]

    constructor CreateFromJSONContext( AContext: TP3DJSONContext );
    procedure LoadFromJSONContext( AContext: TP3DJSONContext );

  end;}

  // Convenience function to create json context which is passed on loading properties from a JSON File.
  function P3DJSONContext( AJSONLoader: TP3DJSONLoader; AData: TJSONData; AOwner: TPersistent; AParent: TJSONData): TP3DJSONContext;


{$ENDIF}

{$IFDEF IMPLEMENTATION}
function P3DJSONContext( AJSONLoader: TP3DJSONLoader; AData: TJSONData; AOwner: TPersistent; AParent: TJSONData): TP3DJSONContext;
begin
  Result.JSONLoader:= AJSONLoader;
  Result.Data:= AData;
  Result.Owner:= AOwner;
  Result.Parent:= AParent;
end;

{ TP3DBinaryFile }

constructor TP3DBinaryFile.Create(AFileName: String; AUserPointer: Pointer; FileMode: Word);
begin
  inherited Create( AFileName, AUserPointer );
  Stream:= TFileStream.Create( AFileName, FileMode );
end;

destructor TP3DBinaryFile.Destroy;
begin
  Stream.Free;
  inherited Destroy;
end;


{ TP3DJSONLoader }

//
// binary file will be opened for read as well.

procedure TP3DJSONLoader.OpenFiles;
var
  P: TJSONParser;
  S: TFileStream;
  BinName: RawByteString;

const
  ReadMode: array [ Boolean ] of Word = ( fmCreate, fmOpenRead );
begin
  if ( CheckFileExists ) then   // Open a JSON File if the filepointer exists or create a new file in memory.
    S:= TFileStream.Create( FileName, fmOpenRead )
  else
    S:= TFileStream.Create( FileName, fmCreate );

  try
    P:= TJSONParser.Create( S, True {[ joUTF8 ]}); // Create a JSON parser from stream
    if ( CheckFileExists ) then
      JSON:= P.Parse; // If a file was loaded then read the file to JSON
    BinName:= ChangeFileExt( FileName, '.p3dbin' ); // look for a corresponding binary file with the same name but different extension
    if ( SysUtils.FileExists( BinName )) then
      BinaryFile:= TP3DBinaryFile.Create( BinName )
    else
      BinaryFile:= TP3DBinaryFile.Create( BinName, nil, fmCreate ); // If not found an empty binary file will be created.
  finally
    P.Free;
  end;
  S.Free;
end;

procedure TP3DJSONLoader.CloseFiles;
begin
  FreeAndNil( FBinaryFile );
  FreeAndNil( FJSON );
end;

function TP3DJSONLoader.DoFindPath(ARoot: TJSONData; APath: String; out ARestPath: String): TJSONData;
begin
  ARestPath:= APath;
  if ( ARestPath = '' ) then
    Exit( ARoot )
  else
    case ARoot.JSONType of
      jtArray: Result:= DoFindPathArray( TJSONArray( ARoot ), APath, ARestPath );
      jtObject: Result:= DoFindPathObject( TJSONObject( ARoot ), APath, ARestPath );
    else
      Exit( nil );
    end;
end;

function TP3DJSONLoader.DoFindPathArray(ARoot: TJSONArray; APath: String; out ARestPath: String): TJSONData;
// From fpjson.pp
Var
  p,i : integer;
  e : String;

  function FindItemByName( AName: String ): Integer;
  var
    Item: TJSONEnum;
  begin
    Result:= -1;
    for Item in ARoot do
      begin
        WriteLn( AName, ' =? ', TJSONObject( Item.Value ).Get( 'Name', '' ));
        if (( Item.Value.JSONType = jtObject ) and ( TJSONObject( Item.Value ).Get( 'Name', '' ) = AName )) then
          begin
            Result:= Item.KeyNum;
            break;
          end;
      end;
  end;

begin
  if ( APath = '' ) then
    Result:= ARoot
  else if ( APath[ 1 ] = '[' ) then
    begin
      p:= Pos( ']', APath );
      i:= -1;
      if ( p > 2 ) then
        if ( APath.Contains( '"' )) then
          i:= FindItemByName( Trim( Copy( APath, 2, p - 2 )).DeQuotedString( '"' ))
        else
          i:= StrToIntDef( Copy( APath, 2, p - 2 ), -1 );
      if ( i >= 0 ) and ( i < ARoot.Count) then
        begin
         e:= APath;
         System.Delete( e, 1, p );
         Result:= DoFindPath( ARoot.Items[ i ], e, ARestPath );
        end
      else
        begin
         Result:= nil;
         if ( p > 0 ) then
           ARestPath:= Copy( APath, 1, p )
         else
           ARestPath:= APath;
         end;
    end;
end;

function TP3DJSONLoader.DoFindPathObject(ARoot: TJSONObject; APath: String; out ARestPath: String): TJSONData;
Var
  n: TJSONStringType;
  l, p, p2: Integer;

begin
  if ( APath = '' ) then
    Exit( ARoot );
  n:= APath;
  l:= Length( n );
  p:= 1;

  while ( p < l ) and ( n[ p ] = '.' ) do
    Inc( p );

  p2:= p;

  while ( p2 <= l ) and ( not ( n[ p2 ] in [ '.', '[' ])) do
    Inc( p2 );
  n:= Copy( APath, p, p2-p );
  if ( n = '' ) then
    Result:= ARoot
  else
    begin
      Result:= ARoot.Find( n );
      if ( Result = nil ) then
        ARestPath:= n + Copy( APath, p2, l-p2 )
      else
        begin
          n:= Copy( APath, p2, l - p2 + 1 );
          Result:= DoFindPath( Result, n, ARestPath );
        end;
    end;
end;


constructor TP3DJSONLoader.Create(AFileName: String; AUserPointer: Pointer);
begin
  inherited Create( AFileName, AUserPointer );
  OpenFiles;
end;

function TP3DJSONLoader.FindPath(APath: String): TJSONData;
var
  RestPath: String;
begin
  Result:= DoFindPath( JSON, APath, RestPath );
end;

destructor TP3DJSONLoader.Destroy;
begin
  CloseFiles;
  inherited Destroy;
end;

procedure TP3DJSONLoader.ReadFile;
begin

end;

procedure TP3DJSONLoader.WriteFile;
begin

end;

procedure TP3DJSONLoader.Reload;
begin
  inherited Reload;
  ReadFile;
  // not implemented
end;

{$ENDIF}
