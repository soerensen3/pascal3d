{ p3dfilepointer.inc
  License:
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

  Description:
    Introduces a file pointer class to check if loaded files have changed and then notify the user. A file pointer list is used to manage all opened files and
    automatically check for changes.
}

{$IFDEF INTERFACE}

type
  TP3DFilePointer = class;

  // callback is executed if the file on the disk is changed. If do reload is set to True in the callback the file is reloaded afterwards.
  TP3DFilePointerOnFileChanged = procedure ( Sender: TP3DFilePointer; out DoReload: Boolean ) of object;
  // callback is executed if the file name property is changed.
  TP3DFilePointerOnFileNameChanged = procedure ( Sender: TP3DFilePointer; ANewValue: String ) of object;

  { TP3DFilePointer

    A pointer to a file which may or may not exist. It is used internally to manage all open files. If Reload is executed the file
    status is updated. True is returned if the file changed. If assigned the file changed callback is run.
  }
  TP3DFilePointer = class ( TPersistent )
    private
      FLastFileAge: LongInt;
      FFileName: String;
      FOnFileChange: TP3DFilePointerOnFileChanged;
      FOnFileNameChange: TP3DFilePointerOnFileNameChanged;
      FUserPointer: Pointer;

      function GetFileAge: LongInt;
      procedure SetFileName(AValue: String);

    public
      constructor Create( AFileName: String; AUserPointer: Pointer = nil ); // Creates a file pointer with the given name. Optionally a user
                                                                            // may be stored with the file pointer. The instance of the file
                                                                            // pointer is automatically added to the P3DFilePointers var.
      destructor Destroy; override;

      function CheckForChange: Boolean; // Checks if a file on the disk has changed. If assigned the file changed callback is run on change.
                                        // If the file does not exist the return value is false. The file status is only updated by this function
                                        // if the DoReload parameter in the callback is set to true.
      function CheckFileExists: Boolean; // Checks if the target of the file pointer is valid.
      procedure Reload; virtual; // Updates the file's status.

      property UserPointer: Pointer read FUserPointer write FUserPointer; // A user pointer to store custom values.

    published
      property FileName: String read FFileName write SetFileName; // Full filename to a file that may or may not exist.
      property LastFileAge: LongInt read FLastFileAge; // The age of the file when it was changed. The returned value is a LongInt generated
                                                       // by SysUtil's FileAge function. If the file does not exist this value is -1.
      property OnFileChange: TP3DFilePointerOnFileChanged read FOnFileChange write FOnFileChange;
      // Executed by CheckForChange
      property OnFileNameChange: TP3DFilePointerOnFileNameChanged read FOnFileNameChange write FOnFileNameChange;
      // Is executed when the filename property is changed
  end;

  { TP3DFilePointerList

    Internally used to manage files opened by the engine. CheckForChange needs to be called in a regular interval if you want notifications to work. This
    is done automatically in the main loop in p3dapplication.inc for P3DFilePointers var.
  }

  TP3DFilePointerList = class( specialize gP3DCustomObjectList < TP3DFilePointer > )
    function AddWatch( AFileName: String; AUserPointer: Pointer = nil ): Integer;

    procedure CheckForChange;
  end;


var
  P3DFilePointers: TP3DFilePointerList = nil; // Global list containing the file pointers of all files opened by the engine. Instances of
                                              // TP3DFilePointer will automatically register themselves here on creation.
{$ENDIF}

{$IFDEF IMPLEMENTATION}


{ TP3DFilePointerList }

function TP3DFilePointerList.AddWatch( AFileName: String; AUserPointer: Pointer ): Integer;
begin
  Result:= Add( TP3DFilePointer.Create( AFileName, AUserPointer ));
end;

procedure TP3DFilePointerList.CheckForChange;
var
  Watch: TP3DFilePointer;
  Reload: Boolean;
begin
  for Watch in Self do
    Watch.CheckForChange;
end;


{ TP3DFilePointer }

function TP3DFilePointer.GetFileAge: LongInt;
begin
  Result:= FileAge( FFileName );
end;

procedure TP3DFilePointer.SetFileName(AValue: String);
var
  FN, FName: String;
begin
  if ( FFileName = AValue ) then
    Exit;

   FN:= AValue;
   if ( FN > '' ) then
    begin
      FName:= P3DSearchPaths.FindFileEx( AValue, GetCurrentDir ); //ExpandFileName( AValue );
      if ( FName > '' ) then
      {if ( SysUtils.FileExists( FName )) then}
        FN:= FName;
    end;
  if ( Assigned( OnFileNameChange )) then
    OnFileNameChange( Self, FN );
  FFileName:= FN;
  Reload;
end;

constructor TP3DFilePointer.Create(AFileName: String; AUserPointer: Pointer);
begin
  inherited Create;
  FFileName:= AFileName;
  UserPointer:= AUserPointer;
  Reload;

  if ( Assigned( P3DFilePointers )) then
    P3DFilePointers.Add( Self );

  //Add Self to FileWatches
  //TP3DData has Filewatch
  //TP3DData has also a LibName which should be unique in Scope
  //Scope means the engine's searchpaths
  //TP3DDatablock has only a string to the corresponding object, <LibName> or similar will reference a different library
  //Descendants, like textures or meshes, can define their own filewatch if needed
end;

destructor TP3DFilePointer.Destroy;
begin
  if ( Assigned( P3DFilePointers )) then
    P3DFilePointers.Remove( Self );
  inherited Destroy;
end;

function TP3DFilePointer.CheckForChange: Boolean;
var
  DoReload: Boolean;
begin
  Result:= ( GetFileAge <> FLastFileAge );

  if (( Result ) and Assigned( OnFileChange )) then
    begin
      OnFileChange( Self, DoReload );
      if ( DoReload ) then // only reload if the call back asks for it
        Reload;
    end;
end;

function TP3DFilePointer.CheckFileExists: Boolean;
begin
  Result:= SysUtils.FileExists( FileName );
end;

procedure TP3DFilePointer.Reload;
begin
  FLastFileAge:= GetFileAge;
end;


{ TP3DPropertyAccessFilePointer }

constructor TP3DPropertyAccessFilePointer.CreateField(AName: String; AField: pItem; const AStoreMode: TP3DPropertyAccessStoreMode; APathMode: TP3DPathMode);
begin
  inherited CreateField( AName, AField, AStoreMode );
  PathMode:= APathMode;
end;

constructor TP3DPropertyAccessFilePointer.CreateFieldSet(AName: String; AField: pItem; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode; APathMode: TP3DPathMode);
begin
  inherited CreateFieldSet( AName, AField, ASetter, AStoreMode );
  PathMode:= APathMode;
end;

constructor TP3DPropertyAccessFilePointer.CreateGetField(AName: String; AGetter: TGetter; AField: pItem; const AStoreMode: TP3DPropertyAccessStoreMode; APathMode: TP3DPathMode);
begin
  inherited CreateGetField( AName, AGetter, AField, AStoreMode );
  PathMode:= APathMode;
end;

constructor TP3DPropertyAccessFilePointer.CreateGetSet(AName: String; AGetter: TGetter; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode; APathMode: TP3DPathMode);
begin
  inherited CreateGetSet( AName, AGetter, ASetter, AStoreMode );
  PathMode:= APathMode;
end;

procedure TP3DPropertyAccessFilePointer.LoadFromJSONContext(AContext: TP3DJSONContext);
var
  Path, FN: String;
  Val: TP3DFilePointer;
begin
  if ( AContext.Data.JSONType = jtString ) then
    begin
      {Path:= P3DSearchPaths.FindFileEx( AContext.Data.AsString, GetCurrentDir );
      if ( Path > '' ) then
        FN:= Path
      else}
        FN:= AContext.Data.AsString;
      SetAsString( FN );
    end
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ JSONTypeName( jtString ), JSONTypeName( AContext.Data.JSONType )]);
end;

function TP3DPropertyAccessFilePointer.Serialize: String;
begin
  Result:= '"' + AsString + '"';
end;

function TP3DPropertyAccessFilePointer.GetAsString: String;
begin
  if ( Assigned( Value )) then
    case PathMode of
      pmAbsolute: Result:= Value.FileName;
      pmRelativeToBinary: Result:= ExtractRelativepath( ExtractFilePath( ParamStr( 0 )), Value.FileName );
      pmRelativeToBaseBath: Result:= ExtractRelativepath( P3DSearchPaths.BaseDir.FileName, Value.FileName );
      pmRelativeToCurrentPath: Result:= ExtractRelativepath( AppendPathDelim( GetCurrentDir()), Value.FileName );
    end;
end;

procedure TP3DPropertyAccessFilePointer.SetAsString(AValue: String);
var
  FN: String;
begin
  if ( FilenameIsAbsolute( AValue )) then
    FN:= AValue
  else
    case FPathMode of
      pmAbsolute: FN:= CreateAbsolutePath( AValue, ExtractFilePath( Value.FileName ));
      pmRelativeToBaseBath: FN:= CreateAbsolutePath( AValue, P3DSearchPaths.BaseDir.FileName );
      pmRelativeToBinary: FN:= CreateAbsolutePath( AValue, ExtractFilePath( ParamStr( 0 )));
      pmRelativeToCurrentPath: FN:= CreateAbsolutePath( AValue, GetCurrentDir );
    end;
  WriteLn( Name, ': ', FN );
  if ( not Assigned( Value )) then
    Value:= TP3DFilePointer.Create( FN )
  else
    Value.FileName:= FN;
end;

function TP3DPropertyAccessFilePointer.GetDefaultValue: TP3DFilePointer;
begin
  Result:= nil;
end;
{$ENDIF}

{$IFDEF INITIALIZATION}
  if ( not Assigned( P3DFilePointers )) then
    P3DFilePointers:= TP3DFilePointerList.Create;
  if ( not Assigned( P3DSearchPaths )) then
    P3DSearchPaths:= TP3DSearchPathContainer.Create;
{$ENDIF}

{$IFDEF FINALIZATION}
  if ( Assigned( P3DFilePointers )) then
    FreeAndNil( P3DFilePointers );
  if ( Assigned( P3DSearchPaths )) then
    FreeAndNil( P3DSearchPaths );
{$ENDIF}

