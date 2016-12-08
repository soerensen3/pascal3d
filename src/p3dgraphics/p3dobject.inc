//p3dobject.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}

  type
    TP3DObject = class;


    { TP3DObjectList }

    TP3DCustomObjectList = specialize gP3DCustomObjectList < TP3DObject >;
    TP3DObjectList = class( TP3DCustomObjectList )
      private
        FOwner: TPersistent;

      published
        constructor Create( AOwner: TPersistent );
        function FindUniqueName( BaseStr: String ): String;
        function FindByName( AName: String ): Integer;
        procedure Clear(const FreeObjects: Boolean = True ); override;
        function DumpObjectList: String;

        property Owner: TPersistent read FOwner write FOwner;
    end;


    { IP3DObject }

    IP3DObject = interface
      ['{713A3737-F83B-4AE6-8C04-E3A88FE26FC5}']
      function GetName: TComponentName;
      procedure SetName( NewName: TComponentName);

      property Name: TComponentName read GetName write SetName;
    end;

    TP3DObject = class( TP3DInterfacedPersistent, IP3DObject )
      private
        FName: TComponentName;
        FParentList: TP3DObjectList;

        function GetName: TComponentName;
        procedure SetName( NewName: TComponentName ); virtual;

      public
        constructor Create( const AParentList: TP3DObjectList = nil );
        destructor Destroy; override;

        property ParentList: TP3DObjectList read FParentList;

      published
        property Name: TComponentName read FName write SetName stored False;
    end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DObjectList }

constructor TP3DObjectList.Create(AOwner: TPersistent);
begin
  inherited Create();
  Owner:= AOwner;
end;

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

function TP3DObjectList.DumpObjectList: String;
var
  i: Integer;
begin
  Result:= '';

  for i:= 0 to Count - 1 do
    if ( i > 0 ) then
      Result+= ', ' + Items[ i ].Name
    else
      Result+= Items[ i ].Name;
end;

{ TP3DObject }

function TP3DObject.GetName: TComponentName;
begin
  Result:= FName;
end;

procedure TP3DObject.SetName(NewName: TComponentName);
begin
  if ( Name = NewName ) then
    exit;
  if ( ParentList.FindByName( NewName ) < 0 ) then
    FName:= NewName
  else
    raise Exception.Create( Format('Error in TP3DObject.SetName(''%s''). An object with that name already exists!', [NewName ]));
end;

constructor TP3DObject.Create(const AParentList: TP3DObjectList);
var
  BaseName: String;
begin
  inherited Create;
  if ( not Assigned( AParentList )) then
    FParentList:= P3DData.DataBlocks
  else
    FParentList:= AParentList;

  if ( ClassName[ 1 ] = 'T' ) then
    BaseName:= Copy( ClassName, 2, Length( ClassName ) - 1 )
  else
    BaseName:= ClassName;

  FName:= ParentList.FindUniqueName( BaseName );
  ParentList.Add( Self );

  Properties.Add( TP3DPropertyAccessString.CreateFieldSet( 'Name', @FName, @SetName ));
end;

destructor TP3DObject.Destroy;
begin
  ParentList.Remove( Self );
  inherited Destroy;
end;

{$ENDIF}
