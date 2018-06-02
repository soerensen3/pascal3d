{$IFDEF INTERFACE}
    { TP3DJSONRoot }

    TP3DJSONRoot = class ( TP3DStreamable, IP3DJSONStream )
      protected
        FJSONLoader: TP3DJSONLoader;

        function GetJSONLoader: TP3DJSONLoader;
        function GetClassFactory: TP3DClassFactory;

      public
        constructor Create( const AOwner: IP3DStreamableContainer = nil ); override;
        destructor Destroy; override;

        property JSONLoader: TP3DJSONLoader read FJSONLoader;
    end;

    { TP3DJSONRootNamed }

    TP3DJSONRootNamed = class ( TP3DNamedStreamable, IP3DJSONStream )
      protected
        FJSONLoader: TP3DJSONLoader;

        function GetJSONLoader: TP3DJSONLoader;
        function GetClassFactory: TP3DClassFactory;

      public
        constructor Create( const AOwner: IP3DStreamableContainer = nil ); override;
        destructor Destroy; override;

        property JSONLoader: TP3DJSONLoader read FJSONLoader;
    end;

    { TP3DJSONRootContainerList }

    TP3DJSONRootContainerList = class( TP3DStreamableContainerList, IP3DJSONStream )
      protected
        function GetJSONLoader(): TP3DJSONLoader;
        function GetClassFactory: TP3DClassFactory;

      public
        constructor Create(AName: String; const AStoreMode: TP3DPropertyAccessStoreMode=smDoNotStore); override;
    end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DJSONRootNamed }

function TP3DJSONRootNamed.GetJSONLoader: TP3DJSONLoader;
begin
  Result:= FJSONLoader;
end;

function TP3DJSONRootNamed.GetClassFactory: TP3DClassFactory;
begin
  Result:= P3DClassFactory;
end;

constructor TP3DJSONRootNamed.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FJSONLoader:= TP3DJSONLoader.Create( '' );
  FJSONLoader.Root:= Self;
end;

destructor TP3DJSONRootNamed.Destroy;
begin
  FreeAndNil( FJSONLoader.FJSON );
  FreeAndNil( FJSONLoader );
  inherited Destroy;
end;

{ TP3DJSONRootContainerList }

function TP3DJSONRootContainerList.GetJSONLoader(): TP3DJSONLoader;
begin
  Result:= nil;
end;

function TP3DJSONRootContainerList.GetClassFactory: TP3DClassFactory;
begin
  Result:= P3DClassFactory;
end;

constructor TP3DJSONRootContainerList.Create(AName: String;
  const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  FRoot:= Self;
end;



{ TP3DJSONRoot }

function TP3DJSONRoot.GetJSONLoader: TP3DJSONLoader;
begin
  Result:= FJSONLoader;
end;

function TP3DJSONRoot.GetClassFactory: TP3DClassFactory;
begin
  Result:= P3DClassFactory;
end;

constructor TP3DJSONRoot.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FJSONLoader:= TP3DJSONLoader.Create( '' );
  FJSONLoader.Root:= Self;
end;

destructor TP3DJSONRoot.Destroy;
begin
  FreeAndNil( FJSONLoader.FJSON );
  FreeAndNil( FJSONLoader );
  inherited Destroy;
end;


{$ENDIF}
