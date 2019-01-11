{ p3d.core.rendercommand.inc }

{$IFDEF INTERFACE}
type
  TP3DPlotCommandSetting = class;
  TP3DPlotCommandDefault = class;

  { TP3DPlotCommandContext }

  TP3DPlotCommandContext = class
    LayerDefault: TP3DPlotCommandDefault;
    NumTextures: Integer;
    NumPointLights: Integer;
    NumDirectionalLights: Integer;
    Shader: TP3DShader;
  end;

  TP3DPlotCommand = class ( TPersistent )
    private
      RefCount: Integer;
      FCategory: String;

      procedure AddRef;
      procedure Release;

    public
      constructor Create;
      destructor Destroy; override;
      procedure Execute( AContext: TP3DPlotCommandContext ); virtual; abstract;
      function Debug: String; virtual abstract;
      property Category: String read FCategory write FCategory;
  end;

  { gP3DPlotList }
  {
  generic gP3DPlotList < T: TP3DPlotCommand > = record
    private type
    //  TAutoType = specialize TAuto < T >;
    //  TAutoTypeArray = array of TAutoType;
      TPlotList = specialize gP3DPlotList < T >;

    private
      FItems: array of T;//TAutoTypeArray;
      //RefCount: PLongint;
      function GetCount: Integer;
      function GetItems( AIndex: Integer ): T;

    public
      function Debug: String;
      procedure Execute;
      procedure Execute( AContext: TP3DPlotCommandContext );
      procedure SmartFinalize();
      procedure Append( Item: T );
      procedure Append( List: TPlotList );

      class operator Initialize(var aRec: specialize gP3DPlotList < T > );
      class operator Finalize(var aRec: specialize gP3DPlotList < T > );
      class operator AddRef(var aRec: specialize gP3DPlotList < T > );
      class operator Copy(constref aSource: specialize gP3DPlotList < T >; var aDest: specialize gP3DPlotList < T > );
      //procedure Assign(const aValue: T );
      //class operator +( const a, b: TPlotList ): TPlotList;

      property Count: Integer read GetCount;
      property Items[ AIndex: Integer ]: T read GetItems;
  end; }

  { gP3DPlotList }

  generic gP3DPlotList < T: TP3DPlotCommand > = class
    private type
      TPlotList = specialize gP3DPlotList < T >;

    private
      FItems: array of T;
      FOwnedByUser: Boolean;
      function GetCount: Integer;
      function GetItems( AIndex: Integer ): T;
      {%H-}constructor CreateTemporary;

    public
      constructor Create( const AOwnedByUser: Boolean = True );
      destructor Destroy; override;
      function CheckDestroy: Boolean;

      function Debug: String;
      procedure Execute;
      procedure Execute( AContext: TP3DPlotCommandContext );
      procedure Append( Item: T ); virtual;
      procedure AppendList( List: TPlotList );
      function FindLast( Start: Integer; Category: String ): Integer;
      function FindNext( Start: Integer; Category: String ): Integer;
      procedure Clear; virtual;

      property Count: Integer read GetCount;
      property Items[ AIndex: Integer ]: T read GetItems;
      property OwnedByUser: Boolean read FOwnedByUser write FOwnedByUser;
  end;

  { TP3DPlotCommandSetting }

  TP3DPlotListSetting = class;

  TP3DPlotCommandSetting = class ( TP3DPlotCommand )
    public
      function Debug: String; override;
      procedure GetExtensions( Extension: TStringList ); virtual;
      procedure MakeShaderModifications( ASender: TP3DPlotListSetting; ASenderIdx: Integer; ABuilder: TP3DShaderBuilder ); virtual;

      function Clone: TP3DPlotCommandSetting; virtual; abstract;
  end;

  { TP3DPlotCommandSettingShader }

  TP3DPlotCommandSettingShader = class ( TP3DPlotCommandSetting )
    procedure BuildShader( AOwner: TP3DStreamableContainer ); virtual; abstract;
  end;

  { TP3DPlotCommandSettingNode }

  { IP3DPlotCommandSettingNode }

  IP3DPlotCommandSettingNode = interface
    ['{CCFBAF96-E769-4B2C-B306-37B615C9A7B6}']

    function GetShaderNode: TP3DNodeClone;
    property ShaderNode: TP3DNodeClone read GetShaderNode;
  end;

  { TP3DPlotListSetting }

  TP3DPlotListSetting = class( specialize gP3DPlotList < TP3DPlotCommandSetting >)
    private
      FBuildShader: TP3DPlotCommandSettingShader;
      FShader: TP3DStreamableContainer;

      function GetShader: TP3DShader;

    public
      constructor Create(const AOwnedByUser: Boolean=True);
      destructor Destroy; override;

      procedure Append(Item: TP3DPlotCommandSetting); override;
      procedure Clear; override;
      procedure UpdateShader;
      procedure Execute(AContext: TP3DPlotCommandContext);

      property BuildShader: TP3DPlotCommandSettingShader read FBuildShader;
      property Shader: TP3DShader read GetShader;
  end;


  { TP3DPlotCommandLayer }
  TP3DPlotCommandLayer = class ( TP3DPlotCommand )
    private
      FShaderSettings: TP3DPlotListSetting;

      procedure SetShaderSettings(AValue: TP3DPlotListSetting);

    public
      destructor Destroy; override;

      procedure Execute( AContext: TP3DPlotCommandContext ); override;
      function Clone: TP3DPlotCommandLayer; virtual; abstract;
      function Debug: String; override;

      property ShaderSettings: TP3DPlotListSetting read FShaderSettings write SetShaderSettings;
  end;

  { TP3DPlotListLayer }

  TP3DPlotListLayer = specialize gP3DPlotList < TP3DPlotCommandLayer >;

  IP3DPlottableLayer = interface
    ['{C0045712-CFEE-4AF8-8198-66E71AEC555C}']

    function GetLayer( ASettings: TP3DPlotListSetting ): TP3DPlotCommandLayer;
  end;

  IP3DPlottableSettings = interface
    ['{9B61C114-C67F-423F-A723-C45E3EBF2BC0}']

    function GetSettings: TP3DPlotListSetting;
  end;

  { TP3DPlotCommandDefault }

  TP3DPlotCommandDefault = class ( TP3DPlotCommandLayer )
    public
      constructor Create( AShaderSettings: TP3DPlotListSetting );

      procedure Execute(AContext: TP3DPlotCommandContext); override;
      function Clone: TP3DPlotCommandLayer; override;
  end;

  //operator := ( a: TP3DPlotCommandSetting ): TP3DPlotListSetting;
  //operator := ( a: TP3DPlotCommandLayer ): TP3DPlotListLayer;

  var
    GlobalPlotCount: Integer = 0;
    GlobalPlotListCount: Integer = 0;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ gP3DPlotList }

function gP3DPlotList.GetCount: Integer;
begin
  Result:= Length( FItems );
end;

function gP3DPlotList.GetItems( AIndex: Integer ): T;
begin
  if ( InRange( AIndex, 0, high( FItems ))) then
    Result:= FItems[ AIndex ]
  else
    Result:= default( T );
end;

constructor gP3DPlotList.CreateTemporary;
begin
  Create;
  OwnedByUser:= False;
end;

constructor gP3DPlotList.Create( const AOwnedByUser: Boolean = True );
begin
  Inc( GlobalPlotListCount );
  FItems := nil;
  OwnedByUser:= AOwnedByUser;
  WriteLn( 'Create PlotList: ', ClassName );
end;

destructor gP3DPlotList.Destroy;
var
  i: Integer;
begin
  Dec( GlobalPlotListCount );
  WriteLn( 'Destroying PlotList: ', ClassName );
  Clear;

  inherited Destroy;
end;

function gP3DPlotList.CheckDestroy: Boolean;
begin
  if ( not Assigned( Self )) then
    exit;
  Result:= not OwnedByUser;
  if ( Result ) then
    Free;
end;

function gP3DPlotList.Debug: String;
var
  i: Integer;
begin
  Result:= '';
  if ( not Assigned( Self )) then
    exit;
  for i:= 0 to Count - 1 do begin
    Result+= FItems[ i ].Debug + LineEnding;
  end;
end;

procedure gP3DPlotList.Execute;
var
  Context: TP3DPlotCommandContext;
begin
  Context:= TP3DPlotCommandContext.Create;
  try
    Execute( Context );
  finally
    FreeAndNil( Context );
  end;
end;

procedure gP3DPlotList.Execute(AContext: TP3DPlotCommandContext);
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do begin
    FItems[ i ].Execute( AContext );
  end;
end;

{procedure gP3DPlotList.SmartFinalize();
var
  i: Integer;
begin
  WriteLn( 'procedure gP3DPlotList.SmartFinalize();' );
  //SetLength( FItems, 0 );
  for i:= low( FItems ) to high( FItems ) do
    FItems[ i ].Release;
  SetLength( FItems, 0 );
  {if RefCount <> nil then
    if InterLockedDecrement(RefCount^)=0 then begin
      FreeMem(RefCount);
      //if ( Assigned( FFirst )) then
      //  WriteLn( 'Freeing FFirst: ', FFirst.ClassName );
      SetLength( FItems, 0 );
    end;}
end;
}
procedure gP3DPlotList.Append(Item: T);
var
  L: Integer;
begin
  if ( not Assigned( Item )) then
    exit;
  L:= Length( FItems );
  SetLength( FItems, L + 1 );
  FItems[ L ]:= Item;
  Item.AddRef;
end;

procedure gP3DPlotList.AppendList(List: TPlotList);
var
  l: Integer;
begin
  if ( Assigned( List )) then
    for l:= 0 to high( List.FItems ) do
      Append( List.FItems[ l ]);
end;

function gP3DPlotList.FindLast(Start: Integer; Category: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= Start downto 0 do
    if ( Items[ i ].Category = Category ) then
      begin
        Result:= i;
        break;
      end;
end;

function gP3DPlotList.FindNext(Start: Integer; Category: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= Start to Count - 1 do
    if ( Items[ i ].Category = Category ) then
      begin
        Result:= i;
        break;
      end;
end;

procedure gP3DPlotList.Clear;
var
  i: Integer;
begin
  for i:= low( FItems ) to high( FItems ) do
    FItems[ i ].Release;
  SetLength( FItems, 0 );
end;




{procedure gP3DPlotList.Assign(const aValue: T );
begin
  if RefCount <> nil then
    SmartFinalize();

  GetMem(RefCount, SizeOf(LongInt));
  RefCount^ := 0;

  InterLockedIncrement(RefCount^);
  SetLength( FItems, 1 );
  FItems[ 0 ].I:= aValue;
end;}

{class operator gP3DPlotList.+( const a, b: TPlotList ): TPlotList;
var
  l: Integer;
begin
  {if ( Length( a.FItems ) = 0 ) then
    Result:= b
  else if ( Length( b.FItems ) = 0 ) then
    Result:= a
  else begin}
  WriteLn( 'class operator gP3DPlotList<', T.ClassName, '>.+(a, b: specialize gP3DPlotList<',T.ClassName,'>): specialize gP3DPlotList <',T.ClassName,'>;' );
    initialize( Result );//default( TPlotList );  //initialize Result
    for l:= 0 to high( a.FItems ) do
      Result.Append( a.FItems[ l ]);
    for l:= 0 to high( b.FItems ) do
      Result.Append( b.FItems[ l ]);
    //WriteLn( 'a = ', a.Debug );
    //WriteLn( 'b = ', b.Debug );
    //WriteLn( 'Result = ', Result.Debug );
    //l:= Length( a.FItems ) + Length( b.FItems );
    //SetLength( Result.FItems, l );
    //Move( a.FItems[ 0 ], Result.FItems[ 0 ], Length( a.FItems ) * SizeOf( TAutoType ));
    //Move( b.FItems[ 0 ], Result.FItems[ Length( a.FItems )], Length( b.FItems ) * SizeOf( TAutoType ));
  //end;
end;
}

{ TP3DPlotCommandSetting }

function TP3DPlotCommandSetting.Debug: String;
begin
  Result:= '  ' + ClassName;
end;

procedure TP3DPlotCommandSetting.GetExtensions(Extension: TStringList);
begin

end;

procedure TP3DPlotCommandSetting.MakeShaderModifications(ASender: TP3DPlotListSetting; ASenderIdx: Integer; ABuilder: TP3DShaderBuilder);
begin

end;

{ TP3DPlotCommandLayer }

procedure TP3DPlotCommandLayer.SetShaderSettings(AValue: TP3DPlotListSetting);
begin
  FShaderSettings:=AValue;
end;

destructor TP3DPlotCommandLayer.Destroy;
begin
  FShaderSettings.CheckDestroy;
  inherited Destroy;
end;

procedure TP3DPlotCommandLayer.Execute(AContext: TP3DPlotCommandContext);
begin
  AContext.NumDirectionalLights:= 0;
  AContext.NumPointLights:= 0;
  AContext.NumTextures:= 0;
  ShaderSettings.Execute( AContext );
end;

function TP3DPlotCommandLayer.Debug: String;
begin
  Result:= ClassName + LineEnding + ShaderSettings.Debug;
end;

{ TP3DPlotCommandDefault }

constructor TP3DPlotCommandDefault.Create(AShaderSettings: TP3DPlotListSetting);
begin
  inherited Create;
  ShaderSettings:= AShaderSettings;
end;

procedure TP3DPlotCommandDefault.Execute(AContext: TP3DPlotCommandContext);
begin
  AContext.LayerDefault:= nil;
  inherited Execute(AContext);
  AContext.LayerDefault:= Self;
end;

function TP3DPlotCommandDefault.Clone: TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandDefault.Create( ShaderSettings );
end;

{ TP3DPlotCommand }

procedure TP3DPlotCommand.AddRef;
begin
  Inc( RefCount );
  WriteLn( ClassName + '.AddRef: ', RefCount );
end;

procedure TP3DPlotCommand.Release;
begin
  Dec( RefCount );
  WriteLn( ClassName + '.Release: ', RefCount );
  if ( RefCount = 0 ) then
    Free;
end;

constructor TP3DPlotCommand.Create;
begin
  Inc( GlobalPlotCount );
  inherited Create;
  RefCount:= 0;
end;

destructor TP3DPlotCommand.Destroy;
begin
  Dec( GlobalPlotCount );
  inherited Destroy;
end;

{$ENDIF}

