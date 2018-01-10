unit p3dpropaccess;

interface
  type
    //p3dpropaccess.inc
    //..........................................................................
    TP3DPropertyAccess = abstract class( TPersistent )
      protected
        procedure SetAsString( AValue: String ); virtual; abstract;
        function GetAsString: String; virtual; abstract;

      public
        procedure LoadFromJSONContext( AContext: TP3DJSONContext ); virtual; abstract;
        procedure Assign( AProp: TP3DPropertyAccess ); virtual;
        property AsString: String read GetAsString write SetAsString;
    end;

    TP3DPropertyAccessArray = abstract class( TP3DPropertyAccess )
      protected
        procedure SetCount( AValue: Integer ); virtual; abstract;
        function GetCount: Integer; virtual; abstract;
        procedure SetAsStrings( Index: Integer; AValue: String ); virtual; abstract;
        function GetAsStrings( Index: Integer ): String; virtual; abstract;

      public
        procedure LoadItemFromJSONContext( AContext: TP3DJSONContext ); virtual; abstract;

        property Count: Integer read GetCount write SetCount;
        property AsStrings[ Index: Integer ] read GetAsStrings write SetAsStrings;
    end;

    TP3DPropertyList = class ( specialize gP3DCustomObjectList < TP3DPropertyAccess >)
      public
        constructor Create( AContext: TP3DInterfacedPersistent );

        function GetPropByName( AName: String ): TP3DPropertyAccess;
        function GetPropByPath( APropPath: String; out RestString: String ): TP3DPropertyAccess;
        function GetPropByPath( APropPath: String; out RestString: String; LastFound: TP3DPropertyAccess ): TP3DPropertyAccess;

        property Context: TP3DStreamable read FContext write FContext;
    end;

    generic gP3DPropAccess < T > = ( TP3DPropertyAccess )
      protected
        procedure SetAsValue( AValue: T ); virtual;
        function GetAsValue: T; virtual;

      public
        constructor CreateGetSet( AName:String; AGetter: TGetter; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
        constructor CreateGetField( AName:String; AGetter: TGetter; AField: pItem; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
        procedure Assign( AProp: TP3DPropertyAccess ); override;
        constructor CreateFieldSet( AName:String; AField: pItem; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
        constructor CreateField( AName:String; AField: pItem; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );

        property AsValue: T read GetAsValue write SetAsValue;
    end;

    generic gP3DPropAccessArray < T > = ( TP3DPropertyAccessArray )
      type
        TGetter = function: TItem of object;
        TSetter = procedure ( AValue: TItem ) of object;
        pT = ^T;

      protected
        procedure SetAsValue( AValue: T ); virtual;
        function GetAsValue: T; virtual;

        FField: pItem;
        FGetter: TGetter;
        FSetter: TSetter;

      public
        constructor CreateGetSet( AName:String; AGetter: TGetter; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
        constructor CreateGetField( AName:String; AGetter: TGetter; AField: pItem; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
        procedure Assign( AProp: TP3DPropertyAccess ); override;
        constructor CreateFieldSet( AName:String; AField: pItem; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
        constructor CreateField( AName:String; AField: pItem; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );

        property AsValue: T read GetAsValue write SetAsValue;
    end;

    generic gP3DPropAccessCustomObject < T > = ( specialize gP3DPropertyAccess < T >)
      public
        function CreateObjectFromJSONContext( AContext: TP3DJSONContext ): T; virtual; abstract;
        procedure LoadFromJSONContext( AContext: TP3DJSONContext ); override;
    end;

    //..........................................................................


    //p3dstreamable.inc
    //..........................................................................
    TP3DStreamable = class ( TInterfacedPersistent )
      private
        FProperties: TP3DPropertyAccessList;

      public
        constructor Create; virtual;
        destructor Destroy; override;

        property Properties: TP3DPropertyAccessList read FProperties;
    end;

    generic gP3DPropAccessStreamable < T: TP3DStreamable > = ( specialize gP3DPropertyAccessCustomObject < T >)
      public
        function CreateObjectFromJSONContext( AContext: TP3DJSONContext ): T; override;
        function GetAsString: String; override;
    end;

    generic gP3DPropAccessStreamablePointer < T: TP3DStreamable > = ( specialize gP3DPropertyAccess < T >)

      public
        procedure LoadFromJSONContext( AContext: TP3DJSONContext ); override;
        function GetAsString: String; override;
        procedure SetAsString( AValue: String ); override;
    end;

    generic gP3DPropAccessStreamableList< TList, T: TP3DStreamable,
                                          TItemProp: TP3DPropertyAccess >
                                          = class( TP3DPropertyAccessArray )
      protected
        procedure SetCount( AValue: Integer ); override;
        function GetCount: Integer; override;
        procedure SetAsStrings( Index: Integer; AValue: String ); override;
        function GetAsStrings( Index: Integer ): String; override;
        procedure SetAsValues( Index: Integer; AValue: T );
        function GetAsValues( Index: Integer ): T;

      public
        constructor Create(
        property AsValues[ Index: Integer ]: T read GetAsValues write SetAsValues;
    end;
    //..........................................................................

    //p3djson.inc
    //..........................................................................

    TP3DJSONLoader = class ( TPersistent )
      function GetPropByPath( APath: String; const DoLoad: Boolean = False ): TP3DPropertyAccess;
    end;
    //..........................................................................


implementation

//p3dpropaccess.inc
//..........................................................................

procedure TP3DPropertyAccess.Assign( AProp: TP3DPropertyAccess );
begin
  if ( Assigned( AProp )) then
    AsString:= AProp.AsString;
end;

constructor TP3DPropertyList.Create( AContext: TP3DInterfacedPersistent );
function TP3DPropertyList.GetPropByName( AName: String ): TP3DPropertyAccess;
function TP3DPropertyList.GetPropByPath( APropPath: String; out RestString: String ): TP3DPropertyAccess;
function TP3DPropertyList.GetPropByPath( APropPath: String; out RestString: String; LastFound: TP3DPropertyAccess ): TP3DPropertyAccess;


procedure gP3DPropAccess.Assign( AProp: TP3DPropertyAccess ); override;
begin
  if ( Assigned( AProp )) then
    if ( AProp is T ) then
      AsValue:= T( AProp ).AsValue
    else
      inherited Assign( AProp );
end;

procedure gP3DPropAccess.SetAsValue( AValue: T );
function gP3DPropAccess.GetAsValue: T;



//..........................................................................

//p3dstreamable.inc
//..........................................................................

function gP3DPropAccessStreamable.CreateObjectFromJSONContext( AContext: TP3DJSONContext ): T;
begin
  Result:= T.Create();
  T.LoadFromJSONContext( AContext );
end;

procedure gP3DPropAccessStreamablePointer.LoadFromJSONContext( AContext: TP3DJSONContext );
var
  Prop: TP3DPropertyAccess;
  Path: String;
begin
  if ( AContext.Path = jtString ) then
    begin
      Path:= AContext.Path.AsString;
      Prop:= AContext.JSONLoader.GetPropByPath( Path, True );
      if ( Assigned( Prop )) then
        Assign( Prop )
      else
        raise Exception.CreateFmt(
          'Cannot load from StreamablePointer from JSON because the pointer "%s" was invalid!',
          [ Path ]);
    end
  else
    raise Exception.Create( 'Cannot load from StreamablePointer from JSON because the context was no pointer!' );
end;

generic gP3DPropAccessStreamableList< TList, T: TP3DStreamable,
                                      TItemProp: TP3DPropertyAccess >
                                      = class( TP3DPropertyAccessArray )
procedure gP3DPropAccessStreamableList.SetCount( AValue: Integer ); override;
begin

end;
function gP3DPropAccessStreamableList.GetCount: Integer; override;
procedure gP3DPropAccessStreamableList.SetAsStrings( Index: Integer; AValue: String ); override;
function gP3DPropAccessStreamableList.GetAsStrings( Index: Integer ): String; override;
procedure gP3DPropAccessStreamableList.SetAsValues( Index: Integer; AValue: T ); override;
function gP3DPropAccessStreamableList.GetAsValues( Index: Integer ): T; override;

  public
    property AsValues[ Index: Integer ]: T read GetAsValues write SetAsValues;
end;

//..........................................................................

//p3djson.inc
//..........................................................................

function TP3DJSONLoader.GetPropByPath( APath: String; const DoLoad: Boolean = False ): TP3DPropertyAccess;
begin

end;
//..........................................................................
