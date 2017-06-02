//p3ddatablock.inc

{$IFDEF INTERFACE}

type

  { TP3DDataBlock }

  TP3DRenderList = class;
  TP3DDataBlockList = class;
  TP3DAction = class;
  TP3DDataBlock = class ( TP3DObject )
    private
      FAABB: TP3DAABB;
      FAction: TP3DAction;
      FOwner: TP3DLibrary;
      FSphere: TP3DSphere;
      FUsers: TP3DDataBlockList;

      function GetDOMPath: String; virtual;
      procedure SetAction(AValue: TP3DAction);
      procedure SetName(NewName: TComponentName); override;

    public
      constructor Create( const AData: TP3DLibrary = nil ); virtual;
      constructor CreateFromDOM( ADOMNode: TDOMElement; const AData: TP3DLibrary = nil ); virtual;

      destructor Destroy; override;
      procedure FastDestroy; virtual;

      procedure Render( world: TMat4; Scene: TP3DScene; RenderObject: TP3DActor ); virtual;
      procedure LoadFromDOM( ADOMNode: TDOMElement ); virtual;
      procedure LoadFromFile( FileName: String );
      procedure LoadFromBinary( FileName: String ); virtual;
      procedure Reload;
      function CalcAABB(): TP3DAABB; virtual;
      function CalcSphere(): TP3DSphere; virtual;
      procedure RemoveFromFields( Block: TP3DDataBlock ); virtual;
      function CastRay( Orig, Dir: TVec3; Actor: TP3DActor; out HitPos: TVec3 ): Boolean; virtual;
      procedure RemoveUser( User: TP3DDataBlock );
      procedure AddUser( User: TP3DDataBlock );
      procedure ClearFields; virtual;
      procedure ClearUsers;
      class function DOMNodeName: String; override;
      procedure AddToRenderList( ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags );  virtual;

      function CollideSphere( Mat: TMat4; ASphere: TP3DSphere; out Intersect: TVec3; out N: TVec3 ): Boolean; virtual;
      function CollideAABB( Mat: TMat4; AAABB: TP3DAABB; out Intersect: TVec3; out N: TVec3 ): Boolean; virtual;
      function CollidePoint( Mat: TMat4; APoint: TVec3 ): Boolean; virtual;

      property AABB: TP3DAABB read FAABB write FAABB;
      property Sphere: TP3DSphere read FSphere write FSphere;
      property Owner: TP3DLibrary read FOwner;
      property Action: TP3DAction read FAction write SetAction;

    published
      property DOMPath: String read GetDOMPath;
      property Users: TP3DDataBlockList read FUsers;
  end;

  TP3DDataBlockType = class of TP3DDataBlock;


  TP3DCustomListOnChangeAction = ( actAdd, actDelete, actClear );

  { gP3DCustomDatablockList }

  generic gP3DCustomDatablockList <TCustomItem: TP3DDataBlock> = class ( TPersistent )
    type
      TP3DCustomObjectListEnumerator = specialize gP3DCustomListEnumeratorClass < TCustomItem >;
      TP3DCustomListOnChangeEvent = procedure ( Sender: {specialize gP3DCustomDatablockList < TCustomItem >}TPersistent; Item: TCustomItem; Action: TP3DCustomListOnChangeAction ) of object;

    private
      FFreeChildrenOnDestroy: Boolean;
      FItems: TList;
      FLastUpdate: Integer;
      FOnChange: TP3DCustomListOnChangeEvent;

      function GetCount: Integer;
      procedure SetCount( AValue: Integer );
      function GetItems( Index: Integer ): TCustomItem;
      function MoveNext( var Index: Integer ): TCustomItem;
      function MoveBack( var Index: Integer ): TCustomItem;
      procedure SetItems( Index: Integer ; AValue: TCustomItem);
      procedure DoUpdate;

    public
      constructor Create;
      destructor Destroy; override;

      function Add( Item: TCustomItem ): Integer; virtual;
      procedure Delete( Index: Integer ); virtual;
      procedure Remove( Item: TCustomItem ); virtual;
      function IndexOf( Item: TCustomItem ): Integer; virtual;
      procedure Clear( const FreeObjects: Boolean = False ); virtual;
      function GetEnumerator(): TP3DCustomObjectListEnumerator;
      function Reversed(): TP3DCustomObjectListEnumerator;
      procedure Insert( Index: Integer; Item: TCustomItem );
      function FindUniqueName( BaseStr: String ): String;
      function FindByName( AName: String ): Integer;
      function GetByName( AName: String ): TCustomItem;
      function FindByFileName( AFileName: String ): Integer;
      procedure SaveToDOM(AParent: TDOMElement);
      //TODO: Implement load from DOM

      property Items[ Index: Integer ]: TCustomItem read GetItems write SetItems; default;

    published
      property Count: Integer read GetCount write SetCount;
      property OnChange: TP3DCustomListOnChangeEvent read FOnChange write FOnChange;
      property FreeChildrenOnDestroy: Boolean read FFreeChildrenOnDestroy write FFreeChildrenOnDestroy;
      property LastUpdate: Integer read FLastUpdate;
  end;

  TP3DCustomDataBlockList = specialize gP3DCustomDatablockList <TP3DDataBlock>;

  { TP3DDataBlockList }

  TP3DDataBlockList = class ( TP3DCustomDataBlockList )
    public
      function DumpUsers: String;

  end;

  { gP3DPropertyAccessDataBlock }

  generic gP3DPropertyAccessDataBlock < TItem: TP3DDataBlock > = class( specialize gP3DPropertyAccessInterfacedPersistent < TItem >)
    protected
      function GetAsString: String; override;
      procedure SetAsString( AValue: String ); override;
      function CheckSameContext: Boolean;
      function GetDefaultValue: TItem; override;
      function GetChildPropByPath(APath: String): TP3DPropertyAccess; override;

    public
      function SaveToDOM( AParentNode: TDOMElement ): Boolean; override;
      procedure LoadFromDOM(AParentNode: TDOMElement); override;

  end;

  TP3DPropertyAccessDataBlock = specialize gP3DPropertyAccessDataBlock < TP3DDataBlock >;

  { TP3DPropertyAccessDataBlockList }

  TP3DPropertyAccessDataBlockList = specialize gP3DListPropertyAccessObject < TP3DDataBlockList, TP3DDataBlock, TP3DPropertyAccessDataBlock >;
  {TP3DPropertyAccessDataBlockList = class( specialize gP3DPropertyAccess < TP3DDataBlockList >)
    private
      function GetCount: Integer;
      function GetValues( idx: Integer ): TP3DDataBlock;
      procedure SetCount(AValue: Integer);
      procedure SetValues( idx: Integer ; AValue: TP3DDataBlock);

    public
      property Values[ idx: Integer ]: TP3DDataBlock read GetValues write SetValues;

    published
      function GetAsString: String; override;
      procedure SetAsString( AValue: String ); override;
      property Count: Integer read GetCount write SetCount;
  end;}

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DPropertyAccessDataBlockList }
{
function TP3DPropertyAccessDataBlockList.GetCount: Integer;
begin
  Result:= Value.Count;
end;

function TP3DPropertyAccessDataBlockList.GetValues( idx: Integer ): TP3DDataBlock;
begin
  Result:= Value[ idx ];
end;

procedure TP3DPropertyAccessDataBlockList.SetCount(AValue: Integer);
begin
  Value.Count:= AValue;
end;

procedure TP3DPropertyAccessDataBlockList.SetValues( idx: Integer ; AValue: TP3DDataBlock);
begin
  Value[ idx ]:= AValue;
end;

function TP3DPropertyAccessDataBlockList.GetAsString: String;
begin

end;

procedure TP3DPropertyAccessDataBlockList.SetAsString(AValue: String);
begin

end;
}
{ TP3DPropertyAccessDataBlock }

function gP3DPropertyAccessDataBlock.GetAsString: String;
begin
  if ( not Assigned( Value )) then
    begin
      Result:= '<None:' + TItem.ClassName + '>';
      exit;
    end;
  Result:= Value.DOMPath; //Value.Owner.Name + '.' + Value.Name;
  if ( Result = '' ) then
    begin
      WriteLn( 'Error: DOMPath is empty for Value ' + Value.Owner.Name + '.' + Value.Name );
      exit;
    end;
  if ( Result[ 1 ] <> '/' ) then
    Result:= '/' + Result;
  if ( not (( Context is TP3DDataBlock ) and ( TP3DDataBlock( Context ).Owner = Value.Owner ))) then
    Result:= Value.Owner.FileWatch.FileName + '$' + Result;
end;

procedure gP3DPropertyAccessDataBlock.SetAsString(AValue: String);
var
  n: Integer;
  Owner: TP3DLibrary;
  Path: TP3DXMLPath;
  FN: String;
begin
  if (( Context is TP3DDataBlock ) and ( AValue > '' )) then
    if (( AValue = '<None>' ) or ( AValue = '<None:' + TItem.ClassName + '>' )) then
      Value:= nil
    else
      begin
        Owner:= TP3DDataBlock( Context ).Owner.GetOwnerFromPath( AValue, True );
        if ( not Assigned( Owner )) then
          raise Exception.Create( 'Could not find resource: ' + AValue );
        Path:= P3DXMLExtractRelativePath( AValue, FN );
        n:= Owner.AppendFromPath( Path{, TItem} ); // No need to check if already loaded, this is done in appendfrompath
        Value:= TItem( Owner.DataBlocks[ n ]);
      end;
end;

function gP3DPropertyAccessDataBlock.CheckSameContext: Boolean;
begin
  Result:= Assigned( Value ) and ( Context is TP3DDataBlock ) and ( TP3DDataBlock( Context ).Owner = Value.Owner );
end;

function gP3DPropertyAccessDataBlock.GetDefaultValue: TItem;
begin
  Result:= nil;
end;

function gP3DPropertyAccessDataBlock.GetChildPropByPath(APath: String
  ): TP3DPropertyAccess;
begin
  if ( Assigned( Value )) then
    Result:= Value.Properties.GetPropByPath( APath );
end;

function gP3DPropertyAccessDataBlock.SaveToDOM( AParentNode: TDOMElement ): Boolean;
var
  root: TDOMElement;
  Path: String;
begin
  Result:= inherited SaveToDOM( AParentNode );
  if ( Result and CheckSameContext ) then
    begin
      root:= AParentNode.OwnerDocument.DocumentElement;
      Path:= Value.DOMPath;
      if ( Path = '' ) then
        begin
          WriteLn( 'Error: empty DOMPath for ' + Value.Owner.Name + '.' + Value.Name );
          exit;
        end;
      if ( not Assigned( P3DXMLFindPath( root, Path ))) then
        if ( Assigned( Value )) then
          Value.SaveToDOM( root );
    end;
end;

procedure gP3DPropertyAccessDataBlock.LoadFromDOM(AParentNode: TDOMElement);
begin
  inherited LoadFromDOM(AParentNode);
end;

procedure StringSplit( S: String; Delim: Char; var S1, S2: String );
begin
  S1:= ExtractWord( 1, S, [ Delim ]);
  S2:= Copy( S, Length( S1 ) + Length( Delim ) + 1, Length( S ));
end;

{
function FindDOMByIdentifier( Identifier: String; RootNode: TDOMElement ): TDOMElement;
var
  Node: TDOMNode;
  RestStr, Name: String;

  function FindNode: TDOMElement;
  var
    Element: TDOMNode;
  begin
    Result:= nil;
    Element:= RootNode.FirstChild;
    while Assigned( Element ) do
      if ( TDOMElement( Element ).GetAttribute( 'name' ) = Name ) then
        begin
          Result:= TDOMElement( Element );
          break;
        end
      else
        Element:= Element.NextSibling;
  end;

begin
  Result:= nil;

  if ( not Assigned( RootNode )) then
    exit;

  StringSplit( Identifier, '/', Name, RestStr );
  Node:= FindNode();
  if ( RestStr > '' ) then
    Result:= FindDOMByIdentifier( RestStr, TDOMElement( Node ))
  else
    Result:= TDOMElement( Node );
end;}


{ gP3DCustomDatablockList }
function gP3DCustomDatablockList.GetCount: Integer;
begin
  Result:= FItems.Count;
end;

procedure gP3DCustomDatablockList.SetCount( AValue: Integer );
begin
  FItems.Count:= AValue;
end;

function gP3DCustomDatablockList.GetItems( Index: Integer ): TCustomItem;
begin
  if ( Index = -1 ) then
    Result:= nil
  else
    Result:= TCustomItem( FItems[ Index ]);
end;

constructor gP3DCustomDatablockList.Create;
begin
  inherited Create;
  FItems:= TList.Create;
end;

destructor gP3DCustomDatablockList.Destroy;
begin
  Clear( FFreeChildrenOnDestroy );
  FItems.Free;
  inherited Destroy;
end;

function gP3DCustomDatablockList.Add( Item: TCustomItem ): Integer;
begin
  Result:= IndexOf( Item );
  if ( Result = -1 ) then
    begin
      Result:= FItems.Add( Item );
      if ( Assigned( OnChange )) then
        OnChange( Self, Item, actAdd );
      DoUpdate;
    end;
end;

procedure gP3DCustomDatablockList.Insert( Index: Integer; Item: TCustomItem );
begin
  FItems.Insert( Index, Item );
  if ( Assigned( OnChange )) then
    OnChange( Self, Item, actAdd );
  DoUpdate;
end;

function gP3DCustomDatablockList.FindUniqueName(BaseStr: String): String;
var
  I: Integer;
begin
  I:= 0;
  repeat
    Inc( I );
    Result:= BaseStr + IntToStr( I );
  until ( FindByName( Result ) = -1 );
end;

function gP3DCustomDatablockList.FindByName(AName: String): Integer;
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

function gP3DCustomDatablockList.GetByName(AName: String): TCustomItem;
var
  idx: Integer;
begin
  idx:= FindByName( AName );
  if ( idx > -1 ) then
    Result:= Items[ idx ]
  else
    Result:= nil;
end;

function gP3DCustomDatablockList.FindByFileName(AFileName: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  {for i:= 0 to Count - 1 do
    if ( Self[ i ].FileWatch.FileName = ExpandFileName( AFileName )) then
      begin
        Result:= i;
        break;
      end;}
end;

procedure gP3DCustomDatablockList.SaveToDOM( AParent: TDOMElement );
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].SaveToDOM( AParent );
end;

procedure gP3DCustomDatablockList.DoUpdate;
begin
  FLastUpdate:= SDL_GetTicks;
end;

procedure gP3DCustomDatablockList.Delete( Index: Integer );
begin
  if ( Assigned( OnChange )) then
    OnChange( Self, Items[ Index ], actDelete );
  FItems.Delete( Index );
  DoUpdate;
end;

procedure gP3DCustomDatablockList.Remove(Item: TCustomItem);
begin
  if ( Assigned( OnChange )) then
    OnChange( Self, Item, actDelete );
  FItems.Remove( Item );
  DoUpdate;
end;

function gP3DCustomDatablockList.IndexOf( Item: TCustomItem ): Integer;
begin
  Result:= Fitems.IndexOf( Item );
end;

procedure gP3DCustomDatablockList.Clear( const FreeObjects: Boolean );
var
  i: Integer;
begin
  for i:= 0 to Self.Count - 1 do
    begin
      if ( Assigned( OnChange )) then
        OnChange( Self, Items[ i ], actClear );
      if ( FreeObjects ) then
        Items[ i ].Free;
    end;
  FItems.Clear;
  DoUpdate;
end;

function gP3DCustomDatablockList.GetEnumerator(): TP3DCustomObjectListEnumerator;
begin
  Result:= TP3DCustomObjectListEnumerator.Create( -1, @MoveNext );
end;

function gP3DCustomDatablockList.Reversed: TP3DCustomObjectListEnumerator;
begin
  Result:= TP3DCustomObjectListEnumerator.Create( Count, @MoveBack );
end;

function gP3DCustomDatablockList.MoveNext( var Index: Integer ): TCustomItem;
begin
  Inc( Index );
  if ( Index < Count ) then
    Result:= Items[ Index ]
  else
    Result:= nil;
end;

function gP3DCustomDatablockList.MoveBack(var Index: Integer): TCustomItem;
begin
  Dec( Index );
  if ( Index > -1 ) then
    Result:= Items[ Index ]
  else
    Result:= nil;
end;

procedure gP3DCustomDatablockList.SetItems( Index: Integer ; AValue: TCustomItem);
var
  Item: TCustomItem;
begin
  Item:= Items[ Index ];
  if ( Item = AValue ) then
    exit;

  if ( Assigned( OnChange )) then
    OnChange( Self, Item, actDelete );

  FItems[ Index ]:= AValue;

  if ( Assigned( OnChange )) then
    OnChange( Self, AValue, actAdd );
  DoUpdate;
end;


{ TP3DDataBlock }

procedure TP3DDataBlock.SetName(NewName: TComponentName);
var
  idx: Integer;
begin
  inherited SetName(NewName);
  idx:= P3DDataBlockCache.FindBlock( Self );
  if ( idx > -1 ) then
    P3DDataBlockCache[ idx ].Name:= Name;
end;

function TP3DDataBlock.GetDOMPath: String;
begin
  Result:= '/' + Name;
end;

procedure TP3DDataBlock.SetAction(AValue: TP3DAction);
begin
  if FAction=AValue then Exit;

  if ( Assigned( FAction )) then
    FAction.RemoveUser( Self );

  FAction:= AValue;

  if ( Assigned( FAction )) then
    FAction.AddUser( Self );
end;

constructor TP3DDataBlock.Create(const AData: TP3DLibrary);
begin
  if ( AData = nil ) then
    FOwner:= P3DData
  else
    FOwner:= AData;
  inherited Create( Owner.DataBlocks );
  //FileWatch:= TP3DFileWatch.Create( '<None>', Self );
  //DOMPath:= '';
  AABB:= P3DInvalidAABB;
  FUsers:= TP3DDataBlockList.Create;
  P3DDataBlockCache.Add( Self );
  Properties.Add( TP3DPropertyAccessAction.CreateFieldSet( 'Action', @FAction, @SetAction, smAttribute ));
  Properties.Add( TP3DPropertyAccessLibrary.CreateFieldSet( 'Owner', @FOwner, nil, smAttribute ));
end;

constructor TP3DDataBlock.CreateFromDOM(ADOMNode: TDOMElement; const AData: TP3DLibrary);
var
  versionNd: TDOMElement;
begin
  Create( AData );

  versionNd:= TDOMElement( ADOMNode.OwnerDocument.DocumentElement );
  if (( Assigned( versionNd )) and ( versionNd.GetAttribute( 'version' ) >= '0.1' )) then
    LoadFromDOMNew( ADOMNode )
  else
    LoadFromDOM( ADOMNode );
end;

destructor TP3DDataBlock.Destroy;
begin
  ClearUsers;
  Owner.RemoveFromFields( Self );
  ClearFields;

  FastDestroy;
end;

procedure TP3DDataBlock.FastDestroy;
begin
  FUsers.Free;
  {$IFDEF DEBUG_DATABLOCKS}
  if ( Assigned( Owner.FileWatch ) and ( Owner.FileWatch.FileName > '' )) then
    WriteLn( '<' + ExtractFileNameOnly( Owner.FileWatch.FileName ) + '.' + Name + ':' + ClassName + '>.FastDestroy' )
  else
    WriteLn( '<main.' + Name + ':' + ClassName + '>.FastDestroy' );
  {$ENDIF}
  inherited Destroy;
end;

procedure TP3DDataBlock.Render(world: TMat4; Scene: TP3DScene;
  RenderObject: TP3DActor);
begin

end;

procedure TP3DDataBlock.LoadFromDOM(ADOMNode: TDOMElement);
  function GetFullPath(): String;
  var
    Node: TDOMElement;
  begin
    Node:= ADOMNode;
    Result:= '';
    Node:= TDOMElement( Node.ParentNode );

    while ( Assigned( Node )) and ( Node.NodeName <> 'p3dfile' ) do
      begin
        Result:= Node.GetAttribute( 'name' ) + '/' + Result;
        Node:= TDOMElement( Node.ParentNode );
      end;
  end;

var
  filename: DOMString;
begin
  //filename:= ADOMNode.OwnerDocument.DocumentElement.GetAttribute( 'filename' );
  Name:= ADOMNode.GetAttribute( 'name' );
  {FileWatch.FileName:= filename + '?' + }//DOMPath:= GetFullPath() + Name;
//  WriteLn( FileWatch.FileName );
end;


procedure TP3DDataBlock.LoadFromFile(FileName: String);
var
  XMLPath, FN, dir: String;
  xml: TXMLDocument;
  Root: TDOMElement;
begin
  StringSplit( FileName, '?', FN, XMLPath );

  if ( lowercase( ExtractFileExt( FN )) = '.p3d' ) then
    begin
      FName:= P3DSearchPaths.FindFileEx( FN, GetCurrentDir );
      if ( FName = '' ) then
        raise Exception.Create( Format( 'Cannot load datablock from file! The specified file "%s" was not found!', [ FileName ]));
      ReadXMLFile( xml, FName );
      dir:= GetCurrentDir;
      try
        xml.DocumentElement.SetAttribute( 'filename', FName );
        SetCurrentDir( ExtractFilePath( FName ));

        Root:= P3DXMLFindPath( xml.DocumentElement, XMLPath );
        if ( not Assigned( Root )) then
          raise Exception.Create( Format( 'Cannot load datablock from file! The specified node "%s" was not found in the file "%s"!', [ XMLPath, FName ]));
        LoadFromDOMNew( Root );
      finally
        SetCurrentDir( dir );
        xml.Free;
      end;
    end
  else
    LoadFromBinary( FN );
end;

procedure TP3DDataBlock.RemoveUser( User: TP3DDataBlock );
var
  idx: Integer;
begin
  {$IFDEF DEBUG_DATABLOCKS}
  if ( Assigned( Parent.FileWatch ) and ( Parent.FileWatch.FileName > '' )) then
    WriteLn( '<' + ExtractFileNameOnly( Parent.FileWatch.FileName ) + '.' + Name + ':' + ClassName + '>.RemoveUser( ' + User.Name + ' )' )
  else
    WriteLn( '<main.' + Name + ':' + ClassName + '>.RemoveUser( ' + User.Name + ' )' );
  WriteLn( Name + '.Users before remove: ' + Users.DumpUsers );
  {$ENDIF}
  if ( P3DData.IsValid( Self )) then
    begin
      Users.Remove( User );
    end
  else
    begin
      idx:= P3DDataBlockCache.FindBlock( Self );
      if ( idx > -1 ) then
        WriteLn( User.Name + ': Orphaned user with cached name ' + P3DDataBlockCache[ idx ].Name )
      else
        WriteLn( User.Name + ': Orphaned user! ' );
    end;
end;

procedure TP3DDataBlock.AddUser( User: TP3DDataBlock );
begin
  if ( User = Self ) then
    raise Exception.Create( 'TP3DDataBlock.AddUser( ' + User.Name + ' ): Users loop detected!' );

  {$IFDEF DEBUG_DATABLOCKS}

  if ( not ( User is TP3DActor )) then
    begin
      if ( Assigned( Owner.FileWatch ) and ( Owner.FileWatch.FileName > '' )) then
        WriteLn( '<' + ExtractFileNameOnly( Owner.FileWatch.FileName ) + '.' + Name + ':' + ClassName + '>.AddUser( ' + User.Name + ' )' )
      else
        WriteLn( '<main.' + Name + ':' + ClassName + '>.AddUser( ' + User.Name + ' )' );
    end;
  {$ENDIF}
  Users.Add( User );
end;

procedure TP3DDataBlock.ClearFields;
begin
  //nothing to do in base class
end;

procedure TP3DDataBlock.ClearUsers;
var
  i: Integer;
begin
  for i:= Users.Count - 1 downto 0 do
    if ( P3DData.IsValid( Users[ i ] )) then
      Users[ i ].RemoveFromFields( Self );
  Users.Clear;
end;

class function TP3DDataBlock.DOMNodeName: String;
begin
  Result:= 'datablock';
end;

procedure TP3DDataBlock.AddToRenderList(ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags);
begin
  //Don't do anything but we don't want to generate an error when that method is not overridden by ancestor class
end;

function TP3DDataBlock.CollideSphere(Mat: TMat4; ASphere: TP3DSphere; out Intersect: TVec3; out N: TVec3): Boolean;
begin
  Result:= False;
end;

function TP3DDataBlock.CollideAABB(Mat: TMat4; AAABB: TP3DAABB; out Intersect: TVec3; out N: TVec3): Boolean;
begin
  Result:= False;
end;

function TP3DDataBlock.CollidePoint(Mat: TMat4; APoint: TVec3): Boolean;
begin
  Result:= False;
end;

procedure TP3DDataBlock.LoadFromBinary(FileName: String);
begin

end;

procedure TP3DDataBlock.Reload;
var
  Node: TDOMElement;
  Dir: String;
begin
  Owner.OpenDOMFile;
  Node:= Owner.FindDOMToPath( DOMPath );
  Dir:= GetCurrentDir;
  SetCurrentDir( ExtractFilePath( Owner.FileWatch.FileName ));
  if ( Assigned( Node )) then
    LoadFromDOM( Node )
  else
    WriteLn( 'Warning, cannot reload. Not found in DOM file!' );
  Owner.CloseDOMFile;
  SetCurrentDir( Dir );
end;

function TP3DDataBlock.CalcAABB: TP3DAABB;
begin
  FAABB.Position:= vec3( 0 );
  FAABB.Max:= vec3( 0 );
  FAABB.Min:= vec3( 0 );
  Result:= AABB;
end;

function TP3DDataBlock.CalcSphere: TP3DSphere;
begin
  Result:= Sphere;
end;

procedure TP3DDataBlock.RemoveFromFields(Block: TP3DDataBlock);
begin
  {$IFDEF DEBUG_DATABLOCKS}
  if ( Assigned( Owner.FileWatch ) and ( Owner.FileWatch.FileName > '' )) then
    WriteLn(  '<' + ExtractFileNameOnly( Owner.FileWatch.FileName ) + '.' + Name + ':' + ClassName + '>.RemoveFromFields( ' + Block.Name + ' )' )
  else
    WriteLn( '<main.' + Name + ':' + ClassName + '>.RemoveFromFields( ' + Block.Name + ' )' );
  {$ENDIF}
  if ( Block = Action ) then
    Action:= nil;
end;

function TP3DDataBlock.CastRay(Orig, Dir: TVec3; Actor: TP3DActor; out
  HitPos: TVec3): Boolean;
begin
  Result:= False;
end;

{$ENDIF}
