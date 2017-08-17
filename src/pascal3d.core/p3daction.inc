//p3daction.inc
//part of p3dgraphics.pas

{$IFDEF INTERFACE}

  { TP3DActionKey }

  TP3DActionKey = class ( TP3DInterfacedPersistent )
    private
      FTime: Float;
      FValue: Float;

    public
      constructor Create( ATime, AValue: Float );
      constructor Create;

      class function DOMNodeName: String; override;

    published
      property Time: Float read FTime write FTime;
      property Value: Float read FValue write FValue;
  end;

  { TP3DActionKeyList }

  TP3DActionKeyList = class ( specialize gP3DCustomObjectList < TP3DActionKey >)
    public
      function AddKey( ATime, AValue: Float ): Integer;
      procedure LoadFromDOM( ADOMNode: TDOMElement );
      procedure LoadFromStream( S: TStream ); //gP3DListPropertyAccessObject needs this to compile
      procedure SaveToStream( S: TStream );

      destructor Destroy; override;
  end;

  TP3DPropertyAccessActionKey = specialize gP3DPropertyAccessInterfacedPersistent < TP3DActionKey >;
  TP3DPropertyAccessActionKeyList = specialize gP3DListPropertyAccessObject < TP3DActionKeyList, TP3DActionKey, TP3DPropertyAccessActionKey >;


  { TP3DActionChannel }

  TP3DActionChannelInterpolationMode = ( imInterpolate, imConstant, imExtrapolate );
  TP3DActionChannelTimeMode = ( tmContinue, tmWrapAround, tmClamp );
  TP3DActionChannel = class( TP3DInterfacedPersistent )
    private
      FArrayIndex: Integer;
      FFilter: String;
      FInterpolationMode: TP3DActionChannelInterpolationMode;
      FKeys: TP3DActionKeyList;
      FPropStr: String;
      FTimeMode: TP3DActionChannelTimeMode;

      function GetFrameCount: Float;
      procedure SetFilter(AValue: String);
      procedure SetPropStr( AValue: String );

      function Interpolate( Value1, Value2, a: Float ): Float;
      function ModeConstant( Value1, Value2, a: Float ): Float;
      function ModeInterpolate( Value1, Value2, a: Float ): Float;
      function ModeExtrapolate( Value1, Value2, a: Float ): Float;
      function PercentageBetweenKeys( Time: Float; Key1, Key2: Integer ): Float;
      function TransformTime( Time: Float ): Float;

    public
      constructor Create;
      constructor CreateFromJSONContext(AContext: TP3DJSONContext); override;
      destructor Destroy; override;

      procedure LoadFromDOMNew(ADOMNode: TDOMElement); override;
      function Evaluate( Time: Float ): Float;

      class function DOMNodeName: String; override;

    published
      property PropStr: String read FPropStr write SetPropStr;
      property Keys: TP3DActionKeyList read FKeys write FKeys;
      property InterpolationMode: TP3DActionChannelInterpolationMode read FInterpolationMode write FInterpolationMode;
      property TimeMode: TP3DActionChannelTimeMode read FTimeMode write FTimeMode;
      property FrameCount: Float read GetFrameCount;
  end;

  { TP3DActionChannelList }

  TP3DActionChannelList = class( specialize gP3DInterfacedPersistentList < TP3DActionChannel >)
    public
      procedure LoadFromDOM( ADOMNode: TDOMElement );
      destructor Destroy; override;
  end;

  TP3DPropertyAccessActionChannel = specialize gP3DPropertyAccessInterfacedPersistent < TP3DActionChannel >;
  TP3DPropertyAccessActionChannelList = specialize gP3DListPropertyAccessObject < TP3DActionChannelList, TP3DActionChannel, TP3DPropertyAccessActionChannel >;

  TP3DAction = class ( TP3DDataBlock )
    private
      FChannels: TP3DActionChannelList;

      class function GetContainer( ALib: TP3DLibrary ): TP3DNamedInterfacedPersistentList; override;
      function GetDataPath: String; override;

    public
      constructor Create( const AData: TP3DLibrary=nil ); override;
      procedure FastDestroy; override;

      procedure LoadFromDOMNew(ADOMNode: TDOMElement); override;
      procedure Perform( Time: Float; AData: TP3DDataBlock );

      class function DOMNodeName: String; override;
      property Channels: TP3DActionChannelList read FChannels write FChannels;
  end;

  { TP3DActionList }

  TP3DActionList = class( specialize gP3DNamedInterfacedPersistentList < TP3DAction >)
    private
      FGlobalTime: Float;
      procedure SetGlobalTime(AValue: Float);

    public
      procedure PerformActions( GlobalTime: Float );

    published
      property GlobalTime: Float read FGlobalTime write SetGlobalTime;
  end;
  TP3DPropertyAccessAction = specialize gP3DPropertyAccessDataBlock < TP3DAction >;
  TP3DPropertyAccessActionList = specialize gP3DPropertyAccessDataBlockList < TP3DActionList, TP3DAction, TP3DPropertyAccessAction >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DActionKey }

constructor TP3DActionKey.Create(ATime, AValue: Float);
begin
  inherited Create;
  FTime:= ATime;
  FValue:= AValue;
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Time', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Value', smAttribute ));
end;

constructor TP3DActionKey.Create;
begin
  inherited Create;
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Time', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Value', smAttribute ));
end;

class function TP3DActionKey.DOMNodeName: String;
begin
  Result:= 'key';
end;

{ TP3DActionKeyList }

function TP3DActionKeyList.AddKey(ATime, AValue: Float): Integer;
begin
  Result:= Add( TP3DActionKey.Create( ATime, AValue ));
end;

procedure TP3DActionKeyList.LoadFromDOM(ADOMNode: TDOMElement);
var
  lst: TDOMNodeList;
  i: Integer;
begin
  Clear();
  lst:= ADOMNode.GetElementsByTagName( TP3DActionKey.DOMNodeName );
  try
    Count:= lst.Count;
    for i:= 0 to lst.Count - 1 do
      begin
        Items[ i ]:= TP3DActionKey.Create();
        Items[ i ].LoadFromDOMNew( TDOMElement( lst[ i ]));
        //WriteLn( 'Key Time=', Items[ i ].Time, ' Value=', Items[ i ].Value );
      end;

  finally
    lst.Free;
  end;
end;

procedure TP3DActionKeyList.LoadFromStream(S: TStream);
begin

end;

procedure TP3DActionKeyList.SaveToStream(S: TStream);
begin

end;

destructor TP3DActionKeyList.Destroy;
begin
  Clear( True );
  inherited Destroy;
end;

{ TP3DActionChannelList }

procedure TP3DActionChannelList.LoadFromDOM(ADOMNode: TDOMElement);
var
  lst: TDOMNodeList;
  i: Integer;
begin
  Clear();
  lst:= ADOMNode.GetElementsByTagName( TP3DActionChannel.DOMNodeName );
  try
    Count:= lst.Count;
    for i:= 0 to lst.Count - 1 do
      begin
        Items[ i ]:= TP3DActionChannel.Create;
        Items[ i ].LoadFromDOMNew( TDOMElement( lst[ i ]));
      end;

  finally
    lst.Free;
  end;
end;

destructor TP3DActionChannelList.Destroy;
begin
  Clear( True );
  inherited Destroy;
end;



{ TP3DActionChannel }

procedure TP3DActionChannel.SetPropStr(AValue: String);
begin
  FPropStr:= AValue;
end;

procedure TP3DActionChannel.SetFilter(AValue: String);
begin
  if FFilter=AValue then Exit;
  FFilter:=AValue;
end;

function TP3DActionChannel.GetFrameCount: Float;
var
  Key: TP3DActionKey;
begin
  Key:= Keys[ -1 ];
  if( Assigned( Key )) then
    Result:= Key.Time
  else
    Result:= 0.0;
end;

function TP3DActionChannel.Interpolate(Value1, Value2, a: Float): Float;
begin
  case InterpolationMode of
    imConstant: Result:= ModeConstant( Value1, Value2, a );
    imExtrapolate: Result:= ModeExtrapolate( Value1, Value2, a );
    imInterpolate: Result:= ModeInterpolate( Value1, Value2, a );
  end;
end;

function TP3DActionChannel.ModeConstant(Value1, Value2, a: Float): Float;
begin
  if ( a >= 1 ) then
    Result:= Value2
  else
    Result:= Value1;
end;

function TP3DActionChannel.ModeInterpolate(Value1, Value2, a: Float): Float;
begin
  Result:= mix( Value1, Value2, clamp( a, 0, 1 ));
end;

function TP3DActionChannel.ModeExtrapolate(Value1, Value2, a: Float): Float;
begin
  Result:= mix( Value1, Value2, a );
end;

function TP3DActionChannel.PercentageBetweenKeys(Time: Float; Key1,
  Key2: Integer): Float;
begin
  if ( Key1 = Key2 ) then
    Result:= 1
  else
    Result:= ( Time - Keys[ Key1 ].Time ) / ( Keys[ Key2 ].Time - Keys[ Key1 ].Time );
end;

function TP3DActionChannel.TransformTime(Time: Float): Float;
begin
  case TimeMode of
    tmContinue: Result:= Time;
    tmWrapAround: Result:= Time mod max( 1, FrameCount ); //prevent mod 0 error
    tmClamp: Result:= clamp( Time, 0, FrameCount );
  end;
end;

constructor TP3DActionChannel.Create;
begin
  inherited Create;
  FKeys:= TP3DActionKeyList.Create;
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'PropStr', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'TimeMode', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'InterpolationMode', smAttribute ));
  Properties.Add( TP3DPropertyAccessActionKeyList.CreateFieldSet( 'Keys', @FKeys, nil, smNode ));
end;

constructor TP3DActionChannel.CreateFromJSONContext(AContext: TP3DJSONContext);
begin
  Create();
  LoadFromJSONContext( AContext );
end;

destructor TP3DActionChannel.Destroy;
begin
  FreeAndNil( FKeys );
  inherited Destroy;
end;

procedure TP3DActionChannel.LoadFromDOMNew(ADOMNode: TDOMElement);
begin
  inherited LoadFromDOMNew(ADOMNode);
  Keys.LoadFromDOM( ADOMNode );
end;

function TP3DActionChannel.Evaluate(Time: Float): Float;
var
  First, Last: Integer;
begin
  Time:= TransformTime( Time );
  Result:= 0;
  if ( Keys.Count < 1 ) then
    exit;
  Last:= 0;
  First:= 0;
  while ( Last < Keys.Count ) and ( Keys[ Last ].Time < Time ) do
    begin
      First:= Last;
      Inc( Last );
    end;
  if ( Last = Keys.Count ) then
    begin
      Dec( Last ); Dec( First );
    end;
  if ( First >= 0 ) then
    Result:= Interpolate( Keys[ First ].Value, Keys[ Last ].Value, PercentageBetweenKeys( Time, First, Last ))
  else
    Result:= Keys[ Last ].Value;
end;

class function TP3DActionChannel.DOMNodeName: String;
begin
  Result:= 'channel';
end;


{ TP3DAction }

class function TP3DAction.GetContainer(ALib: TP3DLibrary): TP3DNamedInterfacedPersistentList;
begin
  Result:= ALib.Actions;
end;

function TP3DAction.GetDataPath: String;
begin
  Result:= 'Actions[ "' + Name + '" ]';
end;

constructor TP3DAction.Create(const AData: TP3DLibrary);
begin
  inherited Create(AData);

  FChannels:= TP3DActionChannelList.Create;
  Owner.Actions.Add( Self );
  Properties.Add( TP3DPropertyAccessActionChannelList.CreateField( 'Channels', @FChannels, smNode ));
end;

procedure TP3DAction.FastDestroy;
begin
  Owner.Actions.Remove( Self );
  FreeAndNil( FChannels );
  inherited FastDestroy;
end;

procedure TP3DAction.LoadFromDOMNew(ADOMNode: TDOMElement);
begin
  inherited LoadFromDOMNew(ADOMNode);
  Channels.LoadFromDOM( ADOMNode );
end;

procedure TP3DAction.Perform(Time: Float; AData: TP3DDataBlock);
var
  Channel: TP3DActionChannel;
  Prop: TP3DPropertyAccess;
  Value: Float;
begin
  if ( Assigned( AData )) then
    for Channel in Channels do
      begin
        Prop:= AData.Properties.GetPropByPath( Channel.PropStr );
        if ( Assigned( Prop )) then
          begin
            Value:= Channel.Evaluate( Time );
{            if ( Prop.IsArray ) then
              Prop.ArrayAsString[ Channel.ArrayIndex ]:= FloatToStr( Value )
            else}
              Prop.AsString:= FloatToStr( Value );
          end;
    end;
end;

class function TP3DAction.DOMNodeName: String;
begin
  Result:= 'action';
end;

{ TP3DActionList }

procedure TP3DActionList.SetGlobalTime(AValue: Float);
begin
  if FGlobalTime=AValue then Exit;
  FGlobalTime:=AValue;
  PerformActions( AValue );
end;

procedure TP3DActionList.PerformActions(GlobalTime: Float);
var
  Action: TP3DAction;
  Block: TP3DDataBlock;
begin
  for Action in Self do
    for Block in Action.Users do
      Action.Perform( GlobalTime, Block );
end;

{$ENDIF}

