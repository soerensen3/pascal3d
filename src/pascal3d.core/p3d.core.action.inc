//p3daction.inc
//part of p3dgraphics.pas

{$IFDEF INTERFACE}

  { TP3DActionKey }

  TP3DActionKey = class ( TP3DStreamable )
    private
      FTime: Float;
      FValue: String;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

    published
      property Time: Float read FTime write FTime;
      property Value: String read FValue write FValue;
  end;

  { TP3DActionKeyList }

  {TP3DActionKeyList = class ( specialize gP3DCustomObjectList < TP3DActionKey >)
    public
      function AddKey(ATime: Float; AValue: String): Integer;
      procedure LoadFromStream( S: TStream ); //gP3DListPropertyAccessObject needs this to compile
      procedure SaveToStream( S: TStream );

      destructor Destroy; override;
  end;}

  {TP3DActionKeyContainer = specialize gP3DStreamableContainer < TP3DActionKey >;
  TP3DActionKeyPointer = specialize gP3DStreamablePointer < TP3DActionKey >;
  TP3DActionKeyPointerList = specialize gP3DStreamableList < TP3DActionKey, TP3DActionKeyPointer >;
  }
  TP3DActionKeyContainerList = specialize gP3DStreamableContainerList < TP3DActionKey >;

  { TP3DActionChannel }

  //TP3DActionChannelInterpolationMode = ( imInterpolate, imConstant, imExtrapolate );
  TP3DActionChannelTimeMode = ( tmContinue, tmWrapAround, tmClamp );
  TP3DActionChannel = class( TP3DStreamable )
    private
      FFilter: String;
      FInterpolationMode: TP3DPropertyAccessInterpolationMode;
      FKeys: TP3DActionKeyContainerList;
      FPropStr: String;
      FTimeMode: TP3DActionChannelTimeMode;

      function GetFrameCount: Float;
      procedure SetFilter(AValue: String);
      procedure SetPropStr( AValue: String );

      {function Interpolate( Value1, Value2, a: Float ): Float;
      function ModeConstant( Value1, Value2, a: Float ): Float;
      function ModeInterpolate( Value1, Value2, a: Float ): Float;
      function ModeExtrapolate( Value1, Value2, a: Float ): Float;}
      function PercentageBetweenKeys( Time: Float; Key1, Key2: Integer ): Float;
      function TransformTime( Time: Float ): Float;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure EvaluateSet( Prop: TP3DPropertyAccess; Time: Float );

    published
      property PropStr: String read FPropStr write SetPropStr;
      property Keys: TP3DActionKeyContainerList read FKeys write FKeys;
      property InterpolationMode: TP3DPropertyAccessInterpolationMode read FInterpolationMode write FInterpolationMode;
      property TimeMode: TP3DActionChannelTimeMode read FTimeMode write FTimeMode;
      property FrameCount: Float read GetFrameCount;
  end;

  { TP3DActionChannelList }

  {TP3DActionChannelList = class( specialize gP3DCustomObjectList < TP3DActionChannel >)
    public
      destructor Destroy; override;
  end;}
  {
  TP3DActionChannelContainer = specialize gP3DStreamableContainer < TP3DActionChannel >;
  TP3DActionChannelPointer = specialize gP3DStreamablePointer < TP3DActionChannel >;
  TP3DActionChannelPointerList = specialize gP3DStreamableList < TP3DActionChannel, TP3DActionChannelPointer >;
  }
  TP3DActionChannelContainerList = specialize gP3DStreamableContainerList < TP3DActionChannel >;

  TP3DAction = class ( TP3DDataBlock )
    protected
      FChannels: TP3DActionChannelContainerList;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      class function GetContainer( ALib: TP3DLibrary ): TP3DPropertyAccess; override;

      procedure Perform( Time: Float; AData: TP3DDataBlock );

      property Channels: TP3DActionChannelContainerList read FChannels write FChannels;
  end;

  {TP3DActionContainer = specialize gP3DStreamableContainer < TP3DAction >;
  TP3DActionPointer = specialize gP3DStreamablePointer < TP3DAction >;
  //TP3DActionContainerList = specialize gP3DNamedStreamableContainerList < TP3DAction, TP3DActionContainer >;
  TP3DActionPointerList = specialize gP3DNamedStreamableList < TP3DAction, TP3DActionPointer >;
  }

  { TP3DActionList }

  TP3DActionContainerList = class( specialize gP3DNamedStreamableContainerList < TP3DAction >)
    private
      FGlobalTime: Float;
      procedure SetGlobalTime(AValue: Float);

    public
      procedure PerformActions( GlobalTime: Float );

    published
      property GlobalTime: Float read FGlobalTime write SetGlobalTime;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DActionKey }

constructor TP3DActionKey.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Time', smText ));
  Properties.Add( TP3DPropAccessString.CreateField( 'Value', @Value, smText ));
end;

{ TP3DActionKeyList }
{
function TP3DActionKeyContainerList.AddKey( ATime: Float; AValue: String ): Integer;
var
  Key: TP3DActionKey;
begin
  Key:= TP3DActionKey.Create();
  Key.Time:= ATime;
  Key.Value:= AValue;
  Result:= Add( Key );
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
}


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
  Key:= Keys.Last;
  if( Assigned( Key )) then
    Result:= Key.Time
  else
    Result:= 0.0;
end;
{
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
}

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

constructor TP3DActionChannel.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FKeys:= TP3DActionKeyContainerList.Create( 'Keys', smText );
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'PropStr', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'TimeMode', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'InterpolationMode', smText ));
  Properties.Add( Keys );
end;

destructor TP3DActionChannel.Destroy;
begin
  //FreeAndNil( FKeys );
  inherited Destroy;
end;

procedure TP3DActionChannel.EvaluateSet(Prop: TP3DPropertyAccess; Time: Float);
var
  First, Last: Integer;
begin
  Time:= TransformTime( Time );
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
    Prop.SetInterpolate( Keys[ First ].Value, Keys[ Last ].Value, PercentageBetweenKeys( Time, First, Last ), InterpolationMode ) //Interpolate( Keys[ First ].Value, Keys[ Last ].Value, PercentageBetweenKeys( Time, First, Last ))
  else
    Prop.AsString:= Keys[ Last ].Value;
end;

{ TP3DAction }

class function TP3DAction.GetContainer(ALib: TP3DLibrary): TP3DPropertyAccess;
begin
  Result:= ALib.Properties.GetPropByName( 'Actions' );
end;

constructor TP3DAction.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );

  FChannels:= TP3DActionChannelContainerList.Create( 'Channels', smText );
  Properties.Add( Channels );
end;

destructor TP3DAction.Destroy;
begin
  //FreeAndNil( FChannels );
  inherited Destroy;
end;


procedure TP3DAction.Perform(Time: Float; AData: TP3DDataBlock);
var
  Channel: TP3DActionChannel;
  Prop: TP3DPropertyAccess;
begin
  if ( Assigned( AData )) then
    for Channel in Channels do
      begin
        Prop:= AData.Properties.GetPropByPath( Channel.PropStr );
        if ( Assigned( Prop )) then
          Channel.EvaluateSet( Prop, Time );
    end;
end;

{ TP3DActionList }

procedure TP3DActionContainerList.SetGlobalTime(AValue: Float);
begin
  if FGlobalTime=AValue then Exit;
  FGlobalTime:=AValue;
  PerformActions( AValue );
end;

procedure TP3DActionContainerList.PerformActions(GlobalTime: Float);
var
  Action: TP3DAction;
  Block: IP3DStreamableUser;
  user: TObject;
begin
  for Action in Self do begin
    for Block in Action.Users do begin
      user:= TP3DPropertyAccess( Block.GetInstance ).Context.GetInstance;
      if ( user is TP3DDataBlock ) then
        Action.Perform( GlobalTime, TP3DDataBlock( user ));
    end;
  end;
end;

{$ENDIF}

