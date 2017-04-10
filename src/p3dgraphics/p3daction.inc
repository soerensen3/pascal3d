//p3daction.inc
//part of p3dgraphics.pas

{$IFDEF INTERFACE}

  { TP3DActionKey }

  TP3DActionKey = class ( TPersistent )
    private
      FTime: Float;
      FValue: Float;

    public
      constructor Create( ATime, AValue: Float );

      property Time: Float read FTime write FTime;
      property Value: Float read FValue write FValue;
  end;

  { TP3DActionKeyList }

  TP3DActionKeyList = class ( specialize gP3DCustomObjectList < TP3DActionKey >)
    public
      function AddKey( ATime, AValue: Float ): Integer;
  end;

  { TP3DActionChannel }

  TP3DActionChannel = class( TP3DInterfacedPersistent )
    private
      FArrayIndex: Integer;
      FKeys: TP3DActionKeyList;
      FPropStr: String;

      procedure SetPropStr( AValue: String );

    public
      constructor Create;
      destructor Destroy; override;

    published
      property ArrayIndex: Integer read FArrayIndex write FArrayIndex;
      property PropStr: String read FPropStr write SetPropStr;
      property Keys: TP3DActionKeyList read FKeys write FKeys;
  end;

  TP3DActionChannelList = specialize gP3DCustomObjectList < TP3DActionChannel >;

  TP3DAction = class ( TP3DDataBlock )
    private
      FChannels: TP3DActionChannelList;

    public
      constructor Create( const AData: TP3DLibrary=nil ); override;
      procedure FastDestroy; override;

      class function DOMNodeName: String; override;
      property Channels: TP3DActionChannelList read FChannels write FChannels;
  end;

  TP3DActionList = specialize gP3DCustomDatablockList < TP3DAction >;
  TP3DPropertyAccessAction = specialize gP3DPropertyAccessDataBlock < TP3DAction >;
  TP3DPropertyAccessActionList = specialize gP3DListPropertyAccess < TP3DActionList, TP3DAction >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DActionKey }

constructor TP3DActionKey.Create(ATime, AValue: Float);
begin
  inherited Create;
  FTime:= ATime;
  FValue:= AValue;
end;

{ TP3DActionKeyList }

function TP3DActionKeyList.AddKey(ATime, AValue: Float): Integer;
begin
  Result:= Add( TP3DActionKey.Create( ATime, AValue ));
end;


{ TP3DActionChannel }

procedure TP3DActionChannel.SetPropStr(AValue: String);
begin

end;

constructor TP3DActionChannel.Create;
begin
  inherited Create;
  FKeys:= TP3DActionKeyList.Create;
end;

destructor TP3DActionChannel.Destroy;
begin
  FreeAndNil( FKeys );
  inherited Destroy;
end;


{ TP3DAction }

constructor TP3DAction.Create(const AData: TP3DLibrary);
begin
  inherited Create(AData);

  FChannels:= TP3DActionChannelList.Create;
  Owner.Actions.Add( Self );
  Properties.Add( TP3DPropertyAccessActionList.CreateField( 'Channels', @FChannels, smNode ));
end;

procedure TP3DAction.FastDestroy;
begin
  Owner.Actions.Remove( Self );
  FreeAndNil( FChannels );
  inherited FastDestroy;
end;

class function TP3DAction.DOMNodeName: String;
begin
  Result:= 'action';
end;



{$ENDIF}

