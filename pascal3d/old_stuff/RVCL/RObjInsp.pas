unit RObjInsp;

interface
  uses
    RCommonCtrls,
    Classes,
    Core,
    RVCL,
    SysUtils,
    dvRTTI;
  type
    TObjectInspector = class( TRevTreeView )
      private
        FObj: TPersistent;
        RTTI: TdvRTTIList;

        procedure SetObj( const Value: TPersistent );
        procedure SetValue( Sender: TObject );
        procedure RecreateList;

        property RootNode;

      public
        constructor Create( AOwner: TBaseObject; AEngine: TEngine;
          _OwnerCtrlBuf: TControlBuffer; _Manager: TVCLManager );
        destructor Destroy; override;

        property Obj: TPersistent read FObj write SetObj;
    end;

implementation

uses TypInfo;

{ TObjectInspector }

constructor TObjectInspector.Create( AOwner: TBaseObject; AEngine: TEngine;
  _OwnerCtrlBuf: TControlBuffer; _Manager: TVCLManager );
begin
  inherited;
  RTTI:= TdvRTTIList.Create;
  RTTI.DontSort:= True;
  RTTI.Filter:= tkProperties;
end;

destructor TObjectInspector.Destroy;
begin
  RTTI.Free;
  inherited;
end;

procedure TObjectInspector.RecreateList;
  procedure AddChilds( ChildObj: TdvRTTIProp; Level: Integer; Node: TTreeNode );
  var
    I: Integer;
    AClass: TPersistentClass;
  begin
    Node:= Node[ Node.Add( ChildObj.Name )];
    Node.Value:= ChildObj.Value;
    Node.Obj:= ChildObj;
    Node.OnSetValue:= SetValue;
    Node.ReadOnly:= ChildObj.Readonly;
    if ( ChildObj.TypeKind = tkClass ) then
      begin
        AClass:= GetClass( ChildObj.TypeName );
        if ( AClass <> nil ) then
          begin
            if ( AClass.InheritsFrom( TBaseObject )) then
              with ( Engine.ObjectEnvironment.ClassContainer.AllObjects ) do
                begin
                  For I:= 0 to Count - 1 do
                    if ( TBaseObject( Items[ I ]).InheritsFrom( AClass )) then
                      Node.DropDownList.Add( TBaseObject( Items[ I ]).Name );
                end;
          end;

      end
    else if ( ChildObj.HasValues ) then
      Node.DropDownList.Assign( ChildObj.GetValues );
    if ( ChildObj.CanHaveChilds ) then
      if ( ChildObj.CreateChilds ) then
        For I:= 0 to ChildObj.Childs.Count - 1 do
          AddChilds( ChildObj.Childs[ I ], Level + 1, Node );
  end;
  var
    I: Integer;
begin
  RootNode.Clear;
  For I:= 0 to RTTI.Count - 1 do
    AddChilds( RTTI.Items[ I ], 0, RootNode );
end;

procedure TObjectInspector.SetObj( const Value: TPersistent );
begin
  Selection:= nil;
  FObj:= Value;
  RTTI.Obj:= Value;
  RecreateList;
end;

procedure TObjectInspector.SetValue( Sender: TObject );
begin
  if ( Sender is TTreeNode ) then
    begin
      if ( TdvRTTIProp( TTreeNode( Sender ).Obj ).TypeKind = tkClass ) then
        begin
          SetOrdProp( TdvRTTIProp( TTreeNode( Sender ).Obj ).Owner.Obj,
            TdvRTTIProp( TTreeNode( Sender ).Obj ).Name,
            Integer( Engine.ObjectEnvironment.ClassContainer.GetObjectByName( TTreeNode( Sender ).Value )));
        end
      else
        TdvRTTIProp( TTreeNode( Sender ).Obj ).Value:= TTreeNode( Sender ).Value;
    end;
end;

initialization
  Rev_RegisterClass( TObjectInspector );


end.
