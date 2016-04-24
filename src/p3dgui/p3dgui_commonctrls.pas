unit p3dgui_commonctrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, p3dgui;

type

  { TP3DTreeView }

  TP3DTreeView = class( TP3DGraphicControl )
    private
      FRootNode: TTreeNode;
      FColorSel: Cardinal;
      FColorSelText: Cardinal;
      FColorText: Cardinal;
      FColor: Cardinal;
      FItemHeight: Integer;
      FSelection: TTreeNode;
      FRowSelect: Boolean;
      FValueList: TRevGraphicControl;
      FItemList: TRevGraphicControl;
      FScrollBar: TRevScrollBar;
      FShowValues: Boolean;
      FCanEditValues: Boolean;
      FValEdit: TValEdit;
      FOnSelectionChange: TNotifyEvent;

      procedure BoxMouseClick( Sender: TRevGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
      procedure DrawItems( Sender: TRevGraphicControl; OffSetX, OffSetY, _Width, _Height: Single );
      procedure DrawValues( Sender: TRevGraphicControl; OffSetX, OffSetY, _Width, _Height: Single );
      procedure SetShowValues( const Value: Boolean );
      procedure SetCanEditValues( const Value: Boolean );
      procedure SetSelection( const Value: TTreeNode );

    public
      constructor Create( AOwner: TBaseObject; AEngine: TEngine; _OwnerCtrlBuf: TControlBuffer; _Manager: TVCLManager );
      destructor Destroy; override;

      class function IsFocusControl: Boolean; override;

      property RootNode: TTreeNode read FRootNode;

    published
      property ItemHeight: Integer read FItemHeight write FItemHeight;
      property Color: Cardinal read FColor write FColor;
      property ColorText: Cardinal read FColorText write FColorText;
      property ColorSel: Cardinal read FColorSel write FColorSel;
      property ColorSelText: Cardinal read FColorSelText write FColorSelText;
      property Selection: TTreeNode read FSelection write SetSelection;
      property RowSelect: Boolean read FRowSelect write FRowSelect;
      property ShowValues: Boolean read FShowValues write SetShowValues;
      property CanEditValues: Boolean read FCanEditValues write SetCanEditValues;
      property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  end;

implementation

{ TP3DTreeView }

procedure TP3DTreeView.BoxMouseClick(Sender: TRevGraphicControl; mb1, mb2,
  mb3: Boolean; X, Y: Integer);
begin
  Result:= True;
end;

procedure TP3DTreeView.DrawItems(Sender: TRevGraphicControl; OffSetX, OffSetY,
  _Width, _Height: Single);
begin

end;

procedure TP3DTreeView.DrawValues(Sender: TRevGraphicControl; OffSetX, OffSetY,
  _Width, _Height: Single);
begin

end;

procedure TP3DTreeView.SetShowValues(const Value: Boolean);
begin

end;

procedure TP3DTreeView.SetCanEditValues(const Value: Boolean);
begin

end;

procedure TP3DTreeView.SetSelection(const Value: TTreeNode);
begin

end;

constructor TP3DTreeView.Create(AOwner: TBaseObject; AEngine: TEngine;
  _OwnerCtrlBuf: TControlBuffer; _Manager: TVCLManager);
begin

end;

destructor TP3DTreeView.Destroy;
begin
  inherited Destroy;
end;

class function TP3DTreeView.IsFocusControl: Boolean;
begin
  Result:=inherited IsFocusControl;
end;

end.

