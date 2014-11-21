unit shadernodesunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterAny,
  LvlGraphCtrl, ExtendedNotebook, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, ExtCtrls, StdCtrls, DOM, XMLRead, strutils, p3dshadernodes;

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    SynAnySyn1: TSynAnySyn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LvlGraphControl1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
  private
    { private declarations }
  public
    procedure CreateBuffer( N: TP3DShaderNode );
    procedure ClearBuffers;
    procedure LoadLibraries;
    { public declarations }
  end;


var
  Form1: TForm1;
  NL: TP3DShaderNodeList;

implementation


{$R *.lfm}

procedure LoadFile( FN: String );
var
  node: TP3DShaderNode;
begin
  ProcessFile( NL, FN );
  for node in NL do
    Form1.CreateBuffer( node );
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  if( OpenDialog1.Execute ) then
    begin
      ClearBuffers;
      LoadFile( OpenDialog1.FileName );
      Caption:= Format( 'Viewing <%s>', [ OpenDialog1.FileName ]);
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LoadLibraries;
  NL:= TP3DShaderNodeList.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ClearBuffers;
  NL.Free;
end;

procedure TForm1.LvlGraphControl1Click(Sender: TObject);
begin

end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  ClearBuffers;
  LoadFile( OpenDialog1.FileName );
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  LoadLibraries;
end;

procedure TForm1.CreateBuffer(N: TP3DShaderNode);
var
  sheet: TTabSheet;
  scrollbox: TScrollBox;
  //ed: TSynEdit;
  //lv: TTreeView;
  //sp: TSplitter;
  {
  procedure AddList( Base: TTreeNode; List: TP3DShaderNodeVariableList );
  var
    frag: TP3DShaderNodeVariable;
  begin
    for frag in List do
      if ( frag is TP3DShaderNodeVariableInput ) then
        AddList(
          lv.Items.AddChild( Base, Format( 'Input: %s (type: "%s", required: %d)',
            [ TP3DShaderNodeVariableInput( frag ).Name,
              TP3DShaderNodeVariableInput( frag ).VarType,
              Ord( TP3DShaderNodeVariableInput( frag ).Required )])), TP3DShaderNodeVariableInput( frag ).Fragments )
      else if ( frag is TP3DShaderNodeVariableLink ) then
        AddList(
        lv.Items.AddChild( Base, Format( 'Link: %s (type: "%s")',
          [ TP3DShaderNodeVariableLink( frag ).Target,
            TP3DShaderNodeVariableLink( frag ).VarType ])), TP3DShaderNodeVariableLink( frag ).Fragments )
  end;}


  procedure AddCtrl( Ctrl: TControl; Parent: TWinControl);
  begin
    if ( Parent = nil ) then
      Parent:= scrollbox;

    Ctrl.Parent:= Parent;
    Ctrl.Align:= alTop;
  end;

  function AddGroup( Name: String; Parent: TWinControl  ): TGroupBox;
  begin
    Result:= TGroupBox.Create( Parent );
    Result.Caption:= Name;
    Result.AutoSize:= True;
    AddCtrl( Result, Parent );
  end;


  procedure AddFragments( Parent: TWinControl; Fragments: TP3DShaderNodeVariableList );
    procedure AddInline( Parent: TWinControl; I: TP3DShaderNodeVariableInline );
    var
      grp: TGroupBox;
      ctrl: TSynEdit;
    begin
      if ( I.Text = LineEnding ) then
        begin
          grp:= AddGroup( '<br/>', Parent );
        end
      else
        begin
          grp:= AddGroup( 'inline', Parent );
          ctrl:= TSynEdit.Create( grp );
          ctrl.Text:= I.Text;
          ctrl.Highlighter:= Form1.SynAnySyn1;
          AddCtrl( ctrl, grp );
        end;
    end;

    procedure AddInput( Parent: TWinControl; I: TP3DShaderNodeVariableInput );
    var
      grp: TGroupBox;
      ctrl: TSynEdit;
    begin
      grp:= AddGroup( 'input: ' + I.Name, Parent );
      AddFragments( grp, I.Fragments );
    end;

    procedure AddLink( Parent: TWinControl; I: TP3DShaderNodeVariableLink );
    var
      grp: TGroupBox;
      ctrl: TComboBox;
    begin
      grp:= AddGroup( 'link', Parent );
      ctrl:= TComboBox.Create( grp );
      ctrl.Text:= I.Target;
      AddCtrl( ctrl, grp);
      AddFragments( Parent, I.Fragments );
    end;

  var
    frag: TP3DShaderNodeVariable;
    grp: TGroupBox;
    ctrl: TSynEdit;
  begin
    if ( Parent = nil ) then
      Parent:= scrollbox;

    for frag in Fragments do
      case frag.ClassName of
        'TP3DShaderNodeVariableInput':
          AddInput( Parent, TP3DShaderNodeVariableInput( frag ));

        'TP3DShaderNodeVariableInline':
          AddInline( Parent, TP3DShaderNodeVariableInline( frag ));

        'TP3DShaderNodeVariableLink':
          AddLink( Parent, TP3DShaderNodeVariableLink( frag ));
      end;
  end;

begin
  sheet:= PageControl1.AddTabSheet;
  sheet.Caption:= N.Name;
  sheet.Tag:= PtrInt( N );
  scrollbox:= TScrollBox.Create( sheet );
  scrollbox.Parent:= sheet;
  scrollbox.Align:= alClient;
  AddFragments( scrollbox, N.Fragments );
  {ed:= TSynEdit.Create( sheet );
  ed.Parent:= sheet;
  ed.Text:= N.GetCode();
  ed.Align:= alClient;
  ed.Highlighter:= SynAnySyn1;
  lv:= TTreeView.Create( sheet );
  lv.Parent:= sheet;
  lv.Align:= alRight;
  sp:= TSplitter.Create( sheet );
  sp.Parent:= sheet;
  sp.Align:= alRight;
  AddList( lv.Items.Add( nil, N.Name ), N.Fragments );}
end;

procedure TForm1.ClearBuffers;
var
  i: Integer;
begin
  for i:= PageControl1.PageCount - 1 downto 0 do
    PageControl1.Pages[ i ].Free;
  NL.Clear();
end;

procedure TForm1.LoadLibraries;
begin
  P3DShaderLib.Clear();
  P3DShaderLib.LoadLibraryPath( '../../engine_runtime/shaders/nodes/core/', '*.xml' );
end;

end.

