unit shadernodesunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterAny, SynMemo,
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
    procedure LinkTargetChange( Sender: TObject );
    procedure InlineTextChange( Sender: TObject );
  private
    { private declarations }
  public
    procedure CreateBuffer( N: TP3DShaderNode );
    procedure ClearBuffers;
    procedure UpdateBuffers;
    procedure LoadLibraries;
    { public declarations }
  end;


var
  Form1: TForm1;
  eds: TList;
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
  eds:= TList.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ClearBuffers;
  NL.Free;
  eds.Free;
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

procedure TForm1.LinkTargetChange(Sender: TObject);
var
  cmb: TComboBox;
  N: TP3DShaderNode;
  ed: TSynEdit;
  L: TP3DShaderNodeFragmentLink;
begin
  if ( Sender is TSynEdit ) then
    begin
      TP3DShaderNodeFragmentLink( TSynEdit( Sender ).Tag ).Target:= Trim( TSynEdit( Sender ).Text );
      UpdateBuffers;
    end
  else
    begin
      cmb:= TComboBox( Sender );
      if ( Pointer( cmb.Tag ) <> nil ) then
        begin
          L:= TP3DShaderNodeFragmentLink( cmb.Tag );
          if ( L.List ) then
            begin
              if ( L.Target > '' ) then
                L.Target:=
                  Trim( L.Target + ';' + cmb.Text )
              else
                L.Target:= Trim( cmb.Text );
              cmb.Text:= '';
              ed:= TSynEdit( cmb.Parent.FindComponent( cmb.Name + '_ed' ));
              if ( Assigned( ed )) then
                ed.Text:= L.Target;
            end
          else
            L.Target:= cmb.Text;
          UpdateBuffers;
        end;
    end;
end;

procedure TForm1.InlineTextChange(Sender: TObject);
var
  syn: TSynEdit;
  N: TP3DShaderNode;
begin
  syn:= TSynEdit( Sender );
  if ( Pointer( syn.Tag ) <> nil ) then
    begin
      TP3DShaderNodeFragmentInline( syn.Tag ).Text:= syn.Text;
      UpdateBuffers;
    end;
end;

procedure TForm1.CreateBuffer(N: TP3DShaderNode);
var
  sheet: TTabSheet;
  scrollbox: TScrollBox;
  ed: TSynEdit;
  sp: TSplitter;
  lv: TTreeView;
  Indent: Integer;
  cnt: Integer;
  inp: TP3DShaderNodeInput;

  procedure AddList( Base: TTreeNode; List: TP3DShaderNodeFragmentList );
  var
    frag: TP3DShaderNodeFragment;
  begin
    for frag in List do
      {if ( frag is TP3DShaderNodeVariableInput ) then
        AddList(
          lv.Items.AddChild( Base, Format( 'Input: %s (type: "%s", required: %d)',
            [ TP3DShaderNodeVariableInput( frag ).Name,
              TP3DShaderNodeVariableInput( frag ).VarType,
              Ord( TP3DShaderNodeVariableInput( frag ).Required )])), TP3DShaderNodeVariableInput( frag ).Fragments )
      else }if ( frag is TP3DShaderNodeFragmentLink ) then
        AddList(
        lv.Items.AddChild( Base, Format( 'Link: %s (type: "%s")',
          [ TP3DShaderNodeFragmentLink( frag ).Target,
            TP3DShaderNodeFragmentLink( frag ).VarType ])), TP3DShaderNodeFragmentLink( frag ).Fragments )
  end;


  procedure AddCtrl( Ctrl: TControl; Parent: TWinControl);
  begin
    if ( Parent = nil ) then
      Parent:= scrollbox;

    Ctrl.Parent:= Parent;
    Ctrl.Top:= Parent.Height + 1000;
    Ctrl.Align:= alTop;
    Ctrl.BorderSpacing.Around:= 5;
  end;

  function AddGroup( Name: String; Parent: TWinControl  ): TGroupBox;
  begin
    Result:= TGroupBox.Create( Parent );
    Result.Caption:= Name;
    Result.AutoSize:= True;

    AddCtrl( Result, Parent );
    Result.BorderSpacing.Left:= 20 + Indent * 20;  ;
  end;


  procedure AddFragments( Parent: TWinControl; Fragments: TP3DShaderNodeFragmentList );
    procedure AddInline( Parent: TWinControl; I: TP3DShaderNodeFragmentInline );
    var
      grp: TGroupBox;
      ctrl: TSynEdit;
    begin
      if ( I.Text = LineEnding ) then
        begin
          grp:= AddGroup( IntToStr( cnt ) + '# <br/>', Parent );
          grp.Color:= $AA8080;
        end
      else
        begin
          grp:= AddGroup( IntToStr( cnt ) + '# inline', Parent );
          grp.Color:= $BB8080;
          ctrl:= TSynEdit.Create( grp );
          ctrl.Options:= ctrl.Options + [ eoShowSpecialChars ];
          ctrl.Text:= I.Text;
          ctrl.Highlighter:= Form1.SynAnySyn1;
          ctrl.Tag:= PtrInt( I );
          ctrl.OnChange:= @Form1.InlineTextChange;
          AddCtrl( ctrl, grp );
        end;
    end;

    procedure AddLink( Parent: TWinControl; I: TP3DShaderNodeFragmentLink );
    var
      grp: TGroupBox;
      ctrl: TComboBox;
      ed: TSynEdit;

      procedure FillComboBox;
      var
        Node: TP3DShaderNode;
      begin
        for Node in P3DShaderLib do
          ctrl.Items.Add( Node.Module + '.' + Node.Name );
      end;

    begin
      grp:= AddGroup( IntToStr( cnt ) + '# link', Parent );
      grp.Color:= $80BB80;
      grp.Tag:= PtrInt( N );

      ctrl:= TComboBox.Create( grp );
      ctrl.Text:= I.Target;
      ctrl.Tag:= PtrInt( I );
      ctrl.OnChange:= @Form1.LinkTargetChange;
      FillComboBox;
      AddCtrl( ctrl, grp );

      if ( I.List ) then
        begin
          ed:= TSynEdit.Create( grp );
          ed.Options:= ed.Options + [ eoShowSpecialChars ];
          ed.Text:= I.Target;
          ed.Highlighter:= Form1.SynAnySyn1;
          ed.Tag:= PtrInt( I );
          ed.OnChange:= @Form1.LinkTargetChange;
          ed.Name:= ctrl.Name + '_ed';
          AddCtrl( ed, grp );
        end;
      Inc( Indent );
      AddFragments( Parent, I.Fragments );
      Dec( Indent );
    end;

  var
    frag: TP3DShaderNodeFragment;
  begin
    if ( Parent = nil ) then
      Parent:= scrollbox;

    for frag in Fragments do
      begin
        Inc( cnt );
        case frag.ClassName of
          'TP3DShaderNodeFragmentInline':
            AddInline( Parent, TP3DShaderNodeFragmentInline( frag ));

          'TP3DShaderNodeFragmentLink':
            AddLink( Parent, TP3DShaderNodeFragmentLink( frag ));
        end;
      end;
  end;

begin
  Indent:= 0;
  cnt:= 0;
  sheet:= PageControl1.AddTabSheet;
  PageControl1.ActivePage:= sheet;
  sheet.Caption:= N.Name;
  sheet.Tag:= PtrInt( N );
  scrollbox:= TScrollBox.Create( sheet );
  scrollbox.Parent:= sheet;
  scrollbox.Align:= alClient;
  scrollbox.Color:= clBlack;
  AddFragments( scrollbox, N.Fragments );
  ed:= TSynEdit.Create( sheet );
  ed.Parent:= sheet;
  ed.Text:= N.GetCode();
  ed.Align:= alBottom;
  ed.Highlighter:= SynAnySyn1;
  eds.Add( ed );
  lv:= TTreeView.Create( sheet );
  lv.Parent:= sheet;
  lv.Align:= alRight;
  for inp in N.Inputs do
    lv.Items.AddChild( nil, Format( 'Input: %s (type: "%s" target: "%S")',
          [ inp.Name, inp.VarType, inp.Target ]));
  AddList( nil, N.Fragments );
  sp:= TSplitter.Create( sheet );
  sp.Parent:= sheet;
  sp.Align:= alBottom;
end;

procedure TForm1.ClearBuffers;
var
  i: Integer;
begin
  eds.Clear;
  for i:= PageControl1.PageCount - 1 downto 0 do
    PageControl1.Pages[ i ].Free;
  NL.Clear();
end;

procedure TForm1.UpdateBuffers;
var
  i: Integer;
begin
  for i:= 0 to NL.Count - 1 do
    TSynEdit( eds[ i ]).Text:= NL[ i ].GetCode( nil );
end;

procedure TForm1.LoadLibraries;
begin
  P3DShaderLib.Clear();
  P3DShaderLib.LoadLibraryPath( '../../engine_runtime/shaders/nodes/core/', '*.xml' );
end;

end.

