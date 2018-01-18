unit p3dbmpfontfile;

{$mode objfpc}{$H+}
{.$DEFINE DEBUG}

interface

uses
  Classes, SysUtils, p3dbmpfont, p3dtexture, DOM, XMLWrite, XMLRead;


function LoadP3DFont( FName: String ): TP3DFont;
procedure SaveP3DFont( FName: String; Font: TP3DFont );

implementation

function LoadP3DFont(FName: String): TP3DFont;
var
  F: TXMLDocument;
  letter: TP3DFontLetter;
  child: TDOMNode;
  i, j: Integer;
  tex: String;
  txIdx: Integer;
begin
  try
    ReadXMLFile( F, FName );
    Result:= TP3DFont.Create;
    Result.FileName:= FName;
    for i:= 0 to F.DocumentElement.ChildNodes.Count - 1 do
//      if ( F.DocumentElement.ChildNodes[ i ].NodeName = 'letters' ) then
//        for j:= 0 to F.DocumentElement.ChildNodes[ i ].ChildNodes.Count - 1 do
          begin
            child:= F.DocumentElement.ChildNodes[ i ];//.ChildNodes[ j ];

            letter:= TP3DFontLetter.Create;
            Result.Letters.Add( letter );
            letter.Letter:= child.Attributes.GetNamedItem( 'code' ).NodeValue[ 1 ];
            letter.uv1.FromString( child.Attributes.GetNamedItem( 'uv1' ).NodeValue );
            letter.uv2.FromString( child.Attributes.GetNamedItem( 'uv2' ).NodeValue );
            {$IFDEF DEBUG}
            WriteLn( 'letter ''' + letter.Letter + '''' );
            WriteLn( 'uv1 ''' + letter.uv1.ToString() + '''' );
            WriteLn( 'uv2 ''' + letter.uv2.ToString() + '''' );
            {$ENDIF}
            tex:= child.Attributes.GetNamedItem( 'tex' ).NodeValue;
            txIdx:= Result.Textures.Find( tex );
            if ( txIdx > -1 ) then
              letter.Texture:= Result.Textures[ txIdx ]
            else
              begin
                txIdx:= Result.Textures.Add( TP3DTexture.Create( tex ));
                letter.Texture:= Result.Textures[ txIdx ];
              end;
            {$IFDEF DEBUG}
            WriteLn( 'tex ''' + tex + ''' = $' + IntToHex( Integer( Pointer( @letter.Texture )), 16 ));
            {$ENDIF}
          end;
    WriteXML( F, FName );
  finally
    F.Free;
  end;
end;

procedure SaveP3DFont(FName: String; Font: TP3DFont);
var
  F: TXMLDocument;
  root: TDOMElement;
  letter: TP3DFontLetter;
  child: TDOMElement;
  i: Integer;
  tex: TP3DTexture;
begin
  try
    F:= TXMLDocument.Create;
{    root:= F.CreateElement( 'font' );
    F.AppendChild( root );
    for tex in Font.Textures do
      begin
        child:= F.CreateElement( 'texture' );
        root.AppendChild( child );
        child.SetAttribute( 'tex', tex.FileName );
      end;}

    root:= F.CreateElement( 'letters' );
    F.AppendChild( root );
    for i:= 0 to Font.Letters.Count - 1 do
      begin
        letter:= Font.Letters[ i ];
        child:= F.CreateElement( 'letter' );
        root.AppendChild( child );
        child.SetAttribute( 'code', '' + letter.Letter + '' );
        child.SetAttribute( 'uv1', '' + letter.uv1.ToString + '' );
        child.SetAttribute( 'uv2', '' + letter.uv2.ToString + '' );
        if ( Assigned( letter.Texture )) then
          child.SetAttribute( 'tex', letter.Texture.FileName );
      end;
    WriteXML( F, FName );
  finally
    F.Free;
  end;
end;

end.

