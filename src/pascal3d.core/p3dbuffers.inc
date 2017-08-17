{$IFDEF INTERFACE}


  const //predefined attribute locations (https://www.opengl.org/sdk/docs/tutorials/ClockworkCoders/attributes.php)
    P3DAttribPosition           = 0;
    P3DAttribNormal             = 2;
    P3DAttribColor              = 3;
    P3DAttribCotangent          = 6;
    P3DAttribTangent            = 7;
{    P3DAttribTexCoord0          = 8;
    P3DAttribTexCoord1          = 9;
    P3DAttribTexCoord2          = 10;
    P3DAttribTexCoord3          = 11;
    P3DAttribTexCoord4          = 12;
    P3DAttribTexCoord5          = 13;
    P3DAttribTexCoord6          = 14;
    P3DAttribTexCoord7          = 15;}


  type

    { TP3DBufferObjectGL }
    IP3DAttribute = interface
      ['{29888C08-513D-4EFA-B916-6C50DDF240B8}']

      procedure UnsetAttribArray();
      procedure SetAttribArray( idx: Integer );
      procedure Free;
    end;

    { TP3DAttributeList }

    TP3DAttributeList = class ( specialize gP3DCustomObjectList < IP3DAttribute > )
      procedure DisableAllAttributes;
    end;

    { TP3DVertexBufferArray }

    TP3DVertexBufferArray = class ( TPersistent )
      private
        VBA: GLuint;

      public
        constructor Create;
        destructor Destroy; override;

        procedure Bind;
        procedure Unbind;
    end;

    {$MACRO ON}

    {$DEFINE USE_ATTRIB}

    {$DEFINE GLTYPE:= GL_INT}
    {$DEFINE GLNUMCOMPONENTS:= 1}
    {$DEFINE TItemList:= TIntList}
    {$DEFINE TP3DBufferObjectGL:= TP3DIntBufferGL}
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}

    {$DEFINE GLTYPE:= GL_FLOAT}
    {$DEFINE TItemList:= TFloatList}
    {$DEFINE TP3DBufferObjectGL:= TP3DFloatBufferGL}
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}

    {$DEFINE GLNUMCOMPONENTS:= 2}
    {$DEFINE TItemList:= TVec2List}
    {$DEFINE TP3DBufferObjectGL:= TP3DVec2BufferGL}
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}

    {$DEFINE GLNUMCOMPONENTS:= 3}
    {$DEFINE TItemList:= TVec3List}
    {$DEFINE TP3DBufferObjectGL:= TP3DVec3BufferGL}
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}

    {$DEFINE GLNUMCOMPONENTS:= 4}
    {$DEFINE TItemList:= TVec4List}
    {$DEFINE TP3DBufferObjectGL:= TP3DVec4BufferGL}
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}

    {$DEFINE GLNUMCOMPONENTS:= 2}
    {$DEFINE GLTYPE:= GL_INT}
    {$DEFINE TItemList:= TIVec2List}
    {$DEFINE TP3DBufferObjectGL:= TP3DIVec2BufferGL}
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}

    {$DEFINE GLNUMCOMPONENTS:= 3}
    {$DEFINE TItemList:= TIVec3List}
    {$DEFINE TP3DBufferObjectGL:= TP3DIVec3BufferGL}
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}

    {$DEFINE GLNUMCOMPONENTS:= 4}
    {$DEFINE TItemList:= TIVec4List}
    {$DEFINE TP3DBufferObjectGL:= TP3DIVec4BufferGL}
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}


    {$UNDEF USE_ATTRIB}

    {$DEFINE GLTYPE:= GL_FLOAT}
    {$DEFINE TItemList:= TMat2List}
    {$DEFINE TP3DBufferObjectGL:= TP3DMat2BufferGL}
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}

    {$DEFINE TItemList:= TMat3List}
    {$DEFINE TP3DBufferObjectGL:= TP3DMat3BufferGL}
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}

    {$DEFINE TItemList:= TMat4List}
    {$DEFINE TP3DBufferObjectGL:= TP3DMat4BufferGL}
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}

    TP3DIntBufferGLList = specialize gP3DCustomObjectList < TP3DIntBufferGL >;
    TP3DFloatBufferGLList = specialize gP3DCustomObjectList < TP3DFloatBufferGL >;
    TP3DVec2BufferGLList = specialize gP3DCustomObjectList < TP3DVec2BufferGL >;
    TP3DVec3BufferGLList = specialize gP3DCustomObjectList < TP3DVec3BufferGL >;
    TP3DVec4BufferGLList = specialize gP3DCustomObjectList < TP3DVec4BufferGL >;
    TP3DIVec2BufferGLList = specialize gP3DCustomObjectList < TP3DIVec2BufferGL >;
    TP3DIVec3BufferGLList = specialize gP3DCustomObjectList < TP3DIVec3BufferGL >;
    TP3DIVec4BufferGLList = specialize gP3DCustomObjectList < TP3DIVec4BufferGL >;
    TP3DMat2BufferGLList = specialize gP3DCustomObjectList < TP3DMat2BufferGL >;
    TP3DMat3BufferGLList = specialize gP3DCustomObjectList < TP3DMat3BufferGL >;
    TP3DMat4BufferGLList = specialize gP3DCustomObjectList < TP3DMat4BufferGL >;

    { gP3DListPropertyAccessBufferGL }

    generic gP3DListPropertyAccessBufferGL  < TListProp, TListData, TPropType > = class ( specialize gP3DListPropertyAccess < TListProp, TListData, TPropType >)
      { GetArrayCount is now performing a check if Value is assigned before access }
      function GetArrayCount: Integer; override;
      { GetValues is now performing a check if Value is assigned before access }
      function GetValues(idx: Integer): TListData; override;
      { SetValues is now performing a check if Value is assigned before access }
      procedure SetValues(idx: Integer; AValue: TListData); override;
      { SetArrayCount is attempting to set Value.Count after performing a check if Value is assigned. }
      procedure SetArrayCount(AValue: Integer); override;
      { ValueCreateNew is now performing a check if Value is assigned and the value will automatically be freed }
      procedure ValueCreateNew( AContext: TP3DJSONContext ); virtual;
      procedure LoadFromStream(S: TStream); override;
    end;

    { gP3DListPropertyAccessBufferGLList }

    generic gP3DListPropertyAccessBufferGLList  < TListProp, TListData, TPropType > = class ( specialize gP3DListPropertyAccess < TListProp, TListData, TPropType >)
      public
        procedure SetArrayCount(AValue: Integer); override;
        procedure ValueCreateNew( AContext: TP3DJSONContext ); virtual;
        procedure LoadItemFromJSONContext(AIndex: Integer; AContext: TP3DJSONContext); override;
        procedure LoadFromStream(S: TStream); override;
    end;

    TP3DPropertyAccessIntBufferGL   = specialize gP3DListPropertyAccessBufferGL < TP3DIntBufferGL, Integer, TP3DPropertyAccessInt >;
    TP3DPropertyAccessFloatBufferGL = specialize gP3DListPropertyAccessBufferGL < TP3DFloatBufferGL, Float, TP3DPropertyAccessFloat >;
    TP3DPropertyAccessVec2BufferGL  = specialize gP3DListPropertyAccessBufferGL < TP3DVec2BufferGL, TVec2, TP3DPropertyAccessVec2 >;
    TP3DPropertyAccessVec3BufferGL  = specialize gP3DListPropertyAccessBufferGL < TP3DVec3BufferGL, TVec3, TP3DPropertyAccessVec3 >;
    TP3DPropertyAccessVec4BufferGL  = specialize gP3DListPropertyAccessBufferGL < TP3DVec4BufferGL, TVec4, TP3DPropertyAccessVec4 >;
    TP3DPropertyAccessIVec2BufferGL = specialize gP3DListPropertyAccessBufferGL < TP3DIVec2BufferGL, TIVec2, TP3DPropertyAccessIVec2 >;
    TP3DPropertyAccessIVec3BufferGL = specialize gP3DListPropertyAccessBufferGL < TP3DIVec3BufferGL, TIVec3, TP3DPropertyAccessIVec3 >;
    TP3DPropertyAccessIVec4BufferGL = specialize gP3DListPropertyAccessBufferGL < TP3DIVec4BufferGL, TIVec4, TP3DPropertyAccessIVec4 >;
//    TP3DPropertyAccessMat2BufferGL = specialize gP3DListPropertyAccessBufferGL < TP3DMat2BufferGL, TMat2, TP3DPropertyAccessMat2 >;
//    TP3DPropertyAccessMat3BufferGL = specialize gP3DListPropertyAccessBufferGL < TP3DMat3BufferGL, TMat3, TP3DPropertyAccessMat3 >;
//    TP3DPropertyAccessMat4BufferGL = specialize gP3DListPropertyAccessBufferGL < TP3DMat4BufferGL, TMat4, TP3DPropertyAccessMat4 >;

    TP3DPropertyAccessIntBufferGLList   = specialize gP3DListPropertyAccessBufferGLList < TP3DIntBufferGLList, TP3DIntBufferGL, TP3DPropertyAccessIntBufferGL >;
    TP3DPropertyAccessFloatBufferGLList = specialize gP3DListPropertyAccessBufferGLList < TP3DFloatBufferGLList, TP3DFloatBufferGL, TP3DPropertyAccessFloatBufferGL >;
    TP3DPropertyAccessVec2BufferGLList  = specialize gP3DListPropertyAccessBufferGLList < TP3DVec2BufferGLList, TP3DVec2BufferGL, TP3DPropertyAccessVec2BufferGL >;
    TP3DPropertyAccessVec3BufferGLList  = specialize gP3DListPropertyAccessBufferGLList < TP3DVec3BufferGLList, TP3DVec3BufferGL, TP3DPropertyAccessVec3BufferGL >;
    TP3DPropertyAccessVec4BufferGLList  = specialize gP3DListPropertyAccessBufferGLList < TP3DVec4BufferGLList, TP3DVec4BufferGL, TP3DPropertyAccessVec4BufferGL >;
    TP3DPropertyAccessIVec2BufferGLList = specialize gP3DListPropertyAccessBufferGLList < TP3DIVec2BufferGLList, TP3DIVec2BufferGL, TP3DPropertyAccessIVec2BufferGL >;
    TP3DPropertyAccessIVec3BufferGLList = specialize gP3DListPropertyAccessBufferGLList < TP3DIVec3BufferGLList, TP3DIVec3BufferGL, TP3DPropertyAccessIVec3BufferGL >;
    TP3DPropertyAccessIVec4BufferGLList = specialize gP3DListPropertyAccessBufferGLList < TP3DIVec4BufferGLList, TP3DIVec4BufferGL, TP3DPropertyAccessIVec4BufferGL >;


{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DVertexBufferArray }

constructor TP3DVertexBufferArray.Create;
begin
  glGenVertexArrays( 1, @VBA ); cle( Self );
  //if ( VBA = 0 ) then
  //  glGetError( );
end;

destructor TP3DVertexBufferArray.Destroy;
begin
  glDeleteVertexArrays( 1, @VBA ); cle( Self );
  inherited Destroy;
end;

procedure TP3DVertexBufferArray.Bind;
begin
  glBindVertexArray( VBA ); cle( Self );
end;

procedure TP3DVertexBufferArray.Unbind;
begin
  glBindVertexArray( 0 ); cle( Self );
end;


{ TP3DAttributeList }

procedure TP3DAttributeList.DisableAllAttributes;
var
  i: Integer;
begin
  for i:= Count - 1 downto 0 do
    Items[ i ].UnsetAttribArray();
end;

{ gP3DListPropertyAccessBufferGL }

procedure gP3DListPropertyAccessBufferGLList.SetArrayCount(AValue: Integer);
begin
  if ( Assigned( Value )) then
    Value.Count:= AValue;
end;

procedure gP3DListPropertyAccessBufferGLList.ValueCreateNew(AContext: TP3DJSONContext);
begin
  if ( Assigned( Value )) then
    Value.Free;
  Value:= TItem.Create;
end;

procedure gP3DListPropertyAccessBufferGLList.LoadItemFromJSONContext(AIndex: Integer; AContext: TP3DJSONContext);
begin
  inherited LoadItemFromJSONContext(AIndex, AContext);
end;

procedure gP3DListPropertyAccessBufferGLList.LoadFromStream(S: TStream);
begin
  if ( not Assigned( Value )) then
    ValueCreateNew( P3DJSONContext( nil, nil, nil, nil )); // param is not used anyways
  inherited LoadFromStream(S);
end;

{ gP3DListPropertyAccessBufferGL }

function gP3DListPropertyAccessBufferGL.GetArrayCount: Integer;
begin
  if ( Assigned( Value )) then
    Result:= inherited GetArrayCount
  else
    Result:= 0;
end;

function gP3DListPropertyAccessBufferGL.GetValues(idx: Integer): TListData;
begin
  if ( Assigned( Value )) then
    Result:= TListData( Value.Items[ idx ])
  else
    Result:= Default( TListData );
end;

procedure gP3DListPropertyAccessBufferGL.SetValues(idx: Integer; AValue: TListData);
begin
  if ( Assigned( Value )) then
    Value.Items[ idx ]:= AValue;
end;

procedure gP3DListPropertyAccessBufferGL.SetArrayCount(AValue: Integer);
begin
  if ( Assigned( Value )) then
    Value.Count:= AValue;
end;

procedure gP3DListPropertyAccessBufferGL.ValueCreateNew(AContext: TP3DJSONContext);
begin
  Value:= TItem.Create( True );
end;

procedure gP3DListPropertyAccessBufferGL.LoadFromStream(S: TStream);
begin
  if ( not Assigned( Value )) then
    ValueCreateNew( P3DJSONContext( nil, nil, nil, nil )); // param is not used anyways

  inherited LoadFromStream(S);
end;


  {$DEFINE USE_ATTRIB}

  {$DEFINE GLNUMCOMPONENTS:= 1}
  {$DEFINE TItemList:= TIntList}
  {$DEFINE TP3DBufferObjectGL:= TP3DIntBufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

  {$DEFINE TItemList:= TFloatList}
  {$DEFINE TP3DBufferObjectGL:= TP3DFloatBufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

  {$DEFINE GLNUMCOMPONENTS:= 2}
  {$DEFINE TItemList:= TVec2List}
  {$DEFINE TP3DBufferObjectGL:= TP3DVec2BufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

  {$DEFINE GLNUMCOMPONENTS:= 3}
  {$DEFINE TItemList:= TVec3List}
  {$DEFINE TP3DBufferObjectGL:= TP3DVec3BufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

  {$DEFINE GLNUMCOMPONENTS:= 4}
  {$DEFINE TItemList:= TVec4List}
  {$DEFINE TP3DBufferObjectGL:= TP3DVec4BufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

  {$DEFINE GLNUMCOMPONENTS:= 2}
  {$DEFINE GLTYPE:= GL_INT}
  {$DEFINE TItemList:= TIVec2List}
  {$DEFINE TP3DBufferObjectGL:= TP3DIVec2BufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

  {$DEFINE GLNUMCOMPONENTS:= 3}
  {$DEFINE TItemList:= TIVec3List}
  {$DEFINE TP3DBufferObjectGL:= TP3DIVec3BufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

  {$DEFINE GLNUMCOMPONENTS:= 4}
  {$DEFINE TItemList:= TIVec4List}
  {$DEFINE TP3DBufferObjectGL:= TP3DIVec4BufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

  {$UNDEF USE_ATTRIB}

  {$DEFINE GLTYPE:= GL_FLOAT}
  {$DEFINE TItemList:= TMat2List}
  {$DEFINE TP3DBufferObjectGL:= TP3DMat2BufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

  {$DEFINE TItemList:= TMat3List}
  {$DEFINE TP3DBufferObjectGL:= TP3DMat3BufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

  {$DEFINE TItemList:= TMat4List}
  {$DEFINE TP3DBufferObjectGL:= TP3DMat4BufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}


{$ENDIF}


