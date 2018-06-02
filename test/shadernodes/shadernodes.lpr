program shadernodes;

uses
  p3d.utils,
  p3d.events,
  p3d.core;

var
  ShaderNodeLib: TP3DShaderNodeLibrary;
  root, test_input1, test_input2: TP3DNodeClone;
  Tree: TP3DShaderNodeTree;
  tmp: TP3DJSONRoot;
  compiled: TP3DShaderCompiled;
  buffer: TP3DShaderBuffer;
begin
  P3DUtilsInit;
  P3DEventsInit;

  tmp:= TP3DJSONRoot.Create();

  ShaderNodeLib:= TP3DShaderNodeLibrary.Create();

  ShaderNodeLib.LoadLibrary( './shaders/test.pmd' );

  Tree:= TP3DShaderNodeTree.Create( 'Tree' );
  tmp.Properties.Add( Tree );
  root:= ShaderNodeLib.FindNode( 'test' ).Clone( Tree );
  test_input1:= ShaderNodeLib.FindNode( 'test_input1' ).Clone( Tree );
  test_input2:= ShaderNodeLib.FindNode( 'test_input2_nested' ).Clone( Tree );
  root.FindInput( 'test1' ).Connected:= test_input1.FindOutput( 'test_output' );
  test_input1.FindInput( 'nested_input' ).Connected:= test_input2.FindOutput( 'test_output' );
  compiled:= Tree.Compile( root );

  for buffer in compiled.Buffers do
    begin
      WriteLn( 'Output of ', buffer.Name );
      WriteLn( '```', buffer.Code, '```' );
    end;
  compiled.Free;
  tmp.Free;
  ShaderNodeLib.Free;
  //P3DCoreInit;
  //P3DCoreFinish;
  P3DEventsFinish;
  P3DUtilsFinish;
end.

