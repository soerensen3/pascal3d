unit p3dnodes;

//PSEUDO CODE

interface

type
  TP3DNodeSocket = class  //Sockets
    property SocketType: TP3DNodeSocketType;
{ - Can be of various types (Int, Float, Vector, Color) 
    The types must be registered the NodeTree
  - Same types can be connected but also some different types 
    can be connected like vector and color.
}    
    property Name: String;
    procedure AcceptConnection( const Accept: Boolean = False );
{ - Procedure to check if socket connection is accepted  }
  end;
  
  TP3DSocketList = List of TP3DNodeSocket;

  TP3DNode = class //Nodes
    property Inputs: TP3DSocketList;
    property Outputs: TP3DSocketList;
//- Sockets for Inputs and Outputs
    property Name: String;
  end;

  TP3DNodeList = List of TP3DNode;

  TP3DNodeTree = class( TP3DNode ) //NodeTree 
    property Nodes: TP3DNodeList; // List of all the nodes.
    //NodeTree is also the class to manage the creation and
    //destruction of all the nodes
{ - Technically the outputs of all nodes are strings which are 
    merged by NodeTree
  - Multiple inputs/outputs
  - Outputs for ShaderNodes are the shaders text files
  - Contains different sections (Output Names), the Node Tree will look for 
    the section in connected nodes
}
  end;
