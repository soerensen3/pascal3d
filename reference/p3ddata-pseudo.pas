TP3DDataBlock.RemoveUser( User: TP3DDataBlock );
  Users.Remove( User )

TP3DDataBlock.AddUser( User: TP3DDataBlock );
  Users.Add( User )

TP3DDataBlock.RemoveFromFields( Block ); virtual;
  //Check if assigned to any fields
  
TP3DData.RemoveFromFields( Block );
  //Remove from datablock and arrays
  Datablocks.Remove( Block )
  //Remove from arrays
  
TP3DData.ClearFields;
  for Obj in Datablocks do
    for User in Obj.Users do
      if ( User.Parent <> Self ) then
        User.RemoveFromFields( Obj )
    Obj.Destroy( JustDestroy:= True ) //Destroy the object without checking fields
  Datablocks.Clear
  Armatures.Clear
  ...


TP3DDataBlock.ClearFields; virtual; //override in descendants
  //Clear all the fields including Objects for Scenes and Children for Actors

TP3DDataBlock.ClearUsers;
  for User in Users do
    RemoveFromFields( Self )
  Users.Clear

TP3DDataBlock.Destroy( const JustDestroy Boolean = False );
  if ( not JustDestroy ) then
    ClearUsers
    Parent.RemoveFromFields( Self )
  
  ClearFields //Need to clear fields to remove connections to other datablocks
    
  inherited Destroy
  
TP3DDataBlock.Create;
  //Register at parent data object
  
TP3DData.Destroy;
  ClearFields
  Datablocks.Free
  Objects.Free
  ...

TP3DActor.SetData, TP3DScene.SetCam, .. ( Value: TP3DDataBlock );
  if ( Assigned( Data )) then
    Data.RemoveUser( Self )
  Data:= Value
  if ( Assigned( Data )) then
    Data.AddUser( Self )

TP3DScene.Objects.OnChange/TP3DActor.Children.OnChange ( Item: TP3DDataBlock; Action: [actAdd, actRemove, actClear]);
  if ( Action = actAdd ) then
    Item.AddUser( Self )
  else 
    Item.RemoveUser( Self )

