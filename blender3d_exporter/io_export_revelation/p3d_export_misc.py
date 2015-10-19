import mathutils

def ExportLight(Config, Object):
    print("Exporting Light")
    Lamp = Object.data #get correct type of lamp
    lampEl = Config.DocStack[ -1 ]
    lampEl.attrib['type'] = str( Lamp.type.lower())
    dir(Object)
    if Lamp.type != 'SUN':
        lampEl.attrib[ 'position' ] = str( "{:6f}, {:6f}, {:6f}".format( *Object.location ))
    if Lamp.type != 'POINT':
        loc, rot, scale = Object.matrix_world.decompose()
        rotm = rot.to_matrix()
        lampEl.attrib[ 'direction' ] = str( "{:6f}, {:6f}, {:6f}".format( *(rotm[ 2 ])))
        print( 'sun' )
    lampEl.attrib[ 'color' ] = str( "{:6f}, {:6f}, {:6f}".format( *Lamp.color ))
    lampEl.attrib[ 'energy' ] = str( Lamp.energy )    
    
def ExportCamera(Config, Object):
    print("Exporting Camera")