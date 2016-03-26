import mathutils
from . p3d_helper import *

def ExportObject(Config, Object):
    WriteMatrix(Config,Object.matrix_world);
    Config.DocStack[ -1 ].attrib["data"] = str( Object.data.name )
    Config.DocStack[ -1 ].attrib["type"] = str( Object.type ).lower()
    
def ExportLights(Config):
    global globalLamps
    for light in globalLamps: 
        objEl = et.Element("light")
        Config.DocStack[ -1 ].append( objEl )
        Config.DocStack.append( objEl )
        objEl.attrib['name'] = LegalName( light.name )  
        ExportLight(Config,light)

        Config.DocStack.pop()

def ExportLight(Config, Lamp):
    print("Exporting Light")
    lampEl = Config.DocStack[ -1 ]
    lampEl.attrib['type'] = str( Lamp.type.lower())
    #dir(Object)
    #if Lamp.type != 'SUN':
    #    lampEl.attrib[ 'position' ] = str( "{:6f}, {:6f}, {:6f}".format( *Object.location ))
    #if Lamp.type != 'POINT':
    #    loc, rot, scale = Object.matrix_local.decompose()
    #    rotm = rot.to_matrix()
    #    lampEl.attrib[ 'direction' ] = str( "{:6f}, {:6f}, {:6f}".format( *(-rotm[ 2 ])))
    #    print( 'sun' )
    lampEl.attrib[ 'color' ] = str( "{:6f}, {:6f}, {:6f}".format( *Lamp.color ))
    lampEl.attrib[ 'energy' ] = str( Lamp.energy ) 
    
def ExportCameras(Config):
    global globalCameras
    for cam in globalCameras: 
        objEl = et.Element("camera")
        Config.DocStack[ -1 ].append( objEl )
        Config.DocStack.append( objEl )
        objEl.attrib['name'] = LegalName( cam.name )
        ExportCamera(Config,cam)

        Config.DocStack.pop()
    
def ExportCamera(Config, Cam):
    print("Exporting Camera")
    lampEl = Config.DocStack[ -1 ]
    lampEl.attrib['near'] = str( Cam.clip_start )
    lampEl.attrib['far'] = str( Cam.clip_end )
    lampEl.attrib['fov'] = str( Cam.angle )    