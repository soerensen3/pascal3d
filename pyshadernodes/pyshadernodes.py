class Node:
    inputs = []

    def as_type( self, defs, socket, typecast="constructor" ):
        self.inputs[ "inject" ] = socket
        self.inputs[ "inject" ].type = text
        return getattr( self, typecast )( defs )


class text ( Node ):
    pass


class Socket:
    connected = None
    visible = True
    value = None
    type = "text"
    def constructor( self, defs ):
        return( self.value )

    def getoutput( self, defs ):
        if self.connected:
            if self.type == self.connected.type:
                return self.connected( defs )
            else:
                if self.type == text:
                    return self.connected.value
                else:
                    return self.type().as_type( defs, self, self.connected.type.__name__ )
        else:
            if self.type == text:
                return str( self.value )
            else:
                return self.type().as_type( defs, self )

    def __init__( self, params ):
        if ( "connected" in params ):
            self.connected = params[ "connected" ]
        if ( "visible" in params ):
            self.visible = params[ "visible" ]
        if ( "value" in params ):
            self.value = params[ "value" ]
        if ( "type" in params ):
            self.type = params[ "type" ]



class vec4 ( Node ):
    inputs = {
      "inject": Socket({"value":"", "type":text})
    }

    def constructor( self, defs ):
        if ( "assign" in defs ):
            return (
              "vec4( " + self.inputs[ "inject" ].getoutput( defs ) + " )"
            )
    def vec4( self, defs ):
        if ( "assign" in defs ):
            return (
              self.inputs[ "inject" ].getoutput( defs )
            )

    def vec3( self, defs ):
        if ( "assign" in defs ):
            return (
              self.inputs[ "inject" ].getoutput( defs ) + ".xyz"
            )

    def vec2( self, defs ):
        if ( "assign" in defs ):
            return (
              self.inputs[ "inject" ].getoutput( defs ) + ".xy"
            )

    def float( self, defs ):
        if ( "assign" in defs ):
            return (
              self.inputs[ "inject" ].getoutput( defs ) + ".x"
            )

    def int( self, defs ):
        if ( "assign" in defs ):
            return (
              "round( " + self.inputs[ "inject" ].getoutput( defs ) + ".x )"
            )

class output( Node ):
    inputs = {
      "Color": Socket({"value":"", "type":vec4, "value":[0.0, 1.0, 2.0, 3.0]})
    }
    def fshader( self, defs ):
        return (
            '#version 120' +
            'void main(){' +
            '  gl_FragColor = ' + self.inputs[ "Color" ].getoutput( defs + ["assign"] ) + ';' +
            '}')

class dummy( Node ):
    def Color( self, defs ):
        return ( '4.0,3.0,2.0' )

def debug():
    o = output()
    v = vec4()
    o.inputs["Color"].connected = vec4().inputs["inject"]
    print( o.fshader([ "assign" ]))
