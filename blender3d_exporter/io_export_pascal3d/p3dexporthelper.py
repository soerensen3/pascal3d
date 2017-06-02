from . import p3ddata, p3ddatablock

indexedprop = "{}[\"{}\"]" #name["index"]
dict_export_class = {}

def find_export_class( obj ):
    try:
        result = dict_export_class[ type( obj )]
    except KeyError:
        result = p3ddatablock.P3DDataBlock
    return result

def export_data_root( block, root, obj = None ):
    if ( block is None ):
        return None
    data_class = find_export_class( block )
    storage = data_class.find_storage( root )
    if ( storage is None ):
        root.Exporter.report({ 'WARNING' }, 'No storage found for datablock ' + repr( type( block )))
        return
    if ( block in storage ):
        return storage[ block ]
    if ( data_class ):
        data = data_class( block, root, '', obj )
        storage[ block ] = data
        return data
    else:
        return None

def export_data( block ):
    root = p3ddata.P3DData()
    export_data_root( block, root )
    return root

def export_data_path( block, root, obj = None ):
    data = export_data_root( block, root, obj )
    if ( data ):
        return data.DataPath
    else:
        return 'None'
