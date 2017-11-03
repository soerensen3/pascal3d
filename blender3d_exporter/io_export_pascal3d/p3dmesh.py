from . import p3ddatablock, p3dexporthelper
import bpy, struct

class P3DMesh( p3ddatablock.P3DDataBlock ):
    def ExportPositions( self, block, root ):
        pos = root.BinFile.getposition()
        root.BinFile.writeint( len( block.vertices ))
        for vertex in block.vertices:
            root.BinFile.writevec( vertex.co )
        return pos

    def ExportNormals( self, block, root ):
        pos = root.BinFile.getposition()
        block.calc_normals_split()
        root.BinFile.writeint( len( block.loops ))
        for l in block.loops:
            root.BinFile.writevec( l.normal )
        return pos

    def ExportTangents( self, block, root ):
        pos = root.BinFile.getposition()
        block.calc_tangents()
        root.BinFile.writeint( len( block.loops ))
        for l in block.loops:
            root.BinFile.writevec( l.tangent )
        return pos

    def ExportCotangents( self, block, root ):
        pos = root.BinFile.getposition()
        root.BinFile.writeint( len( block.loops ))
        for l in block.loops:
            root.BinFile.writevec( l.bitangent )
        return pos

    def ExportLoops( self, block, root ):
        pos = root.BinFile.getposition()
        root.BinFile.writeint( len( block.loops ))
        for l in block.loops:
            root.BinFile.writeint( l.vertex_index ) #l.edge_index )
        return pos

    def ExportEdges( self, block, root ):
        pos = root.BinFile.getposition()
        root.BinFile.writeint( len( block.loops ) * 2 )
        for l in block.loops:
            root.BinFile.writeint( block.edges[ l.edge_index ].vertices[ 0 ])
            root.BinFile.writeint( block.edges[ l.edge_index ].vertices[ 1 ])
        return pos

    def ExportUVs( self, block, root, index ):
        pos = root.BinFile.getposition()
        root.BinFile.writeint( len( block.uv_layers[ index ].data ))
        for uvloop in block.uv_layers[ index ].data:
            root.BinFile.writevec([ uvloop.uv[ 0 ], 1 - uvloop.uv[ 1 ]])
        return pos

    def ExportVertexGroups( self, obj ):
        armature = obj.find_armature()
        if ( armature is None ):
            baseindex = 0
        else:
            baseindex = len( armature.data.bones )

        grps = []
        for vgroup in obj.vertex_groups:
            n = baseindex + vgroup.index
            if ( armature ):
               n = armature.data.bones.find( vgroup.name )
            grps.append({ 'Index': n, 'Name': vgroup.name })
        return grps

    def ExportWeights( self, block, root, groups ):
        weightpos = root.BinFile.getposition()
        bin = b''
        root.BinFile.writeint( len( block.vertices ))

        for vertex in block.vertices: #make dictionary of all groups
            vgrps = { vgroup.group: vgroup.weight for vgroup in vertex.groups }
            from operator import itemgetter
            srt = sorted( vgrps.items(), key=itemgetter( 1 ), reverse=True )

            vec = [ 0, 0, 0, 0 ] # make sure the length of the vecs is always 4
            idx = [ 0, 0, 0, 0 ] # fill with zero indices, weight will be zero if idx not used
            for i in range( 0, min( 4, len( vgrps ))):
                vec[ i ] = srt[ i ][ 1 ]
                idx[ i ] = groups[ srt[ i ][ 0 ]][ 'Index' ]
            lenManh = vec[ 0 ] + vec[ 1 ] + vec[ 2 ] + vec[ 3 ]
            if ( lenManh ):
                vec[:] = [ n / lenManh for n in vec ] #scale vector by 1/manhattan distance

            #if ( len( vgrps ) > 4 ):
            #    self.report({ 'INFO' }, 'vertexweights: [{:2f},{:2f},{:2f},{:2f}]'.format( *vec ) + ' indices: [{:d},{:d},{:d},{:d}]'.format( *idx ))
            root.BinFile.writevec(( vec[ 0 ], vec[ 1 ], vec[ 2 ])) # we only need the first 3 weights as the last can be calculated as 1-other weights
            bin += struct.pack( '4i', *idx ) #write indices separately from weights

        indexpos = root.BinFile.getposition()
        root.BinFile.writeint( len( block.vertices ))
        root.BinFile.file.write( bin )

        return '@' + str( weightpos ), '@' + str( indexpos )

    def ExportFaces( self, block, root ):
        totno = 0
        mesh = block
        pos = root.BinFile.getposition()
        root.BinFile.writeint( len( block.polygons ))
        for polygon in block.polygons:
            bin = struct.pack( 'i', polygon.loop_start )
            root.BinFile.file.write( bin )
            totno += 1
        return pos

    def ExportModifiers( self, block, root, obj ):
        pass

    def __init__( self, block, root = None, path='', obj = None ):
        self.Name = block.name
        super().__init__( block, root, p3dexporthelper.indexedprop.format( 'Meshes', self.Name ), obj )
        self.ClassName = 'TP3DMesh'
        if ( block.materials ):
            bpy.ops.object.mode_set(mode='OBJECT')
            root.createBinFile()
            print( repr( root.BinFile ))

            arm = obj.find_armature()
            if arm and root.Exporter.ExportArmatures and hasattr( arm.data, 'pose_position' ):
                pose = arm.data.pose_position
                arm.data.pose_position = 'REST'
                root.ActiveScene.update()
            else:
                arm = None
                root.Exporter.report({ 'INFO' }, 'Mesh "{}" does not have a pose_position'.format( block.name ))

            if root.Exporter.ApplyModifiers:
                mesh = obj.to_mesh( bpy.context.scene, True, 'PREVIEW', True )
            else:
                mesh = obj.to_mesh( bpy.context.scene, False, 'PREVIEW', True )

            self.PackedPositions = '@' + str( self.ExportPositions( mesh, root ))
            self.PackedNormals = '@' + str( self.ExportNormals( mesh, root ))
            self.Loops = '@' + str( self.ExportLoops( mesh, root ))
            self.Edges = '@' + str( self.ExportEdges( mesh, root ))
            self.PackedFaces = '@' + str( self.ExportFaces( mesh, root ))
            if ( mesh.uv_layers ):
                self.PackedTangents = '@' + str( self.ExportTangents( mesh, root ))
                self.PackedCotangents = '@' + str( self.ExportCotangents( mesh, root ))
                self.TexCoords = []
                index = 0
                for uv in mesh.uv_layers:
                    self.TexCoords.append( '@' + str( self.ExportUVs( mesh, root, index )))
                    index += 1
            if ( obj ):
                grps = self.ExportVertexGroups( obj )
                if ( grps ):
                    self.WeightGroups = grps
                    self.PackedVertexWeights, self.PackedVertexWeightIndices = self.ExportWeights( mesh, root, self.WeightGroups )
            if ( len( block.materials ) > 1 ):
                root.Exporter.report({ 'WARNING' }, 'Mesh "{}" has multiple materials. This is not supported by the exporter yet. Only the first material is exported for the whole mesh. Please separate the mesh by materials'.format( block.name ))
            self.PackedMaterialGroups = [ { "PolyStart": 0, "PolyEnd": len( mesh.polygons ) - 1,  "Material": p3dexporthelper.export_data_path( block.materials[ 0 ], root, block )}]
            self.ExportModifiers( block, root, obj )
            bpy.data.meshes.remove( mesh )
            if arm:
                arm.data.pose_position = pose
                root.ActiveScene.update()
        else:
            root.Exporter.report({ 'ERROR' }, 'Mesh "{}" does not have a material'.format( block.name ))

    @staticmethod
    def find_storage( root ):
        return root.Meshes

p3dexporthelper.dict_export_class[ bpy.types.Mesh ] = P3DMesh
