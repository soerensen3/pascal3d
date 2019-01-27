//p3dcollidable.inc

{$IFDEF INTERFACE}
  type
    IP3DCollidable = interface ( IP3DBaseInterface )
      ['{3E4A48D1-471D-49AF-B73D-A60235CDF63D}']

      function CastRay( Orig, Dir: TVec3; AObject: TP3DObject; out HitPos: TVec3 ): Boolean;
      function CollideSphere( Mat: TMat4; ASphere: TP3DSphere; out Intersect: TVec3; out N: TVec3 ): Boolean;
      function CollideAABB( Mat: TMat4; AAABB: TP3DAABB; out Intersect: TVec3; out N: TVec3 ): Boolean;
      function CollidePoint( Mat: TMat4; APoint: TVec3 ): Boolean;
      function GetAABB(): TP3DAABB;
      procedure SetAABB( AValue: TP3DAABB );
      function GetSphere(): TP3DSphere;
      procedure SetSphere( AValue: TP3DSphere );

      function CalcAABB(): TP3DAABB;
      function CalcSphere(): TP3DSphere;

      property AABB: TP3DAABB read GetAABB write SetAABB;
      property Sphere: TP3DSphere read GetSphere write SetSphere;
    end;

{$ENDIF}


