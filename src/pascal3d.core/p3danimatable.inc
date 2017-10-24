//p3danimatable.inc
{$IFDEF INTERFACE}
  type
    TP3DAction = class;

    IP3DAnimatable = interface
      ['{E1060711-A697-4A0C-BE21-DD2FB9719CE2}']
      function GetAction: TP3DAction;
      procedure SetAction( AValue: TP3DAction );

      property Action: TP3DAction read GetAction write SetAction;
    end;
{$ENDIF}
