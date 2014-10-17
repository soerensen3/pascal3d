{$IFDEF INTERFACE}

private
  {$IF NUMFIELD > 1}
  {@exclude} function GetVec2( AIndex: Integer ): TVec2;
  {$ENDIF}
  {$IF NUMFIELD > 2}
  {@exclude} function GetVec3( AIndex: Integer ): TVec3;
  {$ENDIF}
  {$IF NUMFIELD > 3}
  {@exclude} function GetVec4( AIndex: Integer ): TVec4;
  {$ENDIF}
public
  {$IF NUMFIELD>1}
  {@exclude} property XX: TVec2 index 00 read GetVec2;
  {@exclude} property XY: TVec2 index 01 read GetVec2;
    {$IF NUMFIELD>2}
  {@exclude} property XZ: TVec2 index 02 read GetVec2;
    {$ENDIF}
    {$IF NUMFIELD>3}
  {@exclude} property XW: TVec2 index 03 read GetVec2;
    {$ENDIF}

  {@exclude} property YX: TVec2 index 10 read GetVec2;
  {@exclude} property YY: TVec2 index 11 read GetVec2;
    {$IF NUMFIELD>2}
  {@exclude} property YZ: TVec2 index 12 read GetVec2;
    {$ENDIF}
    {$IF NUMFIELD>3}
  {@exclude} property YW: TVec2 index 13 read GetVec2;
    {$ENDIF}
    {$IF NUMFIELD>2}
  {@exclude} property ZX: TVec2 index 20 read GetVec2;
  {@exclude} property ZY: TVec2 index 21 read GetVec2;
  {@exclude} property ZZ: TVec2 index 22 read GetVec2;
    {$ENDIF}
    {$IF NUMFIELD>3}
  {@exclude} property ZW: TVec2 index 23 read GetVec2;

  {@exclude} property WX: TVec2 index 30 read GetVec2;
  {@exclude} property WY: TVec2 index 31 read GetVec2;
  {@exclude} property WZ: TVec2 index 32 read GetVec2;
  {@exclude} property WW: TVec2 index 33 read GetVec2;
    {$ENDIF}
  {$ENDIF}

  {$IF NUMFIELD>2}
  {@exclude} property XXX: TVec3 index 000 read GetVec3;
  {@exclude} property XXY: TVec3 index 001 read GetVec3;
  {@exclude} property XXZ: TVec3 index 002 read GetVec3;
  {$IF NUMFIELD>3}
  {@exclude} property XXW: TVec3 index 003 read GetVec3;
  {$ENDIF}
  {@exclude} property XYX: TVec3 index 010 read GetVec3;
  {@exclude} property XYY: TVec3 index 011 read GetVec3;
  {@exclude} property XYZ: TVec3 index 012 read GetVec3;
  {$IF NUMFIELD>3}
  {@exclude} property XYW: TVec3 index 013 read GetVec3;
  {$ENDIF}
  {@exclude} property XZX: TVec3 index 020 read GetVec3;
  {@exclude} property XZY: TVec3 index 021 read GetVec3;
  {@exclude} property XZZ: TVec3 index 022 read GetVec3;
  {$IF NUMFIELD>3}
  {@exclude} property XZW: TVec3 index 023 read GetVec3;
  {$ENDIF}

  {@exclude} property YXX: TVec3 index 100 read GetVec3;
  {@exclude} property YXY: TVec3 index 101 read GetVec3;
  {@exclude} property YXZ: TVec3 index 102 read GetVec3;
  {$IF NUMFIELD>3}
  {@exclude} property YXW: TVec3 index 103 read GetVec3;
  {$ENDIF}

  {@exclude} property YYX: TVec3 index 110 read GetVec3;
  {@exclude} property YYY: TVec3 index 111 read GetVec3;
  {@exclude} property YYZ: TVec3 index 112 read GetVec3;
  {$IF NUMFIELD>3}
  {@exclude} property YYW: TVec3 index 113 read GetVec3;
  {$ENDIF}

  {@exclude} property YZX: TVec3 index 120 read GetVec3;
  {@exclude} property YZY: TVec3 index 121 read GetVec3;
  {@exclude} property YZZ: TVec3 index 122 read GetVec3;
  {$IF NUMFIELD>3}
  {@exclude} property YZW: TVec3 index 123 read GetVec3;
  {$ENDIF}

  {@exclude} property ZXX: TVec3 index 200 read GetVec3;
  {@exclude} property ZXY: TVec3 index 201 read GetVec3;
  {@exclude} property ZXZ: TVec3 index 202 read GetVec3;
  {$IF NUMFIELD>3}
  {@exclude} property ZXW: TVec3 index 203 read GetVec3;
  {$ENDIF}

  {@exclude} property ZYX: TVec3 index 210 read GetVec3;
  {@exclude} property ZYY: TVec3 index 211 read GetVec3;
  {@exclude} property ZYZ: TVec3 index 212 read GetVec3;
  {$IF NUMFIELD>3}
   {@exclude} property ZYW: TVec3 index 213 read GetVec3;
  {$ENDIF}

   {@exclude} property ZZX: TVec3 index 220 read GetVec3;
   {@exclude} property ZZY: TVec3 index 221 read GetVec3;
   {@exclude} property ZZZ: TVec3 index 222 read GetVec3;
  {$IF NUMFIELD>3}
   {@exclude} property ZZW: TVec3 index 223 read GetVec3;

   {@exclude} property WXX: TVec3 index 300 read GetVec3;
   {@exclude} property WXY: TVec3 index 301 read GetVec3;
   {@exclude} property WXZ: TVec3 index 302 read GetVec3;
   {@exclude} property WXW: TVec3 index 303 read GetVec3;

   {@exclude} property WYX: TVec3 index 310 read GetVec3;
   {@exclude} property WYY: TVec3 index 311 read GetVec3;
   {@exclude} property WYZ: TVec3 index 312 read GetVec3;
   {@exclude} property WYW: TVec3 index 313 read GetVec3;

   {@exclude} property WZX: TVec3 index 320 read GetVec3;
   {@exclude} property WZY: TVec3 index 321 read GetVec3;
   {@exclude} property WZZ: TVec3 index 322 read GetVec3;
   {@exclude} property WZW: TVec3 index 323 read GetVec3;

   {@exclude} property WWX: TVec3 index 330 read GetVec3;
   {@exclude} property WWY: TVec3 index 331 read GetVec3;
   {@exclude} property WWZ: TVec3 index 332 read GetVec3;
   {@exclude} property WWW: TVec3 index 333 read GetVec3;

   {@exclude} property XXXX: TVec4 index 0000 read GetVec4;
   {@exclude} property XXXY: TVec4 index 0001 read GetVec4;
   {@exclude} property XXXZ: TVec4 index 0002 read GetVec4;
   {@exclude} property XXXW: TVec4 index 0003 read GetVec4;

   {@exclude} property XXYX: TVec4 index 0010 read GetVec4;
   {@exclude} property XXYY: TVec4 index 0011 read GetVec4;
   {@exclude} property XXYZ: TVec4 index 0012 read GetVec4;
   {@exclude} property XXYW: TVec4 index 0013 read GetVec4;

   {@exclude} property XXZX: TVec4 index 0020 read GetVec4;
   {@exclude} property XXZY: TVec4 index 0021 read GetVec4;
   {@exclude} property XXZZ: TVec4 index 0022 read GetVec4;
   {@exclude} property XXZW: TVec4 index 0023 read GetVec4;

   {@exclude} property XXWX: TVec4 index 0030 read GetVec4;
   {@exclude} property XXWY: TVec4 index 0031 read GetVec4;
   {@exclude} property XXWZ: TVec4 index 0032 read GetVec4;
   {@exclude} property XXWW: TVec4 index 0033 read GetVec4;


   {@exclude} property XYXX: TVec4 index 0100 read GetVec4;
   {@exclude} property XYXY: TVec4 index 0101 read GetVec4;
   {@exclude} property XYXZ: TVec4 index 0102 read GetVec4;
   {@exclude} property XYXW: TVec4 index 0103 read GetVec4;

   {@exclude} property XYYX: TVec4 index 0110 read GetVec4;
   {@exclude} property XYYY: TVec4 index 0111 read GetVec4;
   {@exclude} property XYYZ: TVec4 index 0112 read GetVec4;
   {@exclude} property XYYW: TVec4 index 0113 read GetVec4;

   {@exclude} property XYZX: TVec4 index 0120 read GetVec4;
   {@exclude} property XYZY: TVec4 index 0121 read GetVec4;
   {@exclude} property XYZZ: TVec4 index 0122 read GetVec4;
   {@exclude} property XYZW: TVec4 index 0123 read GetVec4;

   {@exclude} property XYWX: TVec4 index 0130 read GetVec4;
   {@exclude} property XYWY: TVec4 index 0131 read GetVec4;
   {@exclude} property XYWZ: TVec4 index 0132 read GetVec4;
   {@exclude} property XYWW: TVec4 index 0133 read GetVec4;


   {@exclude} property XZXX: TVec4 index 0200 read GetVec4;
   {@exclude} property XZXY: TVec4 index 0201 read GetVec4;
   {@exclude} property XZXZ: TVec4 index 0202 read GetVec4;
   {@exclude} property XZXW: TVec4 index 0203 read GetVec4;

   {@exclude} property XZYX: TVec4 index 0210 read GetVec4;
   {@exclude} property XZYY: TVec4 index 0211 read GetVec4;
   {@exclude} property XZYZ: TVec4 index 0212 read GetVec4;
   {@exclude} property XZYW: TVec4 index 0213 read GetVec4;

   {@exclude} property XZZX: TVec4 index 0220 read GetVec4;
   {@exclude} property XZZY: TVec4 index 0221 read GetVec4;
   {@exclude} property XZZZ: TVec4 index 0222 read GetVec4;
   {@exclude} property XZZW: TVec4 index 0223 read GetVec4;

   {@exclude} property XZWX: TVec4 index 0230 read GetVec4;
   {@exclude} property XZWY: TVec4 index 0231 read GetVec4;
   {@exclude} property XZWZ: TVec4 index 0232 read GetVec4;
   {@exclude} property XZWW: TVec4 index 0233 read GetVec4;


   {@exclude} property XWXX: TVec4 index 0300 read GetVec4;
   {@exclude} property XWXY: TVec4 index 0301 read GetVec4;
   {@exclude} property XWXZ: TVec4 index 0302 read GetVec4;
   {@exclude} property XWXW: TVec4 index 0303 read GetVec4;

   {@exclude} property XWYX: TVec4 index 0310 read GetVec4;
   {@exclude} property XWYY: TVec4 index 0311 read GetVec4;
   {@exclude} property XWYZ: TVec4 index 0312 read GetVec4;
   {@exclude} property XWYW: TVec4 index 0313 read GetVec4;

   {@exclude} property XWZX: TVec4 index 0320 read GetVec4;
   {@exclude} property XWZY: TVec4 index 0321 read GetVec4;
   {@exclude} property XWZZ: TVec4 index 0322 read GetVec4;
   {@exclude} property XWZW: TVec4 index 0323 read GetVec4;

   {@exclude} property XWWX: TVec4 index 0330 read GetVec4;
   {@exclude} property XWWY: TVec4 index 0331 read GetVec4;
   {@exclude} property XWWZ: TVec4 index 0332 read GetVec4;
   {@exclude} property XWWW: TVec4 index 0333 read GetVec4;



   {@exclude} property YXXX: TVec4 index 1000 read GetVec4;
   {@exclude} property YXXY: TVec4 index 1001 read GetVec4;
   {@exclude} property YXXZ: TVec4 index 1002 read GetVec4;
   {@exclude} property YXXW: TVec4 index 1003 read GetVec4;

   {@exclude} property YXYX: TVec4 index 1010 read GetVec4;
   {@exclude} property YXYY: TVec4 index 1011 read GetVec4;
   {@exclude} property YXYZ: TVec4 index 1012 read GetVec4;
   {@exclude} property YXYW: TVec4 index 1013 read GetVec4;

   {@exclude} property YXZX: TVec4 index 1020 read GetVec4;
   {@exclude} property YXZY: TVec4 index 1021 read GetVec4;
   {@exclude} property YXZZ: TVec4 index 1022 read GetVec4;
   {@exclude} property YXZW: TVec4 index 1023 read GetVec4;

   {@exclude} property YXWX: TVec4 index 1030 read GetVec4;
   {@exclude} property YXWY: TVec4 index 1031 read GetVec4;
   {@exclude} property YXWZ: TVec4 index 1032 read GetVec4;
   {@exclude} property YXWW: TVec4 index 1033 read GetVec4;


   {@exclude} property YYXX: TVec4 index 1100 read GetVec4;
   {@exclude} property YYXY: TVec4 index 1101 read GetVec4;
   {@exclude} property YYXZ: TVec4 index 1102 read GetVec4;
   {@exclude} property YYXW: TVec4 index 1103 read GetVec4;

   {@exclude} property YYYX: TVec4 index 1110 read GetVec4;
   {@exclude} property YYYY: TVec4 index 1111 read GetVec4;
   {@exclude} property YYYZ: TVec4 index 1112 read GetVec4;
   {@exclude} property YYYW: TVec4 index 1113 read GetVec4;

   {@exclude} property YYZX: TVec4 index 1120 read GetVec4;
   {@exclude} property YYZY: TVec4 index 1121 read GetVec4;
   {@exclude} property YYZZ: TVec4 index 1122 read GetVec4;
   {@exclude} property YYZW: TVec4 index 1123 read GetVec4;

   {@exclude} property YYWX: TVec4 index 1130 read GetVec4;
   {@exclude} property YYWY: TVec4 index 1131 read GetVec4;
   {@exclude} property YYWZ: TVec4 index 1132 read GetVec4;
   {@exclude} property YYWW: TVec4 index 1133 read GetVec4;


   {@exclude} property YZXX: TVec4 index 1200 read GetVec4;
   {@exclude} property YZXY: TVec4 index 1201 read GetVec4;
   {@exclude} property YZXZ: TVec4 index 1202 read GetVec4;
   {@exclude} property YZXW: TVec4 index 1203 read GetVec4;

   {@exclude} property YZYX: TVec4 index 1210 read GetVec4;
   {@exclude} property YZYY: TVec4 index 1211 read GetVec4;
   {@exclude} property YZYZ: TVec4 index 1212 read GetVec4;
   {@exclude} property YZYW: TVec4 index 1213 read GetVec4;

   {@exclude} property YZZX: TVec4 index 1220 read GetVec4;
   {@exclude} property YZZY: TVec4 index 1221 read GetVec4;
   {@exclude} property YZZZ: TVec4 index 1222 read GetVec4;
   {@exclude} property YZZW: TVec4 index 1223 read GetVec4;

   {@exclude} property YZWX: TVec4 index 1230 read GetVec4;
   {@exclude} property YZWY: TVec4 index 1231 read GetVec4;
   {@exclude} property YZWZ: TVec4 index 1232 read GetVec4;
   {@exclude} property YZWW: TVec4 index 1233 read GetVec4;


   {@exclude} property YWXX: TVec4 index 1300 read GetVec4;
   {@exclude} property YWXY: TVec4 index 1301 read GetVec4;
   {@exclude} property YWXZ: TVec4 index 1302 read GetVec4;
   {@exclude} property YWXW: TVec4 index 1303 read GetVec4;

   {@exclude} property YWYX: TVec4 index 1310 read GetVec4;
   {@exclude} property YWYY: TVec4 index 1311 read GetVec4;
   {@exclude} property YWYZ: TVec4 index 1312 read GetVec4;
   {@exclude} property YWYW: TVec4 index 1313 read GetVec4;

   {@exclude} property YWZX: TVec4 index 1320 read GetVec4;
   {@exclude} property YWZY: TVec4 index 1321 read GetVec4;
   {@exclude} property YWZZ: TVec4 index 1322 read GetVec4;
   {@exclude} property YWZW: TVec4 index 1323 read GetVec4;

   {@exclude} property YWWX: TVec4 index 1330 read GetVec4;
   {@exclude} property YWWY: TVec4 index 1331 read GetVec4;
   {@exclude} property YWWZ: TVec4 index 1332 read GetVec4;
   {@exclude} property YWWW: TVec4 index 1333 read GetVec4;

   {@exclude} property ZXXX: TVec4 index 2000 read GetVec4;
   {@exclude} property ZXXY: TVec4 index 2001 read GetVec4;
   {@exclude} property ZXXZ: TVec4 index 2002 read GetVec4;
   {@exclude} property ZXXW: TVec4 index 2003 read GetVec4;

   {@exclude} property ZXYX: TVec4 index 2010 read GetVec4;
   {@exclude} property ZXYY: TVec4 index 2011 read GetVec4;
   {@exclude} property ZXYZ: TVec4 index 2012 read GetVec4;
   {@exclude} property ZXYW: TVec4 index 2013 read GetVec4;

   {@exclude} property ZXZX: TVec4 index 2020 read GetVec4;
   {@exclude} property ZXZY: TVec4 index 2021 read GetVec4;
   {@exclude} property ZXZZ: TVec4 index 2022 read GetVec4;
   {@exclude} property ZXZW: TVec4 index 2023 read GetVec4;

   {@exclude} property ZXWX: TVec4 index 2030 read GetVec4;
   {@exclude} property ZXWY: TVec4 index 2031 read GetVec4;
   {@exclude} property ZXWZ: TVec4 index 2032 read GetVec4;
   {@exclude} property ZXWW: TVec4 index 2033 read GetVec4;


   {@exclude} property ZYXX: TVec4 index 2100 read GetVec4;
   {@exclude} property ZYXY: TVec4 index 2101 read GetVec4;
   {@exclude} property ZYXZ: TVec4 index 2102 read GetVec4;
   {@exclude} property ZYXW: TVec4 index 2103 read GetVec4;

   {@exclude} property ZYYX: TVec4 index 2110 read GetVec4;
   {@exclude} property ZYYY: TVec4 index 2111 read GetVec4;
   {@exclude} property ZYYZ: TVec4 index 2112 read GetVec4;
   {@exclude} property ZYYW: TVec4 index 2113 read GetVec4;

   {@exclude} property ZYZX: TVec4 index 2120 read GetVec4;
   {@exclude} property ZYZY: TVec4 index 2121 read GetVec4;
   {@exclude} property ZYZZ: TVec4 index 2122 read GetVec4;
   {@exclude} property ZYZW: TVec4 index 2123 read GetVec4;

   {@exclude} property ZYWX: TVec4 index 2130 read GetVec4;
   {@exclude} property ZYWY: TVec4 index 2131 read GetVec4;
   {@exclude} property ZYWZ: TVec4 index 2132 read GetVec4;
   {@exclude} property ZYWW: TVec4 index 2133 read GetVec4;


   {@exclude} property ZZXX: TVec4 index 2200 read GetVec4;
   {@exclude} property ZZXY: TVec4 index 2201 read GetVec4;
   {@exclude} property ZZXZ: TVec4 index 2202 read GetVec4;
   {@exclude} property ZZXW: TVec4 index 2203 read GetVec4;

   {@exclude} property ZZYX: TVec4 index 2210 read GetVec4;
   {@exclude} property ZZYY: TVec4 index 2211 read GetVec4;
   {@exclude} property ZZYZ: TVec4 index 2212 read GetVec4;
   {@exclude} property ZZYW: TVec4 index 2213 read GetVec4;

   {@exclude} property ZZZX: TVec4 index 2220 read GetVec4;
   {@exclude} property ZZZY: TVec4 index 2221 read GetVec4;
   {@exclude} property ZZZZ: TVec4 index 2222 read GetVec4;
   {@exclude} property ZZZW: TVec4 index 2223 read GetVec4;

   {@exclude} property ZZWX: TVec4 index 2230 read GetVec4;
   {@exclude} property ZZWY: TVec4 index 2231 read GetVec4;
   {@exclude} property ZZWZ: TVec4 index 2232 read GetVec4;
   {@exclude} property ZZWW: TVec4 index 2233 read GetVec4;


   {@exclude} property ZWXX: TVec4 index 2300 read GetVec4;
   {@exclude} property ZWXY: TVec4 index 2301 read GetVec4;
   {@exclude} property ZWXZ: TVec4 index 2302 read GetVec4;
   {@exclude} property ZWXW: TVec4 index 2303 read GetVec4;

   {@exclude} property ZWYX: TVec4 index 2310 read GetVec4;
   {@exclude} property ZWYY: TVec4 index 2311 read GetVec4;
   {@exclude} property ZWYZ: TVec4 index 2312 read GetVec4;
   {@exclude} property ZWYW: TVec4 index 2313 read GetVec4;

   {@exclude} property ZWZX: TVec4 index 2320 read GetVec4;
   {@exclude} property ZWZY: TVec4 index 2321 read GetVec4;
   {@exclude} property ZWZZ: TVec4 index 2322 read GetVec4;
   {@exclude} property ZWZW: TVec4 index 2323 read GetVec4;

   {@exclude} property ZWWX: TVec4 index 2330 read GetVec4;
   {@exclude} property ZWWY: TVec4 index 2331 read GetVec4;
   {@exclude} property ZWWZ: TVec4 index 2332 read GetVec4;
   {@exclude} property ZWWW: TVec4 index 2333 read GetVec4;

   {@exclude} property WXXX: TVec4 index 3000 read GetVec4;
   {@exclude} property WXXY: TVec4 index 3001 read GetVec4;
   {@exclude} property WXXZ: TVec4 index 3002 read GetVec4;
   {@exclude} property WXXW: TVec4 index 3003 read GetVec4;

   {@exclude} property WXYX: TVec4 index 3010 read GetVec4;
   {@exclude} property WXYY: TVec4 index 3011 read GetVec4;
   {@exclude} property WXYZ: TVec4 index 3012 read GetVec4;
   {@exclude} property WXYW: TVec4 index 3013 read GetVec4;

   {@exclude} property WXZX: TVec4 index 3020 read GetVec4;
   {@exclude} property WXZY: TVec4 index 3021 read GetVec4;
   {@exclude} property WXZZ: TVec4 index 3022 read GetVec4;
   {@exclude} property WXZW: TVec4 index 3023 read GetVec4;

   {@exclude} property WXWX: TVec4 index 3030 read GetVec4;
   {@exclude} property WXWY: TVec4 index 3031 read GetVec4;
   {@exclude} property WXWZ: TVec4 index 3032 read GetVec4;
   {@exclude} property WXWW: TVec4 index 3033 read GetVec4;


   {@exclude} property WYXX: TVec4 index 3100 read GetVec4;
   {@exclude} property WYXY: TVec4 index 3101 read GetVec4;
   {@exclude} property WYXZ: TVec4 index 3102 read GetVec4;
   {@exclude} property WYXW: TVec4 index 3103 read GetVec4;

   {@exclude} property WYYX: TVec4 index 3110 read GetVec4;
   {@exclude} property WYYY: TVec4 index 3111 read GetVec4;
   {@exclude} property WYYZ: TVec4 index 3112 read GetVec4;
   {@exclude} property WYYW: TVec4 index 3113 read GetVec4;

   {@exclude} property WYZX: TVec4 index 3120 read GetVec4;
   {@exclude} property WYZY: TVec4 index 3121 read GetVec4;
   {@exclude} property WYZZ: TVec4 index 3122 read GetVec4;
   {@exclude} property WYZW: TVec4 index 3123 read GetVec4;

   {@exclude} property WYWX: TVec4 index 3130 read GetVec4;
   {@exclude} property WYWY: TVec4 index 3131 read GetVec4;
   {@exclude} property WYWZ: TVec4 index 3132 read GetVec4;
   {@exclude} property WYWW: TVec4 index 3133 read GetVec4;


   {@exclude} property WZXX: TVec4 index 3200 read GetVec4;
   {@exclude} property WZXY: TVec4 index 3201 read GetVec4;
   {@exclude} property WZXZ: TVec4 index 3202 read GetVec4;
   {@exclude} property WZXW: TVec4 index 3203 read GetVec4;

   {@exclude} property WZYX: TVec4 index 3210 read GetVec4;
   {@exclude} property WZYY: TVec4 index 3211 read GetVec4;
   {@exclude} property WZYZ: TVec4 index 3212 read GetVec4;
   {@exclude} property WZYW: TVec4 index 3213 read GetVec4;

   {@exclude} property WZZX: TVec4 index 3220 read GetVec4;
   {@exclude} property WZZY: TVec4 index 3221 read GetVec4;
   {@exclude} property WZZZ: TVec4 index 3222 read GetVec4;
   {@exclude} property WZZW: TVec4 index 3223 read GetVec4;

   {@exclude} property WZWX: TVec4 index 3230 read GetVec4;
   {@exclude} property WZWY: TVec4 index 3231 read GetVec4;
   {@exclude} property WZWZ: TVec4 index 3232 read GetVec4;
   {@exclude} property WZWW: TVec4 index 3233 read GetVec4;


   {@exclude} property WWXX: TVec4 index 3300 read GetVec4;
   {@exclude} property WWXY: TVec4 index 3301 read GetVec4;
   {@exclude} property WWXZ: TVec4 index 3302 read GetVec4;
   {@exclude} property WWXW: TVec4 index 3303 read GetVec4;

   {@exclude} property WWYX: TVec4 index 3310 read GetVec4;
   {@exclude} property WWYY: TVec4 index 3311 read GetVec4;
   {@exclude} property WWYZ: TVec4 index 3312 read GetVec4;
   {@exclude} property WWYW: TVec4 index 3313 read GetVec4;

   {@exclude} property WWZX: TVec4 index 3320 read GetVec4;
   {@exclude} property WWZY: TVec4 index 3321 read GetVec4;
   {@exclude} property WWZZ: TVec4 index 3322 read GetVec4;
   {@exclude} property WWZW: TVec4 index 3323 read GetVec4;

   {@exclude} property WWWX: TVec4 index 3330 read GetVec4;
   {@exclude} property WWWY: TVec4 index 3331 read GetVec4;
   {@exclude} property WWWZ: TVec4 index 3332 read GetVec4;
   {@exclude} property WWWW: TVec4 index 3333 read GetVec4;
  {$ENDIF}

  {$ENDIF}
{$ENDIF}
{$IFDEF IMPLEMENTATION}
  function TVector.GetVec2( AIndex: Integer ): TVec2;
  begin
    case AIndex of
      00: Result:= vec2( X );
      01: Result:= vec2( X, Y );
      {$IF NUMFIELD > 2}
      02: Result:= vec2( X, Z );
      {$ENDIF}
      {$IF NUMFIELD > 3}
      03: Result:= vec2( X, W );
      {$ENDIF}

      10: Result:= vec2( Y, X );
      11: Result:= vec2( Y );
      {$IF NUMFIELD > 2}
      12: Result:= vec2( Y, Z );
      {$ENDIF}
      {$IF NUMFIELD > 3}
      13: Result:= vec2( Y, W );
      {$ENDIF}

      {$IF NUMFIELD > 2}
      20: Result:= vec2( Z, X );
      21: Result:= vec2( Z, Y );
      22: Result:= vec2( Z );
      {$ENDIF}
      {$IF NUMFIELD > 3}
      23: Result:= vec2( Z, W );
      {$ENDIF}
      {$IF NUMFIELD > 3}
      30: Result:= vec2( W, X );
      31: Result:= vec2( W, Y );
      32: Result:= vec2( W, Z );
      33: Result:= vec2( W );
      {$ENDIF}

      else Result:= vec2( -1 ); //THIS SHOULD NEVER HAPPEN
    end;
  end;
  {$IF NUMFIELD > 2}
  function TVector.GetVec3( AIndex: Integer ): TVec3;
  begin
    case AIndex of
      000:  Result:= vec3( X );
      001:  Result:= vec3( X, X, Y );
      002:  Result:= vec3( X, X, Z );
      {$IF NUMFIELD > 3}
      003:  Result:= vec3( X, X, W );
      {$ENDIF}

      010:  Result:= vec3( X, Y, X );
      011:  Result:= vec3( X, Y, Y );
      012:  Result:= vec3( X, Y, Z );
      {$IF NUMFIELD > 3}
      013:  Result:= vec3( X, Y, W );
      {$ENDIF}

      020:  Result:= vec3( X, Z, X );
      021:  Result:= vec3( X, Z, Y );
      022:  Result:= vec3( X, Z, Z );
      {$IF NUMFIELD > 3}
      023:  Result:= vec3( X, Z, W );
      {$ENDIF}

      {$IF NUMFIELD > 3}
      030:  Result:= vec3( X, W, X );
      031:  Result:= vec3( X, W, Y );
      032:  Result:= vec3( X, W, Z );
      033:  Result:= vec3( X, W, W );
      {$ENDIF}

      100:  Result:= vec3( Y, X, X );
      101:  Result:= vec3( Y, X, Y );
      102:  Result:= vec3( Y, X, Z );
      {$IF NUMFIELD > 3}
      103:  Result:= vec3( Y, X, W );
      {$ENDIF}

      110:  Result:= vec3( Y, Y, X );
      111:  Result:= vec3( Y );
      112:  Result:= vec3( Y, Y, Z );
      {$IF NUMFIELD > 3}
      113:  Result:= vec3( Y, Y, W );
      {$ENDIF}

      120:  Result:= vec3( Y, Z, X );
      121:  Result:= vec3( Y, Z, Y );
      122:  Result:= vec3( Y, Z, Z );
      {$IF NUMFIELD > 3}
      123:  Result:= vec3( Y, Z, W );
      {$ENDIF}

      {$IF NUMFIELD > 3}
      130:  Result:= vec3( Y, W, X );
      131:  Result:= vec3( Y, W, Y );
      132:  Result:= vec3( Y, W, Z );
      133:  Result:= vec3( Y, W, W );
      {$ENDIF}

      200:  Result:= vec3( Z, X, X );
      201:  Result:= vec3( Z, X, Y );
      202:  Result:= vec3( Z, X, Z );
      {$IF NUMFIELD > 3}
      203:  Result:= vec3( Z, X, W );
      {$ENDIF}

      210:  Result:= vec3( Z, Y, X );
      211:  Result:= vec3( Z, Y, Y );
      212:  Result:= vec3( Z, Y, Z );
      {$IF NUMFIELD > 3}
      213:  Result:= vec3( Z, Y, W );
      {$ENDIF}

      220:  Result:= vec3( Z, Z, X );
      221:  Result:= vec3( Z, Z, Y );
      222:  Result:= vec3( Z );
      {$IF NUMFIELD > 3}
      223:  Result:= vec3( Z, Z, W );
      {$ENDIF}

      {$IF NUMFIELD > 3}
      230:  Result:= vec3( Z, W, X );
      231:  Result:= vec3( Z, W, Y );
      232:  Result:= vec3( Z, W, Z );
      233:  Result:= vec3( Z, W, W );
      {$ENDIF}

      {$IF NUMFIELD > 3}
      300:  Result:= vec3( W, X, X );
      301:  Result:= vec3( W, X, Y );
      302:  Result:= vec3( W, X, Z );
      303:  Result:= vec3( W, X, W );

      310:  Result:= vec3( W, Y, X );
      311:  Result:= vec3( W, Y, Y );
      312:  Result:= vec3( W, Y, Z );
      313:  Result:= vec3( W, Y, W );

      320:  Result:= vec3( W, Z, X );
      321:  Result:= vec3( W, Z, Y );
      322:  Result:= vec3( W, Z, Z );
      323:  Result:= vec3( W, Z, W );

      330:  Result:= vec3( W, W, X );
      331:  Result:= vec3( W, W, Y );
      332:  Result:= vec3( W, W, Z );
      333:  Result:= vec3( W );
      {$ENDIF}

      else Result:= vec3( -1 ); //THIS SHOULD NEVER HAPPEN
    end;
  end;
  {$ENDIF}

  {$IF NUMFIELD > 3}
  function TVector.GetVec4( AIndex: Integer ): TVec4;
  begin
    case AIndex of
      0000:  Result:= vec4( X, X, X, X );
      0001:  Result:= vec4( X, X, X, Y );
      0002:  Result:= vec4( X, X, X, Z );
      0003:  Result:= vec4( X, X, X, W );

      0010:  Result:= vec4( X, X, Y, X );
      0011:  Result:= vec4( X, X, Y, Y );
      0012:  Result:= vec4( X, X, Y, Z );
      0013:  Result:= vec4( X, X, Y, W );

      0020:  Result:= vec4( X, X, Z, X );
      0021:  Result:= vec4( X, X, Z, Y );
      0022:  Result:= vec4( X, X, Z, Z );
      0023:  Result:= vec4( X, X, Z, W );

      0030:  Result:= vec4( X, X, W, X );
      0031:  Result:= vec4( X, X, W, Y );
      0032:  Result:= vec4( X, X, W, Z );
      0033:  Result:= vec4( X, X, W, W );


      0100:  Result:= vec4( X, Y, X, X );
      0101:  Result:= vec4( X, Y, X, Y );
      0102:  Result:= vec4( X, Y, X, Z );
      0103:  Result:= vec4( X, Y, X, W );

      0110:  Result:= vec4( X, Y, Y, X );
      0111:  Result:= vec4( X, Y, Y, Y );
      0112:  Result:= vec4( X, Y, Y, Z );
      0113:  Result:= vec4( X, Y, Y, W );

      0120:  Result:= vec4( X, Y, Z, X );
      0121:  Result:= vec4( X, Y, Z, Y );
      0122:  Result:= vec4( X, Y, Z, Z );
      0123:  Result:= vec4( X, Y, Z, W );

      0130:  Result:= vec4( X, Y, W, X );
      0131:  Result:= vec4( X, Y, W, Y );
      0132:  Result:= vec4( X, Y, W, Z );
      0133:  Result:= vec4( X, Y, W, W );


      0200:  Result:= vec4( X, Z, X, X );
      0201:  Result:= vec4( X, Z, X, Y );
      0202:  Result:= vec4( X, Z, X, Z );
      0203:  Result:= vec4( X, Z, X, W );

      0210:  Result:= vec4( X, Z, Y, X );
      0211:  Result:= vec4( X, Z, Y, Y );
      0212:  Result:= vec4( X, Z, Y, Z );
      0213:  Result:= vec4( X, Z, Y, W );

      0220:  Result:= vec4( X, Z, Z, X );
      0221:  Result:= vec4( X, Z, Z, Y );
      0222:  Result:= vec4( X, Z, Z, Z );
      0223:  Result:= vec4( X, Z, Z, W );

      0230:  Result:= vec4( X, Z, W, X );
      0231:  Result:= vec4( X, Z, W, Y );
      0232:  Result:= vec4( X, Z, W, Z );
      0233:  Result:= vec4( X, Z, W, W );

      0300:  Result:= vec4( X, W, X, X );
      0301:  Result:= vec4( X, W, X, Y );
      0302:  Result:= vec4( X, W, X, Z );
      0303:  Result:= vec4( X, W, X, W );

      0310:  Result:= vec4( X, W, Y, X );
      0311:  Result:= vec4( X, W, Y, Y );
      0312:  Result:= vec4( X, W, Y, Z );
      0313:  Result:= vec4( X, W, Y, W );

      0320:  Result:= vec4( X, W, Z, X );
      0321:  Result:= vec4( X, W, Z, Y );
      0322:  Result:= vec4( X, W, Z, Z );
      0323:  Result:= vec4( X, W, Z, W );

      0330:  Result:= vec4( X, W, W, X );
      0331:  Result:= vec4( X, W, W, Y );
      0332:  Result:= vec4( X, W, W, Z );
      0333:  Result:= vec4( X, W, W, W );



      1000:  Result:= vec4( Y, X, X, X );
      1001:  Result:= vec4( Y, X, X, Y );
      1002:  Result:= vec4( Y, X, X, Z );
      1003:  Result:= vec4( Y, X, X, W );

      1010:  Result:= vec4( Y, X, Y, X );
      1011:  Result:= vec4( Y, X, Y, Y );
      1012:  Result:= vec4( Y, X, Y, Z );
      1013:  Result:= vec4( Y, X, Y, W );

      1020:  Result:= vec4( Y, X, Z, X );
      1021:  Result:= vec4( Y, X, Z, Y );
      1022:  Result:= vec4( Y, X, Z, Z );
      1023:  Result:= vec4( Y, X, Z, W );

      1030:  Result:= vec4( Y, X, W, X );
      1031:  Result:= vec4( Y, X, W, Y );
      1032:  Result:= vec4( Y, X, W, Z );
      1033:  Result:= vec4( Y, X, W, W );


      1100:  Result:= vec4( Y, Y, X, X );
      1101:  Result:= vec4( Y, Y, X, Y );
      1102:  Result:= vec4( Y, Y, X, Z );
      1103:  Result:= vec4( Y, Y, X, W );

      1110:  Result:= vec4( Y, Y, Y, X );
      1111:  Result:= vec4( Y, Y, Y, Y );
      1112:  Result:= vec4( Y, Y, Y, Z );
      1113:  Result:= vec4( Y, Y, Y, W );

      1120:  Result:= vec4( Y, Y, Z, X );
      1121:  Result:= vec4( Y, Y, Z, Y );
      1122:  Result:= vec4( Y, Y, Z, Z );
      1123:  Result:= vec4( Y, Y, Z, W );

      1130:  Result:= vec4( Y, Y, W, X );
      1131:  Result:= vec4( Y, Y, W, Y );
      1132:  Result:= vec4( Y, Y, W, Z );
      1133:  Result:= vec4( Y, Y, W, W );


      1200:  Result:= vec4( Y, Z, X, X );
      1201:  Result:= vec4( Y, Z, X, Y );
      1202:  Result:= vec4( Y, Z, X, Z );
      1203:  Result:= vec4( Y, Z, X, W );

      1210:  Result:= vec4( Y, Z, Y, X );
      1211:  Result:= vec4( Y, Z, Y, Y );
      1212:  Result:= vec4( Y, Z, Y, Z );
      1213:  Result:= vec4( Y, Z, Y, W );

      1220:  Result:= vec4( Y, Z, Z, X );
      1221:  Result:= vec4( Y, Z, Z, Y );
      1222:  Result:= vec4( Y, Z, Z, Z );
      1223:  Result:= vec4( Y, Z, Z, W );

      1230:  Result:= vec4( Y, Z, W, X );
      1231:  Result:= vec4( Y, Z, W, Y );
      1232:  Result:= vec4( Y, Z, W, Z );
      1233:  Result:= vec4( Y, Z, W, W );

      1300:  Result:= vec4( Y, W, X, X );
      1301:  Result:= vec4( Y, W, X, Y );
      1302:  Result:= vec4( Y, W, X, Z );
      1303:  Result:= vec4( Y, W, X, W );

      1310:  Result:= vec4( Y, W, Y, X );
      1311:  Result:= vec4( Y, W, Y, Y );
      1312:  Result:= vec4( Y, W, Y, Z );
      1313:  Result:= vec4( Y, W, Y, W );

      1320:  Result:= vec4( Y, W, Z, X );
      1321:  Result:= vec4( Y, W, Z, Y );
      1322:  Result:= vec4( Y, W, Z, Z );
      1323:  Result:= vec4( Y, W, Z, W );

      1330:  Result:= vec4( Y, W, W, X );
      1331:  Result:= vec4( Y, W, W, Y );
      1332:  Result:= vec4( Y, W, W, Z );
      1333:  Result:= vec4( Y, W, W, W );



      2000:  Result:= vec4( Z, X, X, X );
      2001:  Result:= vec4( Z, X, X, Y );
      2002:  Result:= vec4( Z, X, X, Z );
      2003:  Result:= vec4( Z, X, X, W );

      2010:  Result:= vec4( Z, X, Y, X );
      2011:  Result:= vec4( Z, X, Y, Y );
      2012:  Result:= vec4( Z, X, Y, Z );
      2013:  Result:= vec4( Z, X, Y, W );

      2020:  Result:= vec4( Z, X, Z, X );
      2021:  Result:= vec4( Z, X, Z, Y );
      2022:  Result:= vec4( Z, X, Z, Z );
      2023:  Result:= vec4( Z, X, Z, W );

      2030:  Result:= vec4( Z, X, W, X );
      2031:  Result:= vec4( Z, X, W, Y );
      2032:  Result:= vec4( Z, X, W, Z );
      2033:  Result:= vec4( Z, X, W, W );


      2100:  Result:= vec4( Z, Y, X, X );
      2101:  Result:= vec4( Z, Y, X, Y );
      2102:  Result:= vec4( Z, Y, X, Z );
      2103:  Result:= vec4( Z, Y, X, W );

      2110:  Result:= vec4( Z, Y, Y, X );
      2111:  Result:= vec4( Z, Y, Y, Y );
      2112:  Result:= vec4( Z, Y, Y, Z );
      2113:  Result:= vec4( Z, Y, Y, W );

      2120:  Result:= vec4( Z, Y, Z, X );
      2121:  Result:= vec4( Z, Y, Z, Y );
      2122:  Result:= vec4( Z, Y, Z, Z );
      2123:  Result:= vec4( Z, Y, Z, W );

      2130:  Result:= vec4( Z, Y, W, X );
      2131:  Result:= vec4( Z, Y, W, Y );
      2132:  Result:= vec4( Z, Y, W, Z );
      2133:  Result:= vec4( Z, Y, W, W );


      2200:  Result:= vec4( Z, Z, X, X );
      2201:  Result:= vec4( Z, Z, X, Y );
      2202:  Result:= vec4( Z, Z, X, Z );
      2203:  Result:= vec4( Z, Z, X, W );

      2210:  Result:= vec4( Z, Z, Y, X );
      2211:  Result:= vec4( Z, Z, Y, Y );
      2212:  Result:= vec4( Z, Z, Y, Z );
      2213:  Result:= vec4( Z, Z, Y, W );

      2220:  Result:= vec4( Z, Z, Z, X );
      2221:  Result:= vec4( Z, Z, Z, Y );
      2222:  Result:= vec4( Z, Z, Z, Z );
      2223:  Result:= vec4( Z, Z, Z, W );

      2230:  Result:= vec4( Z, Z, W, X );
      2231:  Result:= vec4( Z, Z, W, Y );
      2232:  Result:= vec4( Z, Z, W, Z );
      2233:  Result:= vec4( Z, Z, W, W );

      2300:  Result:= vec4( Z, W, X, X );
      2301:  Result:= vec4( Z, W, X, Y );
      2302:  Result:= vec4( Z, W, X, Z );
      2303:  Result:= vec4( Z, W, X, W );

      2310:  Result:= vec4( Z, W, Y, X );
      2311:  Result:= vec4( Z, W, Y, Y );
      2312:  Result:= vec4( Z, W, Y, Z );
      2313:  Result:= vec4( Z, W, Y, W );

      2320:  Result:= vec4( Z, W, Z, X );
      2321:  Result:= vec4( Z, W, Z, Y );
      2322:  Result:= vec4( Z, W, Z, Z );
      2323:  Result:= vec4( Z, W, Z, W );

      2330:  Result:= vec4( Z, W, W, X );
      2331:  Result:= vec4( Z, W, W, Y );
      2332:  Result:= vec4( Z, W, W, Z );
      2333:  Result:= vec4( Z, W, W, W );

      3000:  Result:= vec4( W, X, X, X );
      3001:  Result:= vec4( W, X, X, Y );
      3002:  Result:= vec4( W, X, X, Z );
      3003:  Result:= vec4( W, X, X, W );

      3010:  Result:= vec4( W, X, Y, X );
      3011:  Result:= vec4( W, X, Y, Y );
      3012:  Result:= vec4( W, X, Y, Z );
      3013:  Result:= vec4( W, X, Y, W );

      3020:  Result:= vec4( W, X, Z, X );
      3021:  Result:= vec4( W, X, Z, Y );
      3022:  Result:= vec4( W, X, Z, Z );
      3023:  Result:= vec4( W, X, Z, W );

      3030:  Result:= vec4( W, X, W, X );
      3031:  Result:= vec4( W, X, W, Y );
      3032:  Result:= vec4( W, X, W, Z );
      3033:  Result:= vec4( W, X, W, W );


      3100:  Result:= vec4( W, Y, X, X );
      3101:  Result:= vec4( W, Y, X, Y );
      3102:  Result:= vec4( W, Y, X, Z );
      3103:  Result:= vec4( W, Y, X, W );

      3110:  Result:= vec4( W, Y, Y, X );
      3111:  Result:= vec4( W, Y, Y, Y );
      3112:  Result:= vec4( W, Y, Y, Z );
      3113:  Result:= vec4( W, Y, Y, W );

      3120:  Result:= vec4( W, Y, Z, X );
      3121:  Result:= vec4( W, Y, Z, Y );
      3122:  Result:= vec4( W, Y, Z, Z );
      3123:  Result:= vec4( W, Y, Z, W );

      3130:  Result:= vec4( W, Y, W, X );
      3131:  Result:= vec4( W, Y, W, Y );
      3132:  Result:= vec4( W, Y, W, Z );
      3133:  Result:= vec4( W, Y, W, W );


      3200:  Result:= vec4( W, Z, X, X );
      3201:  Result:= vec4( W, Z, X, Y );
      3202:  Result:= vec4( W, Z, X, Z );
      3203:  Result:= vec4( W, Z, X, W );

      3210:  Result:= vec4( W, Z, Y, X );
      3211:  Result:= vec4( W, Z, Y, Y );
      3212:  Result:= vec4( W, Z, Y, Z );
      3213:  Result:= vec4( W, Z, Y, W );

      3220:  Result:= vec4( W, Z, Z, X );
      3221:  Result:= vec4( W, Z, Z, Y );
      3222:  Result:= vec4( W, Z, Z, Z );
      3223:  Result:= vec4( W, Z, Z, W );

      3230:  Result:= vec4( W, Z, W, X );
      3231:  Result:= vec4( W, Z, W, Y );
      3232:  Result:= vec4( W, Z, W, Z );
      3233:  Result:= vec4( W, Z, W, W );

      3300:  Result:= vec4( W, W, X, X );
      3301:  Result:= vec4( W, W, X, Y );
      3302:  Result:= vec4( W, W, X, Z );
      3303:  Result:= vec4( W, W, X, W );

      3310:  Result:= vec4( W, W, Y, X );
      3311:  Result:= vec4( W, W, Y, Y );
      3312:  Result:= vec4( W, W, Y, Z );
      3313:  Result:= vec4( W, W, Y, W );

      3320:  Result:= vec4( W, W, Z, X );
      3321:  Result:= vec4( W, W, Z, Y );
      3322:  Result:= vec4( W, W, Z, Z );
      3323:  Result:= vec4( W, W, Z, W );

      3330:  Result:= vec4( W, W, W, X );
      3331:  Result:= vec4( W, W, W, Y );
      3332:  Result:= vec4( W, W, W, Z );
      3333:  Result:= vec4( W, W, W, W )

      else Result:= vec4( -1 ); //THIS SHOULD NEVER HAPPEN
    end;
  end;
  {$ENDIF}
{$ENDIF}
