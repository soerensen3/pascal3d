const
  P3DK_UNKNOWN = 0;

  {**
   *  Usage page $07
   *
   *  These values are from usage page $07 (USB keyboard page).
   *}

  P3DK_A = 4;
  P3DK_B = 5;
  P3DK_C = 6;
  P3DK_D = 7;
  P3DK_E = 8;
  P3DK_F = 9;
  P3DK_G = 10;
  P3DK_H = 11;
  P3DK_I = 12;
  P3DK_J = 13;
  P3DK_K = 14;
  P3DK_L = 15;
  P3DK_M = 16;
  P3DK_N = 17;
  P3DK_O = 18;
  P3DK_P = 19;
  P3DK_Q = 20;
  P3DK_R = 21;
  P3DK_S = 22;
  P3DK_T = 23;
  P3DK_U = 24;
  P3DK_V = 25;
  P3DK_W = 26;
  P3DK_X = 27;
  P3DK_Y = 28;
  P3DK_Z = 29;

  P3DK_1 = 30;
  P3DK_2 = 31;
  P3DK_3 = 32;
  P3DK_4 = 33;
  P3DK_5 = 34;
  P3DK_6 = 35;
  P3DK_7 = 36;
  P3DK_8 = 37;
  P3DK_9 = 38;
  P3DK_0 = 39;

  P3DK_RETURN = 40;
  P3DK_ESCAPE = 41;
  P3DK_BACKSPACE = 42;
  P3DK_TAB = 43;
  P3DK_SPACE = 44;

  P3DK_MINUS = 45;
  P3DK_EQUALS = 46;
  P3DK_LEFTBRACKET = 47;
  P3DK_RIGHTBRACKET = 48;
  P3DK_BACKSLASH = 49; {**< Located at the lower left of the return
                                *   key on ISO keyboards and at the right end
                                *   of the QWERTY row on ANSI keyboards.
                                *   Produces REVERSE SOLIDUS (backslash) and
                                *   VERTICAL LINE in a US layout; REVERSE
                                *   SOLIDUS and VERTICAL LINE in a UK Mac
                                *   layout; NUMBER SIGN and TILDE in a UK
                                *   Windows layout; DOLLAR SIGN and POUND SIGN
                                *   in a Swiss German layout; NUMBER SIGN and
                                *   APOSTROPHE in a German layout; GRAVE
                                *   ACCENT and POUND SIGN in a French Mac
                                *   layout; and ASTERISK and MICRO SIGN in a
                                *   French Windows layout.
                                *}
  P3DK_NONUSHASH = 50; {**< ISO USB keyboards actually use this code
                                *   instead of 49 for the same key; but all
                                *   OSes I've seen treat the two codes
                                *   identically. So; as an implementor; unless
                                *   your keyboard generates both of those
                                *   codes and your OS treats them differently;
                                *   you should generate P3DK_BACKSLASH
                                *   instead of this code. As a user; you
                                *   should not rely on this code because SDL
                                *   will never generate it with most (all?)
                                *   keyboards.
                                *}
  P3DK_SEMICOLON = 51;
  P3DK_APOSTROPHE = 52;
  P3DK_GRAVE = 53;     {**< Located in the top left corner (on both ANSI
                                *   and ISO keyboards). Produces GRAVE ACCENT and
                                *   TILDE in a US Windows layout and in US and UK
                                *   Mac layouts on ANSI keyboards; GRAVE ACCENT
                                *   and NOT SIGN in a UK Windows layout; SECTION
                                *   SIGN and PLUS-MINUS SIGN in US and UK Mac
                                *   layouts on ISO keyboards; SECTION SIGN and
                                *   DEGREE SIGN in a Swiss German layout (Mac:
                                *   only on ISO keyboards); CIRCUMFLEX ACCENT and
                                *   DEGREE SIGN in a German layout (Mac: only on
                                *   ISO keyboards); SUPERSCRIPT TWO and TILDE in a
                                *   French Windows layout; COMMERCIAL AT and
                                *   NUMBER SIGN in a French Mac layout on ISO
                                *   keyboards; and LESS-THAN SIGN and GREATER-THAN
                                *   SIGN in a Swiss German; German; or French Mac
                                *   layout on ANSI keyboards.
                                *}
  P3DK_COMMA = 54;
  P3DK_PERIOD = 55;
  P3DK_SLASH = 56;

  P3DK_CAPSLOCK = 57;

  P3DK_F1 = 58;
  P3DK_F2 = 59;
  P3DK_F3 = 60;
  P3DK_F4 = 61;
  P3DK_F5 = 62;
  P3DK_F6 = 63;
  P3DK_F7 = 64;
  P3DK_F8 = 65;
  P3DK_F9 = 66;
  P3DK_F10 = 67;
  P3DK_F11 = 68;
  P3DK_F12 = 69;

  P3DK_PRINTSCREEN = 70;
  P3DK_SCROLLLOCK = 71;
  P3DK_PAUSE = 72;
  P3DK_INSERT = 73; {**< insert on PC; help on some Mac keyboards (but
                                 does send code 73; not 117) *}
  P3DK_HOME = 74;
  P3DK_PAGEUP = 75;
  P3DK_DELETE = 76;
  P3DK_END = 77;
  P3DK_PAGEDOWN = 78;
  P3DK_RIGHT = 79;
  P3DK_LEFT = 80;
  P3DK_DOWN = 81;
  P3DK_UP = 82;

  P3DK_NUMLOCKCLEAR = 83; {**< num lock on PC; clear on Mac keyboards
                                   *}
  P3DK_KP_DIVIDE = 84;
  P3DK_KP_MULTIPLY = 85;
  P3DK_KP_MINUS = 86;
  P3DK_KP_PLUS = 87;
  P3DK_KP_ENTER = 88;
  P3DK_KP_1 = 89;
  P3DK_KP_2 = 90;
  P3DK_KP_3 = 91;
  P3DK_KP_4 = 92;
  P3DK_KP_5 = 93;
  P3DK_KP_6 = 94;
  P3DK_KP_7 = 95;
  P3DK_KP_8 = 96;
  P3DK_KP_9 = 97;
  P3DK_KP_0 = 98;
  P3DK_KP_PERIOD = 99;

  P3DK_NONUSBACKSLASH = 100; {**< This is the additional key that ISO
                                      *   keyboards have over ANSI ones;
                                      *   located between left shift and Y.
                                      *   Produces GRAVE ACCENT and TILDE in a
                                      *   US or UK Mac layout; REVERSE SOLIDUS
                                      *   (backslash) and VERTICAL LINE in a
                                      *   US or UK Windows layout; and
                                      *   LESS-THAN SIGN and GREATER-THAN SIGN
                                      *   in a Swiss German; German; or French
                                      *   layout. *}
  P3DK_APPLICATION = 101;    {**< windows contextual menu; compose *}
  P3DK_POWER = 102;          {**< The USB document says this is a status flag;
                                       *  not a physical key - but some Mac keyboards
                                       *  do have a power key. *}
  P3DK_KP_EQUALS = 103;
  P3DK_F13 = 104;
  P3DK_F14 = 105;
  P3DK_F15 = 106;
  P3DK_F16 = 107;
  P3DK_F17 = 108;
  P3DK_F18 = 109;
  P3DK_F19 = 110;
  P3DK_F20 = 111;
  P3DK_F21 = 112;
  P3DK_F22 = 113;
  P3DK_F23 = 114;
  P3DK_F24 = 115;
  P3DK_EXECUTE = 116;
  P3DK_HELP = 117;
  P3DK_MENU = 118;
  P3DK_SELECT = 119;
  P3DK_STOP = 120;
  P3DK_AGAIN = 121;   {**< redo *}
  P3DK_UNDO = 122;
  P3DK_CUT = 123;
  P3DK_COPY = 124;
  P3DK_PASTE = 125;
  P3DK_FIND = 126;
  P3DK_MUTE = 127;
  P3DK_VOLUMEUP = 128;
  P3DK_VOLUMEDOWN = 129;
  {* not sure whether there's a reason to enable these *}
  {*     P3DK_LOCKINGCAPSLOCK = 130;  *}
  {*     P3DK_LOCKINGNUMLOCK = 131; *}
  {*     P3DK_LOCKINGSCROLLLOCK = 132; *}
  P3DK_KP_COMMA = 133;
  P3DK_KP_EQUALSAS400 = 134;

  P3DK_INTERNATIONAL1 = 135; {**< used on Asian keyboards; see footnotes in USB doc *}
  P3DK_INTERNATIONAL2 = 136;
  P3DK_INTERNATIONAL3 = 137; {**< Yen *}
  P3DK_INTERNATIONAL4 = 138;
  P3DK_INTERNATIONAL5 = 139;
  P3DK_INTERNATIONAL6 = 140;
  P3DK_INTERNATIONAL7 = 141;
  P3DK_INTERNATIONAL8 = 142;
  P3DK_INTERNATIONAL9 = 143;
  P3DK_LANG1 = 144; {**< HangulEnglish toggle *}
  P3DK_LANG2 = 145; {**< Hanja conversion *}
  P3DK_LANG3 = 146; {**< Katakana *}
  P3DK_LANG4 = 147; {**< Hiragana *}
  P3DK_LANG5 = 148; {**< ZenkakuHankaku *}
  P3DK_LANG6 = 149; {**< reserved *}
  P3DK_LANG7 = 150; {**< reserved *}
  P3DK_LANG8 = 151; {**< reserved *}
  P3DK_LANG9 = 152; {**< reserved *}

  P3DK_ALTERASE = 153; {**< Erase-Eaze *}
  P3DK_SYSREQ = 154;
  P3DK_CANCEL = 155;
  P3DK_CLEAR = 156;
  P3DK_PRIOR = 157;
  P3DK_RETURN2 = 158;
  P3DK_SEPARATOR = 159;
  P3DK_OUT = 160;
  P3DK_OPER = 161;
  P3DK_CLEARAGAIN = 162;
  P3DK_CRSEL = 163;
  P3DK_EXSEL = 164;

  P3DK_KP_00 = 176;
  P3DK_KP_000 = 177;
  P3DK_THOUSANDSSEPARATOR = 178;
  P3DK_DECIMALSEPARATOR = 179;
  P3DK_CURRENCYUNIT = 180;
  P3DK_CURRENCYSUBUNIT = 181;
  P3DK_KP_LEFTPAREN = 182;
  P3DK_KP_RIGHTPAREN = 183;
  P3DK_KP_LEFTBRACE = 184;
  P3DK_KP_RIGHTBRACE = 185;
  P3DK_KP_TAB = 186;
  P3DK_KP_BACKSPACE = 187;
  P3DK_KP_A = 188;
  P3DK_KP_B = 189;
  P3DK_KP_C = 190;
  P3DK_KP_D = 191;
  P3DK_KP_E = 192;
  P3DK_KP_F = 193;
  P3DK_KP_XOR = 194;
  P3DK_KP_POWER = 195;
  P3DK_KP_PERCENT = 196;
  P3DK_KP_LESS = 197;
  P3DK_KP_GREATER = 198;
  P3DK_KP_AMPERSAND = 199;
  P3DK_KP_DBLAMPERSAND = 200;
  P3DK_KP_VERTICALBAR = 201;
  P3DK_KP_DBLVERTICALBAR = 202;
  P3DK_KP_COLON = 203;
  P3DK_KP_HASH = 204;
  P3DK_KP_SPACE = 205;
  P3DK_KP_AT = 206;
  P3DK_KP_EXCLAM = 207;
  P3DK_KP_MEMSTORE = 208;
  P3DK_KP_MEMRECALL = 209;
  P3DK_KP_MEMCLEAR = 210;
  P3DK_KP_MEMADD = 211;
  P3DK_KP_MEMSUBTRACT = 212;
  P3DK_KP_MEMMULTIPLY = 213;
  P3DK_KP_MEMDIVIDE = 214;
  P3DK_KP_PLUSMINUS = 215;
  P3DK_KP_CLEAR = 216;
  P3DK_KP_CLEARENTRY = 217;
  P3DK_KP_BINARY = 218;
  P3DK_KP_OCTAL = 219;
  P3DK_KP_DECIMAL = 220;
  P3DK_KP_HEXADECIMAL = 221;

  P3DK_LCTRL = 224;
  P3DK_LSHIFT = 225;
  P3DK_LALT = 226; {**< alt; option *}
  P3DK_LGUI = 227; {**< windows; command (apple); meta *}
  P3DK_RCTRL = 228;
  P3DK_RSHIFT = 229;
  P3DK_RALT = 230; {**< alt gr; option *}
  P3DK_RGUI = 231; {**< windows; command (apple); meta *}

  P3DK_MODE = 257;    {**< I'm not sure if this is really not covered
                               *   by any of the above; but since there's a
                               *   special KMOD_MODE for it I'm adding it here
                               *}

  {*Usage page $07*}

  {**
   *  Usage page $0C
   *
   *  These values are mapped from usage page $0C (USB consumer page).
   *}

  P3DK_AUDIONEXT = 258;
  P3DK_AUDIOPREV = 259;
  P3DK_AUDIOSTOP = 260;
  P3DK_AUDIOPLAY = 261;
  P3DK_AUDIOMUTE = 262;
  P3DK_MEDIASELECT = 263;
  P3DK_WWW = 264;
  P3DK_MAIL = 265;
  P3DK_CALCULATOR = 266;
  P3DK_COMPUTER = 267;
  P3DK_AC_SEARCH = 268;
  P3DK_AC_HOME = 269;
  P3DK_AC_BACK = 270;
  P3DK_AC_FORWARD = 271;
  P3DK_AC_STOP = 272;
  P3DK_AC_REFRESH = 273;
  P3DK_AC_BOOKMARKS = 274;

  {*Usage page $0C*}

  {**
   *  Walther keys
   *
   *  These are values that Christian Walther added (for mac keyboard?).
   *}

  P3DK_BRIGHTNESSDOWN = 275;
  P3DK_BRIGHTNESSUP = 276;
  P3DK_DISPLAYSWITCH = 277; {**< display mirroringdual display
                                         switch; video mode switch *}
  P3DK_KBDILLUMTOGGLE = 278;
  P3DK_KBDILLUMDOWN = 279;
  P3DK_KBDILLUMUP = 280;
  P3DK_EJECT = 281;
  P3DK_SLEEP = 282;
