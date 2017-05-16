unit direct_input_scan_codes;

interface
uses windows;

//Funkcja zamienia sta³a klawisza Delphi na staa uyzwana pryey Dx-a
//integer key - sta³a klawisza w delphi
//integer - sta³a klawisza w Dx
Function VKToDIK(key: integer): integer;

//Funkcja zamienia sta³a klawisza na jego nazwe
//integer key - sta³a klawisza
//string - nazwa klawisza
Function DIKToStr(key: integer): string;

{****************************************************************************
*
* DirectInput keyboard scan codes
*
****************************************************************************}
const
  DIK_ESCAPE = $001;
  DIK_1 = $002;
  DIK_2 = $003;
  DIK_3 = $004;
  DIK_4 = $005;
  DIK_5 = $006;
  DIK_6 = $007;
  DIK_7 = $008;
  DIK_8 = $009;
  DIK_9 = $00A;
  DIK_0 = $00B;
  DIK_MINUS = $00C ;//// - on main keyboard */
  DIK_EQUALS = $00D;
  DIK_BACK = $00E;/// backspace */
  DIK_TAB = $00F;
  DIK_Q = $010;
  DIK_W = $011;
  DIK_E = $012;
  DIK_R = $013 ;
  DIK_T = $014;
  DIK_Y = $015;
  DIK_U = $016;
  DIK_I = $017;
  DIK_O = $018;
  DIK_P = $019;
  DIK_LBRACKET = $01A;
  DIK_RBRACKET = $01B;
  DIK_RETURN = $01C; // Enter on main keyboard */
  DIK_LCONTROL = $01D;
  DIK_A = $01E;
  DIK_S = $01F;
  DIK_D = $020;
  DIK_F = $021;
  DIK_G = $022;
  DIK_H = $023;
  DIK_J = $024;
  DIK_K = $025;
  DIK_L = $026;
  DIK_SEMICOLON = $027;
  DIK_APOSTROPHE = $028;
  DIK_GRAVE = $029; // accent grave */
  DIK_LSHIFT = $02A;
  DIK_BACKSLASH = $02B;
  DIK_Z = $02C;
  DIK_X = $02D;
  DIK_C = $02E;
  DIK_V = $02F;
  DIK_B = $030;
  DIK_N = $031;
  DIK_M = $032;
  DIK_COMMA = $033;
  DIK_PERIOD = $034; // . on main keyboard */
  DIK_SLASH = $035; // / on main keyboard */
  DIK_RSHIFT = $036;
  DIK_MULTIPLY = $037; // * on numeric keypad */
  DIK_LMENU = $038; // left Alt */
  DIK_SPACE = $039;
  DIK_CAPITAL = $03A;
  DIK_F1 = $03B;
  DIK_F2 = $03C;
  DIK_F3 = $03D;
  DIK_F4 = $03E;
  DIK_F5 = $03F;
  DIK_F6 = $040;
  DIK_F7 = $041;
  DIK_F8 = $042;
  DIK_F9 = $043;
  DIK_F10 = $044;
  DIK_NUMLOCK = $045;
  DIK_SCROLL = $046; // Scroll Lock */
  DIK_NUMPAD7 = $047;
  DIK_NUMPAD8 = $048;
  DIK_NUMPAD9 = $049;
  DIK_SUBTRACT = $04A; // - on numeric keypad */
  DIK_NUMPAD4 = $04B;
  DIK_NUMPAD5 = $04C;
  DIK_NUMPAD6 = $04D;
  DIK_ADD = $04E ;// + on numeric keypad */
  DIK_NUMPAD1 = $04F;
  DIK_NUMPAD2 = $050;
  DIK_NUMPAD3 = $051;
  DIK_NUMPAD0 = $052;
  DIK_DECIMAL = $053 ;// . on numeric keypad */
  DIK_OEM_102 = $056 ;// < > | on UK/Germany keyboards */
  DIK_F11 = $057      ;
  DIK_F12 = $058;
  DIK_F13 = $064 ;// (NEC PC98) */
  DIK_F14 = $065 ;// (NEC PC98) */
  DIK_F15 = $066 ;// (NEC PC98) */
  DIK_KANA = $070 ;// (Japanese keyboard) */
  DIK_ABNT_C1 = $073; // / ? on Portugese (Brazilian) keyboards */
  DIK_CONVERT = $079; // (Japanese keyboard) */
  DIK_NOCONVERT = $07B; // (Japanese keyboard) */
  DIK_YEN = $07D; // (Japanese keyboard) */
  DIK_ABNT_C2 = $07E; // Numpad . on Portugese (Brazilian) keyboards */
  DIK_NUMPADEQUALS = $08D; // = on numeric keypad (NEC PC98) */
  DIK_PREVTRACK = $090; // Previous Track (DIK_CIRCUMFLEX on Japanese keyboard) */
  DIK_AT = $091; // (NEC PC98) */
  DIK_COLON = $092; // (NEC PC98) */
  DIK_UNDERLINE = $093; // (NEC PC98) */
  DIK_KANJI = $094; // (Japanese keyboard) */
  DIK_STOP = $095; // (NEC PC98) */
  DIK_AX = $096; // (Japan AX) */
  DIK_UNLABELED = $097; // (J3100) */
  DIK_NEXTTRACK = $099; // Next Track */
  DIK_NUMPADENTER = $09C; // Enter on numeric keypad */
  DIK_RCONTROL = $09D;
  DIK_MUTE = $0A0; // Mute */
  DIK_CALCULATOR = $0A1; // Calculator */
  DIK_PLAYPAUSE = $0A2; // Play / Pause */
  DIK_MEDIASTOP = $0A4; // Media Stop */
  DIK_VOLUMEDOWN = $0AE; // Volume - */
  DIK_VOLUMEUP = $0B0; // Volume + */
  DIK_WEBHOME = $0B2; // Web home */
  DIK_NUMPADCOMMA = $0B3; // , on numeric keypad (NEC PC98) */
  DIK_DIVIDE = $0B5; // / on numeric keypad */
  DIK_SYSRQ = $0B7 ;
  DIK_RMENU = $0B8; // right Alt */
  DIK_PAUSE = $0C5; // Pause */
  DIK_HOME = $0C7; // Home on arrow keypad */
  DIK_UP = $0C8; // UpArrow on arrow keypad */
  DIK_PRIOR = $0C9; // PgUp on arrow keypad */
  DIK_LEFT = $0CB; // LeftArrow on arrow keypad */
  DIK_RIGHT = $0CD; // RightArrow on arrow keypad */
  DIK_END = $0CF; // End on arrow keypad */
  DIK_DOWN = $0D0; // DownArrow on arrow keypad */
  DIK_NEXT = $0D1; // PgDn on arrow keypad */
  DIK_INSERT = $0D2; // Insert on arrow keypad */
  DIK_DELETE = $0D3; // Delete on arrow keypad */
  DIK_LWIN = $0DB ;// Left Windows key */
  DIK_RWIN = $0DC; // Right Windows key */
  DIK_APPS = $0DD; // AppMenu key */
  DIK_POWER = $0DE; // System Power */
  DIK_SLEEP = $0DF; // System Sleep */
  DIK_WAKE = $0E3; // System Wake */
  DIK_WEBSEARCH = $0E5; // Web Search */
  DIK_WEBFAVORITES = $0E6; // Web Favorites */
  DIK_WEBREFRESH = $0E7; // Web Refresh */
  DIK_WEBSTOP = $0E8; // Web Stop */
  DIK_WEBFORWARD = $0E9; // Web Forward */
  DIK_WEBBACK = $0EA ;// Web Back */
  DIK_MYCOMPUTER = $0EB; // My Computer */
  DIK_MAIL = $0EC ;// Mail */
  DIK_MEDIASELECT = $0ED; // Media Select */
//
//* Alternate names for keys, to facilitate transition from DOS.

  DIK_BACKSPACE = DIK_BACK; // backspace */
  DIK_NUMPADSTAR = DIK_MULTIPLY; // * on numeric keypad */
  DIK_LALT = DIK_LMENU; // left Alt */
  DIK_CAPSLOCK = DIK_CAPITAL; // CapsLock */
  DIK_NUMPADMINUS = DIK_SUBTRACT; // - on numeric keypad */
  DIK_NUMPADPLUS = DIK_ADD; // + on numeric keypad */
  DIK_NUMPADPERIOD = DIK_DECIMAL; // . on numeric keypad */
  DIK_NUMPADSLASH = DIK_DIVIDE; // / on numeric keypad */
  DIK_RALT = DIK_RMENU; // right Alt */
  DIK_UPARROW = DIK_UP; // UpArrow on arrow keypad */
  DIK_PGUP = DIK_PRIOR; // PgUp on arrow keypad */
  DIK_LEFTARROW = DIK_LEFT; // LeftArrow on arrow keypad */
  DIK_RIGHTARROW = DIK_RIGHT; // RightArrow on arrow keypad */
  DIK_DOWNARROW = DIK_DOWN; // DownArrow on arrow keypad */
  DIK_PGDN = DIK_NEXT; // PgDn on arrow keypad */
//
//* Alternate names for keys originally not used on US keyboards.
//*/
  DIK_CIRCUMFLEX = DIK_PREVTRACK; // Japanese keyboard */

implementation


Function DIKToStr(key: integer): string;
begin
  Result := '';
  case key of
    0             : Result := 'None';
    DIK_0         : Result := '0';
    DIK_1..DIK_9  : Result := char(key - DIK_1 + 1 + $30);
    DIK_A         : Result := 'A';
    DIK_ADD       : Result := 'NUM +';
    DIK_APOSTROPHE: Result := 'Apostrophe';
    DIK_B         : Result := 'B';
    DIK_BACK      : Result := 'Backspace';
    DIK_BACKSLASH : Result := 'Backslash';
    DIK_C         : Result := 'C';
    DIK_CALCULATOR: Result := 'Calculator';
    DIK_CAPITAL   : Result := 'Caps Lock';
    DIK_COMMA     : Result := ',';
    DIK_D         : Result := 'D';
    DIK_DECIMAL   : Result := 'NUM ,';
    DIK_DELETE    : Result := 'Delete';
    DIK_DIVIDE    : Result := 'NUM /';
    DIK_DOWN      : Result := 'Down Arrow';
    DIK_E         : Result := 'E';
    DIK_END       : Result := 'End';
    DIK_EQUALS    : Result := '=';
    DIK_ESCAPE    : Result := 'Esc';
    DIK_F         : Result := 'F';
    DIK_F1        : Result := 'F1';
    DIK_F2        : Result := 'F2';
    DIK_F3        : Result := 'F3';
    DIK_F4        : Result := 'F4';
    DIK_F5        : Result := 'F5';
    DIK_F6        : Result := 'F6';
    DIK_F7        : Result := 'F7';
    DIK_F8        : Result := 'F8';
    DIK_F9        : Result := 'F9';
    DIK_F10       : Result := 'F10';
    DIK_F11       : Result := 'F11';
    DIK_F12       : Result := 'F12';
    DIK_F13       : Result := 'F13';
    DIK_F14       : Result := 'F14';
    DIK_F15       : Result := 'F15';
    DIK_G         : Result := 'G';
    DIK_GRAVE     : Result := 'Grave';
    DIK_H         : Result := 'H';
    DIK_HOME      : Result := 'Home';
    DIK_I         : Result := 'I';
    DIK_INSERT    : Result := 'Insert';
    DIK_J         : Result := 'J';
    DIK_K         : Result := 'K';
    DIK_L         : Result := 'L';
    DIK_LBRACKET  : Result := '[';
    DIK_LCONTROL  : Result := 'Left Ctrl';
    DIK_LEFT      : Result := 'Left Arrow';
    DIK_LMENU     : Result := 'Left Alt';
    DIK_LSHIFT    : Result := 'Left Shift';
    DIK_M         : Result := 'M';
    DIK_MINUS     : Result := '-';
    DIK_MULTIPLY  : Result := 'NUM *';
    DIK_N         : Result := 'N';
    DIK_NEXT      : Result := 'Page Down';
    DIK_NUMPAD0   : Result := 'NUM 0';
    DIK_NUMPAD1   : Result := 'NUM 1';
    DIK_NUMPAD2   : Result := 'NUM 2';
    DIK_NUMPAD3   : Result := 'NUM 3';
    DIK_NUMPAD4   : Result := 'NUM 4';
    DIK_NUMPAD5   : Result := 'NUM 5';
    DIK_NUMPAD6   : Result := 'NUM 6';
    DIK_NUMPAD7   : Result := 'NUM 7';
    DIK_NUMPAD8   : Result := 'NUM 8';
    DIK_NUMPAD9   : Result := 'NUM 9';
    DIK_NUMPADENTER: Result := 'NUM Enter';
    DIK_O : Result := 'O';
    DIK_OEM_102     : Result := 'OEM_102';
    DIK_P : Result := 'P';
    DIK_PERIOD : Result := '.';
    DIK_PRIOR  : Result := 'Page Up';
    DIK_Q : Result := 'Q';
    DIK_R : Result := 'R';
    DIK_RBRACKET : Result := ']';
    DIK_RCONTROL  : Result := 'Right Ctrl';
    DIK_RETURN  : Result := 'Enter';
    DIK_RIGHT  : Result := 'Right Arrow';
    DIK_RMENU  : Result := 'Right Alt';
    DIK_RSHIFT : Result := 'Right Shift';
    DIK_S  : Result := 'S';
    DIK_SEMICOLON : Result := ';';
    DIK_SLASH  : Result := '/';
    DIK_SPACE  : Result := 'Space';
    DIK_SUBTRACT : Result := 'NUM -';
    DIK_SYSRQ : Result := 'SysRQ';
    DIK_T        : Result := 'T';
    DIK_TAB     : Result := 'Tab';
    DIK_U      : Result := 'U';
    DIK_UP      : Result := 'Up Arrow';
    DIK_V      : Result := 'V';
    DIK_W      : Result := 'W';
    DIK_X     : Result := 'X';
    DIK_Y      : Result := 'Y';
    DIK_Z     : Result := 'Z';
  end;
end;


Function VKToDIK(key: integer): integer;
begin
  case key of
    VK_BACK: Result := DIK_BACK;
    VK_TAB: Result := DIK_TAB;
    VK_CLEAR: Result := DIK_NUMPAD5;
    VK_RETURN: Result := DIK_RETURN;
    VK_SHIFT, VK_LSHIFT: Result := DIK_LSHIFT;
    VK_RSHIFT: Result := DIK_RSHIFT;
    VK_CONTROL, VK_LCONTROL: Result := DIK_LCONTROL;
    VK_RCONTROL: Result := DIK_RCONTROL;
    VK_MENU, VK_LMENU: Result := DIK_LMENU;
    VK_RMENU: Result := DIK_RMENU;
    VK_CAPITAL: Result := DIK_CAPITAL;
    VK_ESCAPE: Result := DIK_ESCAPE;
    VK_SPACE: Result := DIK_SPACE;
    VK_PRIOR: Result := DIK_PRIOR;
    VK_NEXT: Result := DIK_NEXT;
    VK_END: Result := DIK_END;
    VK_HOME: Result := DIK_HOME;
    VK_LEFT: Result := DIK_LEFT;
    VK_UP: Result := DIK_UP;
    VK_RIGHT: Result := DIK_RIGHT;
    VK_DOWN: Result := DIK_DOWN;
    VK_INSERT: Result := DIK_INSERT;
    VK_DELETE: Result := DIK_DELETE;
    $30: Result := DIK_0;
    $31..$39: Result := DIK_1 - $31 + key;
    $41: Result := DIK_A;
    $42: Result := DIK_B;
    $43: Result := DIK_C;
    $44: Result := DIK_D;
    $45: Result := DIK_E;
    $46: Result := DIK_F;
    $47: Result := DIK_G;
    $48: Result := DIK_H;
    $49: Result := DIK_I;
    $4A: Result := DIK_J;
    $4B: Result := DIK_K;
    $4C: Result := DIK_L;
    $4D: Result := DIK_M;
    $4E: Result := DIK_N;
    $4F: Result := DIK_O;
    $50: Result := DIK_P;
    $51: Result := DIK_Q;
    $52: Result := DIK_R;
    $53: Result := DIK_S;
    $54: Result := DIK_T;
    $55: Result := DIK_U;
    $56: Result := DIK_V;
    $57: Result := DIK_W;
    $58: Result := DIK_X;
    $59: Result := DIK_Y;
    $5A: Result := DIK_Z;
    VK_LWIN: Result := DIK_LWIN;
    VK_RWIN: Result := DIK_RWIN;
    VK_APPS: Result := DIK_APPS;
    VK_NUMPAD0: Result := DIK_NUMPAD0;
    VK_NUMPAD1: Result := DIK_NUMPAD1;
    VK_NUMPAD2: Result := DIK_NUMPAD2;
    VK_NUMPAD3: Result := DIK_NUMPAD3;
    VK_NUMPAD4: Result := DIK_NUMPAD4;
    VK_NUMPAD5: Result := DIK_NUMPAD5;
    VK_NUMPAD6: Result := DIK_NUMPAD6;
    VK_NUMPAD7: Result := DIK_NUMPAD7;
    VK_NUMPAD8: Result := DIK_NUMPAD8;
    VK_NUMPAD9: Result := DIK_NUMPAD9;
    VK_MULTIPLY: Result := DIK_MULTIPLY;
    VK_ADD: Result := DIK_ADD;
    VK_SUBTRACT: Result := DIK_SUBTRACT;
    VK_DECIMAL: Result := DIK_DECIMAL;
    VK_DIVIDE: Result := DIK_DIVIDE;
    VK_F1: Result := DIK_F1;
    VK_F2: Result := DIK_F2;
    VK_F3: Result := DIK_F3;
    VK_F4: Result := DIK_F4;
    VK_F5: Result := DIK_F5;
    VK_F6: Result := DIK_F6;
    VK_F7: Result := DIK_F7;
    VK_F8: Result := DIK_F8;
    VK_F9: Result := DIK_F9;
    VK_F10: Result := DIK_F10;
    VK_F11: Result := DIK_F11;
    VK_F12: Result := DIK_F12;
    VK_NUMLOCK: Result := DIK_NUMLOCK;
    VK_SCROLL: Result := DIK_SCROLL;

    192: Result := DIK_SEMICOLON;
    221: Result := DIK_EQUALS;
    188: Result := DIK_COMMA;
    219: Result := DIK_MINUS;
    190: Result := DIK_PERIOD;
    189: Result := DIK_SLASH;
    220: Result := DIK_GRAVE;
    186: Result := DIK_LBRACKET;
    191: Result := DIK_BACKSLASH;
    187: Result := DIK_RBRACKET;
    222: Result := DIK_APOSTROPHE;
    else Result:= 0;
  end;
end;

end.
