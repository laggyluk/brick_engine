program LogicEditor;

uses
  FMX.Forms,
  mega_editor_form in 'mega_editor_form.pas' {formEditor},
  Lua in 'src\Lua.pas',
  LuaLib in 'src\LuaLib.pas',
  mega_lua_bindings_win in 'src\mega_lua_bindings_win.pas',
  mega_win_functions in 'src\mega_win_functions.pas',
  SendInputHelper in 'src\SendInputHelper.pas',
  GLCrossPlatform in 'src\GLCrossPlatform.pas',
  vectorGeometry in 'src\vectorGeometry.pas',
  VectorTypes in 'src\VectorTypes.pas',
  direct_input_scan_codes in 'src\direct_input_scan_codes.pas',
  AllocateHWnd in 'src\AllocateHWnd.pas',
  fire_hotkey in 'src\fire_hotkey.pas',
  meta_behaviour_tree in 'src\meta_behaviour_tree.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TformEditor, formEditor);
  Application.Run;
end.