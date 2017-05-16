program game;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Forms, brickUImain, toolbar_log, Interfaces, core_types, core_ui,
  core_ui_controls, core_renderable, core_material_library, core_ui_obsolete,
  core_ui_base, ui_main_menu;

{$R *.res}

begin
  gameMode:=true;
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TlogForm, logForm);
  Application.CreateForm(TUI, UI);
  Application.Run;
end.

