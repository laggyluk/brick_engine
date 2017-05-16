program brickUI;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, brickUImain, dglOpenGL, VectorGeometry, core_main, core_render,
  core_material_library, renderable_object_types, core_types, core_key_bindings,
  core_terrain_generator, core_camera, core_chunk, core_classes, core_utils,
  GeometryBB, toolbar_lights, topPanelUnit, core_renderable, core_lights,
  core_gBuffer, core_forward_shaders, core_shader_program, core_editor,
  menu_pointLight, blocksToolbarUnit, core_defered_shaders, toolbar_brushes,
  core_block_brush, menu_brush_box, toolbar_game_objs, menu_rotate,
  core_dynamic_object, core_mesh, core_texture, core_logic, core_actor,
  core_orders, core_sound, core_physics, toolbar_flat_units,
  core_behaviour_tree, core_process, core_pathfinding, nxModel, PerlinNoise,
  lnetvisual, core_noise, core_octree, toolbar_terrain,
  toolbar_newMap, core_plant_generator, core_LSystemHelper, stack,
  toolbar_plant_generator, core_job, core_jump_point_search,
  astar3d_pathFinder, core_listofrecords, core_ui,
  core_ui_controls, core_bullets, sunvox, core_game, map_properties_form,
  toolbar_log, core_weapons, core_atmosphere,
  core_text_reader, core_input, FIFOQUEUE_MPMC, core_player,
  ui_list_of_actors, core_beh_tree_types, core_editor_states;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TlogForm, logForm);
  Application.CreateForm(TUI, UI);
  Application.CreateForm(TtoolbarLights, toolbarLights);
  Application.CreateForm(TmenuPointLight, menuPointLight);
  Application.CreateForm(TtoolbarBlocks, toolbarBlocks);
  Application.CreateForm(TtoolbarBrushes, toolbarBrushes);
  Application.CreateForm(TmenuBrushBox, menuBrushBox);
  Application.CreateForm(TtoolbarGameObjs, toolbarGameObjs);
  Application.CreateForm(TmenuRotate, menuRotate);
  Application.CreateForm(TtoolbarFlats, toolbarFlats);
  Application.CreateForm(TtoolbarTerra, toolbarTerra);
  Application.CreateForm(TtoolbarNewMap, toolbarNewMap);
  Application.CreateForm(TtoolbarPlantGen, toolbarPlantGen);
  Application.CreateForm(TmapProperties, mapProperties);
  Application.CreateForm(TactorsListForm, actorsListForm);
  Application.CreateForm(TtopPanel, topPanel);

  topPanel.initTimer.Enabled:=true;
  Application.Run;

end.

