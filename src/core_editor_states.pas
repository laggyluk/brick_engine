unit core_editor_states;

{$mode objfpc}{$H+}

interface

type

  //different mouse/keyboard handling modes
  eEditorStates = (esNone,
      esPointLight,//editing point light
      esSingleBlock,// sible block brush is set
      esSelectMode,//only mouse picking of existing obj/lights/whatever
      esEditBrush,//editing brush object
      esPaintWithBrush,//painting world with brush
      esEditFrame,//editing animation frame
      esPaintWithObject,//dynamic obj is selected as active brush tool
      esActor,//one of flat units is selected and ready for dropping
      //places order for troops to carry out rather than directly alter cubes
      esDrawSelection,
      esWaterEmitter, //place water emmiter
      esOxygenEmitter,
      //pick block for brush color fill
      esPickBlockColor ,
      esPaintColors
      );

implementation

end.

