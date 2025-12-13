program tildy_gui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  // Forms
  Tildy.GUI.Forms.Main,
  // Dialogs
  Tildy.GUI.Dialogs.AddLayers,
  Tildy.GUI.Dialogs.EditAreaName,
  // Translate
  Tildy.GUI.i18n.Runtime;

{$R *.res}

begin
  RequireDerivedFormResource :=True;
  Application.Title:='Tildy';
  Application.Scaled:=True;
  Application.{%H-}MainFormOnTaskbar := True;
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  ApplyGUILanguage('ru');
  //InitGUILanguageFromSystem;
  Application.Run;
end.

