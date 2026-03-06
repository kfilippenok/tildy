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
  // Translate
  Tildy.GUI.i18n.Runtime;

{$R *.res}

begin
  RequireDerivedFormResource :=True;
  Application.Title:='Tildy';
  Application.Scaled:=True;
  Application.{%H-}MainFormOnTaskbar := True;
  Application.Initialize;
  SetLanguageFromSystem;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.

