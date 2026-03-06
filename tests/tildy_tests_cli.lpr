program tildy_tests_cli;

{$mode objfpc}{$H+}

uses
  Classes,
  consoletestrunner,
  fpcunittestinsight,
  jsonparser,
  Tildy.Tests.Core.Validators;

type

  { TMyTestRunner }

  TTildyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TTildyTestRunner;

begin
  if IsTestInsightListening() then
  begin
    RunRegisteredTests('','');
    Exit;
  end;

  DefaultRunAllTests:=True;
  DefaultFormat := fPlain;
  Application := TTildyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'Tildy console test runner';
  Application.Run;
  Application.Free;
end.
