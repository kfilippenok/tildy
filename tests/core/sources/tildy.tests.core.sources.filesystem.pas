unit Tildy.Tests.Core.Sources.FileSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry,
  // Core
  Tildy.Core.Sources.FileSystem,
  // Tests
  Tildy.Tests.Core.Sources.Base;

type

  TFileSystemSourceTest = class(TSourceTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
  end;

implementation

procedure TFileSystemSourceTest.TestHookUp;
begin
  Fail('Напишите ваш тест');
end;

procedure TFileSystemSourceTest.SetUp;
begin

end;

procedure TFileSystemSourceTest.TearDown;
begin

end;

initialization

  RegisterTest(TFileSystemSourceTest);
end.


