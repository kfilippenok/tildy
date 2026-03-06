unit Tildy.Tests.Core.Sources.HTTPS;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry,
  // Core
  Tildy.Core.Sources.HTTPS,
  // Tests
  Tildy.Tests.Core.Sources.Base,
  Tildy.Tests.Core.Sources.HTTP;

type

  THTTPSSourceTest = class(THTTPSourceTest)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
  end;

implementation

uses
  Tildy.Core.Sources.HTTP;

procedure THTTPSourceTest.TestHookUp;
begin
  Fail('Напишите ваш тест');
end;

procedure THTTPSourceTest.SetUp;
begin

end;

procedure THTTPSourceTest.TearDown;
begin

end;

initialization

  RegisterTest(THTTPSourceTest);
end.

