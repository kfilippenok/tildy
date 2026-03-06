unit Tildy.Tests.Core.Sources.HTTP;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry,
  // Core
  Tildy.Core.Sources.FpHTTP,
  // Tests
  Tildy.Tests.Core.Sources.Base;

type

  THTTPSourceTest = class(TSourceTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
  end;

implementation

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

