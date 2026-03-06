unit Tildy.Tests.Core.Sources.Base;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  // Core
  Tildy.Core.Contracts.Coordinates,
  Tildy.Core.Contracts.Source,
  // Tests
  Tildy.Tests.Core.Base;

type

  { TSourceTestCase }

  TSourceTestCase = class(TCoreTestCase)
  strict protected
    FSource: ISource;
  strict protected
    procedure AssertValidTileCoord(const ACoord: TTileCoord);
    procedure AssertSourceName(const AName: string);
  protected
    procedure TearDown; override;
  published
    procedure TestSourceName;
  end;

implementation

procedure TSourceTestCase.AssertValidTileCoord(const ACoord: TTileCoord);
var
  MaxXY: QWord;
begin
  Assert(ACoord.Zoom <= 30, 'Zoom level out of range');
  MaxXY := QWord(1) shl ACoord.Zoom;

  Assert(ACoord.X < MaxXY, 'Tile X out of range');
  Assert(ACoord.Y < MaxXY, 'Tile Y out of range');
end;

procedure TSourceTestCase.AssertSourceName(const AName: string);
begin

end;

procedure TSourceTestCase.TearDown;
begin
  FSource := nil;
  inherited TearDown;
end;

procedure TSourceTestCase.TestSourceName;
begin
  AssertSourceName(FSource.Name);
end;

end.

