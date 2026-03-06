unit Tildy.Tests.Core.Validators.Monochrome;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Graphics,
  fpcunit,
  testregistry,
  BGRABitmap,
  // Core
  Tildy.Core.Contracts.Coordinates,
  Tildy.Core.Contracts.Validator,
  Tildy.Core.Validators.Monochrome,
  // Tests
  Tildy.Tests.Core.Validators.Base;

type

  { TMonochromeTestCase }

  TMonochromeTestCase = class(TValidatorTestCase)
  strict private
    FTestColors: array of TColor;
  protected
    procedure SetUp; override;
  published
    procedure ShouldSkipTile_AllPixelsWhite_ReturnsTrue;
    procedure ShouldSkipTile_OneRedPixelPresent_ReturnsFalse;
  end;

implementation

{ TMonochromeTestCase }

procedure TMonochromeTestCase.SetUp;
begin
  inherited SetUp;

  FTestColors := [clRed, clWhite];
  FValidator := TMonochromeValidator.Create(FTestColors);
end;

procedure TMonochromeTestCase.ShouldSkipTile_AllPixelsWhite_ReturnsTrue;
var
  Bitmap: TBGRABitmap;
begin
  try
    Bitmap := TBGRABitmap.Create(100, 100, clWhite);
    AssertTrue(
      FValidator.ShouldSkipTile(
        TTileCoord.Create(0, 0, 0),
        Bitmap
      ).ShouldSkip
    );
  finally
    Bitmap.Free;
  end;
end;

procedure TMonochromeTestCase.ShouldSkipTile_OneRedPixelPresent_ReturnsFalse;
var
  Bitmap: TBGRABitmap;
begin
  try
    Bitmap := TBGRABitmap.Create(100, 100, clWhite);
    Bitmap.Colors[10, 10] := TColorToFPColor(clRed);
    AssertFalse(
      FValidator.ShouldSkipTile(
        TTileCoord.Create(0, 0, 0),
        Bitmap
      ).ShouldSkip
    );
  finally
    Bitmap.Free;
  end;
end;

initialization

  RegisterTest(TMonochromeTestCase);
end.

