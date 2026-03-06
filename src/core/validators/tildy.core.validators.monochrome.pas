unit Tildy.Core.Validators.Monochrome;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Graphics,
  BGRABitmap,
  BGRABitmapTypes,
  Tildy.Core.Contracts.Validator,
  Tildy.Core.Contracts.Coordinates;

type

  { TMonochromeValidator }

  TMonochromeValidator = class(TInterfacedObject, IValidator)
  strict private
    function GetName: String;
    function GetDescription: String;
    function InColors(const AColor: TColor): Boolean;
  strict protected
    FColors: array of TColor;
    FName: String;
    FDescription: String;
  public
    constructor Create(const AColors: array of TColor;
                       const AName: String = 'Monochrome validator.';
                       const ADescription: String = 'Validate monochrome tiles.'); reintroduce;

    function ShouldSkipTile(const ACoord: TTileCoord; ATile: TBGRABitmap): TValidateResult;

    property Name: String read GetName;
    property Description: String read GetDescription;
  end;

implementation

{ TMonochromeValidator }

function TMonochromeValidator.GetName: String;
begin
  Result := FName;
end;

function TMonochromeValidator.GetDescription: String;
begin
  Result := FDescription;
end;

function TMonochromeValidator.InColors(const AColor: TColor): Boolean;
var
  Color: TColor;
begin
  Result := False;
  for Color in FColors do
    if AColor = Color then
      Exit(True);
end;

constructor TMonochromeValidator.Create(const AColors: array of TColor;
  const AName: String; const ADescription: String);
begin
  inherited Create;

  CopyArray(@FColors, @AColors, TypeInfo(AColors), 1);
  FName := AName;
  FDescription := ADescription;
end;

function TMonochromeValidator.ShouldSkipTile(const ACoord: TTileCoord;
  ATile: TBGRABitmap): TValidateResult;
var
  ix, iy: Integer;
  PrevPixel, CurrentPixel: TBGRAPixel;
begin
  Result.ShouldSkip := True;
  Result.ValidatorName := FName;
  Result.Reason := 'Tile is monochrome.';
  for ix := 0 to ATile.Width - 1 do
  for iy := 0 to ATile.Height - 1 do
  begin
    CurrentPixel := ATile.GetPixel(ix, iy);

    if Assigned(FColors) and not InColors(CurrentPixel.ToColor) then
    begin
      Result.ShouldSkip := False;
      Exit;
    end;

    if (ix = 0) and (iy = 0) then
      PrevPixel := CurrentPixel;
    if PrevPixel <> CurrentPixel then
    begin
      Result.ShouldSkip := False;
      Exit;
    end;
    PrevPixel := CurrentPixel;
  end;
end;

end.



