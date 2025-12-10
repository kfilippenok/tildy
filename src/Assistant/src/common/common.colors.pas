unit Common.Colors;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, System.UITypes;

type

  IDefColors = interface
    ['{90808EA2-E14B-476F-9F36-F8A29DAE6067}']
    function GetAccent   : TColor;
    function GetSecondary: TColor;
    property Accent      : TColor read GetAccent;
    property Secondary   : TColor read GetSecondary;
  end;

  function DefColors: IDefColors;

  function IsDarkTheme: Boolean;

implementation

type

  { TDefColors }

  TDefColors = class sealed(TInterfacedObject, IDefColors)
  private
    FAccent                : TColor;
    FSecondary             : TColor;
    function GetAccent                : TColor;
    function GetSecondary             : TColor;
  public
    constructor Create;
  public
    property Accent   : TColor read GetAccent;
    property Secondary: TColor read GetSecondary;
  end;

var
  _DefColors: IDefColors;

function DefColors: IDefColors;
begin
  Result := _DefColors;
end;

function IsDarkTheme: Boolean;

  function _Level(AColor: TColor): double;
  begin
    Result:= Red(AColor)*0.3 + Green(AColor)*0.59 + Blue(AColor)*0.11;
  end;

begin
  Result:= _Level(ColorToRGB(clWindow)) < _Level(ColorToRGB(clWindowText));
end;

{ TDefColors }

function TDefColors.GetAccent: TColor;
begin
  Result := FAccent;
end;

function TDefColors.GetSecondary: TColor;
begin
  Result := FSecondary;
end;

constructor TDefColors.Create;
begin
  FAccent    := RGBToColor(58, 140, 228);
  FSecondary := RGBToColor(58, 122, 180);
end;

initialization
  _DefColors := TDefColors.Create;
end.

