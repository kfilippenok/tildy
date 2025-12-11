unit Tildy.GUI.Brand.Colors;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Graphics, System.UITypes;

type

  BrandColors = record
  const
    Accent   : TColor = TColor($00E48C3A);
    Secondary: TColor = TColor($00B47A37);
  end;

  function IsDarkTheme: Boolean;

implementation

function IsDarkTheme: Boolean;

  function _Level(AColor: TColor): double;
  begin
    Result:= Red(AColor)*0.3 + Green(AColor)*0.59 + Blue(AColor)*0.11;
  end;

begin
  Result:= _Level(ColorToRGB(clWindow)) < _Level(ColorToRGB(clWindowText));
end;

end.

