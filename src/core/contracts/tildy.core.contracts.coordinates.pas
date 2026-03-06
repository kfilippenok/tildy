unit Tildy.Core.Contracts.Coordinates;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

type

  { Географические координаты (внутренний формат)
    - Широта: -90..90 (юг..север)
    - Долгота: -180..180 (запад..восток)
    - EPSG:4326 (WGS84) }
  TGeoPoint = record
    Lat, Lon: Double;
    class function Create(const ALat, ALon: Double): TGeoPoint; static;
    function IsValid: Boolean;
  end;

  { Границы в географических координатах }
  TGeoBounds = record
    MinLat, MaxLat, MinLon, MaxLon: Double;
    class function Create(const AMinLat, AMaxLat, AMinLon, AMaxLon: Double): TGeoBounds; static;
    function IsValid: Boolean;
    function Center: TGeoPoint;
  end;

  { Проекционные координаты (для расчетов)
    - X: метры от нулевого меридиана
    - Y: метры от экватора
    - Зависит от проекции (Web Mercator, UTM, итд.) }
  TProjectedPoint = record
    X, Y: Double;
    class function Create(const AX, AY: Double): TProjectedPoint; static;
  end;

  { Границы в проекционных координатах }
  TProjectedBounds = record
    MinX, MaxX, MinY, MaxY: Double;
  end;

  { Пиксельные координаты на карте }
  TPixelCoord = record
    X, Y: Double;
    Zoom: Byte;
  end;

  { Тайловые координаты (финальный результат)
    - Zoom: уровень масштаба
    - X, Y: номера плиток }
  TTileCoord = record
    Zoom: Byte;
    X, Y: QWord;
    class function Create(const AZoom: Byte; const AX, AY: QWord): TTileCoord; static;
    function ToString: string;
  end;

  { Начало координат тайловой сетки }
  TTileOrigin = (toTopLeft, toBottomLeft);

implementation

uses
  SysUtils;

{ TGeoPoint }

class function TGeoPoint.Create(const ALat, ALon: Double): TGeoPoint;
begin
  Result.Lat := ALat;
  Result.Lon := ALon;
end;

function TGeoPoint.IsValid: Boolean;
begin
  Result := (Lat >= -90) and (Lat <= 90) and (Lon >= -180) and (Lon <= 180);
end;

{ TGeoBounds }

class function TGeoBounds.Create(const AMinLat, AMaxLat, AMinLon, AMaxLon: Double): TGeoBounds;
begin
  Result.MinLat := AMinLat;
  Result.MaxLat := AMaxLat;
  Result.MinLon := AMinLon;
  Result.MaxLon := AMaxLon;
end;

function TGeoBounds.IsValid: Boolean;
begin
  Result := (MinLat >= -90) and (MaxLat <= 90) and
            (MinLon >= -180) and (MaxLon <= 180) and
            (MinLat < MaxLat) and (MinLon < MaxLon);
end;

function TGeoBounds.Center: TGeoPoint;
begin
  Result.Lat := (MinLat + MaxLat) / 2;
  Result.Lon := (MinLon + MaxLon) / 2;
end;

{ TProjectedPoint }

class function TProjectedPoint.Create(const AX, AY: Double): TProjectedPoint;
begin
  Result.X := AX;
  Result.Y := AY;
end;

{ TTileCoord }

class function TTileCoord.Create(const AZoom: Byte; const AX, AY: QWord): TTileCoord;
begin
  Result.Zoom := AZoom;
  Result.X := AX;
  Result.Y := AY;
end;

function TTileCoord.ToString: string;
begin
  Result := Format('%d/%d/%d', [Zoom, X, Y]);
end;

end.
