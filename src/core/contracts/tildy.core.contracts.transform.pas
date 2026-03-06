unit Tildy.Core.Contracts.Transform;

{$mode ObjFPC}{$H+}

interface

uses
  Tildy.Core.Contracts.Coordinates;

type
  { Трансформатор систем координат
    Преобразует внешние СК (UTM, Pulkovo, Web Mercator) в/из WGS84 }
  ICoordinateTransformer = interface
    ['{C3D4E5F6-A7B8-4901-2345-67890ABCDEF0}']

    { Внешняя СК → WGS84 (для передачи в Engine) }
    function ToWGS84(const APoint: TProjectedPoint): TGeoPoint;

    { WGS84 → внешняя СК (для отображения пользователю) }
    function FromWGS84(const APoint: TGeoPoint): TProjectedPoint;

    { Преобразование границ: внешняя СК → WGS84 }
    function BoundsToWGS84(const AOrigin, ASize: TProjectedPoint): TGeoBounds;

    { EPSG код внешней системы координат }
    function GetEPSGCode: Integer;

    { Название системы координат }
    function GetName: string;
  end;

implementation

end.

