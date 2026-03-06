unit Tildy.Core.Contracts.Scheme;

{$mode ObjFPC}{$H+}

interface

uses
  Tildy.Core.Contracts.Coordinates,
  Tildy.Core.Contracts.Projection;

type

  { Схема нарезки карты на тайлы
    Использует IProjection внутри для математических преобразований }
  IScheme = interface
    ['{B2C3D4E5-F6A7-4890-1234-567890ABCDEF}']

    { Transformations Geo ↔ Tile }
    function GeoToTile(const AGeo: TGeoPoint; AZoom: Byte): TTileCoord;
    function TileToGeo(const ATile: TTileCoord): TGeoPoint;
    function TileBounds(const ATile: TTileCoord): TGeoBounds;

    { Transformations Geo ↔ Tile Pixel }
    function GeoToPixel(const AGeo: TGeoPoint; AZoom: Byte): TPixelCoord;
    function PixelToGeo(const APixel: TPixelCoord): TGeoPoint;

    function GetTileSize: Integer;
    function GetOrigin: TTileOrigin;
    function GetMinZoom: Byte;
    function GetMaxZoom: Byte;
    function GetProjection: IProjection;

    property TileSize: Integer read GetTileSize;
    property Origin: TTileOrigin read GetOrigin;
    property MinZoom: Byte read GetMinZoom;
    property MaxZoom: Byte read GetMaxZoom;
    property Projection: IProjection read GetProjection;
  end;

implementation

end.

