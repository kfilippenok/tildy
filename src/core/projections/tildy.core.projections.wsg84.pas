unit Tildy.Core.Projections.WSG84;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fphttpclient,
  BGRABitmap,
  // Contracts
  Tildy.Core.Contracts.Projection,
  Tildy.Core.Contracts.Coordinates;

type

  { TWSG84Projection }

  TWSG84Projection = class(TInterfacedObject, IProjection)
  strict protected
    function GetBounds: TProjectedBounds;
    function GetEPSGCode: Integer;
    function GetName: string;
  public
    function Project(const AGeo: TGeoPoint): TProjectedPoint;
    function Unproject(const AProjected: TProjectedPoint): TGeoPoint;
  public
    property Bounds: TProjectedBounds read GetBounds;
    property EPSGCode: Integer read GetEPSGCode;
    property Name: String read GetName;
  end;

implementation

{ TWSG84Projection }

{
// TMapViewerEngine.DegreesToPixelsEPSG3857
  WorldAreaEPSG3395: TRealArea = (
    TopLeft: (lon: -180; lat: 80);
    BottomRight: (lon: 180; lat: -84)
  );
  // TMapViewerEngine.DegreesToPixelsEPSG3857
  WorldAreaEPSG3857: TRealArea = (
    TopLeft : (lon: -180; lat: 85.05112878);
    BottomRight : (lon: 180; lat: -85.05112878)
  );
}

function TWSG84Projection.GetBounds: TProjectedBounds;
begin
  Result.MaxX := 180;
  Result.MinX := -Result.MaxX;
  Result.MaxY := 85.05112878;
  Result.MinY := -Result.MaxY;
end;

function TWSG84Projection.GetEPSGCode: Integer;
begin
  Result := 3857;
end;

function TWSG84Projection.GetName: string;
begin

end;

function TWSG84Projection.Project(const AGeo: TGeoPoint): TProjectedPoint;
begin

end;

function TWSG84Projection.Unproject(const AProjected: TProjectedPoint
  ): TGeoPoint;
begin

end;

end.

