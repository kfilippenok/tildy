unit Tildy.Core.Schemes.OSM;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fphttpclient,
  BGRABitmap,
  // Contracts
  Tildy.Core.Contracts.Projection,
  Tildy.Core.Contracts.Coordinates,
  Tildy.Core.Contracts.Scheme;

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

function TWSG84Projection.GetBounds: TProjectedBounds;
begin

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

