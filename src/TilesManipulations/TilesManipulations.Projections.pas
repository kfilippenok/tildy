{
  Copyright (c) 2024-2025 Kirill Filippenok

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License. }

unit TilesManipulations.Projections;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TilesManipulations.Base;

type

  { TEPSG3857 }

  TEPSG3857 = class(TInterfacedObject, IProjection)
  const
    // https://epsg.io/3857
    LAT_MIN = -85.0511;
    LAT_MAX = 85.0511;
    LON_MIN = -179.99999;
    LON_MAX = 179.99999;
  public
    function MinLat: Extended;
    function MaxLat: Extended;
    function MinLon: Extended;
    function MaxLon: Extended;
    function CalcTileX(const AZoom: Byte; const ALongitude: Extended): QWord;
    function CalcTileY(const AZoom: Byte; const ALatitude: Extended): QWord;
  end;

implementation

uses
  Math;

{ TEPSG3857 }

function TEPSG3857.MinLat: Extended;
begin
  Result := LAT_MIN;
end;

function TEPSG3857.MaxLat: Extended;
begin
  Result := LAT_MAX;
end;

function TEPSG3857.MinLon: Extended;
begin
  Result := LON_MIN;
end;

function TEPSG3857.MaxLon: Extended;
begin
  Result := LON_MAX;
end;

function TEPSG3857.CalcTileX(const AZoom: Byte; const ALongitude: Extended): QWord;
var
  n: Extended;
begin
  if AZoom = 0 then Exit(0);

  n := Power(2, AZoom);
  Result := Trunc(((ALongitude + 180) / 360) * n);
end;

function TEPSG3857.CalcTileY(const AZoom: Byte; const ALatitude: Extended): QWord;
var
  lat_rad, n, x1, x2, x3, x4, x5: Extended;
begin
  if AZoom = 0 then Exit(0);

  n := Power(2, AZoom);
  lat_rad := DegToRad(ALatitude);
  x1 := Tan(lat_rad);
  x2 := ArcSinH(x1);
  x3 := x2 / Pi;
  x4 := (1 - x3);
  x5 := x4 / 2.0;
  Result := Trunc(x5 * n);
end;

end.

