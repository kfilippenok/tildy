{ Copyright (c) 2024-2025 Kirill Filippenok

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License. }


unit Tildy.Core.Contracts.Projection;

{$mode ObjFPC}{$H+}

interface

uses
  Tildy.Core.Contracts.Coordinates;

type
  { Картографическая проекция
    Преобразует географические координаты (WGS84) в проекционные (метры) }
  IProjection = interface
    ['{A1B2C3D4-E5F6-4789-0123-456789ABCDEF}']

    // WGS84 (градусы) → проекционные координаты (метры)
    function Project(const AGeo: TGeoPoint): TProjectedPoint;

    // Проекционные координаты (метры) → WGS84 (градусы)
    function Unproject(const AProjected: TProjectedPoint): TGeoPoint;

    function GetBounds: TProjectedBounds;
    function GetEPSGCode: Integer;
    function GetName: string;

    // Границы проекции в метрах
    property Bounds: TProjectedBounds read GetBounds;
    // EPSG код проекции
    property EPSGCode: Integer read GetEPSGCode;
    // Название проекции
    property Name: String read GetName;
  end;

implementation

end.


implementation

end.

