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

unit Tildy.Core.Contracts.Source;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  BGRABitmap,
  Tildy.Core.Contracts.Coordinates;

type

  { ISource - источник плиток

    Примеры источников:
    - Интернет-провайдер карт (скачивание)
    - Файловая система (локальные файлы)
    - Моковый источник (для тестов) }
  ISource = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']

    { Получение плитки по координатам
      @param ACoord Координаты плитки
      @return Изображение, или nil если отсутствует
      @raises ESourceError если возникли проблемы при загрузке }
    function GetTile(const ACoord: TTileCoord): TBGRABitmap;

    { Проверка доступности плитки без загрузки
      @param ACoord Координаты плитки
      @return True если плитка доступна }
    function IsTileAvailable(const ACoord: TTileCoord): Boolean;

    // Название источника
    function GetName: string;
    property Name: string read GetName;
  end;

  ESourceError = class(Exception);

  function TileCoord(const AZoom: Byte; const AX, AY: QWord): TTileCoord;

implementation

function TileCoord(const AZoom: Byte; const AX, AY: QWord): TTileCoord;
begin
  Result.Zoom := AZoom;
  Result.X := AX;
  Result.Y := AY;
end;

end.

