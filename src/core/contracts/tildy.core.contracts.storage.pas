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

unit Tildy.Core.Contracts.Storage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  BGRABitmap,
  Tildy.Core.Contracts.Coordinates;

type

  { IStorage - хранилище плиток

    Примеры хранилищ:
    - Файловое хранилище
    - Оперативная память
    - База данных
    - Моковое хранилище (для тестов) }
  IStorage = interface
    ['{B2C3D4E5-F6A7-8901-BCDE-F12345678901}']

    { Сохранение плитки
      @param ACoord Координаты плитки
      @param ATile Изображение плитки
      @param AMetadata Метадата (опционально)
      @raises EStorageError если возникли проблемы при сохранении }
    procedure SaveTile(const ACoord: TTileCoord;
                      const ATile: TBGRABitmap;
                      const AMetadata: string = '');

    { Проверка доступности плитки
      @param ACoord Координаты плитки
      @return True Если плитка имеется в хранилище }
    function TileExists(const ACoord: TTileCoord): Boolean;

    // Название
    function GetName: string;
    property Name: string read GetName;
  end;

  EStorageError = class(Exception);

implementation

end.

