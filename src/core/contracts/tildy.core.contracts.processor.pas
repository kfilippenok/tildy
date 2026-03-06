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

unit Tildy.Core.Contracts.Processor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  BGRABitmap,
  Tildy.Core.Contracts.Coordinates;

type

  { IProcessor - обработчик плиток (валидаторы, фильтры, композитор слоёв)

    Примеры обработчиков:
    - Фильтры (применение визуальных фильтров к изображению плитки)
    - Редактор размера (изменение размера изображения плитки)
    - Конвейер (применение множества визуальных фильтров к изображению плитки) }
  IProcessor = interface
    ['{C3D4E5F6-A7B8-9012-CDEF-123456789012}']

    { Обработка изображения плитки
      @param ACoord Координаты плитки
      param ABGRABitmap Изображение для обработки (resized in place) }
    procedure Process(const ACoord: TTileCoord; var ATile: TBGRABitmap);

    // Name of the processor
    function GetName: string;
    property Name: string read GetName;
  end;

  IProcessorChain = interface(IProcessor)
    ['{E49A9613-6F4D-407F-B5BF-2F02F05253AC}']

    function GetProcessor(const AIndex: Integer): IProcessor;

    procedure AddProcessor(AProcessor: IProcessor);
    procedure RemoveProcessor(AProcessor: IProcessor);
    procedure ClearProcessors;

    function GetProcessorCount: Integer;
    property ProcessorCount: Integer read GetProcessorCount;
    property Processors[AIndex: Integer]: IProcessor read GetProcessor;
  end;

  IProcessorPipeline = interface(IProcessorChain)
    ['{61E85708-418C-4E30-8140-E07B4475EE8A}']
  end;

  EProcessorError = class(Exception);

implementation

end.

