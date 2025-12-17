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

unit Tildy.CLI.Options;

{$mode ObjFPC}{$H+}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils;

type

  EOption = class(Exception);
  EOpProvider = class(EOption);
  EOpProviders = class(EOption);
  EOpLayers = class(EOption);
  EOpOutput = class(EOption);
  EOpMinZoom = class(EOption);
  EOpMaxZoom = class(EOption);
  EOpLeft = class(EOption);
  EOpTop = class(EOption);
  EOpRight = class(EOption);
  EOpBottom = class(EOption);
  EOpTileRes = class(EOption);
  EOpFilter = class(EOption);
  EOpBoundingBox = class(EOption);
  EOpCache = class(EOption);
  EOpAreas = class(EOption);
  EOpMonochrome = class(EOption);
  EOpMonochromes = class(EOption);

  TOptionKind = (okHelp,
                 okProvider,
                 okProviders,
                 okLayers,
                 okOut,
                 okMinZoom,
                 okMaxZoom,
                 okLeft,
                 okTop,
                 okRight,
                 okBottom,
                 okShowFileType,
                 okTileRes,
                 okSkipMissing,
                 okSkipExisting,
                 okFilter,
                 okVersion,
                 okBoundingBox,
                 okCache,
                 okUseCacheOnly,
                 okAreas,
                 okMonochrome,
                 okMonochromes);

  TOptions = Set of TOptionKind;

  { THelperOptionKind }

  THelperOptionKind = type Helper for TOptionKind
    function Name : String;
    function Ident: String;
  end;

const
  OptionName: array [TOptionKind] of String =
  (
    'help',
    'provider',
    'providers',
    'layers',
    'out',
    'min-zoom',
    'max-zoom',
    'left',
    'top',
    'right',
    'bottom',
    'show-file-type',
    'tile-res',
    'skip-missing',
    'skip-existing',
    'filter',
    'version',
    'bbox',
    'cache',
    'use,cache-only',
    'areas',
    'monochrome',
    'monochromes'
  );

  OptionIdent: array [TOptionKind] of String =
  (
    'h',
    'p',
    'ps',
    'ls',
    'o',
    'z',
    'Z',
    'l',
    't',
    'r',
    'b',
    'sft',
    'res',
    'skm',
    'ske',
    'f',
    'v',
    'bb',
    'c',
    'uco',
    'as',
    'm',
    'ms'
  );

implementation

{ THelperOptionKind }

function THelperOptionKind.Name: String;
begin
  Result := OptionName[Self];
end;

function THelperOptionKind.Ident: String;
begin
  Result := OptionIdent[Self];
end;

end.

