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

unit Tildy.Options;

{$mode ObjFPC}{$H+}

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
                 okUseCacheOnly);

  TOptions = Set of TOptionKind;

  function getOptionName(Option: TOptionKind): String;
  function getOptionIdent(Option: TOptionKind): String;

implementation

function getOptionName(Option: TOptionKind): String;
begin
  case Option of
    okHelp          : Exit('help');
    okProvider      : Exit('provider');
    okProviders     : Exit('providers');
    okLayers        : Exit('layers');
    okOut           : Exit('out');
    okMinZoom       : Exit('min-zoom');
    okMaxZoom       : Exit('max-zoom');
    okLeft          : Exit('left');
    okTop           : Exit('top');
    okRight         : Exit('right');
    okBottom        : Exit('bottom');
    okShowFileType  : Exit('show-file-type');
    okTileRes       : Exit('tile-res');
    okSkipMissing   : Exit('skip-missing');
    okSkipExisting  : Exit('skip-existing');
    okFilter        : Exit('filter');
    okVersion       : Exit('version');
    okBoundingBox   : Exit('bbox');
    okCache         : Exit('cache');
    okUseCacheOnly  : Exit('use-cache-only');
  else
    Exit('unknown');
  end;
end;

function getOptionIdent(Option: TOptionKind): String;
begin
  case Option of
    okHelp          : Exit('h');
    okProvider      : Exit('p');
    okProviders     : Exit('ps');
    okLayers        : Exit('ls');
    okOut           : Exit('o');
    okMinZoom       : Exit('z');
    okMaxZoom       : Exit('Z');
    okLeft          : Exit('l');
    okTop           : Exit('t');
    okRight         : Exit('r');
    okBottom        : Exit('b');
    okShowFileType  : Exit('sft');
    okTileRes       : Exit('res');
    okSkipMissing   : Exit('skm');
    okSkipExisting  : Exit('ske');
    okFilter        : Exit('f');
    okVersion       : Exit('v');
    okBoundingBox   : Exit('bb');
    okCache         : Exit('c');
    okUseCacheOnly  : Exit('uco');
  else
    Exit('unknown');
  end;
end;

end.

