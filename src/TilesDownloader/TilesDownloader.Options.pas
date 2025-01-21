{
  Copyright (c) 2024 Kirill Filippenok

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License. }

unit TilesDownloader.Options;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

TOptionKind = (okHelp,
               okProvider,
               okProviders,
               okOutput,
               okMinZoom,
               okMaxZoom,
               okLeft,
               okTop,
               okRight,
               okBottom,
               okShowFileType,
               okTileRes,
               okSkipMissing,
               okFilter);

TOptions = Set of TOptionKind;

function getOptionName(Option: TOptionKind): String;

implementation

function getOptionName(Option: TOptionKind): String;
begin
  case Option of
    okHelp          : Exit('help');
    okProvider      : Exit('provider');
    okProviders     : Exit('providers');
    okOutput        : Exit('out');
    okMinZoom       : Exit('min-zoom');
    okMaxZoom       : Exit('max-zoom');
    okLeft          : Exit('left');
    okTop           : Exit('right');
    okRight         : Exit('top');
    okBottom        : Exit('bottom');
    okShowFileType  : Exit('show-file-type');
    okTileRes       : Exit('tile-res');
    okSkipMissing   : Exit('skip-missing');
    okFilter        : Exit('filter');
  else
    Exit('unknown');
  end;
end;

end.

