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

unit TilesDownload.Types;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

TOptionKind = (okHelp,
               okProvider,
               okProviderName,
               okProviderLink,
               okOutput,
               okSaveMethod,
               okDivider,
               okMinZoom,
               okMaxZoom,
               okFirstCoordLat,
               okFirstCoordLon,
               okSecondCoordLat,
               okSecondCoordLon);

TOptions = Set of TOptionKind;

function getOptionName(Option: TOptionKind): String;

implementation

function getOptionName(Option: TOptionKind): String;
begin
  case Option of
    okHelp          : Exit('help');
    okProvider      : Exit('provider');
    okProviderName  : Exit('provider-name');
    okProviderLink  : Exit('provider-link');
    okOutput        : Exit('output');
    okSaveMethod    : Exit('save-method');
    okDivider       : Exit('divider');
    okMinZoom       : Exit('min-zoom');
    okMaxZoom       : Exit('max-zoom');
    okFirstCoordLat : Exit('fсoord-lat');
    okFirstCoordLon : Exit('fсoord-lon');
    okSecondCoordLat: Exit('scoord-lat');
    okSecondCoordLon: Exit('scoord-lon');
  else
    Exit('unknown');
  end;
end;

end.

