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

unit TilesDownload.Utilities;

{$mode ObjFPC}{$H+}{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, TilesDownload.Classes, TilesDownload.Exceptions;

  function GetTDClassOnIdent(Ident: String): RefCTilesDownloader;
  function GetMergedTDClassOnIdent(Ident: String): RefCMergedTD;

implementation

function GetTDClassOnIdent(Ident: String): RefCTilesDownloader;
begin
  case Ident of
    'osm'      : Result := CTDOpenStreetMap;
    'otm'      : Result := CTDOpenTopotMap;
    'osm-cycle': Result := CTDCycleOSM;
    'railway'  : Result := CTDOpenRailwayMap;
  else
    raise EUnknownIdentProvider.Create('Unknown provider identificator.');
  end;
end;

function GetMergedTDClassOnIdent(Ident: String): RefCMergedTD;
begin
   case Ident of
    'osm'      : Result := CMrgTDOpenStreetMap;
    'otm'      : Result := CMrgTDOpenTopotMap;
    'osm-cycle': Result := CMrgTDCycleOSM;
    'railway'  : Result := CMrgTDOpenRailwayMap;
  else
    raise EUnknownIdentProvider.Create('Unknown provider identificator.');
  end;
end;

end.

