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

program tilesdownloader;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Interfaces, // For BGRABitmap
  {$ENDIF}
  SysUtils, Classes, CustApp, TilesManipulations;

var
  //OptionParameter: array[TOptionKind] of String;
  //glOptions: TOptions;
  FormatSettings: TFormatSettings;

type

  { ATilesDownloader }

  ATilesDownloader = class(TCustomApplication)
  strict private
    FProviders: TProviders;
    procedure SetupProviders;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    property Providers: TProviders read FProviders write FProviders;
  end;

  procedure ATilesDownloader.SetupProviders;
  begin
    Providers.Add('osm-standard'            , 'OpenStreetMap-Standard'        , 'http://a.tile.openstreetmap.org');
    Providers.Add('railway-standard'        , 'OpenRailwayMap-Standard'       , 'http://b.tiles.openrailwaymap.org/standard');
    Providers.Add('railway-maxspeed'        , 'OpenRailwayMap-Maxspeed'       , 'http://b.tiles.openrailwaymap.org/maxspeed');
    Providers.Add('railway-electrification' , 'OpenRailwayMap-Electrification', 'http://b.tiles.openrailwaymap.org/electrification');
  end;

  procedure ATilesDownloader.DoRun;
  var
    TilesManipulator: TTilesManipulator;
  begin
    TilesManipulator := TTilesManipulator.Create;
    TilesManipulator.Layers.Add(Providers['osm-standard']);
    //TilesManipulator.Layers.Load(0, 0, 0);
    //TilesManipulator.Download(1, 1);

    WriteLn(TilesManipulator.CalcTileX(0, 180));
    WriteLn(TilesManipulator.CalcTileY(0, -85.0511));

    //ProviderClient := TProviderClient.Create(nil);
    //ProviderClient.;

    TilesManipulator.Free;

    Terminate;
  end;

  constructor ATilesDownloader.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);

    FProviders := TProviders.Create;
    SetupProviders;

    StopOnException := True;
    FormatSettings.DecimalSeparator := '.';
  end;

  destructor ATilesDownloader.Destroy;
  begin
    inherited Destroy;

    FreeAndNil(FProviders);
  end;

var
  appTilesDownloader: ATilesDownloader;
begin
  appTilesDownloader := ATilesDownloader.Create(nil);
  appTilesDownloader.Title := 'Tiles downloader';
  appTilesDownloader.Run;
  appTilesDownloader.Free;
end.

