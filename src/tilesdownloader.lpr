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
  SysUtils, Classes, CustApp,
  TilesDownloader.Options, IniFiles,//TilesDownloader.IniFiles,
  TilesManipulations.Base, TilesManipulations.Projections, TilesManipulations.Filters;

var
  OptionParameter: array[TOptionKind] of String;
  glOptions: TOptions;
  FormatSettings: TFormatSettings;

type

  { ATilesDownloader }

  ATilesDownloader = class(TCustomApplication)
  strict private
    FProviders: TProviders;
    FFilters  : TFilters;
    procedure SetupProviders;
    procedure SetupFilters;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure WriteHelp;
    function ImportProviders(AFilePath: String): Boolean;
    function ImportLayers(ATilesManipulator: TTilesManipulator; AFilePath: String): Boolean;
  public
    property Providers: TProviders read FProviders write FProviders;
    property Filters  : TFilters   read FFilters   write FFilters;
  end;

  procedure ATilesDownloader.SetupProviders;
  begin
    Providers.Add('osm-standard', 'OpenStreetMap-Standard', 'http://a.tile.openstreetmap.org/{z}/{x}/{y}.png', TEPSG3857.Create);
    Providers.Add('railway-standard', 'OpenRailwayMap-Standard', 'http://b.tiles.openrailwaymap.org/standard/{z}/{x}/{y}.png', TEPSG3857.Create);
    Providers.Add('railway-maxspeed', 'OpenRailwayMap-Maxspeed', 'http://b.tiles.openrailwaymap.org/maxspeed/{z}/{x}/{y}.png', TEPSG3857.Create);
    Providers.Add('railway-electrification' , 'OpenRailwayMap-Electrification', 'http://b.tiles.openrailwaymap.org/electrification/{z}/{x}/{y}.png', TEPSG3857.Create);
  end;

  procedure ATilesDownloader.SetupFilters;
  begin
    Filters.Add('grayscale', TFilterGrayscale.Create);
  end;

  procedure ATilesDownloader.DoRun;
  var
    TilesManipulator: TTilesManipulator;
  begin
    if hasOption(getOptionName(okHelp)) then
    begin
      writeHelp;
      Terminate;
      Exit;
    end;

    TilesManipulator := TTilesManipulator.Create;
    ImportProviders('layers.ini');
    ImportLayers(TilesManipulator, 'layers.ini');
    TilesManipulator.Path := 'tiles/{p}/{z}/{x}/{y}';
    TilesManipulator.Download(0, 1);
    TilesManipulator.Free;

    Terminate;
  end;

  constructor ATilesDownloader.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);

    FFilters   := TFilters.Create  ; SetupFilters;
    FProviders := TProviders.Create; SetupProviders;

    StopOnException := True;
    FormatSettings.DecimalSeparator := '.';
  end;

  destructor ATilesDownloader.Destroy;
  begin
    inherited Destroy;

    FreeAndNil(FFilters);
    FreeAndNil(FProviders);
  end;

  procedure ATilesDownloader.WriteHelp;
  begin
    WriteLn('tilesdownloader : Usage : ');
    WriteLn('    ./tilesdownloader [OPTION] [PARAMETER]...');
    WriteLn('');
    WriteLn('Donwload tiles from map providers.');
    WriteLn('');
    WriteLn('       Option               Value                       Description');
    WriteLn('    -provider              [String]          use prepared provider. prepared providers:');
    WriteLn('                                               - osm-standard - OpenStreetMap Standard');
    WriteLn('                                               - railway-standard - OpenRailwayMap Standard');
    WriteLn('                                               - railway-maxspeed - OpenRailwayMap Maxspeed');
    WriteLn('                                               - railway-electrification - OpenRailwayMap Electrification');
    WriteLn('    -providers             [String]          import providers from ini file.');
    WriteLn('    -layers                [String]          use layers ini file. the following options are');
    WriteLn('                                             not taken into account when using:');
    WriteLn('                                               - filter');
    WriteLn('                                               - provider');
    WriteLn('    -min-zoom         [Unsigned Integer]     lower zoom limit. the range is specific to each provider.');
    WriteLn('    -max-zoom         [Unsigned Integer]     highest zoom limit. the range is specific to each provider.');
    WriteLn('    -left                  [Double]          left border of the downloaded area (longitude).');
    WriteLn('    -top                   [Double]          top border of the downloaded area (latitude).');
    WriteLn('    -right                 [Double]          right border of the downloaded area (longitude).');
    WriteLn('    -bottom                [Double]          bottom border of the downloaded area (latitude).');
    WriteLn('    -out                   [String]          out path, default is current path in dir "tiles",');
    WriteLn('                                             support a pattern-generated path. keywords:');
    WriteLn('                                               - {p} - provider name');
    WriteLn('                                               - {x} - x number of tile');
    WriteLn('                                               - {y} - y number of tile');
    WriteLn('                                               - {z} - zoom number');
    WriteLn('    -filter                [String]          applying a filter. available filters:');
    WriteLn('                                               - grayscale');
    WriteLn('    -show-file-type          [x]             show file extension in filename.');
    WriteLn('    -tile-res         [Unsigned Integer]     resolution of the saved images.');
    WriteLn('');
    WriteLn('Examples:');
    WriteLn('    ./tilesdownloader -provider osm-standard -min-zoom 1 -max-zoom 7');
    WriteLn('    ./tilesdownloader -provider osm-standard -min-zoom 1 -max-zoom 7 -out tiles/{p}/{z}/{x}_{y}');
  end;

  function ATilesDownloader.ImportProviders(AFilePath: String): Boolean;
  const
    _ProviderSectionStr = 'Provider';
  var
    LIniFile: TMemIniFile;
    LSection: TStringList;
    LIdent, LName, LURL: String;
    LMemoryStream: TMemoryStream;
  begin
    Result := True;
    try
      LMemoryStream := TMemoryStream.Create;
      LMemoryStream.LoadFromFile(AFilePath);
      LIniFile := TMemIniFile.Create(LMemoryStream);
      LSection := TStringList.Create;

      LIniFile.ReadSection(_ProviderSectionStr, LSection);
      while LSection.Count > 0 do
      begin
        LIdent := LIniFile.ReadString(_ProviderSectionStr, 'ident', '');
        LName  := LIniFile.ReadString(_ProviderSectionStr, 'name', '');
        LURL   := LIniFile.ReadString(_ProviderSectionStr, 'url', '');
        Providers.Add(LIdent, LName, LURL, TEPSG3857.Create);

        LIniFile.EraseSection(_ProviderSectionStr);
        LIniFile.ReadSection(_ProviderSectionStr, LSection);
      end;

      LSection.Free;
      LIniFile.Free;
      LMemoryStream.Free;
    except
      on E: Exception do
      begin
        Result := False;
        if Assigned(LSection) then FreeAndNil(LSection);
        if Assigned(LIniFile) then FreeAndNil(LIniFile);
        if Assigned(LMemoryStream) then FreeAndNil(LMemoryStream);
        WriteLn(E.ClassName + ': ' + E.Message);
      end;
    end;
  end;

  function ATilesDownloader.ImportLayers(ATilesManipulator: TTilesManipulator; AFilePath: String): Boolean;
  const
    _LayerSectionStr = 'Layer';
  var
    LIniFile: TMemIniFile;
    LSection: TStringList;
    LCreatedLayerIndex: Integer;
    LByte: Byte;
    LString: String;
    LMemoryStream: TMemoryStream;
  begin
    Result := True;
    try
      LMemoryStream := TMemoryStream.Create;
      LMemoryStream.LoadFromFile(AFilePath);
      LIniFile := TMemIniFile.Create(LMemoryStream);
      LSection := TStringList.Create;

      LIniFile.ReadSection(_LayerSectionStr, LSection);
      while LSection.Count > 0 do
      begin
        LString := LIniFile.ReadString(_LayerSectionStr, 'Provider', '');
        LCreatedLayerIndex := ATilesManipulator.Layers.Add(Providers[LString]);
        LByte := LIniFile.ReadInteger(_LayerSectionStr, 'Opacity', 255);
        ATilesManipulator.Layers[LCreatedLayerIndex].Opacity := LByte;
        LString := LIniFile.ReadString(_LayerSectionStr, 'Filter', '');
        if not LString.IsEmpty then
          ATilesManipulator.Layers[LCreatedLayerIndex].Filter := Filters[LString];

        LIniFile.EraseSection(_LayerSectionStr);
        LIniFile.ReadSection(_LayerSectionStr, LSection);
      end;

      LSection.Free;
      LIniFile.Free;
      LMemoryStream.Free;
    except
      on E: Exception do
      begin
        Result := False;
        if Assigned(LSection) then FreeAndNil(LSection);
        if Assigned(LIniFile) then FreeAndNil(LIniFile);
        if Assigned(LMemoryStream) then FreeAndNil(LMemoryStream);
        WriteLn(E.ClassName + ': ' + E.Message);
      end;
    end;
  end;



var
  appTilesDownloader: ATilesDownloader;
begin
  appTilesDownloader := ATilesDownloader.Create(nil);
  appTilesDownloader.Title := 'Tiles downloader';
  appTilesDownloader.Run;
  appTilesDownloader.Free;
end.

