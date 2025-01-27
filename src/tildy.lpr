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

program tildy;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Interfaces, // For BGRABitmap
  {$ENDIF}
  SysUtils, Classes, CustApp, IniFiles, fileinfo,
  Tildy.Options,
  TilesManipulations.Base, TilesManipulations.Projections, TilesManipulations.Filters;

var
  OptionParameter: array[TOptionKind] of String;
  glOptions: TOptions;

type

  { ATildy }

  ATildy = class(TCustomApplication)
  strict private
    FProviders: TProviders;
    FFilters  : TFilters;
  strict private
    procedure SetupProviders;
    procedure SetupFilters;
    procedure ParseParameters;
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

  procedure ATildy.SetupProviders;
  begin
    Providers.Add('osm-standard', 'OpenStreetMap-Standard', 'http://a.tile.openstreetmap.org/{z}/{x}/{y}.png', TEPSG3857.Create);
    Providers.Add('railway-standard', 'OpenRailwayMap-Standard', 'http://b.tiles.openrailwaymap.org/standard/{z}/{x}/{y}.png', TEPSG3857.Create);
    Providers.Add('railway-maxspeed', 'OpenRailwayMap-Maxspeed', 'http://b.tiles.openrailwaymap.org/maxspeed/{z}/{x}/{y}.png', TEPSG3857.Create);
    Providers.Add('railway-electrification' , 'OpenRailwayMap-Electrification', 'http://b.tiles.openrailwaymap.org/electrification/{z}/{x}/{y}.png', TEPSG3857.Create);
  end;

  procedure ATildy.SetupFilters;
  begin
    Filters.Add('grayscale', TFilterGrayscale.Create);
  end;

  procedure ATildy.parseParameters;
  var
    OptionKind: TOptionKind;
  begin
    for OptionKind := Low(TOptionKind) to High(TOptionKind) do
    begin
      {$IFDEF DEBUG}
      write(getOptionName(OptionKind));
      {$ENDIF}
      if HasOption(getOptionName(OptionKind)) then
      begin
        Include(glOptions, OptionKind);
        {$IFDEF DEBUG}
        write(' finded, value = ');
        {$ENDIF}
        try
          OptionParameter[OptionKind] := getOptionValue(getOptionName(OptionKind));
        finally end;
        {$IFDEF DEBUG}
        writeLn(OptionParameter[OptionKind]);
        {$ENDIF}
        Continue;
      end
      else if HasOption(getOptionIdent(OptionKind)) then
      begin
        Include(glOptions, OptionKind);
        {$IFDEF DEBUG}
        write(' finded, value = ');
        {$ENDIF}
        try
          OptionParameter[OptionKind] := getOptionValue(getOptionIdent(OptionKind));
        finally end;
        {$IFDEF DEBUG}
        writeLn(OptionParameter[OptionKind]);
        {$ENDIF}
      end
      else
        {$IFDEF DEBUG}
        WriteLn;
        {$ENDIF}
    end;
  end;

  procedure ATildy.DoRun;
  var
    TilesManipulator: TTilesManipulator = nil;
    LMinZoom, LMaxZoom: Byte;
    LLeft, LTop, LRight, LBottom: Extended;
    LUseArea: Boolean = False;
    LProgramVersion: TProgramVersion;
  begin
    if hasOption(getOptionName(okHelp)) then
    begin
      writeHelp;
      Terminate;
      Exit;
    end;

    if hasOption(getOptionName(okVersion)) then
    begin
      GetProgramVersion(LProgramVersion);
      WriteLn(Format('tildy %d.%d', [LProgramVersion.Major, LProgramVersion.Minor]));
      Terminate;
      Exit;
    end;

    ParseParameters;
    TilesManipulator := TTilesManipulator.Create;

    try
      // -providers
      if okProviders in glOptions then
        if not ImportProviders(OptionParameter[okProviders]) then
          raise EOpProviders.Create('Error when processing providers.');

      // -layers
      if okLayers in glOptions then
      begin
        if not ImportLayers(TilesManipulator, OptionParameter[okLayers]) then
          raise EOpLayers.Create('Error when processing layers.');
      end
      else
      begin
        // -provider
        if okProvider in glOptions then
        begin
          if not Providers.Contains(OptionParameter[okProvider]) then
            raise EOpProvider.Create('The specified provider was not found.');
          TilesManipulator.Layers.Add(Providers[OptionParameter[okProvider]])
        end
        else
          raise EOpProvider.Create('The provider is not specified.');

        // -filter
        if okFilter in glOptions then
        begin
          if not Filters.Contains(OptionParameter[okFilter]) then
            raise EOpFilter.Create('The specified filter was not found.');
          TilesManipulator.Layers[0].Filter := Filters[OptionParameter[okFilter]];
        end;
      end;

      // -out
      if okOut in glOptions then
        TilesManipulator.Path := OptionParameter[okOut];

      // -min-zoom
      if okMinZoom in glOptions then
      begin
        try
          LMinZoom := OptionParameter[okMinZoom].ToInteger
        except
          on E: Exception do
            raise EOpMinZoom.Create(E.Message);
        end;
      end
      else
        raise EOpMinZoom.Create('The required "-min-zoom" option is missing.');

      // -max-zoom
      if okMaxZoom in glOptions then
      begin
        try
          LMaxZoom := OptionParameter[okMaxZoom].ToInteger;
        except
          on E: Exception do
            raise EOpMaxZoom.Create(E.Message);
        end;
      end
      else
        raise EOpMaxZoom.Create('The required "-max-zoom" option is missing.');

      // -left, -top, -right, -bottom
      LUseArea := (okLeft   in glOptions)
               or (okTop    in glOptions)
               or (okRight  in glOptions)
               or (okBottom in glOptions);

      // -left
      if okLeft in glOptions then
      begin
        try
          LLeft := OptionParameter[okLeft].ToExtended;
        except
          on E: Exception do
            raise EOpLeft.Create(E.Message);
        end;
      end
      else if LUseArea then
        raise EOpLeft.Create('The "-left" option is missing.');

      // -top
      if okTop in glOptions then
      begin
        try
          LTop := OptionParameter[okTop].ToExtended;
        except
          on E: Exception do
            raise EOpTop.Create(E.Message);
        end;
      end
      else if LUseArea then
        raise EOpTop.Create('The "-top" option is missing.');

      // -right
      if okRight in glOptions then
      begin
        try
          LRight := OptionParameter[okRight].ToExtended;
        except
          on E: Exception do
            raise EOpRight.Create(E.Message);
        end;
      end
      else if LUseArea then
        raise EOpRight.Create('The "-right" option is missing.');

      // -bottom
      if okBottom in glOptions then
      begin
        try
          LBottom := OptionParameter[okBottom].ToExtended;
        except
          on E: Exception do
            raise EOpBottom.Create(E.Message);
        end;
      end
      else if LUseArea then
        raise EOpBottom.Create('The "-bottom" option is missing.');

      // -show-file-type
      if okShowFileType in glOptions then
        TilesManipulator.ShowFileType := True;

      // skip-missing
      if okSkipMissing in glOptions then
        TilesManipulator.SkipMissing := True;

      // -tile-res
      if okTileRes in glOptions then
        try
          TilesManipulator.TileRes := OptionParameter[okTileRes].ToInteger;
        except
          on E: Exception do
            raise EOpTileRes.Create(E.Message);
        end;

      if LUseArea then
        TilesManipulator.Download(LMinZoom, LMaxZoom, LBottom, LTop, LLeft, LRight)
      else
        TilesManipulator.Download(LMinZoom, LMaxZoom);
    except
      on E: Exception do
        WriteLn(E.ClassName + ': ' + E.Message);
    end;

    TilesManipulator.Free;
    Terminate;
  end;

  constructor ATildy.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;

    FFilters   := TFilters.Create  ; SetupFilters;
    FProviders := TProviders.Create; SetupProviders;

    FormatSettings.DecimalSeparator := '.';
  end;

  destructor ATildy.Destroy;
  begin
    inherited Destroy;

    FreeAndNil(FFilters);
    FreeAndNil(FProviders);
  end;

  procedure ATildy.WriteHelp;
  begin
    WriteLn('tildy : Usage : ');
    WriteLn('    ./tildy [OPTION] [PARAMETER]...');
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
    WriteLn('    ./tildy -provider osm-standard -min-zoom 1 -max-zoom 7');
    WriteLn('    ./tildy -provider osm-standard -min-zoom 1 -max-zoom 7 -out tiles/{p}/{z}/{x}_{y}');
  end;

  function ATildy.ImportProviders(AFilePath: String): Boolean;
  const
    _ProviderSectionStr = 'Provider';
  var
    LIniFile: TMemIniFile = nil;
    LSection: TStringList = nil;
    LIdent, LName, LURL: String;
    LMemoryStream: TMemoryStream = nil;
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

  function ATildy.ImportLayers(ATilesManipulator: TTilesManipulator; AFilePath: String): Boolean;
  const
    _LayerSectionStr = 'Layer';
  var
    LIniFile: TMemIniFile = nil;
    LSection: TStringList = nil;
    LCreatedLayerIndex: Integer;
    LByte: Byte;
    LString: String;
    LMemoryStream: TMemoryStream = nil;
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
  apptildy: ATildy;

{$R *.res}

begin
  apptildy := ATildy.Create(nil);
  apptildy.Title := 'tildy';
  apptildy.Run;
  apptildy.Free;
end.

