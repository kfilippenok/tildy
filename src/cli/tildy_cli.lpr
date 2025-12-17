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

program tildy_cli;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, CustApp, IniFiles, fileinfo, Math,
  // CLI
  Tildy.CLI.Options,
  // Core
  Tildy.Core.Engine, Tildy.Core.Projections, Tildy.Core.Filters;

var
  OptionParameter: array[TOptionKind] of String;
  glOptions: TOptions;

type

  { ATildy }

  ATildy = class(TCustomApplication)
  type
    TAreaArray = array of TArea;
  strict private
    FProviders: TProviders;
    FFilters  : TFilters;
  strict private
    procedure SetupProviders;
    procedure SetupFilters;
    procedure ParseParameters;
    procedure ParseBoundingBox(ABoundingBoxString: String; out ABottom, ATop, ALeft, ARight: Extended);
    procedure ProgressCLI(const AZoom, X, Y: QWord; const ACurrentCount, ATotalCount, AZoomCurrentCount,
    AZoomTotalCount: QWord; const AMilliSeconds: Int64; const ADownloadInfo: TDownloadInfo);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure WriteHelp;
    function ImportAreas(var AAreaArray: TAreaArray; AFilePath: String): Boolean;
    function ImportLayers(ATildyEngine: TTildyEngine; AFilePath: String): Boolean;
    function ImportMonochromes(ATildyEngine: TTildyEngine; AFilePath: String): Boolean;
    function ImportProviders(AFilePath: String): Boolean;
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

  procedure ATildy.ParseParameters;
  var
    Option: TOptionKind;
  begin
    for Option := Low(TOptionKind) to High(TOptionKind) do
    begin
      {$IFDEF DEBUG}
      write(Option.Name);
      {$ENDIF}
      if HasOption(Option.Name) then
      begin
        Include(glOptions, Option);
        {$IFDEF DEBUG}
        write(' finded, value = ');
        {$ENDIF}
        try
          OptionParameter[Option] := getOptionValue(Option.Name);
        finally end;
        {$IFDEF DEBUG}
        writeLn(OptionParameter[Option]);
        {$ENDIF}
        Continue;
      end
      else if HasOption(Option.Ident) then
      begin
        Include(glOptions, Option);
        {$IFDEF DEBUG}
        write(' finded, value = ');
        {$ENDIF}
        try
          OptionParameter[Option] := getOptionValue(Option.Ident);
        finally end;
        {$IFDEF DEBUG}
        writeLn(OptionParameter[Option]);
        {$ENDIF}
      end
      else
        {$IFDEF DEBUG}
        WriteLn;
        {$ENDIF}
    end;
  end;

  procedure ATildy.ParseBoundingBox(ABoundingBoxString: String; out ABottom, ATop, ALeft, ARight: Extended);
  const
    _Delim = ',';
  var
    StartPosition, EndPosition: Integer;
  begin
    // MinLon,MinLat,MaxLon,MaxLat -> ALeft,ABottom,ARight,ATop
    // ALeft
    StartPosition := 1;
    EndPosition := Pos(_Delim, ABoundingBoxString);
    ALeft := Copy(ABoundingBoxString, StartPosition, EndPosition - StartPosition).ToExtended;
    // ABottom
    StartPosition := EndPosition + 1;
    EndPosition := Pos(_Delim, ABoundingBoxString, StartPosition);
    ABottom := Copy(ABoundingBoxString, StartPosition, EndPosition - StartPosition).ToExtended;
    // ARight
    StartPosition := EndPosition + 1;
    EndPosition := Pos(_Delim, ABoundingBoxString, StartPosition);
    ARight := Copy(ABoundingBoxString, StartPosition, EndPosition - StartPosition).ToExtended;
    // ATop
    StartPosition := EndPosition + 1;
    EndPosition := Length(ABoundingBoxString) + 1;
    ATop := Copy(ABoundingBoxString, StartPosition, EndPosition - StartPosition).ToExtended;
  end;

  procedure ATildy.ProgressCLI(const AZoom, X, Y: QWord; const ACurrentCount, ATotalCount, AZoomCurrentCount,
    AZoomTotalCount: QWord; const AMilliSeconds: Int64; const ADownloadInfo: TDownloadInfo);
  const
    EmptyProgress: String = '░';
    FillProgress: String = '█';

    function getProgressString(AProcents: QWord): String;
    var
      LFillCount: Byte;
      i: Byte;
    begin
      Result := String.Empty;
      LFillCount := Floor(AProcents / 10);
      for i := 1 to LFillCount do
        Result := Result + FillProgress;
      for i := 1 to (10 - LFillCount) do
        Result := Result + EmptyProgress;
    end;


  var
    LTotalProcents, LZoomProcents: Byte;
  begin
    LTotalProcents := Floor((ACurrentCount / ATotalCount) * 100);
    LZoomProcents  := Floor((AZoomCurrentCount / AZoomTotalCount) * 100);
    WriteLn(
      Format(
        'Total: %s %d%% (%d/%d)',
        [getProgressString(LTotalProcents), LTotalProcents, ACurrentCount, ATotalCount]
      )
      + ' | ' +
      Format(
        'Zoom: %s %d%% (%d/%d)',
        [getProgressString(LZoomProcents), LZoomProcents, AZoomCurrentCount, AZoomTotalCount]

      )
      + ' | ' +
      Format(
        '%d ms',
        [AMilliSeconds]

      )
    );
  end;

  procedure ATildy.DoRun;
  var
    TildyEngine: TTildyEngine = nil;
    LMinZoom, LMaxZoom: Byte;
    LLeft, LTop, LRight, LBottom: Extended;
    LUseArea: Boolean = False;
    LUseRects: Boolean = False;
    LUseMonochromes: Boolean = False;
    LAreaArray: TAreaArray;
    ia, LAreasCount: Integer;
    LProgramVersion: TProgramVersion;
  begin
    if hasOption(OptionName[okHelp]) or hasOption(OptionIdent[okHelp]) then
    begin
      writeHelp;
      Terminate;
      Exit;
    end;

    if hasOption(OptionName[okVersion]) or hasOption(OptionIdent[okVersion]) then
    begin
      GetProgramVersion(LProgramVersion);
      WriteLn(Format('tildy %d.%d', [LProgramVersion.Major, LProgramVersion.Minor]));
      Terminate;
      Exit;
    end;

    ParseParameters;
    TildyEngine := TTildyEngine.Create;

    try
      // -providers
      if okProviders in glOptions then
        if not ImportProviders(OptionParameter[okProviders]) then
          raise EOpProviders.Create('Error when processing providers.');

      // -layers
      if okLayers in glOptions then
      begin
        if not ImportLayers(TildyEngine, OptionParameter[okLayers]) then
          raise EOpLayers.Create('Error when processing layers.');
      end
      else
      begin
        // -provider
        if okProvider in glOptions then
        begin
          if not Providers.Contains(OptionParameter[okProvider]) then
            raise EOpProvider.Create('The specified provider was not found.');
          TildyEngine.Layers.Add(Providers[OptionParameter[okProvider]])
        end
        else
          raise EOpProvider.Create('The provider is not specified.');

        // -filter
        if okFilter in glOptions then
        begin
          if not Filters.Contains(OptionParameter[okFilter]) then
            raise EOpFilter.Create('The specified filter was not found.');
          TildyEngine.Layers[0].Filter := Filters[OptionParameter[okFilter]];
        end;

        // -cache
        if okCache in glOptions then
        begin
          if OptionParameter[okCache].IsEmpty then
            raise EOpCache.Create('Cache path is empty.');
          TildyEngine.Layers[0].Provider.CachePath := OptionParameter[okCache];
        end;

        // -use-cache-only
        if okUseCacheOnly in glOptions then
          TildyEngine.Layers[0].Provider.UseCacheOnly := True;
      end;

      // -out
      if okOut in glOptions then
        TildyEngine.Path := OptionParameter[okOut];

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

      // -areas
      if okAreas in glOptions then
      begin
        if LUseArea then
          raise EOpAreas.Create('Conflicting options are used to define the area.');
        { #todo : Rename LUseRects to LUseAreas }
        LUseRects := True;
        SetLength(LAreaArray, 0);
        if not ImportAreas(LAreaArray, OptionParameter[okAreas]) then
          raise EOpAreas.Create('Error when processing rects.');
      end;

      // -bbox
      if okBoundingBox in glOptions then
      begin
        if LUseArea or LUseRects then
          raise EOpBoundingBox.Create('Conflicting options are used to define the area.');
        try
          ParseBoundingBox(OptionParameter[okBoundingBox], LBottom, LTop, LLeft, LRight);
        except
          on E: Exception do
            raise EOpBoundingBox.Create(E.Message);
        end;
      end;

      // -show-file-type
      if okShowFileType in glOptions then
        TildyEngine.ShowFileType := True;

      // skip-missing
      if okSkipMissing in glOptions then
        TildyEngine.SkipMissing := True;

      // skip-existing
      if okSkipExisting in glOptions then
        TildyEngine.SkipExisting := True;

      // -tile-res
      if okTileRes in glOptions then
        try
          TildyEngine.TileRes := OptionParameter[okTileRes].ToInteger;
        except
          on E: Exception do
            raise EOpTileRes.Create(E.Message);
        end;

      // -monochromes
      if okMonochromes in glOptions then
      begin
        LUseMonochromes := True;
        if not ImportMonochromes(TildyEngine, OptionParameter[okMonochromes]) then
          raise EOpMonochromes.Create('Error when processing monochromes.');
      end;

      // -monochrome
      if okMonochrome in glOptions then
      begin
        if LUseMonochromes then
          raise EOpMonochrome.Create('Conflicting options (-monochromes) are used to define the area.');
        try
          TildyEngine.Monochromes.Add(OptionParameter[okMonochrome]);
        except
          on E: Exception do
            raise EOpMonochrome.Create(E.Message);
        end;
      end;

      TildyEngine.OnProgress := @ProgressCLI;
      if LUseArea then
        TildyEngine.Download(LMinZoom, LMaxZoom, LBottom, LTop, LLeft, LRight)
      else if LUseRects then
      begin
        LAreasCount := Length(LAreaArray);
        for ia := 0 to LAreasCount - 1 do
        begin
          WriteLn('[Area ' + (ia + 1).ToString + '/' + LAreasCount.ToString + ']');
          TildyEngine.Download(LMinZoom, LMaxZoom, LAreaArray[ia].Bottom, LAreaArray[ia].Top, LAreaArray[ia].Left, LAreaArray[ia].Right);
          WriteLn();
        end
      end
      else
        TildyEngine.Download(LMinZoom, LMaxZoom);
    except
      on E: Exception do
        WriteLn(E.ClassName + ': ' + E.Message);
    end;

    TildyEngine.Free;
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
    WriteLn('       Option                   Value                       Description');
    WriteLn('    -provider, -p              [String]            use prepared provider. prepared providers:');
    WriteLn('                                                     - osm-standard - OpenStreetMap Standard');
    WriteLn('                                                     - railway-standard - OpenRailwayMap Standard');
    WriteLn('                                                     - railway-maxspeed - OpenRailwayMap Maxspeed');
    WriteLn('                                                     - railway-electrification - OpenRailwayMap Electrification');
    WriteLn('    -providers,-ps             [String]            import providers from ini file. read usage instructions.');
    WriteLn('    -layers, -ls               [String]            use layers ini file. read usage instructions.');
    WriteLn('    -areas, -as                [String]            use areas ini file. read usage instructions.');
    WriteLn('    -monochrome, -m            [String]            color of monochrome tiles that should not be saved.');
    WriteLn('    -monochromes, -ms          [String]            use monochromes ini file. read usage instructions.');
    WriteLn('    -out, -o                   [String]            out path, default is current path in dir "tiles",');
    WriteLn('                                                   support a pattern-generated path. keywords:');
    WriteLn('                                                     - {p} - provider name');
    WriteLn('                                                     - {x} - x number of tile');
    WriteLn('                                                     - {y} - y number of tile');
    WriteLn('                                                     - {z} - zoom number');
    WriteLn('    -cache, -c                 [String]            path to the folder with the provider''s tiles that have already been downloaded. keywords:');
    WriteLn('                                                     - {p} - provider name');
    WriteLn('                                                     - {x} - x number of tile');
    WriteLn('                                                     - {y} - y number of tile');
    WriteLn('                                                     - {z} - zoom number');
    WriteLn('    -use-cache-only, -uco        [x]               utility will only work with cached tiles.');
    WriteLn('    -min-zoom, -z         [Unsigned Integer]       lower zoom limit. the range is specific to each provider.');
    WriteLn('    -max-zoom, -Z         [Unsigned Integer]       highest zoom limit. the range is specific to each provider.');
    WriteLn('    -left, -l                  [Double]            left border of the downloaded area (longitude).');
    WriteLn('    -top, -t                   [Double]            top border of the downloaded area (latitude).');
    WriteLn('    -right, -r                 [Double]            right border of the downloaded area (longitude).');
    WriteLn('    -bottom, -b                [Double]            bottom border of the downloaded area (latitude).');
    WriteLn('    -bbox, -b       [Double,Double,Double,Double]  MinLon,MinLat,MaxLon,MaxLat.');
    WriteLn('    -show-file-type, -sft        [x]               show file extension in filename.');
    WriteLn('    -skip-existing, -ske         [x]               skipping tiles that already exist on the disk when received from the server');
    WriteLn('    -skip-missing, -skm          [x]               skim missing tiles from provider.');
    WriteLn('    -filter, -f                [String]            applying a filter. available filters:');
    WriteLn('                                                     - grayscale');
    WriteLn('    -tile-res, -res       [Unsigned Integer]       resolution of the saved images.');
    WriteLn('    -version, -v                 [x]               show the version.');
    WriteLn('    -help, -h                    [x]               show current help.');
    WriteLn('');
    WriteLn('Examples:');
    WriteLn('    ./tildy -p osm-standard -z 1 -Z 7');
    WriteLn('    ./tildy -p osm-standard -z 1 -Z 7 -o tiles/{p}/{z}/{x}_{y}');
  end;

  function ATildy.ImportProviders(AFilePath: String): Boolean;
  const
    _ProviderSectionStr = 'Provider';
  var
    LIniFile: TMemIniFile = nil;
    LSection: TStringList = nil;
    LIdent, LName, LURL, LUseCacheOnly: String;
    LMemoryStream: TMemoryStream = nil;
    LLastProvider: Integer;
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
        LLastProvider := Providers.Add(LIdent, LName, LURL, TEPSG3857.Create);
        if LIniFile.ValueExists(_ProviderSectionStr, 'cache') then
          Providers.Data[LLastProvider].CachePath := LIniFile.ReadString(_ProviderSectionStr, 'cache', '');
        if LIniFile.ValueExists(_ProviderSectionStr, 'use-cache-only') then
        begin
          LUseCacheOnly := LIniFile.ReadString(_ProviderSectionStr, 'use-cache-only', 'yes');
          if LUseCacheOnly = 'yes' then
            Providers.Data[LLastProvider].UseCacheOnly := True
        end;

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

  function ATildy.ImportLayers(ATildyEngine: TTildyEngine; AFilePath: String): Boolean;
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
        LCreatedLayerIndex := ATildyEngine.Layers.Add(Providers[LString]);
        LByte := LIniFile.ReadInteger(_LayerSectionStr, 'Opacity', 255);
        ATildyEngine.Layers[LCreatedLayerIndex].Opacity := LByte;
        LString := LIniFile.ReadString(_LayerSectionStr, 'Filter', '');
        if not LString.IsEmpty then
          ATildyEngine.Layers[LCreatedLayerIndex].Filter := Filters[LString];

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

  function ATildy.ImportMonochromes(ATildyEngine: TTildyEngine;
    AFilePath: String): Boolean;
  const
    _MonochromeSectionStr = 'Monochrome';
  var
    LIniFile: TMemIniFile = nil;
    LSection: TStringList = nil;
    LString: String;
    LMemoryStream: TMemoryStream = nil;
  begin
    Result := True;
    try
      LMemoryStream := TMemoryStream.Create;
      LMemoryStream.LoadFromFile(AFilePath);
      LIniFile := TMemIniFile.Create(LMemoryStream);
      LSection := TStringList.Create;

      LIniFile.ReadSection(_MonochromeSectionStr, LSection);
      while LSection.Count > 0 do
      begin
        LString := LIniFile.ReadString(_MonochromeSectionStr, 'Color', '');
        ATildyEngine.Monochromes.Add(LString);

        LIniFile.EraseSection(_MonochromeSectionStr);
        LIniFile.ReadSection(_MonochromeSectionStr, LSection);
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

  function ATildy.ImportAreas(var AAreaArray: TAreaArray; AFilePath: String): Boolean;
  const
    _SectionStr = 'Area';
  var
    LIniFile: TMemIniFile = nil;
    LSection: TStringList = nil;
    LMemoryStream: TMemoryStream = nil;
    LCount: Integer = 0;
    LAllValuesExists: Boolean = False;
  begin
    Result := True;
    try
      LMemoryStream := TMemoryStream.Create;
      LMemoryStream.LoadFromFile(AFilePath);
      LIniFile := TMemIniFile.Create(LMemoryStream);
      LSection := TStringList.Create;

      LIniFile.ReadSection(_SectionStr, LSection);
      while LSection.Count > 0 do
      begin
        Inc(LCount);
        SetLength(AAreaArray, LCount);
        LAllValuesExists := (LIniFile.ValueExists(_SectionStr, 'left')
                         and LIniFile.ValueExists(_SectionStr, 'top')
                         and LIniFile.ValueExists(_SectionStr, 'right')
                         and LIniFile.ValueExists(_SectionStr, 'bottom'));
        if not LAllValuesExists then
          raise Exception.Create(_SectionStr + ' number ' + LCount.ToString + ' has not all values');

        AAreaArray[LCount-1].Left := LIniFile.ReadFloat(_SectionStr, 'left', 0.0);
        AAreaArray[LCount-1].Top := LIniFile.ReadFloat(_SectionStr, 'top', 0.0);
        AAreaArray[LCount-1].Right := LIniFile.ReadFloat(_SectionStr, 'right', 0.0);
        AAreaArray[LCount-1].Bottom := LIniFile.ReadFloat(_SectionStr, 'bottom', 0.0);

        LIniFile.EraseSection(_SectionStr);
        LIniFile.ReadSection(_SectionStr, LSection);
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

