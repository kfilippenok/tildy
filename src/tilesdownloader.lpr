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
  SysUtils, Classes, CustApp,
  TilesDownload.Classes, TilesDownload.Types, TilesDownload.Exceptions, TilesDownload.Utilities;

var
  OptionParameter: array[TOptionKind] of String;
  glOptions: TOptions;

type

  { ATilesDownloader }

  ATilesDownloader = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    procedure parseParameters;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure writeHelp; virtual;
  end;

  procedure ATilesDownloader.DoRun;
  var
    objTilesDownloader: CTilesDownloader;
    Coordinate: RCoordinate;
  begin
    if hasOption(getOptionName(okHelp)) then
    begin
      writeHelp;
      Terminate;
      Exit;
    end;

    parseParameters;

    if (okMerge in glOptions) then
    begin
      try
        objTilesDownloader := GetMergedTDClassOnIdent(OptionParameter[okProvider])
                                .Create(Self, GetTDClassOnIdent(OptionParameter[okMerge]).Create(Self));
      except
        on E: Exception do
        begin
          WriteLn(E.Message);
          if Assigned(objTilesDownloader) then
            objTilesDownloader.Free;
          Halt(1);
        end;
      end;
    end
    else
    begin
      if (okProvider in glOptions) then
      try
        objTilesDownloader := GetTDClassOnIdent(OptionParameter[okProvider]).Create(Self);
      except
        on E: Exception do
        begin
          WriteLn('Error: ' + E.Message);
          if Assigned(objTilesDownloader) then
            objTilesDownloader.Free;
          Halt(1);
        end;
      end
    else
      objTilesDownloader := CTilesDownloader.Create(Self);

    end;

    with objTilesDownloader do
    begin
      if not OptionParameter[okMinZoom].IsEmpty then
        MinZoom := OptionParameter[okMinZoom].ToInteger;
      if not OptionParameter[okMaxZoom].IsEmpty then
        MaxZoom := OptionParameter[okMaxZoom].ToInteger;
      if not OptionParameter[okProviderName].IsEmpty then
        ProviderName := OptionParameter[okProviderName];
      if not OptionParameter[okOutput].IsEmpty then
        OutPath := OptionParameter[okProviderName];

      if not (okFullMap in glOptions) then
      if OptionParameter[okFirstCoordLat].IsEmpty
      or OptionParameter[okFirstCoordLon].IsEmpty
      or OptionParameter[okSecondCoordLat].IsEmpty
      or OptionParameter[okSecondCoordLon].IsEmpty then
      begin
        WriteLn('error: Not all coordinate values are specified');
        Halt(1);
      end
      else
      begin
        Coordinate.lat := OptionParameter[okFirstCoordLat].ToDouble;
        Coordinate.lon := OptionParameter[okFirstCoordLon].ToDouble;
        Coordinates[0] := Coordinate;
        Coordinate.lat := OptionParameter[okSecondCoordLat].ToDouble;
        Coordinate.lon := OptionParameter[okSecondCoordLon].ToDouble;
        Coordinates[1] := Coordinate;
      end;

      if not OptionParameter[okTileRes].IsEmpty then
        TileRes := OptionParameter[okTileRes].ToInteger;

      if not OptionParameter[okPattern].IsEmpty then
        Pattern := OptionParameter[okPattern];

       if not OptionParameter[okTileRes].IsEmpty then
        TileRes := OptionParameter[okTileRes].ToInteger;
    end;
    try
      if (okFullMap in glOptions) then
        objTilesDownloader.DownloadFullMap
      else
        objTilesDownloader.Download;
    finally
      objTilesDownloader.Free;
    end;

    // stop program loop
    Terminate;
  end;

  procedure ATilesDownloader.parseParameters;
  var
    OptionKind: TOptionKind;
    //ErrorMsg: String;
  begin
    //ErrorMsg:=CheckOptions('h', 'help');
    //if ErrorMsg<>'' then begin
    //  ShowException(Exception.Create(ErrorMsg));
    //  Terminate;
    //  Exit;
    //end;

    for OptionKind := Low(TOptionKind) to High(TOptionKind) do
    begin
      writeLn(getOptionName(OptionKind));
      if hasOption(getOptionName(OptionKind)) then
      begin
         Include(glOptions, OptionKind);
         {$IFDEF DEBUG}
         write(getOptionName(OptionKind) + ' finded, value = ');
         {$ENDIF}
         OptionParameter[OptionKind] := getOptionValue(getOptionName(OptionKind));
         {$IFDEF DEBUG}
         writeLn(OptionParameter[OptionKind]);
         {$ENDIF}
      end;
    end;
  end;

  constructor ATilesDownloader.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException:=True;
  end;

  destructor ATilesDownloader.Destroy;
  begin
    inherited Destroy;
  end;

  procedure ATilesDownloader.writeHelp;
  begin
    WriteLn('tilesdownloader : Usage : ');
    WriteLn('    ./tilesdownloader [OPTION] [PARAMETER]...');
    WriteLn('');
    WriteLn('Donwload tiles from map providers.');
    WriteLn('');
    WriteLn('    -provider            prepared provider.');
    WriteLn('    -provider-name       specify provider name.');
    WriteLn('    -provider-link       custom link to provider.');
    WriteLn('    -output              out path, default is current path in dir "tiles".');
    WriteLn('    -save-method         save by folders or by pattern in one dir.');
    WriteLn('    -divider             divider which using in pattern save method.');
    WriteLn('    -min-zoom            lower zoom limit, in range 0..19.');
    WriteLn('    -max-zoom            highest zoom limit, in range 0..19.');
    WriteLn('    -fсoord-lat          latitude of first coordinate.');
    WriteLn('    -fсoord-lon          longtitude of first coordinate.');
    WriteLn('    -sсoord-lat          latitude of second coordinate.');
    WriteLn('    -sсoord-lon          longtitude of second coordinate.');
    WriteLn('    -show-file-type      show file extension in filename.');
    WriteLn('    -full-map            download full map.');
    WriteLn('');
    WriteLn('Examples:');
    WriteLn('    ./tilesdownloader -provider osm -min-zoom 1 -max-zoom 7 -full-map');
    WriteLn('    ./tilesdownloader -provider osm -min-zoom 1 -max-zoom 7 -provider-name MyProviderName --fсoord-lat=56.674619 --fсoord-lon=60.287416 --sсoord-lat=57.029763 --sсoord-lat=60.921877');
  end;

var
  appTilesDownloader: ATilesDownloader;
begin
  appTilesDownloader:=ATilesDownloader.Create(nil);
  appTilesDownloader.Title:='Tiles downloader';
  appTilesDownloader.Run;
  appTilesDownloader.Free;
end.

