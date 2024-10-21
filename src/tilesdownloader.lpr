program tilesdownloader;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, CustApp,
  TilesDownload;

type
  TOptionKind = (okHelp,
                 okProvider,
                 okOutput,
                 okSaveMethod,
                 okProviderName,
                 okProviderLink,
                 okMinZoom,
                 okMaxZoom,
                 okFirstCoordLat,
                 okFirstCoordLon,
                 okSecondCoordLat,
                 okSecondCoordLon);
var
  OptionName : array[TOptionKind] of String = ('help',
                                              'provider',
                                              'output',
                                              'save-method',
                                              'provider-name',
                                              'provider-link',
                                              'min-zoom',
                                              'max-zoom',
                                              'fсoord-lat',
                                              'fсoord-lon',
                                              'scoord-lat',
                                              'scoord-lon');
  OptionParameter: array[TOptionKind] of String;
  Options: Set of TOptionKind;

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
    ConcreteCTilesDownloader: class of CTilesDownloader;
    Coordinate: RCoordinate;
  begin
    parseParameters;

    case OptionParameter[okProvider] of
      'osm': ConcreteCTilesDownloader := CTDOpenStreetMap;
      'otm': ConcreteCTilesDownloader := CTDOpenTopotMap;
      'cycle-osm': ConcreteCTilesDownloader := CTDCycleOSM;
      'railway': ConcreteCTilesDownloader := CTDOpenRailwayMap;
    else
      ConcreteCTilesDownloader := CTilesDownloader;
    end;

    objTilesDownloader := ConcreteCTilesDownloader.Create(nil);
    with objTilesDownloader do
    begin
        MinZoom := OptionParameter[okMinZoom].ToInteger;
        MaxZoom := OptionParameter[okMaxZoom].ToInteger;
        Coordinate.lat := OptionParameter[okFirstCoordLat].ToDouble; // 42.7
        Coordinate.lon := OptionParameter[okFirstCoordLon].ToDouble; // 120
        Coordinates[0] := Coordinate;
        Coordinate.lat := OptionParameter[okSecondCoordLat].ToDouble; // 57.02137756
        Coordinate.lon := OptionParameter[okSecondCoordLon].ToDouble; // 143.1
        Coordinates[1] := Coordinate;
    end;
    try
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
      writeLn(OptionName[OptionKind]);
      if hasOption(OptionName[OptionKind]) then
      begin
         Include(Options, OptionKind);
         {$IFDEF DEBUG}
         write(OptionName[OptionKind] + ' finded, value = ');
         {$ENDIF}
         OptionParameter[OptionKind] := getOptionValue(OptionName[OptionKind]);
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
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
  end;

var
  appTilesDownloader: ATilesDownloader;
begin
  appTilesDownloader:=ATilesDownloader.Create(nil);
  appTilesDownloader.Title:='Tiles downloader';
  appTilesDownloader.Run;
  appTilesDownloader.Free;
end.

