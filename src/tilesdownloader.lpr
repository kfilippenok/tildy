program tilesdownloader;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, CustApp,
  TilesDownload;

type

  ATilesDownloader = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  procedure ATilesDownloader.DoRun;
  var
    ErrorMsg: String;
    objTilesDownloader: CTilesDownloader;
    Coordinate: RCoordinate;
  begin
    // quick check parameters
    ErrorMsg:=CheckOptions('h', 'help');
    if ErrorMsg<>'' then begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    objTilesDownloader := CTilesDownloader.Create(nil);
    with objTilesDownloader do
    begin
        ProviderName := 'OpenStreetMap-Mapnik';
        ProviderLink := 'http://a.tile.openstreetmap.org';
        //ProviderLink := 'http://b.tiles.openrailwaymap.org/standard';
        MinZoom := 6;
        MaxZoom := 8;
        DownloadDir := 'tiles';
        Coordinate.lat := 42.7;
        Coordinate.lon := 120;
        Coordinates[0] := Coordinate;
        Coordinate.lat := 57.02137756;
        Coordinate.lon := 143.1;
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

  constructor ATilesDownloader.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException:=True;
  end;

  destructor ATilesDownloader.Destroy;
  begin
    inherited Destroy;
  end;

  procedure ATilesDownloader.WriteHelp;
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

