program tiledownloader;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, fphttpclient, openssl, opensslsockets;

type

  RMapProvider = record
    Name: String;
    Link: String;
  end;

  { CTileDownloader }

  { CTilesDownloader }

  CTilesDownloader = class(TFPCustomHTTPClient)
  private
    FMapProvider:  RMapProvider;
    FDownloadDir:  String;
    FMinZoom:      Integer;
    FMaxZoom:      Integer;
    function  GetProviderLink: String;
    procedure SetProviderLink(AValue: String);
    function  GetProviderName: String;
    procedure SetProviderName(AValue: String);
  public
    property ProviderName: String read GetProviderName write SetProviderName;
    property ProviderLink: String read GetProviderLink write SetProviderLink;
    property DownloadDir:  String read FDownloadDir    write FDownloadDir;
    property MinZoom: Integer read FMinZoom write FMinZoom;
    property MaxZoom: Integer read FMaxZoom write FMaxZoom;
    procedure Download;
  end;

  { CTilesDownloader }

  function CTilesDownloader.GetProviderLink: String;
  begin
    Result := FMapProvider.Link;
  end;

  procedure CTilesDownloader.SetProviderLink(AValue: String);
  begin
    if FMapProvider.Link = AValue then Exit;
    FMapProvider.Link := AValue;
  end;

  function CTilesDownloader.GetProviderName: String;
  begin
    Result := FMapProvider.Name;
  end;

  procedure CTilesDownloader.SetProviderName(AValue: String);
  begin
    if FMapProvider.Name = AValue then Exit;
    FMapProvider.Name := AValue;
  end;

  procedure CTilesDownloader.Download;
  begin
    if not DirectoryExists(DownloadDir) then
    if not CreateDir(GetCurrentDir + PathDelim + DownloadDir) then
      Halt(1);
  end;

var
  TilesDownloader: CTilesDownloader;
begin
  TilesDownloader := CTilesDownloader.Create(nil);
  with TilesDownloader do
  begin
      ProviderName := 'OpenStreetMap';
      ProviderLink := 'http://a.tile.openstreetmap.org';
      MinZoom :=  6;
      MaxZoom := 13;
      DownloadDir := 'tiles';
  end;
  try
    TilesDownloader.Download;
  finally
    TilesDownloader.Free;
  end;
end.

