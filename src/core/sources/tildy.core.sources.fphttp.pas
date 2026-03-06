unit Tildy.Core.Sources.FpHTTP;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fphttpclient,
  BGRABitmap,
  // Contracts
  Tildy.Core.Contracts.Coordinates,
  Tildy.Core.Contracts.Source;

type

  { TFpHTTPSource }

  TFpHTTPSource = class(TInterfacedObject, ISource)
  strict private
    FName: string;
    FURLTemplate: string;  // Example: 'https://tile.osm.org/{z}/{x}/{y}.png'
    FUserAgent: string;
    FTimeout: Integer;
  strict protected
    function GetName: string;
    function BuildURL(const ACoord: TTileCoord): string;
    function DownloadTile(const AURL: string): TBGRABitmap;
  public
    constructor Create(const AName, AURLTemplate: string;
                       const AUserAgent: string = String.Empty;
                       const ATimeoutMS: Integer = 10000); virtual; reintroduce;
  public
    function GetTile(const ACoord: TTileCoord): TBGRABitmap;
    function IsTileAvailable(const ACoord: TTileCoord): Boolean;
    property Name: string read GetName;
  end;

implementation

uses
  StrUtils;

{ TFpHTTPSource }

function TFpHTTPSource.BuildURL(const ACoord: TTileCoord): string;
begin
  Result := FURLTemplate;
  Result := StringReplace(Result, '{z}', ACoord.Zoom.ToString, [rfReplaceAll]);
  Result := StringReplace(Result, '{x}', ACoord.X.ToString, [rfReplaceAll]);
  Result := StringReplace(Result, '{y}', ACoord.Y.ToString, [rfReplaceAll]);
end;

function TFpHTTPSource.DownloadTile(const AURL: string): TBGRABitmap;
var
  Client: TFPHTTPClient = nil;
  Stream: TMemoryStream = nil;
begin
  Result := nil;
  try
    try
      Client := TFPHTTPClient.Create(nil);
      Stream := TMemoryStream.Create;
      Client.AddHeader('User-Agent', FUserAgent);
      Client.ConnectTimeout := FTimeout;
      Client.AllowRedirect := True;

      Client.Get(AURL, Stream);
      Stream.Position := 0;

      if Stream.Size > 0 then
        Result := TBGRABitmap.Create(Stream);
    finally
      Client.Free;
      Stream.Free;
    end;
  except
    on E: Exception do
    begin
      if Assigned(Result) then
        FreeAndNil(Result);
      raise ESourceError.CreateFmt(
        'Failed to download tile from %s: %s', [AURL, E.Message]);
    end;
  end;
end;

function TFpHTTPSource.GetName: string;
begin
  Result := FName;
end;

constructor TFpHTTPSource.Create(const AName, AURLTemplate: string;
  const AUserAgent: string; const ATimeoutMS: Integer = 10000);
begin
  inherited Create;

  FName := AName;
  FURLTemplate := AURLTemplate;
  FUserAgent := IfThen(AUserAgent <> '', AUserAgent,
                       'Mozilla/5.0 (compatible; Tildy)');
  FTimeout := ATimeoutMS;
end;

function TFpHTTPSource.GetTile(const ACoord: TTileCoord): TBGRABitmap;
var
  URL: String;
begin
  URL := BuildURL(ACoord);
  Result := DownloadTile(URL);
end;

function TFpHTTPSource.IsTileAvailable(const ACoord: TTileCoord): Boolean;
var
  Client: TFPHTTPClient = nil;
  URL: string;
begin
  Result := False;
  URL := BuildURL(ACoord);
  try
    try
      Client := TFPHTTPClient.Create(nil);
      Client.AddHeader('User-Agent', FUserAgent);
      Client.ConnectTimeout := FTimeout;
      Client.HTTPMethod('HEAD', URL, nil, []);  // HEAD запрос
      Result := Client.ResponseStatusCode = 200;
    finally
      Client.Free;
    end;
  except
    Result := False;
  end;
end;

end.

