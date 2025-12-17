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

unit Tildy.Core.Engine;

{$CODEPAGE UTF-8}
{$mode ObjFPC}{$H+}{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  SysUtils, Classes, StrUtils, Math, FGL,
  fphttpclient, openssl, opensslsockets,
  BGRABitmap, BGRABitmapTypes;

const
  defOutPath = 'tiles';
  defMinZoom = 6;
  defMaxZoom = 7;
  defShowFileType = False;
  defTileRes = 256;
  defOtherTileRes = False;

type

  IFilter = interface
    ['{5DBCB3D5-A14F-48CD-9E72-1F38448C717E}']
    procedure Transform(var ABGRABitmap: TBGRABitmap);
  end;

  _TFilters = specialize TFPGMap<String, IFilter>;

  { TFilters }

  TFilters = class(_TFilters)
  public
    function Add(AKey: String; AFilter: IFilter): Integer; virtual; reintroduce;
    function Contains(AKey: String): Boolean; virtual;
  end;

  IProjection = interface
    ['{1F79F1B6-41F0-4B3F-83DD-0FA312C73BCB}']
    function MinLat: Extended;
    function MaxLat: Extended;
    function MinLon: Extended;
    function MaxLon: Extended;
    function CalcTileX(const AZoom: Byte; const ALongitude: Extended): QWord;
    function CalcTileY(const AZoom: Byte; const ALatitude: Extended): QWord;
  end;

  { TProviderClient }

  TProviderClient = class(TFPCustomHTTPClient)
  strict private
    FUserAgents: TStringList;
    FUserAgent: String;
    FUASelected: Boolean;
  strict private
    procedure SetupUserAgents; virtual;
    procedure AutoSelectUserAgent(const AURL: String); virtual; final;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    function ReceiveTile(const AURL: String): TBGRABitmap;
  end;

  { IProvider }

  IProvider = interface
    ['{9DF4214B-DE9F-4882-8E62-C31AB8F51A1C}']
    function GetCachePath: String;
    procedure SetCachePath(AValue: String);
    function GetUseCacheOnly: Boolean;
    procedure SetUseCacheOnly(AValue: Boolean);
    function GetName: String;
    procedure SetName(AValue: String);
    function GetProjection: IProjection;
    procedure SetProjection(AValue: IProjection);

    function GiveTile(AZoom: Integer; AX, AY: Integer): TBGRABitmap;

    property CachePath: String read GetCachePath write SetCachePath;
    property UseCacheOnly: Boolean read GetUseCacheOnly write SetUseCacheOnly;
    property Name: String read GetName write SetName;
    property Projection: IProjection read GetProjection write SetProjection;
  end;

  EProvider = class(Exception);

  { TProvider }

  TProvider = class(TInterfacedObject, IProvider)
  const
    defUseCache = False;
    defUseCacheOnly = False;
  strict private
    FCachePath: String;
    FUseCache: Boolean;
    FUseCacheOnly: Boolean;
    FClient: TProviderClient;
    FName: String;
    FProjection: IProjection;
    FURL: String;
  strict private
    function  GetCachePath: String;
    procedure SetCachePath(AValue: String);
    function GetUseCacheOnly: Boolean;
    procedure SetUseCacheOnly(AValue: Boolean);
    function  GetName: String;
    procedure SetName(AValue: String);
    function  GetProjection: IProjection;
    procedure SetProjection(AValue: IProjection);
  strict private
    function GetTileLink(AZoom: Integer; AX, AY: Integer): String;
    function GetTilePath(AZoom: Integer; AX, AY: Integer): String;
  public
    constructor Create(AName, AURL: String; AProjection: IProjection); virtual; reintroduce;
    destructor Destroy; override;
  public
    function GiveTile(AZoom: Integer; AX, AY: Integer): TBGRABitmap;
  public
    property CachePath: String read GetCachePath write SetCachePath;
    property UseCacheOnly: Boolean read GetUseCacheOnly write SetUseCacheOnly default defUseCacheOnly;
    property Name: String read GetName write SetName;
    property Projection: IProjection read GetProjection write SetProjection;
    property URL: String read FURL write FURL;
  end;

  _TProviders = specialize TFPGMap<String, IProvider>;

  { TProviders }

  TProviders = class(_TProviders)
  public
    function Add(AKey: String; AName, AURL: String; AProjection: IProjection): Integer; virtual; reintroduce;
    function Contains(AKey: String): Boolean; virtual;
  end;

  ELayer = class(Exception);

  { TLayer }

  TLayer = class
  const
    defOpacity = 255;
  strict private
    FBuffer: TBGRABitmap;
    FFilter: IFIlter;
    FProvider: IProvider;
    FOpacity: Byte;
  public
    constructor Create(AProvider: IProvider); virtual; reintroduce;
    destructor Destroy; override;
  public
    procedure Load(const AZoom, AX, AY: Integer);
    procedure ResampleAndPaintTo(var ABGRABitmap: TBGRABitmap);
  public
    property Buffer: TBGRABitmap read FBuffer write FBuffer;
    property Filter: IFilter read FFilter write FFilter;
    property Provider: IProvider read FProvider write FProvider;
    property Opacity: Byte read FOpacity write FOpacity default defOpacity;
  end;

  { TLayers }

  _TLayers = specialize TFPGObjectList<TLayer>;

  TLayers = class(_TLayers)
  public
    function Add(AProvider: IProvider): Integer; virtual; reintroduce;
    procedure Load(const AZoom: Integer; const AX, AY: Integer); virtual;
  end;

  { TMonochromes }

  TMonochromes = class
  private
    FItems: array of TBGRAPixel;
    function GetCount: Integer;
    function GetItem(Index: Integer): TBGRAPixel;
    procedure PutItem(Index: Integer; AValue: TBGRAPixel);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    function Add(BGRAPixel: TBGRAPixel): Integer; virtual; overload;
    {
      Supported variations:
        #FFFFFF
        rgb(255, 255, 255)
        rgba(255, 255, 255, 255)
    }
    function Add(AColorString: String): Integer; virtual; overload;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TBGRAPixel read GetItem write PutItem; default;
  end;

  ETildyEngine = class(Exception);
  ETESave = class(ETildyEngine);
  ETEDownload = class(ETildyEngine);
  ETEMonochrome = class(ETildyEngine);

  TTile = record
    X, Y: Integer;
    Zoom: Integer;
  end;

  TArea = record
    Left  : Extended;
    Top   : Extended;
    Right : Extended;
    Bottom: Extended;
  end;

  TAreaBounds = record
    Left  : QWord;
    Top   : QWord;
    Right : QWord;
    Bottom: QWord;
  end;

  TDownloadStatus = (
    dsOK,
    dsSkipExist,
    dsSkipMonochrome,
    dsNotLoaded,
    dsNotSaved
  );

  TDownloadInfo = record
    Status  : TDownloadStatus;
    ErrorMsg: String;
  end;

  TProgressEvent = procedure (const AZoom, X, Y: QWord; const ACurrentCount, ATotalCount, AZoomCurrentCount,
    AZoomTotalCount: QWord; const AMilliSeconds: Int64; const ADownloadInfo: TDownloadInfo) of object;

  { TTildyEngine }

  TTildyEngine = class
  const
    defPath = 'tiles/{p}/{z}/{x}/{y}';
    defSkipExisting = False;
    defSkipMissing = False;
    defSkipMonochrome = False;
    defShowFileType = False;
    defUseOtherTileRes = False;
  strict private
    FLayers: TLayers;
    FMonochromes: TMonochromes;
    FPath: String;
    FShowFileType: Boolean;
    FSkipExisting: Boolean;
    FSkipMissing: Boolean;
    FSkipMonochrome: Boolean;
    FTileRes: Word;
    FUseOtherTileRes: Boolean;
  strict private // Getters and Setters
    FOnProgress: TProgressEvent;
    procedure SetTileRes(AValue: Word);
  protected
    function ProcessPath(const AProviderName: String; const AZoom: Integer; const AX, AY: Integer): String;
    function IsMonochrome(var ATileImg: TBGRABitmap): Boolean;
    function InMonochromes(var ATileImg: TBGRABitmap): Boolean;
    procedure ResizeIfNeeded(var ATileImg: TBGRABitmap);
    procedure SaveTile(const ATileImg: TBGRABitmap; AFilePath: String);
  public // Calculations
    class function CalcRowTilesCount(const AMinX, AMaxX: QWord): QWord; static;
    class function CalcColumnTilesCount(const AMinY, AMaxY: QWord): QWord; static;
    class function CalcZoomTilesCount(AProjecion: IProjection; const AZoom: Byte; const AMinLatitude, AMaxLatitude, AMinLongitude, AMaxLongitude: Float): QWord; static;
    class function CalcTotalTilesCount(AProjecion: IProjection; const AMinZoom, AMaxZoom: Byte; const AMinLatitude, AMaxLatitude, AMinLongitude, AMaxLongitude: Float): QWord; static;
    function CalcAreaBounds(const AZoom: Integer; const AArea: TArea): TAreaBounds;
  public // Create and Destroy
    constructor Create; virtual;
    destructor Destroy; override;
  public
    function DownloadTile(ATile: TTile): TDownloadInfo;
    procedure Download(const AZoom: Integer); virtual; overload;
    procedure Download(const AMinZoom, AMaxZoom: Integer); virtual; overload;
    procedure Download(const AMinZoom, AMaxZoom: Integer; const AArea: TArea); virtual; overload;
    procedure Download(const AMinZoom, AMaxZoom: Integer; const AMinLatitude, AMaxLatitude, AMinLongitude, AMaxLongitude: Float); virtual; overload;
  public
    property Layers      : TLayers        read FLayers          write FLayers;
    property Monochromes : TMonochromes   read FMonochromes     write FMonochromes;
    property Path        : String         read FPath            write FPath;
    property ShowFileType: Boolean        read FShowFileType    write FShowFileType    default defShowFileType;
    property SkipExisting: Boolean        read FSkipExisting    write FSkipExisting    default defSkipExisting;
    property SkipMissing : Boolean        read FSkipMissing     write FSkipMissing     default defSkipMissing;
    property TileRes     : Word           read FTileRes         write SetTileRes;
    property OnProgress  : TProgressEvent read FOnProgress      write FOnProgress;
  end;

implementation

uses
  ssockets, Tildy.Utilities.Time;

{ TProviderClient }

procedure TProviderClient.SetupUserAgents;
begin
  if not Assigned(FUserAgents) then Exit;

  FUserAgents.Add('Mozilla/5.0 (compatible; fpweb)');
  FUserAgents.Add('Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 YaBrowser/24.6.0.0 Safari/537.36\');

  FUserAgent := FUserAgents[0];
end;

procedure TProviderClient.AutoSelectUserAgent(const AURL: String);
var
  i: Integer;
begin
  for i := 0 to FUserAgents.Count-1 do
    try
      Self.AddHeader('User-Agent', FUserAgents[i]);
      Self.Get(AURL);
      FUASelected := True;
      Break;
    except
      on E: Exception do
      begin
        if i = FUserAgents.Count-1 then
        begin
          raise;
        end
        else
          Continue;
      end;
    end;
end;

constructor TProviderClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  InitSSLInterface;
  Self.AllowRedirect := true;
  Self.ConnectTimeOut := 10000;
  FUserAgents := TStringList.Create;
  SetupUserAgents;
  FUASelected := False;
end;

destructor TProviderClient.Destroy;
begin
  FreeAndNil(FUserAgents);

  inherited Destroy;
end;

function TProviderClient.ReceiveTile(const AURL: String): TBGRABitmap;
var
  LMemoryStream: TMemoryStream;
begin
  Result := nil;

  if not FUASelected then
    try
      AutoSelectUserAgent(AURL);
    except
      on E: Exception do
        raise;
    end;

  try
    LMemoryStream := TMemoryStream.Create;
    while True do
      try
        Self.Get(AURL, LMemoryStream);
        break;
      except
        on E: ESocketError do
          continue;
      end;
    LMemoryStream.Position := 0;
    Result := TBGRABitmap.Create(LMemoryStream);
    LMemoryStream.Free;
  except
    on E: Exception do
    begin
      if Assigned(LMemoryStream) then FreeAndNil(LMemoryStream);
      if Assigned(Result) then FreeAndNil(Result);
      raise Exception.Create('Failed receive file.');
    end;
  end;
end;

function TProvider.GetName: String;
begin
  Result := FName;
end;

procedure TProvider.SetName(AValue: String);
begin
  if FName = AValue then Exit;
  FName := AValue;
end;

function TProvider.GetProjection: IProjection;
begin
  Result := FProjection;
end;

procedure TProvider.SetProjection(AValue: IProjection);
begin
  if FProjection = AValue then Exit;
  FProjection := AValue;
end;

function TProvider.GetTileLink(AZoom: Integer; AX, AY: Integer): String;
begin
  Result := URL;
  Result := StringReplace(Result, '{z}', AZoom.ToString, [rfReplaceAll]);
  Result := StringReplace(Result, '{x}', AX.ToString, [rfReplaceAll]);
  Result := StringReplace(Result, '{y}', AY.ToString, [rfReplaceAll]);
end;

function TProvider.GetTilePath(AZoom: Integer; AX, AY: Integer): String;
begin
  Result := CachePath;
  Result := StringReplace(Result, '{p}', Name, [rfReplaceAll]);
  Result := StringReplace(Result, '{z}', AZoom.ToString, [rfReplaceAll]);
  Result := StringReplace(Result, '{x}', AX.ToString, [rfReplaceAll]);
  Result := StringReplace(Result, '{y}', AY.ToString, [rfReplaceAll]);
end;

function TProvider.GetCachePath: String;
begin
  Result := FCachePath;
end;

procedure TProvider.SetCachePath(AValue: String);
begin
  if FCachePath = AValue then Exit;
  FUseCache := true;
  FCachePath := AValue;
end;

function TProvider.GetUseCacheOnly: Boolean;
begin
  Result := FUseCacheOnly;
end;

procedure TProvider.SetUseCacheOnly(AValue: Boolean);
begin
  if FUseCacheOnly = AValue then Exit;
  FUseCacheOnly := AValue;
end;

constructor TProvider.Create(AName, AURL: String; AProjection: IProjection);
begin
  inherited Create;

  FCachePath := '';
  FUseCache := defUseCache;
  FUseCacheOnly := defUseCacheOnly;
  FName := AName;
  FProjection := AProjection;
  FUrl := AURL;
  FClient := TProviderClient.Create(nil);
end;

destructor TProvider.Destroy;
begin
  FreeAndNil(FClient);

  inherited Destroy;
end;

function TProvider.GiveTile(AZoom: Integer; AX, AY: Integer): TBGRABitmap;
var
  LFilePath: String;
begin
  Result := nil;
  try
    LFilePath := GetTilePath(AZoom, AX, AY);

    if FUseCache and FileExists(LFilePath) then
      Result := TBGRABitmap.Create(LFilePath);

    if FUseCacheOnly then
      Exit;

    Result := FClient.ReceiveTile(GetTileLink(AZoom, AX, AY));
  except
    on E: Exception do
    begin
      if Assigned(Result) then FreeAndNil(Result);
      if FUseCacheOnly then
        raise EProvider.Create('The file corresponding to the tile number is missing from the disk.')
      else
        raise EProvider.Create('An error occurred while downloading');
    end;
  end;
end;

{ TFilters }

function TFilters.Add(AKey: String; AFilter: IFilter): Integer;
begin
  Result := inherited Add(AKey, AFilter);
end;

function TFilters.Contains(AKey: String): Boolean;
begin
  Result := IndexOf(AKey) <> -1;
end;

{ TProviders }

function TProviders.Add(AKey: String; AName, AURL: String; AProjection: IProjection): Integer;
begin
  Result := inherited Add(AKey, TProvider.Create(AName, AUrl, AProjection));
end;

function TProviders.Contains(AKey: String): Boolean;
begin
  Result := IndexOf(AKey) <> -1;
end;

{ TLayer }

constructor TLayer.Create(AProvider: IProvider);
begin
  inherited Create;

  FProvider := AProvider;
  FFilter := nil;
  FOpacity := defOpacity;
end;

destructor TLayer.Destroy;
begin
  inherited Destroy;

  if Assigned(FBuffer) then
    FreeAndNil(FBuffer);
end;

procedure TLayer.Load(const AZoom: Integer; const AX, AY: Integer);
begin
  if Assigned(FBuffer) then
    FreeAndNil(FBuffer);
  try
    FBuffer := Provider.GiveTile(AZoom, AX, AY);
    if not Assigned(FBuffer) then
      raise ELayer.Create('Layer of ' + Provider.Name + ' did not load.');
    if Assigned(Filter) then
      Filter.Transform(FBuffer);
  except
    on E: Exception do
      raise ELayer.Create(E.Message);
  end;
end;

procedure TLayer.ResampleAndPaintTo(var ABGRABitmap: TBGRABitmap);
var
  LResampledBuffer: TBGRABitmap;
begin
  if not Assigned(FBuffer) then
    Exit;

  LResampledBuffer := FBuffer.Resample(ABGRABitmap.Width, ABGRABitmap.Height);
  ABGRABitmap.PutImage(0, 0, LResampledBuffer, dmDrawWithTransparency, Opacity);
  LResampledBuffer.Free;
end;

{ TLayers }

function TLayers.Add(AProvider: IProvider): Integer;
begin
  Result := inherited Add(Tlayer.Create(AProvider));
end;

procedure TLayers.Load(const AZoom: Integer; const AX, AY: Integer);
var
  Layer: TLayer;
begin
  for Layer in Self do
    Layer.Load(AZoom, AX, AY);
end;

{ TMonochromes }

function TMonochromes.GetItem(Index: Integer): TBGRAPixel;
begin
  Result := FItems[Index];
end;

function TMonochromes.GetCount: Integer;
begin
  Result := Length(FItems);
end;

procedure TMonochromes.PutItem(Index: Integer; AValue: TBGRAPixel);
begin
  FItems[Index] := AValue;
end;

constructor TMonochromes.Create;
begin
  SetLength(FItems, 0);
end;

destructor TMonochromes.Destroy;
begin
  inherited Destroy;

  SetLength(FItems, 0);
end;

function TMonochromes.Add(BGRAPixel: TBGRAPixel): Integer;
begin
  SetLength(FItems, Succ(Count));
  FItems[Pred(Count)] := BGRAPixel;
end;

function TMonochromes.Add(AColorString: String): Integer;
var
  LBGRAPixel: TBGRAPixel;
begin
  LBGRAPixel.FromString(AColorString);

  if LBGRAPixel = BGRAPixelTransparent then
    raise Exception.Create('Incorrect color string');

  Result := Add(LBGRAPixel);
end;

{ TTildyEngine }

function TTildyEngine.ProcessPath(const AProviderName: String;
                                            const AZoom: Integer; const AX, AY: Integer): String;
begin
  Result := Path;
  Result := StringReplace(Result, '{p}', AProviderName, [rfReplaceAll]);
  Result := StringReplace(Result, '{z}', AZoom.ToString, [rfReplaceAll]);
  Result := StringReplace(Result, '{x}', AX.ToString, [rfReplaceAll]);
  Result := StringReplace(Result, '{y}', AY.ToString, [rfReplaceAll]);
  Result := Result + IfThen(ShowFileType, '.png');
end;

function TTildyEngine.IsMonochrome(var ATileImg: TBGRABitmap): Boolean;
var
  ix, iy: Integer;
  LPrevPixel, LCurrentPixel: TBGRAPixel;
begin
  Result := True;
  for ix := 0 to Pred(ATileImg.Width) do
  for iy := 0 to Pred(ATileImg.Height) do
  begin
    LCurrentPixel := ATileImg.GetPixel(ix, iy);;
    if (ix = 0) and (iy = 0) then
      LPrevPixel := LCurrentPixel;
    if LPrevPixel <> LCurrentPixel then
      Exit(False);
    LPrevPixel := LCurrentPixel;
  end;
end;

function TTildyEngine.InMonochromes(var ATileImg: TBGRABitmap): Boolean;
var
  i: Integer;
  LFirstTilePixel, LMonochromePixel: TBGRAPixel;
begin
  Result := False;

  if (ATileImg.Width = 0) or (ATileImg.Height = 0) then
    Exit;

  LFirstTilePixel := ATileImg.GetPixel(0, 0);
  for i := 0 to Pred(FMonochromes.Count) do
  begin
    LMonochromePixel := FMonochromes.Items[i];
    if LMonochromePixel = LFirstTilePixel then
      Exit(True);
  end;
end;

procedure TTildyEngine.ResizeIfNeeded(var ATileImg: TBGRABitmap);
var
  OldTileImg: TBGRABitmap;
begin
  if not FUseOtherTileRes then Exit;
  if not Assigned(ATileImg) then Exit;
  if (ATileImg.Width = TileRes) and (ATileImg.Height = TileRes) then Exit;

  OldTileImg := ATileImg;
  ATileImg := ATileImg.Resample(TileRes, TileRes);
  OldTileImg.Free;
end;

procedure TTildyEngine.SaveTile(const ATileImg: TBGRABitmap; AFilePath: String);
var
  LFileStream: TFileStream = nil;
begin
  if not ForceDirectories(ExtractFilePath(AFilePath)) then
    raise ETESave.Create('Failed create dirs.');
  try
    LFileStream := TFileStream.Create(AFilePath, fmCreate or fmOpenWrite);
    ATileImg.SaveToStreamAsPng(LFileStream);
    FreeAndNil(LFileStream);
  except
    on E: Exception do
    begin
      if Assigned(LFileStream) then FreeAndNil(LFileStream);
      raise ETESave.Create('Failed save file with error: ' + E.Message);
    end;
  end;
end;

procedure TTildyEngine.SetTileRes(AValue: Word);
begin
  FUseOtherTileRes := True;
  if FTileRes = AValue then Exit;
  FTileRes := AValue;
end;

class function TTildyEngine.CalcRowTilesCount(const AMinX, AMaxX: QWord): QWord;
begin
  Result := AMaxX - AMinX + 1;
end;

class function TTildyEngine.CalcColumnTilesCount(const AMinY, AMaxY: QWord): QWord;
begin
  Result := AMaxY - AMinY + 1;
end;

class function TTildyEngine.CalcZoomTilesCount(AProjecion: IProjection; const AZoom: Byte;
                                                    const AMinLatitude, AMaxLatitude, AMinLongitude, AMaxLongitude: Float): QWord;
var
  MinX, MaxX, MinY, MaxY: QWord;
begin
  MinX := AProjecion.CalcTileX(AZoom, AMinLongitude);
  MaxX := AProjecion.CalcTileX(AZoom, AMaxLongitude);
  MinY := AProjecion.CalcTileY(AZoom, AMaxLatitude);
  MaxY := AProjecion.CalcTileY(AZoom, AMinLatitude);
  Result := CalcRowTilesCount(MinX, MaxX) * CalcColumnTilesCount(MinY, MaxY);
end;

class function TTildyEngine.CalcTotalTilesCount(AProjecion: IProjection; const AMinZoom, AMaxZoom: Byte;
                                                     const AMinLatitude, AMaxLatitude, AMinLongitude, AMaxLongitude: Float): QWord;
var
  iz: Byte;
begin
  Result := 0;
  for iz := AMinZoom to AMaxZoom do
    Result := Result + CalcZoomTilesCount(AProjecion, iz, AMinLatitude, AMaxLatitude, AMinLongitude, AMaxLongitude);
end;

function TTildyEngine.CalcAreaBounds(const AZoom: Integer; const AArea: TArea): TAreaBounds;
begin
  Result := Default(TAreaBounds);

  if FLayers.Count = 0 then Exit;

  Result.Left   := FLayers[0].Provider.Projection.CalcTileY(AZoom, AArea.Left);
  Result.Top    := FLayers[0].Provider.Projection.CalcTileY(AZoom, AArea.Top);
  Result.Right  := FLayers[0].Provider.Projection.CalcTileY(AZoom, AArea.Right);
  Result.Bottom := FLayers[0].Provider.Projection.CalcTileY(AZoom, AArea.Bottom);
end;

constructor TTildyEngine.Create;
begin
  inherited Create;

  FLayers := TLayers.Create(True);
  FMonochromes := TMonochromes.Create;
  FUseOtherTileRes := defUseOtherTileRes;
  FPath := defPath;
  FShowFileType := defShowFileType;
  FSkipExisting := defSkipExisting;
  FSkipMissing := defSkipMissing;
  FSkipMonochrome := defSkipMonochrome;
end;

destructor TTildyEngine.Destroy;
begin
  inherited Destroy;

  FreeAndNil(FLayers);
  FreeAndNil(FMonochromes);
end;

function TTildyEngine.DownloadTile(ATile: TTile): TDownloadInfo;
var
  LBuffer  : TBGRABitmap = nil;
  il       : Integer;
  LSavePath: String;
begin
  Result.Status   := dsOK;
  Result.ErrorMsg := String.Empty;

  LSavePath := ProcessPath(Layers[0].Provider.Name, ATile.Zoom, ATile.X, ATile.Y);

  if (SkipExisting and FileExists(LSavePath)) then
  begin
    Result.Status := dsSkipExist;
    Exit;
  end;

  try
    try
      if Assigned(LBuffer) then FreeAndNil(LBuffer);
      for il := 0 to Layers.Count-1 do
      begin
        Layers[il].Load(ATile.Zoom, ATile.X, ATile.Y);
        if not Assigned(LBuffer) then
        begin
          LBuffer := Layers[il].Buffer.Duplicate(True);
          ResizeIfNeeded(LBuffer);
          Continue;
        end
        else
          Layers[il].ResampleAndPaintTo(LBuffer);
      end;

      if FSkipMonochrome and IsMonochrome(LBuffer) and InMonochromes(LBuffer) then
        raise ETEMonochrome.Create('');

      SaveTile(LBuffer, LSavePath);
      FreeAndNil(LBuffer);
    except
      on E: Exception do
        Result.ErrorMsg := E.Message;
      on E: ELayer do
        Result.Status := dsNotLoaded;
      on E: ETESave do
        Result.Status := dsNotSaved;
      on E: ETEMonochrome do
        Result.Status := dsSkipMonochrome;
    end;
  finally
    FreeAndNil(LBuffer);
  end;
end;

procedure TTildyEngine.Download(const AZoom: Integer);
begin
  if FLayers.Count < 1 then Exit;

  Download(AZoom, AZoom);
end;

procedure TTildyEngine.Download(const AMinZoom, AMaxZoom: Integer);
var
  LMainProjection: IProjection;
begin
  if FLayers.Count < 1 then Exit;

  LMainProjection := FLayers[0].Provider.Projection;
  Download(AMinZoom, AMaxZoom, LMainProjection.MinLat, LMainProjection.MaxLat, LMainProjection.MinLon, LMainProjection.MaxLon);
end;

procedure TTildyEngine.Download(const AMinZoom, AMaxZoom: Integer;
  const AArea: TArea);
begin
  Download(AMinZoom, AMaxZoom, AArea.Left, AArea.Right, AArea.Top, AArea.Bottom);
end;

procedure TTildyEngine.Download(const AMinZoom, AMaxZoom: Integer; const AMinLatitude, AMaxLatitude, AMinLongitude, AMaxLongitude: Float);
var
  MinX, MaxX, MinY, MaxY: QWord;
  iz: Byte;
  ix, iy: Longword;
  LZoomCurrentCount, LZoomTotalCount, LCurrentCount, LTotalCount: QWord;
  LMainProjection: IProjection;
  LBeginTime, LEndTime: Int64;
  LTile: TTile;
  LDownloadInfo: TDownloadInfo;
begin
  if FLayers.Count = 0 then Exit;

  LMainProjection := FLayers[0].Provider.Projection;
  LTotalCount := CalcTotalTilesCount(LMainProjection, AMinZoom, AMaxZoom, AMinLatitude, AMaxLatitude, AMinLongitude, AMaxLongitude);
  LCurrentCount := 0;

  FSkipMonochrome := Monochromes.Count > 0;

  for iz := AMinZoom to AMaxZoom do
  begin
    LZoomTotalCount := CalcZoomTilesCount(LMainProjection, iz, AMinLatitude, AMaxLatitude, AMinLongitude, AMaxLongitude);
    LZoomCurrentCount := 0;
    MinX := LMainProjection.CalcTileX(iz, AMinLongitude);
    MaxX := LMainProjection.CalcTileX(iz, AMaxLongitude);
    for ix := MinX to MaxX do
    begin
      MinY := LMainProjection.CalcTileY(iz, AMaxLatitude);
      MaxY := LMainProjection.CalcTileY(iz, AMinLatitude);
      for iy := MinY to MaxY do
      begin
        LTile.X    := ix;
        LTile.Y    := iy;
        LTile.Zoom := iz;

        LBeginTime := GetTickCountMS;
        LDownloadInfo := DownloadTile(LTile);
        LEndTime := GetTickCountMS;

        if (LDownloadInfo.Status in [dsOK, dsSkipExist]) then
        begin
          Inc(LCurrentCount);
          Inc(LZoomCurrentCount);
        end;

        if Assigned(FOnProgress) then
          FOnProgress(iz, ix, iy, LCurrentCount, LTotalCount, LZoomCurrentCount, LZoomTotalCount, LEndTime - LBeginTime, LDownloadInfo);

        case LDownloadInfo.Status of
          dsNotLoaded:
            if not SkipMissing then ETEDownload.Create('Error occured while downloading.');
          dsNotSaved: ETESave.Create('Error occured while downloading.');
        end;
      end;
    end;
  end;
end;

end.



