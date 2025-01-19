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

unit TilesManipulations.Base;

{$mode ObjFPC}{$H+}{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  SysUtils, Classes, StrUtils, Math, FGL,
  fphttpclient, openssl, opensslsockets, BGRABitmap,
  TilesDownload.Exceptions;

type

  RMapProvider = record
    Name: String;
    Link: String;
  end;

  RCoordinate = record
    Lat: Float;
    Lon: Float;
  end;

  RTile = record
    X: QWord;
    Y: QWord;
  end;

  { HTileHelper }

  HTileHelper = record helper for RTile
    procedure SetValues(x, y: Integer);
  end;

  TSaveMethod = (smFolders, smPattern);
  TPatternItem = (piProviderName, piZoom, piX, piY);

var
  PatternItemsStr: array[TPatternItem] of String = ('%provider-name%',
                                                    '%z%',
                                                    '%x%',
                                                    '%y%');

const
  defUserAgent = 'Mozilla/5.0 (compatible; fpweb)';
  defProvider = 'osm';
  defSaveMethod = smFolders;
  defProviderName = 'OpenStreetMap';
  defProviderLink = 'http://a.tile.openstreetmap.org';
  defOutPath = 'tiles';
  defMinZoom = 6;
  defMaxZoom = 7;
  defShowFileType = False;
  defSkipMissing = False;
  defTileRes = 256;
  defOtherTileRes = False;

type

  TGetFileName = function (const AZoom, AX, AY: Integer): String of object;
  TGetOutPath  = function : String of object;
  TFilterTile  = procedure (var ATileImg: TBGRABitmap) of object;

  GPatternItems = specialize TFPGMap<TPatternItem, integer>;
  HPatternItemsHelper = class helper for GPatternItems
    procedure SortOnData;
  end;

  IFilter = interface
    ['{5DBCB3D5-A14F-48CD-9E72-1F38448C717E}']
    procedure Transform(var ABGRABitmap: TBGRABitmap);
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
    function GiveTile(AZoom: Integer; AX, AY: Integer): TBGRABitmap;
    function GetProjection: IProjection;
    procedure SetProjection(AValue: IProjection);
    property Projection: IProjection read GetProjection write SetProjection;
    function GetName: String;
    procedure SetName(AValue: String);
    property Name: String read GetName write SetName;
  end;

  EProvider = class(Exception);

  { TProvider }

  TProvider = class(TInterfacedObject, IProvider)
  strict private
    FClient: TProviderClient;
    FName: String;
    FProjection: IProjection;
    FURL: String;
  strict private
    function  GetName: String;
    procedure SetName(AValue: String);
    function  GetProjection: IProjection;
    procedure SetProjection(AValue: IProjection);
  strict private
    function GetTileLink(AZoom: Integer; AX, AY: Integer): String;
  public
    constructor Create(AName, AURL: String; AProjection: IProjection); virtual; reintroduce;
    destructor Destroy; override;
  public
    function GiveTile(AZoom: Integer; AX, AY: Integer): TBGRABitmap;
  public
    property Name: String read GetName write SetName;
    property Projection: IProjection read GetProjection write SetProjection;
    property URL: String read FURL write FURL;
  end;

  _TProviders = specialize TFPGMap<String, IProvider>;

  { TProviders }

  TProviders = class(_TProviders)
  public
    function Add(AKey: String; AName, AURL: String; AProjection: IProjection): Integer; virtual; reintroduce;
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

  ETilesManipulator = class(Exception);
  ETMSave = class(ETilesManipulator);

  { TTilesManipulator }

  TTilesManipulator = class
  strict private
    FLayers: TLayers;
    FPath: String;
  strict private
    function GetProcessedPath(const AProviderName: String; const AZoom: Integer; const AX, AY: Integer): String;
    procedure SaveTile(const ATileImg: TBGRABitmap; AFilePath: String);
  public // Calculations
    class function CalcRowTilesCount(const AMinX, AMaxX: QWord): QWord; overload; static;
    class function CalcColumnTilesCount(const AMinY, AMaxY: QWord): QWord; static;
    class function CalcZoomTilesCount(AProjecion: IProjection; const AZoom: Byte; const AMinLatitude, AMaxLatitude, AMinLongitude, AMaxLongitude: Float): QWord; static;
    class function CalcTotalTilesCount(AProjecion: IProjection; const AMinZoom, AMaxZoom: Byte; const AMinLatitude, AMaxLatitude, AMinLongitude, AMaxLongitude: Float): QWord; static;
  public // Create and Destroy
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Download(const AZoom: Integer); virtual;
    procedure Download(const AMinZoom, AMaxZoom: Integer); virtual;
    procedure Download(const AMinZoom, AMaxZoom: Integer; const AMinLatitude, AMaxLatitude, AMinLongitude, AMaxLongitude: Float); virtual; overload;
  public
    property Layers: TLayers read FLayers write FLayers;
    property Path  : String  read FPath write FPath;
  end;

  { TTilesDownloader }

  TTilesDownloader = class(TFPCustomHTTPClient)
  protected
    FUserAgent: String;
    FMapProvider: RMapProvider;
    FOutPath: String;
    FTileRes: Integer;
    FPattern, FInsertPattern: String;
    FGetFileName: TGetFileName;
    FGetOutPath : TGetOutPath;
    FFilterTile : TFilterTile;
    FPatternItems: GPatternItems;
    FOtherTileRes: Boolean;
    FSaveMethod: TSaveMethod;
    FMinZoom: Integer;
    FMaxZoom: Integer;
    FCoordinates: array[0..1] of RCoordinate;
    FShowFileType: Boolean;
    FSkipMissing: Boolean;
  strict private
    function GetOutPathAuto: String;
    function GetOutPathCustom: String;
    function GetOutPath: String;
    procedure SetOutPath(AOutPath: String);
    function GetProviderLink: String;
    procedure SetProviderLink(AValue: String);
    function GetProviderName: String;
    procedure SetProviderName(AValue: String);
    procedure SetPattern(APattern: String);
    function GetCoordinate(Index: Integer): RCoordinate;
    procedure SetCoordinate(Index: Integer; AValue: RCoordinate);
    procedure SetTileRes(AValue: Integer);
    function GetTotalTilesCount: Longword;
    function GetTotalTilesCountOnCoordinates: Longword;
  public
    procedure Init;
    procedure CalcTileNumber(const ACoordinate: RCoordinate; const AZoom: Integer; out Tile: RTile);
    function CalcRowTilesCount(AZoom: Byte): Longword; overload;
    function CalcRowTilesCount(ATile1, ATile2: RTile): Longword; overload;
    function CalcColumnTilesCount(ATile1, ATile2: RTile): Longword;
    function CalcZoomTilesCount(AZoom: Byte): Longword; overload;
    function CalcZoomTilesCount(ATile1, ATile2: RTile): Longword; overload;
    function GetFileNameDir(const AZoom, AX, AY: Integer): String;
    function GetFileNamePattern(const AZoom, AX, AY: Integer): String;
    function GetFileName(const AZoom, AX, AY: Integer): String;
    procedure ReceiveTile(var ATileImg: TBGRABitmap; const AProviderLink: String; const AZoom: Integer; const ATile: RTile);
    procedure ResampleTile(var ATileImg: TBGRABitmap; const ATileRes: Integer);
    procedure GrayscaleTile(var ATileImg: TBGRABitmap);
    procedure FilterTile(var ATileImg: TBGRABitmap);
    procedure SaveTile(const ATileImg: TBGRABitmap; AFilePath: String);
    procedure DownloadTile(const AZoom: Integer; const ATile: RTile); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ProviderName   : String      read GetProviderName write SetProviderName;
    property ProviderLink   : String      read GetProviderLink write SetProviderLink;
    property OutPath        : String      read GetOutPath      write SetOutPath;
    property TileRes        : Integer     read FTileRes        write SetTileRes;
    property Pattern        : String      read FPattern        write SetPattern;
    property SaveMethod     : TSaveMethod read FSaveMethod     write FSaveMethod   default defSaveMethod;
    property MinZoom        : Integer     read FMinZoom        write FMinZoom      default defMinZoom;
    property MaxZoom        : Integer     read FMaxZoom        write FMaxZoom      default defMaxZoom;
    property ShowFileType   : Boolean     read FShowFileType   write FShowFileType default defShowFileType;
    property SkipMissing    : Boolean     read FSkipMissing    write FSkipMissing  default defSkipMissing;
    property Coordinates[Index: Integer] : RCoordinate read GetCoordinate write SetCoordinate;
    property TotalTilesCount             : Longword    read GetTotalTilesCount;
    property TotalTilesCountOnCoordinates: Longword    read GetTotalTilesCountOnCoordinates;
    procedure Download; virtual;
    procedure DownloadFullMap; virtual;
  end;

  { CMergedTD }

  RefCMergedTD = class of CMergedTD;

  CMergedTD = class(TTilesDownloader)
  strict private
    FMergedDownloader: TTilesDownloader;
  public
    constructor Create(AOwner: TComponent; AMergedTD: TTilesDownloader); virtual; overload;
    procedure DownloadTile(const AZoom: Integer; const ATile: RTile); override;
    property MergedDownloader: TTilesDownloader read FMergedDownloader write FMergedDownloader;
    procedure Download; override;
    procedure DownloadFullMap; override;
  end;

  { CMrgTDOpenStreetMap }

  CMrgTDOpenStreetMap = class(CMergedTD)
  public
    Constructor Create(AOwner: TComponent; AMergedTD: TTilesDownloader); override;
  end;

  { CMrgTDOpenTopotMap }

  CMrgTDOpenTopotMap = class(CMergedTD)
  public
    Constructor Create(AOwner: TComponent; AMergedTD: TTilesDownloader); override;
  end;

  { CMrgTDCycleOSM }

  CMrgTDCycleOSM = class(CMergedTD)
  public
    Constructor Create(AOwner: TComponent; AMergedTD: TTilesDownloader); override;
  end;

  { CMrgTDOpenRailwayMap }

  CMrgTDOpenRailwayMap = class(CMergedTD)
  public
    Constructor Create(AOwner: TComponent; AMergedTD: TTilesDownloader); override;
  end;

implementation

uses ssockets, BGRABitmapTypes;

{ HPatternItemsHelper }

procedure HPatternItemsHelper.SortOnData;
var
  AllSorted: Boolean;
  ipatit: Integer;
begin
  while True do
  begin
    AllSorted := True;
    for ipatit := 0 to Count-2 do
    begin
      if Data[ipatit] > Data[ipatit+1] then
      begin
        Move(ipatit, ipatit+1);
        AllSorted := False;
      end;
    end;

    if AllSorted then Exit;
  end;
end;

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
  Complete: Boolean;
  LException: Exception;
begin
  Write('Setting up connection... ');

  Complete := False;
  for i := 0 to FUserAgents.Count-1 do
    try
      Self.AddHeader('User-Agent', FUserAgents[i]);
      Self.Get(AURL);
      Complete := True;
      WriteLn('Ok');
      FUASelected := True;
      Break;
    except
      on E: Exception do
      begin
        LException := E;
        Continue;
      end;
    end;

  {$IFDEF DEBUG}
  WriteLn(FUserAgent);
  {$ENDIF}

  if not Complete then
  begin
    WriteLn('Fail');
    raise LException;
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

  WriteLn(Format('TileLink: %s', [AURL]));
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
      WriteLn(E.ClassName + ' ' + E.Message);
      if Assigned(LMemoryStream) then FreeAndNil(LMemoryStream);
      if Assigned(Result) then FreeAndNil(Result);
      raise ETDReceive.Create('Failed receive file.');
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

constructor TProvider.Create(AName, AURL: String; AProjection: IProjection);
begin
  inherited Create;

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
begin
  Write(Name + ': ');
  Result := nil;
  try
    Result := FClient.ReceiveTile(GetTileLink(AZoom, AX, AY));
  except
    on E: Exception do
      raise EProvider.Create('An error occurred while downloading');
  end;
end;

{ TProviders }

function TProviders.Add(AKey: String; AName, AURL: String; AProjection: IProjection): Integer;
begin
  Result := inherited Add(AKey, TProvider.Create(AName, AUrl, AProjection));
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
    FBuffer.Free;
  try
    FBuffer := Provider.GiveTile(AZoom, AX, AY);
    if Assigned(Filter) then
      Filter.Transform(FBuffer);
  except
    on E: Exception do
      raise ELayer.Create(E.ClassName + ': ' + E.Message);
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

{ TTilesManipulator }

function TTilesManipulator.GetProcessedPath(const AProviderName: String;
                                            const AZoom: Integer; const AX, AY: Integer): String;
begin
  Result := Path;
  Result := StringReplace(Result, '{p}', AProviderName, [rfReplaceAll]);
  Result := StringReplace(Result, '{z}', AZoom.ToString, [rfReplaceAll]);
  Result := StringReplace(Result, '{x}', AX.ToString, [rfReplaceAll]);
  Result := StringReplace(Result, '{y}', AY.ToString, [rfReplaceAll]);
end;

procedure TTilesManipulator.SaveTile(const ATileImg: TBGRABitmap;
  AFilePath: String);
var
  LFileStream: TFileStream;
begin
  WriteLn(Format('FilePath: %s', [AFilePath]));
  ForceDirectories(ExtractFilePath(AFilePath));
  try
    LFileStream := TFileStream.Create(AFilePath, fmCreate or fmOpenWrite);
    ATileImg.SaveToStreamAsPng(LFileStream);
    LFileStream.Free;
  except
    on E: Exception do
    begin
      WriteLn(E.Message);
      LFileStream.Free;
      raise ETMSave.Create('Failed save file.');
    end;
  end;
end;

class function TTilesManipulator.CalcRowTilesCount(const AMinX, AMaxX: QWord): QWord;
begin
  Result := AMaxX - AMinX + 1;
end;

class function TTilesManipulator.CalcColumnTilesCount(const AMinY, AMaxY: QWord): QWord;
begin
  Result := AMaxY - AMinY + 1;
end;

class function TTilesManipulator.CalcZoomTilesCount(AProjecion: IProjection; const AZoom: Byte;
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

class function TTilesManipulator.CalcTotalTilesCount(AProjecion: IProjection; const AMinZoom, AMaxZoom: Byte;
                                                     const AMinLatitude, AMaxLatitude, AMinLongitude, AMaxLongitude: Float): QWord;
var
  iz: Byte;
begin
  Result := 0;
  for iz := AMinZoom to AMaxZoom do
    Result := Result + CalcZoomTilesCount(AProjecion, iz, AMinLatitude, AMaxLatitude, AMinLongitude, AMaxLongitude);
end;

constructor TTilesManipulator.Create;
begin
  inherited Create;

  FLayers := TLayers.Create(True);
end;

destructor TTilesManipulator.Destroy;
begin
  inherited Destroy;

  FreeAndNil(FLayers);
end;

procedure TTilesManipulator.Download(const AZoom: Integer);
begin
  if FLayers.Count < 1 then Exit;

  Download(AZoom, AZoom);
end;

procedure TTilesManipulator.Download(const AMinZoom, AMaxZoom: Integer);
var
  LMainProjection: IProjection;
begin
  if FLayers.Count < 1 then Exit;

  LMainProjection := FLayers[0].Provider.Projection;
  Download(AMinZoom, AMaxZoom, LMainProjection.MinLat, LMainProjection.MaxLat, LMainProjection.MinLon, LMainProjection.MaxLon);
end;

procedure TTilesManipulator.Download(const AMinZoom, AMaxZoom: Integer; const AMinLatitude, AMaxLatitude, AMinLongitude, AMaxLongitude: Float);
var
  MinX, MaxX, MinY, MaxY: QWord;
  iz: Byte;
  ix, iy, il: Longword;
  LZoomCurrentCount, LZoomTotalCount, LCurrentCount, LTotalCount: QWord;
  LMainProjection: IProjection;
  LBuffer: TBGRABitmap;
begin
  if FLayers.Count < 1 then Exit;

  LMainProjection := FLayers[0].Provider.Projection;
  LTotalCount := CalcTotalTilesCount(LMainProjection, AMinZoom, AMaxZoom, AMinLatitude, AMaxLatitude, AMinLongitude, AMaxLongitude);
  LCurrentCount := 0;

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
        try
          for il := 0 to Layers.Count-1 do
          begin
            Layers[il].Load(iz, ix, iy);
            if il = 0 then
            begin
              LBuffer := Layers[il].Buffer.Duplicate(True);
              Continue;
            end;
            Layers[il].ResampleAndPaintTo(LBuffer);
          end;
          SaveTile(LBuffer, GetProcessedPath(Layers[0].Provider.Name, iz, ix, iy));
          FreeAndNil(LBuffer);
        except
          on E: Exception do
          begin
            WriteLn(E.ClassName + ': ' + E.Message);
            if Assigned(LBuffer) then FreeAndNil(LBuffer);
          end;
        end;
        Inc(LCurrentCount);
        Inc(LZoomCurrentCount);
        WriteLn(Format('Total: %d/%d <- (Zoom %d: %d/%d)', [LCurrentCount, LTotalCount, iz, LZoomCurrentCount, LZoomTotalCount]));
      end;
    end;
  end;

  //for iz := MinZoom to MaxZoom do
  //begin
  //  if SaveMethod = smFolders then
  //  if not DirectoryExists(Format('%s/%d', [OutPath, iz])) then
  //  if not ForceDirectories(Format('%s/%d', [OutPath, iz])) then
  //    raise Exception.Create('The necessary paths could not be created. Check the specified path');
  //
  //  CalcTileNumber(Coordinates[0], iz, LTile1);
  //  CalcTileNumber(Coordinates[1], iz, LTile2);
  //  {$IFDEF DEBUG}
  //  WriteLn(Format('Coordinates[0]: %f, %f, Zoom: %d -> Tile: %d, %d', [Coordinates[0].lat, Coordinates[0].lon, iz,  LTile1.x, LTile1.y]));
  //  WriteLn(Format('Coordinates[1]: %f, %f, Zoom: %d -> Tile: %d, %d', [Coordinates[1].lat, Coordinates[1].lon, iz,  LTile2.x, LTile2.y]));
  //  {$ENDIF}
  //
  //  LZoomCurrentCount := 0;
  //  LZoomTotalCount := CalcZoomTilesCount(LTile1, Ltile2);
  //
  //  for ix := LTile1.X to LTile2.x do
  //  begin
  //    if SaveMethod = smFolders then
  //    if not DirectoryExists(Format('%s/%d/%d', [OutPath, iz, ix])) then
  //      ForceDirectories(Format('%s/%d/%d', [OutPath, iz, ix]));
  //
  //    for iy := LTile1.y to LTile2.y do
  //    begin
  //      LTileTmp.x := ix;
  //      LTileTmp.y := iy;
  //      try
  //        DownloadTile(iz, LTileTmp);
  //        Inc(LZoomCurrentCount);
  //        Inc(LCurrentCount);
  //        WriteLn(Format('Total: %d/%d <- (Zoom %d: %d/%d)', [LCurrentCount, LTotalCount, iz, LZoomCurrentCount, LZoomTotalCount]));
  //      except
  //        on E: ETileDownload do
  //        begin
  //          if SkipMissing and (E is ETDReceive) then
  //          begin
  //            WriteLn('! Skip missing tile');
  //            Continue;
  //          end;
  //
  //          WriteLn;
  //          WriteLn('Error: ', E.Message);
  //          Exit;
  //        end;
  //      end;
  //    end;
  //  end;
  //
  //end;
end;

operator = (const First, Second: RCoordinate) R : boolean;
begin
  R := SameValue(First.lat, Second.lat) and SameValue(First.lon, Second.lon);
end;

{ HTileHelper }

procedure HTileHelper.SetValues(x, y: Integer);
begin
  Self.x := x; Self.y := y;
end;

function TTilesDownloader.GetProviderLink: String;
begin
  Result := FMapProvider.link;
end;

function TTilesDownloader.GetTotalTilesCount: Longword;
var iz: Byte;
begin
  Result := 0;
  for iz := MinZoom to MaxZoom do
    Result := Result + CalcZoomTilesCount(iz);
end;

function TTilesDownloader.GetTotalTilesCountOnCoordinates: Longword;
var
  LTile1, LTile2: RTile;
  iz: Integer;
begin
  Result := 0;
  for iz := MinZoom to MaxZoom do
  begin
    calcTileNumber(Coordinates[0], iz, LTile1);
    calcTileNumber(Coordinates[1], iz, LTile2);
    Result := Result + (CalcRowTilesCount(LTile1, LTile2) * CalcColumnTilesCount(LTile1, LTile2));
  end;
end;

procedure TTilesDownloader.SetOutPath(AOutPath: String);
begin
  if FOutPath = AOutPath then Exit;

  FOutPath := AOutPath;
  FGetOutPath := @GetOutPathCustom;
end;

function TTilesDownloader.GetOutPathAuto: String;
begin
  Result := 'tiles' + PathDelim + ProviderName;
end;

function TTilesDownloader.GetOutPathCustom: String;
begin
  Result := FOutPath;
end;

function TTilesDownloader.GetOutPath: String;
begin
  Result := FGetOutPath();
end;

procedure TTilesDownloader.SetProviderLink(AValue: String);
begin
  if FMapProvider.Link = AValue then Exit;
  FMapProvider.Link := AValue;
end;

function TTilesDownloader.GetProviderName: String;
begin
  Result := FMapProvider.Name;
end;

procedure TTilesDownloader.SetProviderName(AValue: String);
begin
  if FMapProvider.name = AValue then Exit;
  FMapProvider.name := AValue;
end;

procedure TTilesDownloader.SetPattern(APattern: String);
var
  LInsertPattern, LSearchSource: String;
  tpos, ipatit: Integer;
  LPatternItem: TPatternItem;
begin
  if FPattern = APattern then Exit;

  FPattern := APattern;
  FGetFileName := @GetFileNamePattern;
  SaveMethod := smPattern;

  LInsertPattern := APattern;
  LSearchSource := LowerCase(APattern);

  // Filling in for further sorting
  for LPatternItem := Low(TPatternItem) to High(TPatternItem) do
  begin
    tpos := Pos(PatternItemsStr[LPatternItem], LSearchSource);
    if tpos > 0 then
    begin
      FPatternItems.Add(LPatternItem, tpos);
    end;
  end;

  // Sort on data
  FPatternItems.SortOnData;

  for ipatit := 0 to FPatternItems.Count-1 do
  begin
    tpos := Pos(PatternItemsStr[FPatternItems.Keys[ipatit]], LSearchSource);
    System.Delete(LInsertPattern, tpos, Length(PatternItemsStr[FPatternItems.Keys[ipatit]]));
    System.Delete(LSearchSource, tpos, Length(PatternItemsStr[FPatternItems.Keys[ipatit]]));
    FPatternItems.Data[ipatit] := tpos;
  end;

  FInsertPattern := LInsertPattern;
end;

function TTilesDownloader.GetCoordinate(Index: Integer): RCoordinate;
begin
  Result := FCoordinates[Index];
end;

procedure TTilesDownloader.SetCoordinate(Index: Integer; AValue: RCoordinate);
begin
  if FCoordinates[Index] = AValue then Exit;
  FCoordinates[Index] := AValue;
end;

constructor TTilesDownloader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOtherTileRes := defOtherTileRes;
  FTileRes := defTileRes;
  FGetFileName := @GetFileNameDir;
  FGetOutPath := @GetOutPathAuto;
  FPatternItems := GPatternItems.Create;
  FOutPath := defOutPath;
  FSaveMethod := smFolders;
  FMinZoom := defMinZoom;
  FMaxZoom := defMinZoom;
  FShowFileType := defShowFileType;
  FSkipMissing := defSkipMissing;
end;

destructor TTilesDownloader.Destroy;
begin
  inherited Destroy;

  FreeAndNil(FPatternItems);
end;

procedure TTilesDownloader.CalcTileNumber(const ACoordinate: RCoordinate;
  const AZoom: Integer; out Tile: RTile);
var
  lat_rad, n: Float;
begin
  lat_rad := DegToRad(ACoordinate.lat);
  n := Power(2, AZoom);
  Tile.x := Trunc(((ACoordinate.lon + 180) / 360) * n);
  Tile.y := Trunc((1 - ArcSinH(Tan(lat_rad)) / Pi) / 2.0 * n);
end;

function TTilesDownloader.CalcRowTilesCount(AZoom: Byte): Longword;
begin
  Result := Trunc(Power(2, AZoom));
end;

function TTilesDownloader.CalcRowTilesCount(ATile1, ATile2: RTile): Longword;
begin
  Result := Max(ATile1.X, ATile2.X) - Min(ATile1.X, ATile2.X) + 1;
end;

function TTilesDownloader.CalcColumnTilesCount(ATile1, ATile2: RTile): Longword;
begin
  Result := Max(ATile1.Y, ATile2.Y) - Min(ATile1.Y, ATile2.Y) + 1;
end;

function TTilesDownloader.CalcZoomTilesCount(AZoom: Byte): Longword;
var
  LRowTilesCount: Longword;
begin
  LRowTilesCount := CalcRowTilesCount(AZoom);
  Result := LRowTilesCount * LRowTilesCount;
end;

function TTilesDownloader.CalcZoomTilesCount(ATile1, ATile2: RTile): Longword;
begin
  Result := CalcRowTilesCount(ATile1, ATile2) * CalcColumnTilesCount(ATile1, ATile2)
end;

function TTilesDownloader.GetFileNameDir(const AZoom, AX, AY: Integer): String;
begin
  Result := Format('%d%s%d%s%d%s', [AZoom, PathDelim, AX, PathDelim, AY, IfThen(ShowFileType, '.png')]);
end;

function TTilesDownloader.GetFileNamePattern(const AZoom, AX, AY: Integer
  ): String;
var
  ipi: Integer;
begin
  Result := FInsertPattern;
  for ipi := FPatternItems.Count-1 downto 0 do
  begin
    case FPatternItems.Keys[ipi] of
      piProviderName: Insert(ProviderName,   Result, FPatternItems.Data[ipi]);
      piZoom        : Insert(AZoom.ToString, Result, FPatternItems.Data[ipi]);
      piX           : Insert(AX.ToString,    Result, FPatternItems.Data[ipi]);
      piY           : Insert(AY.ToString,    Result, FPatternItems.Data[ipi]);
    end;
  end;
  Result := Result + IfThen(ShowFileType, '.png');
end;

function TTilesDownloader.GetFileName(const AZoom, AX, AY: Integer): String;
begin
  Result := FGetFileName(AZoom, AX, AY);
end;

procedure TTilesDownloader.ReceiveTile(var ATileImg: TBGRABitmap; const AProviderLink: String; const AZoom: Integer; const ATile: RTile);
var
  LMemoryStream: TMemoryStream;
begin
  WriteLn(Format('TileLink: %s/%d/%d/%d.png', [AProviderLink, AZoom,  ATile.x, ATile.y]));
  try
    LMemoryStream := TMemoryStream.Create;
    while True do
      try
        Self.Get(Format('%s/%d/%d/%d.png', [AProviderLink, AZoom,  ATile.x, ATile.y]), LMemoryStream);
        break;
      except
        on E: ESocketError do
          continue;
      end;
    LMemoryStream.Position := 0;
    ATileImg.LoadFromStream(LMemoryStream);
    LMemoryStream.Free;
  except
    on E: Exception do
    begin
      WriteLn(E.Message);
      LMemoryStream.Free;
      raise ETDReceive.Create('Failed receive file.');
    end;
  end;
end;

procedure TTilesDownloader.ResampleTile(var ATileImg: TBGRABitmap; const ATileRes: Integer);
var
  LOldTile: TBGRABitmap;
begin
  try
    LOldTile := ATileImg;
    ATileImg := ATileImg.Resample(ATileRes, ATileRes);
    LOldTile.Free;
  except
    on E: Exception do
    begin
      WriteLn(E.Message);
      raise ETDResample.Create('Failed resample file.');
    end;
  end;
end;

procedure TTilesDownloader.GrayscaleTile(var ATileImg: TBGRABitmap);
var
  LOldTile: TBGRABitmap;
begin
  try
    LOldTile := ATileImg;
    ATileImg := ATileImg.FilterGrayscale(true);
    LOldTile.Free;
  except
    on E: Exception do
    begin
      WriteLn(E.Message);
      raise ETDFilter.Create('Failed filter file.');
    end;
  end;
end;

procedure TTilesDownloader.FilterTile(var ATileImg: TBGRABitmap);
begin
  if Assigned(FFilterTile) then
    FFilterTile(ATileImg);
end;

procedure TTilesDownloader.SaveTile(const ATileImg: TBGRABitmap; AFilePath: String);
var
  LFileStream: TFileStream;
begin
  WriteLn(Format('FilePath: %s', [AFilePath]));
  try
    LFileStream := TFileStream.Create(AFilePath, fmCreate or fmOpenWrite);
    ATileImg.SaveToStreamAsPng(LFileStream);
    LFileStream.Free;
  except
    on E: Exception do
    begin
      WriteLn(E.Message);
      LFileStream.Free;
      raise ETDSave.Create('Failed save file.');
    end;
  end;
end;

procedure TTilesDownloader.DownloadTile(const AZoom: Integer; const ATile: RTile);
var
  LFileName, LFilePath: String;
  LTileImg: TBGRABitmap;
begin
  try
    LTileImg := TBGRABitmap.Create;
    ReceiveTile(LTileImg, ProviderLink, AZoom, ATile);
    if FOtherTileRes then
      ResampleTile(LTileImg, TileRes);
    FilterTile(LTileImg);
    LFileName := GetFileName(AZoom, ATile.X, ATile.Y);
    LFilePath := Format('%s%s%s', [OutPath, PathDelim, LFileName]);
    SaveTile(LTileImg, LFilePath);
    LTileImg.Free;
  except
    on ETileDownload do
    begin
      if Assigned(LTileImg) then
        LTileImg.Free;
      raise;
    end;
  end;
end;

procedure TTilesDownloader.SetTileRes(AValue: Integer);
begin
  FOtherTileRes := True;
  if FTileRes=AValue then Exit;
  FTileRes:=AValue;
end;

procedure TTilesDownloader.Init;
begin
  InitSSLInterface;
  Self.AllowRedirect := true;
  Self.ConnectTimeOut := 10000;
  Self.AddHeader('User-Agent', FUserAgent);
end;

procedure TTilesDownloader.Download;
var
  LTile1, LTile2, LTileTmp: RTile;
  iz: Byte;
  ix, iy: Longword;
  LZoomCurrentCount, LZoomTotalCount, LCurrentCount, LTotalCount: Longword;
begin
  Init;

  if not DirectoryExists(OutPath) then
  if not ForceDirectories(OutPath) then
    Halt(1);

  LCurrentCount := 0;
  LTotalCount := TotalTilesCountOnCoordinates;

  for iz := MinZoom to MaxZoom do
  begin
    if SaveMethod = smFolders then
    if not DirectoryExists(Format('%s/%d', [OutPath, iz])) then
    if not ForceDirectories(Format('%s/%d', [OutPath, iz])) then
      raise Exception.Create('The necessary paths could not be created. Check the specified path');

    CalcTileNumber(Coordinates[0], iz, LTile1);
    CalcTileNumber(Coordinates[1], iz, LTile2);
    {$IFDEF DEBUG}
    WriteLn(Format('Coordinates[0]: %f, %f, Zoom: %d -> Tile: %d, %d', [Coordinates[0].lat, Coordinates[0].lon, iz,  LTile1.x, LTile1.y]));
    WriteLn(Format('Coordinates[1]: %f, %f, Zoom: %d -> Tile: %d, %d', [Coordinates[1].lat, Coordinates[1].lon, iz,  LTile2.x, LTile2.y]));
    {$ENDIF}

    LZoomCurrentCount := 0;
    LZoomTotalCount := CalcZoomTilesCount(LTile1, Ltile2);

    for ix := LTile1.X to LTile2.x do
    begin
      if SaveMethod = smFolders then
      if not DirectoryExists(Format('%s/%d/%d', [OutPath, iz, ix])) then
        ForceDirectories(Format('%s/%d/%d', [OutPath, iz, ix]));

      for iy := LTile1.y to LTile2.y do
      begin
        LTileTmp.x := ix;
        LTileTmp.y := iy;
        try
          DownloadTile(iz, LTileTmp);
          Inc(LZoomCurrentCount);
          Inc(LCurrentCount);
          WriteLn(Format('Total: %d/%d <- (Zoom %d: %d/%d)', [LCurrentCount, LTotalCount, iz, LZoomCurrentCount, LZoomTotalCount]));
        except
          on E: ETileDownload do
          begin
            if SkipMissing and (E is ETDReceive) then
            begin
              WriteLn('! Skip missing tile');
              Continue;
            end;

            WriteLn;
            WriteLn('Error: ', E.Message);
            Exit;
          end;
        end;
      end;
    end;

  end;
end;

procedure TTilesDownloader.DownloadFullMap;
var
  LTile: RTile;
  iz: Byte;
  ix, iy: Longword;
  LZoomCurrentCount, LZoomTotalCount, LCurrentCount, LTotalCount, LRowCount: Longword;
begin
  Init;

  if not DirectoryExists(OutPath) then
  if not ForceDirectories(OutPath) then
    raise Exception.Create('The necessary paths could not be created. Check the specified path');

  LCurrentCount := 0;
  LTotalCount := TotalTilesCount;

  for iz := MinZoom to MaxZoom do
  begin
    LRowCount := CalcRowTilesCount(iz);
    LZoomCurrentCount := 0;
    LZoomTotalCount := CalcZoomTilesCount(iz);

    if SaveMethod = smFolders then
    if not DirectoryExists(Format('%s%s%d', [OutPath, PathDelim, iz])) then
      ForceDirectories(Format('%s%s%d', [OutPath, PathDelim, iz]));

    for ix := 0 to LRowCount-1 do
    begin
      if SaveMethod = smFolders then
      if not DirectoryExists(Format('%s%s%d%s%d', [OutPath, PathDelim, iz, PathDelim, ix])) then
        ForceDirectories(Format('%s%s%d%s%d', [OutPath, PathDelim, iz, PathDelim, ix]));

      for iy := 0 to LRowCount-1 do
      begin
        LTile.SetValues(ix, iy);
        try
          DownloadTile(iz, LTile);
          Inc(LZoomCurrentCount);
          Inc(LCurrentCount);
          WriteLn(Format('Total: %d/%d <- (Zoom %d: %d/%d)', [LCurrentCount, LTotalCount, iz, LZoomCurrentCount, LZoomTotalCount]));
        except
          on E: ETileDownload do
          begin
            if SkipMissing and (E is ETDReceive) then
            begin
              WriteLn('! Skip missing tile');
              Continue;
            end;

            WriteLn;
            WriteLn('Error: ', E.Message);
            Exit;
          end;
        end;
      end;
    end;

  end;
end;

//{ CTDOpenStreetMap }
//
//constructor CTDOpenStreetMap.Create(AOwner: TComponent);
//begin
//  inherited Create(AOwner);
//end;
//
//{ CTDOpenTopotMap }
//
//constructor CTDOpenTopotMap.Create(AOwner: TComponent);
//begin
//  inherited Create(AOwner);
//
//  ProviderName := 'Open Topo Map';
//  ProviderLink := 'http://a.tile.opentopomap.org';
//end;
//
//{ CTDCycleOSM }
//
//constructor CTDCycleOSM.Create(AOwner: TComponent);
//begin
//  inherited Create(AOwner);
//
//  ProviderName := 'CycleOSM';
//  ProviderLink := 'https://c.tile-cyclosm.openstreetmap.fr/cyclosm/';
//end;
//
//{ CTDOpenRailwayMap }
//
//constructor CTDOpenRailwayMap.Create(AOwner: TComponent);
//begin
//  inherited Create(AOwner);
//
//  FUserAgent := 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 YaBrowser/24.6.0.0 Safari/537.36\';
//  ProviderName := 'OpenRailwayMap';
//  ProviderLink := 'http://b.tiles.openrailwaymap.org/standard';
//end;

{ CMergedTD }

procedure CMergedTD.DownloadTile(const AZoom: Integer; const ATile: RTile);
var
  LFileName, LFilePath: String;
  LTileImg, LMergedTileImg: TBGRABitmap;
begin
  try
    LTileImg := TBGRABitmap.Create;
    LMergedTileImg := TBGRABitmap.Create;

    ReceiveTile(LTileImg, ProviderLink, AZoom, ATile);
    MergedDownloader.ReceiveTile(LMergedTileImg, MergedDownloader.ProviderLink, AZoom, ATile);

    if FOtherTileRes then
    begin
      ResampleTile(LTileImg, TileRes);
    end;
    ResampleTile(LMergedTileImg, LTileImg.Width);
    FilterTile(LTileImg);
    LTileImg.PutImage(0, 0, LMergedTileImg, dmDrawWithTransparency);

    LFileName := GetFileName(AZoom, ATile.X, ATile.Y);
    LFilePath := Format('%s%s%s', [OutPath, PathDelim, LFileName]);
    SaveTile(LTileImg, LFilePath);
    LMergedTileImg.Free;
    LTileImg.Free;
  except
    on E: Exception do
    begin
      if Assigned(LTileImg) then
        LTileImg.Free;
      if Assigned(LMergedTileImg) then
        LMergedTileImg.Free;
      raise;
    end;
  end;
end;

constructor CMergedTD.Create(AOwner: TComponent; AMergedTD: TTilesDownloader);
begin
  Create(AOwner);

  FMergedDownloader := AMergedTD;
end;

procedure CMergedTD.Download;
begin
  MergedDownloader.Init;
  inherited Download;
end;

procedure CMergedTD.DownloadFullMap;
begin
  MergedDownloader.Init;
  inherited DownloadFullMap;
end;

{ CMrgTDOpenStreetMap }

constructor CMrgTDOpenStreetMap.Create(AOwner: TComponent; AMergedTD: TTilesDownloader);
begin
  inherited Create(AOwner, AMergedTD);
end;

{ CMrgTDOpenTopotMap }

constructor CMrgTDOpenTopotMap.Create(AOwner: TComponent; AMergedTD: TTilesDownloader);
begin
  inherited Create(AOwner, AMergedTD);

  ProviderName := 'Open Topo Map';
  ProviderLink := 'http://a.tile.opentopomap.org';
end;

{ CMrgTDCycleOSM }

constructor CMrgTDCycleOSM.Create(AOwner: TComponent; AMergedTD: TTilesDownloader);
begin
  inherited Create(AOwner, AMergedTD);

  ProviderName := 'CycleOSM';
  ProviderLink := 'https://c.tile-cyclosm.openstreetmap.fr/cyclosm/';
end;

{ CMrgTDOpenRailwayMap }

constructor CMrgTDOpenRailwayMap.Create(AOwner: TComponent; AMergedTD: TTilesDownloader);
begin
  inherited Create(AOwner, AMergedTD);

  FUserAgent := 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 YaBrowser/24.6.0.0 Safari/537.36\';
  ProviderName := 'OpenRailwayMap';
  ProviderLink := 'http://b.tiles.openrailwaymap.org/standard';
end;

end.



