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

unit TilesManipulations.Base;

{$mode ObjFPC}{$H+}{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  SysUtils, Classes, StrUtils, Math, FGL,
  fphttpclient, openssl, opensslsockets, BGRABitmap;

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

  ETilesManipulator = class(Exception);
  ETMSave = class(ETilesManipulator);
  ETMDownload = class(ETilesManipulator);

  { TTilesManipulator }

  TTilesManipulator = class
  const
    defPath = 'tiles/{p}/{z}/{x}/{y}';
    defSkipExisting = False;
    defSkipMissing = False;
    defShowFileType = False;
    defUseOtherTileRes = False;
  strict private
    FLayers: TLayers;
    FPath: String;
    FShowFileType: Boolean;
    FSkipExisting: Boolean;
    FSkipMissing: Boolean;
    FTileRes: Word;
    FUseOtherTileRes: Boolean;
  strict private // Getters and Setters
    procedure SetTileRes(AValue: Word);
  strict private
    function ProcessPath(const AProviderName: String; const AZoom: Integer; const AX, AY: Integer): String;
    procedure ResizeIfNeeded(var ATileImg: TBGRABitmap);
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
    property Layers      : TLayers read FLayers       write FLayers;
    property Path        : String  read FPath         write FPath;
    property ShowFileType: Boolean read FShowFileType write FShowFileType default defShowFileType;
    property SkipExisting: Boolean read FSkipExisting write FSkipExisting default defSkipExisting;
    property SkipMissing : Boolean read FSkipMissing  write FSkipMissing  default defSkipMissing;
    property TileRes     : Word    read FTileRes      write SetTileRes;
  end;

implementation

uses
  ssockets, BGRABitmapTypes, TilesManipulations.Utilities;

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
  Write('Setting up connection... ');

  for i := 0 to FUserAgents.Count-1 do
    try
      Self.AddHeader('User-Agent', FUserAgents[i]);
      Self.Get(AURL);
      WriteLn('Ok');
      FUASelected := True;
      Break;
    except
      on E: Exception do
      begin
        if i = FUserAgents.Count-1 then
        begin
          WriteLn('Fail');
          raise;
        end
        else
          Continue;
      end;
    end;

  {$IFDEF DEBUG}
  WriteLn(FUserAgent);
  {$ENDIF}
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

  Write(Format('TileLink: %s', [AURL]));
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
  Write(Name + ': ');
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
var
  LStartTime, LFinishTime: Int64;
begin
  if Assigned(FBuffer) then
    FreeAndNil(FBuffer);
  try
    LStartTime := GetTickCountMS;
    FBuffer := Provider.GiveTile(AZoom, AX, AY);
    if not Assigned(FBuffer) then
      raise ELayer.Create('Layer of ' + Provider.Name + ' did not load.');
    if Assigned(Filter) then
      Filter.Transform(FBuffer);
    LFinishTime := GetTickCountMS;
    WriteLn(' ' + Format('%d', [LFinishTime - LStartTime]));
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

{ TTilesManipulator }

function TTilesManipulator.ProcessPath(const AProviderName: String;
                                            const AZoom: Integer; const AX, AY: Integer): String;
begin
  Result := Path;
  Result := StringReplace(Result, '{p}', AProviderName, [rfReplaceAll]);
  Result := StringReplace(Result, '{z}', AZoom.ToString, [rfReplaceAll]);
  Result := StringReplace(Result, '{x}', AX.ToString, [rfReplaceAll]);
  Result := StringReplace(Result, '{y}', AY.ToString, [rfReplaceAll]);
  Result := Result + IfThen(ShowFileType, '.png');
end;

procedure TTilesManipulator.ResizeIfNeeded(var ATileImg: TBGRABitmap);
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

procedure TTilesManipulator.SaveTile(const ATileImg: TBGRABitmap; AFilePath: String);
var
  LFileStream: TFileStream = nil;
  LSaveStartTime, LSaveFinishTime: Int64;
begin
  LSaveStartTime := GetTickCountMS;
  Write(Format('FilePath: %s', [AFilePath]));
  if not ForceDirectories(ExtractFilePath(AFilePath)) then
    raise ETMSave.Create('Failed create dirs.');
  try
    LFileStream := TFileStream.Create(AFilePath, fmCreate or fmOpenWrite);
    ATileImg.SaveToStreamAsPng(LFileStream);
    FreeAndNil(LFileStream);
  except
    on E: Exception do
    begin
      WriteLn('');
      WriteLn(E.Message);
      if Assigned(LFileStream) then FreeAndNil(LFileStream);
      raise ETMSave.Create('Failed save file.');
    end;
  end;
  LSaveFinishTime := GetTickCountMS;
  WriteLn(' ' + Format('%d', [LSaveFinishTime - LSaveStartTime]));
end;

procedure TTilesManipulator.SetTileRes(AValue: Word);
begin
  FUseOtherTileRes := True;
  if FTileRes = AValue then Exit;
  FTileRes := AValue;
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
  FUseOtherTileRes := defUseOtherTileRes;
  FPath := defPath;
  FShowFileType := defShowFileType;
  FSkipExisting := defSkipExisting;
  FSkipMissing := defSkipMissing;
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
  LBuffer: TBGRABitmap = nil;
  LSavePath: String;
  LBeginTime, LEndTime: Int64;
begin
  if FLayers.Count < 1 then Exit;

  LBeginTime := GetTickCountMS;

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
        LSavePath := ProcessPath(Layers[0].Provider.Name, iz, ix, iy);

        if not (SkipExisting and FileExists(LSavePath)) then
          try
            if Assigned(LBuffer) then FreeAndNil(LBuffer);
            for il := 0 to Layers.Count-1 do
            begin
              Layers[il].Load(iz, ix, iy);
              if not Assigned(LBuffer) then
              begin
                LBuffer := Layers[il].Buffer.Duplicate(True);
                ResizeIfNeeded(LBuffer);
                Continue;
              end
              else
                Layers[il].ResampleAndPaintTo(LBuffer);
            end;
            SaveTile(LBuffer, LSavePath);
            FreeAndNil(LBuffer);
          except
            on E: ELayer do
              begin
                if Assigned(LBuffer) then FreeAndNil(LBuffer);
                if SkipMissing then
                begin
                  WriteLn('! Skip missing tile');
                  Continue;
                end;

                WriteLn;
                WriteLn('Error: ', E.Message);
                Exit;
              end;
            on E: ETMSave do
              begin
                if Assigned(LBuffer) then FreeAndNil(LBuffer);
                WriteLn;
                WriteLn('Error: ', E.Message);
                Exit;
              end;
          end
        else
          WriteLn('! SkipExisting');
        Inc(LCurrentCount);
        Inc(LZoomCurrentCount);
        WriteLn(Format('Total: %d/%d <- (Zoom %d: %d/%d)', [LCurrentCount, LTotalCount, iz, LZoomCurrentCount, LZoomTotalCount]));
      end;
    end;
  end;
  LEndTime := GetTickCountMS;
  WriteLn('Time: ' + Format('%d', [LEndTime - LBeginTime]));
end;

end.



