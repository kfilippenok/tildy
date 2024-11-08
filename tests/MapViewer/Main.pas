unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, ActnList, EditBtn, CheckLst, Buttons, BGRAShape, BCPanel,
  BCListBox, mvMapViewer, mvDLECache, indSliders, LazFileUtils, mvEngine,
  mvTypes, mvDE_BGRA, DlgAddLayers, DlgCoordinatesHelp, mvGpsObj, mvDrawingEngine;

type

  { TfMain }

  TfMain = class(TForm)
    actStartStop: TAction;
    ActionList: TActionList;
    btnDeleteLayer: TSpeedButton;
    btnStartDownload: TButton;
    chkShowCoordinates: TCheckBox;
    chkConcreteZone: TCheckBox;
    AreaCoordFirstLatitude: TLabeledEdit;
    AreaCoordFirstLongitude: TLabeledEdit;
    AreaCoordSecondLatitude: TLabeledEdit;
    AreaCoordSecondLongitude: TLabeledEdit;
    groupConcreteArea: TGroupBox;
    groupCAreaZoom: TGroupBox;
    LayersList: TCheckListBox;
    chShowFileType: TCheckBox;
    chkOutput: TCheckBox;
    chkProviderName: TCheckBox;
    chkActive: TCheckBox;
    chkCyclic: TCheckBox;
    chkDoubleBuff: TCheckBox;
    chkDebugTiles: TCheckBox;
    chkPreviewTiles: TCheckBox;
    chkUseThreads: TCheckBox;
    chkZoomToCursor: TCheckBox;
    CoordSecondLatitude: TLabeledEdit;
    groupLayers: TGroupBox;
    lblCAreaMaxZoom: TLabel;
    lblCAreaMaxZoomValue: TLabel;
    lblCAreaMinZoom: TLabel;
    lblCAreaMinZoomValue: TLabel;
    MvBGRADrawingEngine: TMvBGRADrawingEngine;
    panCAreaZoom: TPanel;
    PathExecutable: TFileNameEdit;
    groupCoordinates: TGroupBox;
    groupExecutable: TGroupBox;
    groupZoom: TGroupBox;
    CoordFirstLatitude: TLabeledEdit;
    CoordFirstLongtitude: TLabeledEdit;
    CoordSecondLongtitude: TLabeledEdit;
    lblDebugZoom: TLabel;
    lblDebugLon: TLabel;
    lblDebugLat: TLabel;
    lblMaxZoom: TLabel;
    lblMinZoomValue: TLabel;
    lblMaxZoomValue: TLabel;
    lblMinZoom: TLabel;
    FullyOrPartially: TComboBox;
    panDebug: TPanel;
    ProcessTilesdownloader: TProcess;
    LayersUpDown: TUpDown;
    btnAddLayer: TSpeedButton;
    Shape1: TShape;
    btnCoordinateHelp: TSpeedButton;
    ZoomRange: TMultiSlider;
    panZoom: TPanel;
    SaveMethodVariations: TComboBox;
    DirectoryCache: TDirectoryEdit;
    DirectoryOutput: TDirectoryEdit;
    edDivider: TEdit;
    groupOutput: TGroupBox;
    groupOther: TGroupBox;
    groupCache: TGroupBox;
    groupSaveMethod: TGroupBox;
    groupProviderTiles: TGroupBox;
    groupProviderMap: TGroupBox;
    ProviderName: TLabeledEdit;
    MapView: TMapView;
    ConsoleOutput: TMemo;
    MVDECache: TMVDECache;
    panMV: TPanel;
    ProviderVariationsTiles: TComboBox;
    ProviderVariationsMap: TComboBox;
    mvScrollBox: TScrollBox;
    tdScrollBox: TScrollBox;
    Settings: TPageControl;
    pageMapView: TTabSheet;
    pageTilesDownloader: TTabSheet;
    mvSsettings: TSplitter;
    optionsSoutlog: TSplitter;
    ZoomCAreaRange: TMultiSlider;
    procedure actStartStopExecute(Sender: TObject);
    procedure btnAddLayerClick(Sender: TObject);
    procedure btnDeleteLayerClick(Sender: TObject);
    procedure btnStartDownloadClick(Sender: TObject);
    procedure chkActiveChange(Sender: TObject);
    procedure chkConcreteZoneChange(Sender: TObject);
    procedure chkCyclicChange(Sender: TObject);
    procedure chkDebugTilesChange(Sender: TObject);
    procedure chkDoubleBuffChange(Sender: TObject);
    procedure chkOutputChange(Sender: TObject);
    procedure chkPreviewTilesChange(Sender: TObject);
    procedure chkProviderNameChange(Sender: TObject);
    procedure chkShowCoordinatesChange(Sender: TObject);
    procedure chkUseThreadsChange(Sender: TObject);
    procedure chkZoomToCursorChange(Sender: TObject);
    procedure CoordChange(Sender: TObject);
    procedure DirectoryCacheChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FullyOrPartiallySelect(Sender: TObject);
    procedure groupCoordinatesResize(Sender: TObject);
    procedure LayersListClick(Sender: TObject);
    procedure LayersListClickCheck(Sender: TObject);
    procedure LayersListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LayersListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure MapViewCenterMoving(Sender: TObject; var NewCenter: TRealPoint;
      var Allow: Boolean);
    procedure MapViewDrawGpsPoint(Sender: TObject;
      ADrawer: TMvCustomDrawingEngine; APoint: TGpsPoint);
    procedure MapViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MapViewZoomChange(Sender: TObject);
    procedure MapViewZoomChanging(Sender: TObject; NewZoom: Integer;
      var Allow: Boolean);
    procedure PathExecutableChange(Sender: TObject);
    procedure ProviderVariationsMapChange(Sender: TObject);
    procedure LayersUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure btnCoordinateHelpClick(Sender: TObject);
    procedure ZoomRangePositionChange(Sender: TObject; AKind: TThumbKind;
      AValue: Integer);
  private
    BMP_FC, BMP_SC: TPicture;
    FirstCoordinate, SecondCoordinate: TGpsPoint;
    ShowCoordinates: Boolean;
    FConcreteArea: TRealArea;
    procedure ReloadLayersList;
  public

  end;

const
  _CLICKED_POINTS_ = 10;

var
  fMain: TfMain;

implementation

{$R *.lfm}

{ TfMain }

procedure TfMain.actStartStopExecute(Sender: TObject);
begin
  MapView.Active := not MapView.Active;

  if MapView.Active then
    actStartStop.Caption := 'Stop'
  else
    actStartStop.Caption := 'Start';
end;

procedure TfMain.btnAddLayerClick(Sender: TObject);
begin
  fAddLayers := TfAddLayers.CreateEx(MapView);
  if fAddLayers.ShowModal = mrAll then
  begin
    ReloadLayersList;
  end;
  FreeAndNil(fAddLayers);
end;

procedure TfMain.btnDeleteLayerClick(Sender: TObject);
begin
  MapView.Layers.Delete(LayersList.ItemIndex);
  LayersList.Items.Delete(LayersList.ItemIndex);
  btnDeleteLayer.Enabled := (LayersList.ItemIndex <> -1) and (LayersList.Count > 0);
  LayersUpDown.Enabled := btnDeleteLayer.Enabled;
end;

procedure TfMain.btnStartDownloadClick(Sender: TObject);
var
  StrVariation: String;
begin
  ProcessTilesdownloader.Executable := PathExecutable.FileName;
  ProcessTilesdownloader.Options := [poWaitOnExit, poUsePipes];

  // -provider
  StrVariation := ProviderVariationsTiles.Items[ProviderVariationsTiles.ItemIndex];
  case StrVariation of
    'OpenStreetMap Mapnik'      : StrVariation := 'osm';
    'Open Topo Map'             : StrVariation := 'otm';
    'OpenStreetMap.fr Cycle Map': StrVariation := 'osm-cycle';
    'OpenRailwayMap'            : StrVariation := 'railway';
  end;
  ProcessTilesdownloader.Parameters.Add('-provider ' + StrVariation);

  // -provider-name
  if chkProviderName.Checked then
    ProcessTilesdownloader.Parameters.Add('-provider-name ' + ProviderName.Text);

  // -output
  if chkOutput.Checked then
    ProcessTilesdownloader.Parameters.Add('-output ' + DirectoryOutput.Directory);

  // -save-method
  StrVariation := SaveMethodVariations.Items[SaveMethodVariations.ItemIndex];
  case StrVariation of
    'pattern': ProcessTilesdownloader.Parameters.Add('-save-method pattern ' + '-divider ' + edDivider.Text);
  end;

  // -min-zoom
  ProcessTilesdownloader.Parameters.Add('-min-zoom ' + ZoomRange.MinPosition.ToString);
  // max zoom
  ProcessTilesdownloader.Parameters.Add('-max-zoom ' + ZoomRange.MaxPosition.ToString);

  // coordinates or full-map
  StrVariation := FullyOrPartially.Items[FullyOrPartially.ItemIndex];
  case StrVariation of
    'Full map': ProcessTilesdownloader.Parameters.Add('-full-map');
    'Coordinates' :;
  end;

  ProcessTilesdownloader.Execute;

  ConsoleOutput.Lines.LoadFromStream(ProcessTilesdownloader.Output);
end;

procedure TfMain.chkActiveChange(Sender: TObject);
begin
  MapView.Active := chkActive.Checked;
end;

procedure TfMain.chkConcreteZoneChange(Sender: TObject);
var
  FirstLat, FirstLon, SecondLat, SecondLon: Extended;
begin
  if chkConcreteZone.Checked then
  begin
    FirstLat  := StrToFloat(AreaCoordFirstLatitude.Text);
    FirstLon  := StrToFloat(AreaCoordFirstLongitude.Text);
    SecondLat := StrToFloat(AreaCoordSecondLatitude.Text);
    SecondLon := StrToFloat(AreaCoordSecondLongitude.Text);
    FConcreteArea.Init(FirstLon, FirstLat, SecondLon, SecondLat);
    MapView.Zoom := ZoomCAreaRange.MinPosition;
    MapView.MapCenter.Latitude := (FirstLat + SecondLat) / 2;
    MapView.MapCenter.Longitude := (FirstLon + SecondLon) / 2;
    MapView.OnZoomChanging := @MapViewZoomChanging;
    MapView.OnCenterMoving := @MapViewCenterMoving;
  end
  else
  begin
    MapView.OnZoomChanging := nil;
    MapView.OnCenterMoving := nil;
  end;
end;

procedure TfMain.chkCyclicChange(Sender: TObject);
begin
  MapView.Cyclic := chkCyclic.Checked;
end;

procedure TfMain.chkDebugTilesChange(Sender: TObject);
begin
  MapView.DebugTiles := chkDebugTiles.Checked;
end;

procedure TfMain.chkDoubleBuffChange(Sender: TObject);
begin
  MapView.DoubleBuffered := chkDoubleBuff.Checked;
end;

procedure TfMain.chkOutputChange(Sender: TObject);
begin
  DirectoryOutput.Enabled := chkOutput.Checked;
end;

procedure TfMain.chkPreviewTilesChange(Sender: TObject);
begin
  MapView.DrawPreviewTiles := chkPreviewTiles.Checked;
end;

procedure TfMain.chkProviderNameChange(Sender: TObject);
begin
  ProviderName.Enabled := chkProviderName.Checked;
end;

procedure TfMain.chkShowCoordinatesChange(Sender: TObject);
begin
  ShowCoordinates := chkShowCoordinates.Checked;
  FirstCoordinate.Visible := ShowCoordinates;
  SecondCoordinate.Visible := ShowCoordinates;
  MapView.Refresh;
end;

procedure TfMain.chkUseThreadsChange(Sender: TObject);
begin
  MapView.UseThreads := chkUseThreads.Checked;
end;

procedure TfMain.chkZoomToCursorChange(Sender: TObject);
begin
  MapView.ZoomToCursor := chkZoomToCursor.Checked;
end;

procedure TfMain.CoordChange(Sender: TObject);

  function _TryTextToDouble(var DoubleVar: Extended; Text: String): Boolean;
  begin
    try
      DoubleVar := StrToFloat(Text);
      Result := True;
    except
      on E: EConvertError do
      begin
        Result := False;
      end;
    end;
  end;

var
  FirstLat, FirstLon, SecondLat, SecondLon: Extended;

  function _FieldsConverts: Boolean;
  begin
    Result := True;
    try
      FirstLat  := StrToFloat(AreaCoordFirstLatitude.Text);
      FirstLon  := StrToFloat(AreaCoordFirstLongitude.Text);
      SecondLat := StrToFloat(AreaCoordSecondLatitude.Text);
      SecondLon := StrToFloat(AreaCoordSecondLongitude.Text);
    except
      on E: EConvertError do
      begin
        Exit(False);
      end;
    end;
  end;

  function _ValuesCorrect: Boolean;
  begin
    Result := (FirstLat > SecondLat) and (FirstLon < SecondLon)
  end;

begin
  if not _FieldsConverts then
  begin
    chkShowCoordinates.Enabled := False;
    chkConcreteZone.Enabled := False;
    Exit;
  end;
  chkConcreteZone.Enabled := _ValuesCorrect;
  chkShowCoordinates.Enabled := chkConcreteZone.Enabled;
  if chkConcreteZone.Enabled then
  begin
    FirstCoordinate.Lon := FirstLon;
    FirstCoordinate.Lat := FirstLat;
    SecondCoordinate.Lon := SecondLon;
    SecondCoordinate.Lat := SecondLat;
  end;
end;

procedure TfMain.DirectoryCacheChange(Sender: TObject);
begin
  MapView.CacheLocation := clCustom;
  MapView.CachePath := DirectoryCache.Text + PathDelim;
end;

procedure TfMain.FormActivate(Sender: TObject);
begin
  chkActiveChange(nil);
  chkCyclicChange(nil);
  chkDebugTilesChange(nil);
  chkDoubleBuffChange(nil);
  chkPreviewTilesChange(nil);
  chkUseThreadsChange(nil);
  chkZoomToCursorChange(nil);

  FirstCoordinate  := TGpsPoint.Create(StrToFloat(AreaCoordFirstLongitude.Text), StrToFloat(AreaCoordFirstLatitude.Text));
  SecondCoordinate := TGpsPoint.Create(StrToFloat(AreaCoordSecondLongitude.Text), StrToFloat(AreaCoordSecondLatitude.Text));
  CoordChange(nil);
  ShowCoordinates := chkShowCoordinates.Checked;
  FirstCoordinate.Visible := ShowCoordinates;
  SecondCoordinate.Visible := ShowCoordinates;
  BMP_FC := TPicture.Create; BMP_FC.LoadFromFile('img' + PathDelim + 'first_coordinate.png');
  BMP_SC := TPicture.Create; BMP_SC.LoadFromFile('img' + PathDelim + 'second_coordinate.png');
  MapView.GPSItems.Add(FirstCoordinate, _CLICKED_POINTS_);
  MapView.GPSItems.Add(SecondCoordinate, _CLICKED_POINTS_);


  MapView.GetMapProviders(ProviderVariationsMap.Items);
  MapView.GetMapProviders(ProviderVariationsTiles.Items);
  ReloadLayersList;

  // This is necessary so that the slider does not run away to the side.
  // The problem is in the component, but at the moment it's like a temporary solution.
  ZoomCAreaRange.MinPosition := 6;
  ZoomCAreaRange.MaxPosition := 13;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  if Assigned(BMP_FC) then BMP_FC.Free;
  if Assigned(BMP_SC) then BMP_SC.Free;
end;

procedure TfMain.FullyOrPartiallySelect(Sender: TObject);
begin
  case FullyOrPartially.ItemIndex of
    0:
      begin
        CoordFirstLatitude.Enabled := False;
        CoordFirstLongtitude.Enabled := False;
        CoordSecondLatitude.Enabled := False;
        CoordSecondLongtitude.Enabled := False;
      end;
    1:
      begin
        CoordFirstLatitude.Enabled := True;
        CoordFirstLongtitude.Enabled := True;
        CoordSecondLatitude.Enabled := True;
        CoordSecondLongtitude.Enabled := True;
      end;
  end;
end;

procedure TfMain.groupCoordinatesResize(Sender: TObject);
begin
  CoordFirstLatitude.Width := Trunc(FullyOrPartially.Width / 2);
  CoordSecondLatitude.Width := Trunc(FullyOrPartially.Width / 2);
end;

procedure TfMain.LayersListClick(Sender: TObject);
begin
  btnDeleteLayer.Enabled := (LayersList.ItemIndex <> -1) and (LayersList.Count > 0);
  LayersUpDown.Enabled := btnDeleteLayer.Enabled;
end;

procedure TfMain.LayersListClickCheck(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to MapView.Layers.Count-1 do
    (MapView.Layers.Items[i] as TMapLayer).Visible := LayersList.Checked[i];
end;

procedure TfMain.LayersListDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  clbox : TCheckListBox;
  OldIndex, NewIndex: Integer;
begin
  if (Sender <> Source) then Exit; // accept dragging only within LayersList

  clbox := Sender as TCheckListBox;
  if clbox.Count < 2 then
    Exit;

  newIndex := clbox.GetIndexAtXY(X, Y);
  oldIndex := clbox.ItemIndex;

  if newIndex = -1 then             // if dragging to empty area
    newIndex := clbox.Count-1;

  if newIndex = oldIndex then Exit; // Проверка на смену позиции

  clbox.Items.Move(oldIndex, newIndex); // Передвигаем элемент

  clbox.ItemIndex := newIndex;
end;

procedure TfMain.LayersListDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if LayersList.Count > 0 then
    Accept := True;
end;

procedure TfMain.MapViewCenterMoving(Sender: TObject;
  var NewCenter: TRealPoint; var Allow: Boolean);
begin
  Allow := True;
end;

procedure TfMain.MapViewDrawGpsPoint(Sender: TObject;
  ADrawer: TMvCustomDrawingEngine; APoint: TGpsPoint);
var
  P: TPoint;
  LeftShift, TopShift: Integer;
begin
  P := TMapView(Sender).LatLonToScreen(APoint.RealPoint);

  if APoint = FirstCoordinate then
  begin
    if not Assigned(BMP_FC) then Exit;
    LeftShift := Trunc(BMP_FC.Bitmap.Width / 2);
    TopShift := Trunc(BMP_FC.Bitmap.Height / 2);
    ADrawer.DrawBitmap(P.X - LeftShift, P.Y - TopShift, BMP_FC.Bitmap, True);
  end
  else if APoint = SecondCoordinate then
  begin
    if not Assigned(BMP_SC) then Exit;
    LeftShift := Trunc(BMP_SC.Bitmap.Width / 2);
    TopShift := Trunc(BMP_SC.Bitmap.Height / 2);
    ADrawer.DrawBitmap(P.X - LeftShift, P.Y - TopShift, BMP_SC.Bitmap, True);
  end;
end;

procedure TfMain.MapViewMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  LRealPoint: TRealPoint;
  LPoint: TPoint;
begin
  LPoint.X := X; LPoint.Y := Y;
  LRealPoint := MapView.Engine.ScreenToLatLon(LPoint);
  lblDebugLat.Caption := 'Lat: ' + Format('%13.10f', [LRealPoint.Lat]);
  lblDebugLon.Caption := 'Lon: ' + Format('%14.10f', [LRealPoint.Lon]);
end;

procedure TfMain.MapViewZoomChange(Sender: TObject);
begin
  lblDebugZoom.Caption := 'Zoom: ' + MapView.Zoom.ToString;
end;

procedure TfMain.MapViewZoomChanging(Sender: TObject; NewZoom: Integer;
  var Allow: Boolean);
begin
  Allow := (NewZoom >= ZoomCAreaRange.MinPosition) and (NewZoom <= ZoomCAreaRange.MaxPosition);
end;

procedure TfMain.PathExecutableChange(Sender: TObject);
begin
  if FileIsExecutable(PathExecutable.FileName) then
  begin
    btnStartDownload.Enabled := True
  end
  else
    btnStartDownload.Enabled := False;
end;

procedure TfMain.ProviderVariationsMapChange(Sender: TObject);
begin
  with ProviderVariationsMap do
    MapView.MapProvider := Items[ItemIndex];
  chkActive.Enabled := True;
end;

procedure TfMain.LayersUpDownClick(Sender: TObject; Button: TUDBtnType);
var
  NewIndex: Integer;
begin
  if LayersList.ItemIndex = -1 then Exit;

  case Button of
    btNext:
      begin
        if LayersList.ItemIndex = 0 then Exit;
        NewIndex := LayersList.ItemIndex - 1;
        MapView.Layers.Move(LayersList.ItemIndex, NewIndex);
        LayersList.Items.Move(LayersList.ItemIndex, NewIndex);
        LayersList.Selected[NewIndex] := True;
      end;
    btPrev:
      begin
        if LayersList.ItemIndex = LayersList.Count-1 then Exit;
        NewIndex := LayersList.ItemIndex + 1;
        MapView.Layers.Move(LayersList.ItemIndex, NewIndex);
        LayersList.Items.Move(LayersList.ItemIndex, NewIndex);
        LayersList.Selected[NewIndex] := True;
      end;
  end;
end;

procedure TfMain.btnCoordinateHelpClick(Sender: TObject);
begin
  fCoordinatesHelp.ShowModal;
end;

procedure TfMain.ZoomRangePositionChange(Sender: TObject; AKind: TThumbKind;
  AValue: Integer);
begin
  case AKind of
    tkMin: lblCAreaMinZoomValue.Caption := AValue.ToString;
    tkMax: lblCAreaMaxZoomValue.Caption := AValue.ToString;
  else
  end;
end;

procedure TfMain.ReloadLayersList;
var
  il: Integer;
  MapLayer: TMapLayer;
begin
  LayersList.Clear;
  for il := 0 to MapView.Layers.Count-1 do
  begin
    MapLayer := MapView.Layers[il] as TMapLayer;
    LayersList.Items.Add(MapLayer.MapProvider);
    if MapLayer.Visible then
      LayersList.Checked[il] := True;
  end;
end;

end.

