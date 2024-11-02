unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, ActnList, EditBtn, CheckLst, mvMapViewer,
  mvDLECache, indSliders, LazFileUtils;

type

  { TfMain }

  TfMain = class(TForm)
    actStartStop: TAction;
    ActionList: TActionList;
    btnStartDownload: TButton;
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
    procedure actStartStopExecute(Sender: TObject);
    procedure btnStartDownloadClick(Sender: TObject);
    procedure chkActiveChange(Sender: TObject);
    procedure chkCyclicChange(Sender: TObject);
    procedure chkDebugTilesChange(Sender: TObject);
    procedure chkDoubleBuffChange(Sender: TObject);
    procedure chkOutputChange(Sender: TObject);
    procedure chkPreviewTilesChange(Sender: TObject);
    procedure chkProviderNameChange(Sender: TObject);
    procedure chkUseThreadsChange(Sender: TObject);
    procedure chkZoomToCursorChange(Sender: TObject);
    procedure DirectoryCacheChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FullyOrPartiallySelect(Sender: TObject);
    procedure groupCoordinatesResize(Sender: TObject);
    procedure LayersListClickCheck(Sender: TObject);
    procedure LayersListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LayersListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure MapViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MapViewZoomChange(Sender: TObject);
    procedure PathExecutableChange(Sender: TObject);
    procedure ProviderVariationsMapChange(Sender: TObject);
    procedure LayersUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure ZoomRangePositionChange(Sender: TObject; AKind: TThumbKind;
      AValue: Integer);
  private

  public

  end;

var
  fMain: TfMain;

implementation

{$R *.lfm}

uses
  mvEngine, mvTypes, mvDrawingEngine, mvMapProvider;

{ TfMain }

procedure TfMain.actStartStopExecute(Sender: TObject);
begin
  MapView.Active := not MapView.Active;

  if MapView.Active then
    actStartStop.Caption := 'Stop'
  else
    actStartStop.Caption := 'Start';
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

procedure TfMain.chkUseThreadsChange(Sender: TObject);
begin
  MapView.UseThreads := chkUseThreads.Checked;
end;

procedure TfMain.chkZoomToCursorChange(Sender: TObject);
begin
  MapView.ZoomToCursor := chkZoomToCursor.Checked;
end;

procedure TfMain.DirectoryCacheChange(Sender: TObject);
begin
  MapView.CacheLocation := clCustom;
  MapView.CachePath := DirectoryCache.Text + PathDelim;
end;

procedure TfMain.FormActivate(Sender: TObject);
begin
  chkActiveChange(Self);
  chkCyclicChange(Self);
  chkDebugTilesChange(Self);
  chkDoubleBuffChange(Self);
  chkPreviewTilesChange(Self);
  chkUseThreadsChange(Self);
  chkZoomToCursorChange(Self);
  ProviderVariationsMapChange(Self);

  ZoomRange.MinPosition := 1;
  ZoomRange.MaxPosition := 4;
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

procedure TfMain.LayersListClickCheck(Sender: TObject);
var
  LMapLayer: TMapLayer;
  LMapProvider: TMapProvider;
  LMapProviders: TStringList;
begin
  case LayersList.Items[LayersList.ItemIndex] of
    'OpenStreetMap Mapnik'      :
      begin
        LMapProvider := MapView.Engine.MapProviderByName('OpenStreetMap Mapnik');
        ShowMessage(LMapProvider.Name);
        LMapLayer := MapView.Layers.Add as TMapLayer;
        LMapLayer := TMapLayer.Create(MapView.Layers);
        LMapProviders := TStringList.Create;
        MapView.GetMapProviders(LMapProviders);
        ShowMessage(LMapProviders.Text);
        LMapProviders.Free;
        LMapLayer.MapProvider := 'OpenStreetMap Mapnik';
        LMapLayer.DrawMode := idmUseSourceAlpha;
        LMapLayer.UseThreads := MapView.UseThreads;
        LMapLayer.Visible := True;
      end;
    'Open Topo Map'             :;
    'OpenStreetMap.fr Cycle Map':;
    'OpenRailwayMap'            :
      begin
        LMapProvider := MapView.Engine.MapProviderByName('OpenRailwayMap');
        ShowMessage(LMapProvider.Name);
        LMapLayer := MapView.Layers.Add as TMapLayer;
        LMapLayer := TMapLayer.Create(MapView.Layers);
        LMapProviders := TStringList.Create;
        MapView.GetMapProviders(LMapProviders);
        ShowMessage(LMapProviders.Text);
        LMapProviders.Free;
        LMapLayer.MapProvider := 'OpenRailwayMap';
        LMapLayer.DrawMode := idmUseSourceAlpha;
        LMapLayer.UseThreads := MapView.UseThreads;
        LMapLayer.Visible := True;
      end;
  end;
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
        LayersList.Items.Move(LayersList.ItemIndex, NewIndex);
        LayersList.Selected[NewIndex] := True;
      end;
    btPrev:
      begin
        if LayersList.ItemIndex = LayersList.Count-1 then Exit;
        NewIndex := LayersList.ItemIndex + 1;
        LayersList.Items.Move(LayersList.ItemIndex, NewIndex);
        LayersList.Selected[NewIndex] := True;
      end;
  end;
end;

procedure TfMain.ZoomRangePositionChange(Sender: TObject; AKind: TThumbKind;
  AValue: Integer);
begin
  case AKind of
    tkMin: lblMinZoomValue.Caption := AValue.ToString;
    tkMax: lblMaxZoomValue.Caption := AValue.ToString;
  else
  end;
end;

end.

