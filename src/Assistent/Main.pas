unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, ActnList, EditBtn, CheckLst, Buttons, Menus, nullable,
  indSliders, LazFileUtils,
  // MapView
  mvMapViewer, mvDLECache, mvEngine, mvTypes, mvDE_BGRA, mvDrawingEngine,
  mvGpsObj, mvDLEFpc, mvPluginCommon, mvAreaSelectionPlugin,
  // Dialogs
  DlgAddLayers, DlgCoordinatesHelp, Types;

type

  TNullableInt = specialize TNullable<Integer>;

  { TfMain }

  TfMain = class(TForm)
    actAreasAdd: TAction;
    actAreasDelete: TAction;
    actAreasImport: TAction;
    actAreasExport: TAction;
    ActionsAreas: TActionList;
    actStartStop: TAction;
    ActionList: TActionList;
    btnAddArea: TSpeedButton;
    btnDeleteLayer: TSpeedButton;
    btnDeleteArea: TSpeedButton;
    btnImportAreas: TSpeedButton;
    btnStartDownload: TButton;
    chkCache: TCheckBox;
    chkShowCoordinates: TCheckBox;
    chkConcreteZone: TCheckBox;
    AreaCoordFirstLatitude: TLabeledEdit;
    AreaCoordFirstLongitude: TLabeledEdit;
    AreaCoordSecondLatitude: TLabeledEdit;
    AreaCoordSecondLongitude: TLabeledEdit;
    AreasUpDown: TUpDown;
    lblConcreteZone: TLabel;
    lblAreas: TLabel;
    lblZoom: TLabel;
    lblOther: TLabel;
    lblProviderMap: TLabel;
    lblCache: TLabel;
    lblLayers: TLabel;
    lblDebugObjects: TLabel;
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
    lblCAreaMaxZoom: TLabel;
    lblCAreaMaxZoomValue: TLabel;
    lblCAreaMinZoom: TLabel;
    lblCAreaMinZoomValue: TLabel;
    AreasList: TListBox;
    MapView: TMapView;
    miCoordinatesHelp: TMenuItem;
    MvBGRADrawingEngine: TMvBGRADrawingEngine;
    MVDEFPC: TMVDEFPC;
    MvPluginManager: TMvPluginManager;
    OpenDialog: TOpenDialog;
    panConcreteZone: TPanel;
    panAreas: TPanel;
    panZoom: TPanel;
    panOther: TPanel;
    panProviderMap: TPanel;
    panCache: TPanel;
    panLayers: TPanel;
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
    PopupMapView: TPopupMenu;
    ProcessTilesdownloader: TProcess;
    LayersUpDown: TUpDown;
    btnAddLayer: TSpeedButton;
    btnCoordinateHelp: TSpeedButton;
    btnExportAreas: TSpeedButton;
    SaveDialog: TSaveDialog;
    ZoomRange: TMultiSlider;
    panZoom2: TPanel;
    SaveMethodVariations: TComboBox;
    DirectoryCache: TDirectoryEdit;
    DirectoryOutput: TDirectoryEdit;
    edDivider: TEdit;
    groupOutput: TGroupBox;
    groupOther: TGroupBox;
    groupSaveMethod: TGroupBox;
    groupProviderTiles: TGroupBox;
    ProviderName: TLabeledEdit;
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
    procedure actAreasAddExecute(Sender: TObject);
    procedure actAreasDeleteExecute(Sender: TObject);
    procedure actAreasExportExecute(Sender: TObject);
    procedure actAreasImportExecute(Sender: TObject);
    procedure ActionsAreasUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure actStartStopExecute(Sender: TObject);
    procedure AreasListClick(Sender: TObject);
    procedure AreasListSelectionChange(Sender: TObject; User: boolean);
    procedure btnAddAreaClick(Sender: TObject);
    procedure btnAddLayerClick(Sender: TObject);
    procedure btnDeleteAreaClick(Sender: TObject);
    procedure btnDeleteLayerClick(Sender: TObject);
    procedure btnEditAreaClick(Sender: TObject);
    procedure btnStartDownloadClick(Sender: TObject);
    procedure chkActiveChange(Sender: TObject);
    procedure chkCacheChange(Sender: TObject);
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FullyOrPartiallySelect(Sender: TObject);
    procedure panConcreteZoneResize(Sender: TObject);
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
    procedure MapViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MapViewZoomChange(Sender: TObject);
    procedure MapViewZoomChanging(Sender: TObject; NewZoom: Integer;
      var Allow: Boolean);
    procedure miCoordinatesHelpClick(Sender: TObject);
    procedure PathExecutableChange(Sender: TObject);
    procedure PopupMapViewPopup(Sender: TObject);
    procedure ProviderVariationsMapChange(Sender: TObject);
    procedure LayersUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure btnCoordinateHelpClick(Sender: TObject);
    procedure ProviderVariationsMapSelect(Sender: TObject);
    procedure ZoomRangePositionChange(Sender: TObject; AKind: TThumbKind;
      AValue: Integer);
    procedure OnSelectedAreaBeginChange(Sender: TObject);
  private
    BMP_FC, BMP_SC: TPicture;
    FirstCoordinate, SecondCoordinate: TGpsPoint;
    ShowCoordinates: Boolean;
    SelectedCoordinates: TGPSObjarray;
    FConcreteArea: TRealArea;
    FPluginCount: Integer;
    FPrevAreaIndex: TNullableInt;
    procedure ReloadLayersList;
    procedure SetEnableAreaConrols;
  public

  end;

const
  _CLICKED_POINTS_ = 10;

var
  fMain: TfMain;

implementation

{$R *.lfm}

uses
  IniFiles;

{ TfMain }

procedure TfMain.actStartStopExecute(Sender: TObject);
begin
  MapView.Active := not MapView.Active;

  if MapView.Active then
    actStartStop.Caption := 'Stop'
  else
    actStartStop.Caption := 'Start';
end;

procedure TfMain.ActionsAreasUpdate(AAction: TBasicAction; var Handled: Boolean);
begin
  btnDeleteArea.Enabled := (AreasList.ItemIndex <> -1);
  AreasUpDown.Enabled := btnDeleteArea.Enabled and (AreasList.Count > 1);
  btnExportAreas.Enabled := AreasList.Count > 0;

  Handled := True;
end;

procedure TfMain.actAreasImportExecute(Sender: TObject);
const
    _SectionStr = 'Area';
var
  LIniFile: TMemIniFile = nil;
  LSection: TStringList = nil;
  LMemoryStream: TMemoryStream = nil;
  LCount: Integer = 0;
  LAllValuesExists: Boolean = False;
  LStringList: TStringList = nil;
  LRealAreaArray: array of TRealArea;
  LAreaSelectionPlugin: TAreaSelectionPlugin;
  i: Integer;
begin
  if not OpenDialog.Execute then Exit;

  try
    LStringList := TStringList.Create;
    LMemoryStream := TMemoryStream.Create;
    LSection := TStringList.Create;
    try
      LMemoryStream.LoadFromFile(OpenDialog.FileName);
      LIniFile := TMemIniFile.Create(LMemoryStream);
      SetLength(LRealAreaArray, LCount);

      LIniFile.ReadSection(_SectionStr, LSection);
      while LSection.Count > 0 do
      begin
        Inc(LCount);
        LStringList.Add(LCount.ToString);
        SetLength(LRealAreaArray, LCount);
        LAllValuesExists := (LIniFile.ValueExists(_SectionStr, 'left')
                         and LIniFile.ValueExists(_SectionStr, 'top')
                         and LIniFile.ValueExists(_SectionStr, 'right')
                         and LIniFile.ValueExists(_SectionStr, 'bottom'));
        if not LAllValuesExists then
          raise Exception.Create(_SectionStr + ' number ' + LCount.ToString + ' has not all values');

        LRealAreaArray[LCount-1].TopLeft.Lon := LIniFile.ReadFloat(_SectionStr, 'left', 0.0);
        LRealAreaArray[LCount-1].TopLeft.Lat := LIniFile.ReadFloat(_SectionStr, 'top', 0.0);
        LRealAreaArray[LCount-1].BottomRight.Lon := LIniFile.ReadFloat(_SectionStr, 'right', 0.0);
        LRealAreaArray[LCount-1].BottomRight.Lat := LIniFile.ReadFloat(_SectionStr, 'bottom', 0.0);

        LIniFile.EraseSection(_SectionStr);
        LIniFile.ReadSection(_SectionStr, LSection);
      end;

      for i := 0 to AreasList.Count-1 do
      begin
        LAreaSelectionPlugin := AreasList.Items.Objects[i] as TAreaSelectionPlugin;
        MvPluginManager.PluginList.Delete(MvPluginManager.PluginList.IndexOf(LAreaSelectionPlugin));
      end;

      if LStringList.Count > 0 then
      begin
        FPrevAreaIndex.Clear;
        AreasList.Clear;
        for i := 0 to LStringList.Count-1 do
        begin
          LAreaSelectionPlugin := TAreaSelectionPlugin.Create(MvPluginManager);
          LAreaSelectionPlugin.MapView := MapView;
          LAreaSelectionPlugin.OnSelectedAreaBeginChange := @OnSelectedAreaBeginChange;
          LAreaSelectionPlugin.SelectedArea.Area := LRealAreaArray[i];
          AreasList.AddItem(LStringList[i], LAreaSelectionPlugin);
        end;
      end;
    except
      on E: Exception do
      begin
        WriteLn(E.ClassName + ': ' + E.Message);
      end;
    end;
  finally
    LStringList.Free;
    LSection.Free;
    LIniFile.Free;
    LMemoryStream.Free;
  end;
end;

procedure TfMain.actAreasExportExecute(Sender: TObject);
const
  _SectionStr = '[Area]';
var
  LStringList: TStringList = nil;
  LAreaSelectionPlugin: TAreaSelectionPlugin;
  i: Integer;
begin
  if not SaveDialog.Execute then Exit;

  try
    LStringList := TStringList.Create;
    for i := 0 to AreasList.Count-1 do
    begin
      LStringList.Add(_SectionStr);
      LAreaSelectionPlugin := AreasList.Items.Objects[i] as TAreaSelectionPlugin;
      LStringList.Add('left=' + LAreaSelectionPlugin.SelectedArea.West.ToString);
      LStringList.Add('top=' + LAreaSelectionPlugin.SelectedArea.North.ToString);
      LStringList.Add('right=' + LAreaSelectionPlugin.SelectedArea.East.ToString);
      LStringList.Add('bottom=' + LAreaSelectionPlugin.SelectedArea.South.ToString);
      LStringList.Add('');
    end;
    LStringList.SaveToFile(SaveDialog.FileName);
  finally
    if Assigned(LStringList) then FreeAndNil(LStringList);
  end;
end;

procedure TfMain.actAreasAddExecute(Sender: TObject);
var
  LAreaSelectPlugin: TAreaSelectionPlugin;
begin
  LAreaSelectPlugin := TAreaSelectionPlugin.Create(MvPluginManager);
  LAreaSelectPlugin.MapView := MapView;
  LAreaSelectPlugin.OnSelectedAreaBeginChange := @OnSelectedAreaBeginChange;
  Inc(FPluginCount);
  AreasList.AddItem(FPluginCount.ToString, LAreaSelectPlugin);
end;

procedure TfMain.actAreasDeleteExecute(Sender: TObject);
var
  LMvCustomPlugin: TMvCustomPlugin;
begin
  if AreasList.ItemIndex = FPrevAreaIndex.Value then
    FPrevAreaIndex.Clear;

  LMvCustomPlugin := AreasList.Items.Objects[AreasList.ItemIndex] as TMvCustomPlugin;
  MvPluginManager.PluginList.Delete(MvPluginManager.PluginList.IndexOf(LMvCustomPlugin));
  AreasList.Items.Delete(AreasList.ItemIndex);

  MapView.Refresh;
end;

procedure TfMain.AreasListClick(Sender: TObject);
begin
  SetEnableAreaConrols;
end;

procedure TfMain.AreasListSelectionChange(Sender: TObject; User: boolean);
var
  LAreaSelectPlugin: TAreaSelectionPlugin;
begin
  if AreasList.ItemIndex = -1 then Exit;

  if FPrevAreaIndex.HasValue then
  begin
    LAreaSelectPlugin := AreasList.Items.Objects[FPrevAreaIndex.Value] as TAreaSelectionPlugin;
    LAreaSelectPlugin.Pen.Color := clBlue;
  end;

  LAreaSelectPlugin := AreasList.Items.Objects[AreasList.ItemIndex] as TAreaSelectionPlugin;
  LAreaSelectPlugin.Pen.Color := clRed;
  FPrevAreaIndex := AreasList.ItemIndex;
end;

procedure TfMain.btnAddAreaClick(Sender: TObject);
var
  LAreaSelectPlugin: TAreaSelectionPlugin;
begin
  LAreaSelectPlugin := TAreaSelectionPlugin.Create(MvPluginManager);
  LAreaSelectPlugin.MapView := MapView;
  Inc(FPluginCount);
  AreasList.AddItem(FPluginCount.ToString, LAreaSelectPlugin);

  SetEnableAreaConrols;
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

procedure TfMain.btnDeleteAreaClick(Sender: TObject);
var
  LMvCustomPlugin: TMvCustomPlugin;
begin
  if AreasList.ItemIndex = FPrevAreaIndex.Value then
    FPrevAreaIndex.Clear;

  LMvCustomPlugin := AreasList.Items.Objects[AreasList.ItemIndex] as TMvCustomPlugin;
  MvPluginManager.PluginList.Delete(MvPluginManager.PluginList.IndexOf(LMvCustomPlugin));
  AreasList.Items.Delete(AreasList.ItemIndex);

  MapView.Refresh;
  SetEnableAreaConrols;
end;

procedure TfMain.btnDeleteLayerClick(Sender: TObject);
begin
  MapView.Layers.Delete(LayersList.ItemIndex);
  LayersList.Items.Delete(LayersList.ItemIndex);
  btnDeleteLayer.Enabled := (LayersList.ItemIndex <> -1) and (LayersList.Count > 0);
  LayersUpDown.Enabled := btnDeleteLayer.Enabled;
end;

procedure TfMain.btnEditAreaClick(Sender: TObject);
var
  LMvCustomPlugin: TMvCustomPlugin;
begin
  if AreasList.ItemIndex = FPrevAreaIndex.Value then
    FPrevAreaIndex.Clear;

  LMvCustomPlugin := AreasList.Items.Objects[AreasList.ItemIndex] as TMvCustomPlugin;
  MvPluginManager.PluginList.Delete(MvPluginManager.PluginList.IndexOf(LMvCustomPlugin));
  AreasList.Items.Delete(AreasList.ItemIndex);

  MapView.Refresh;
  SetEnableAreaConrols;
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

procedure TfMain.chkCacheChange(Sender: TObject);
begin
  MapView.Engine.ClearCache;
  if chkCache.Checked then
  begin
    MapView.DownloadEngine := MVDECache;
    MapView.CacheLocation := clCustom;
    MapView.CacheOnDisk := True;
  end
  else
  begin
    MapView.DownloadEngine := MVDEFPC;
    MapView.CacheOnDisk := False;
  end;
  MapView.Invalidate;
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

  if (MapView.GPSItems.IndexOf(FirstCoordinate) = -1) and (MapView.GPSItems.IndexOf(SecondCoordinate) = -1) then
  begin
    SetLength(SelectedCoordinates, 0);
    FirstCoordinate  := TGpsPoint.Create(StrToFloat(AreaCoordFirstLongitude.Text), StrToFloat(AreaCoordFirstLatitude.Text));
    SecondCoordinate := TGpsPoint.Create(StrToFloat(AreaCoordSecondLongitude.Text), StrToFloat(AreaCoordSecondLatitude.Text));
    CoordChange(nil);
    ShowCoordinates := chkShowCoordinates.Checked;
    FirstCoordinate.Visible := ShowCoordinates;
    SecondCoordinate.Visible := ShowCoordinates;
    MapView.GPSItems.Add(FirstCoordinate, _CLICKED_POINTS_);
    MapView.GPSItems.Add(SecondCoordinate, _CLICKED_POINTS_);
  end;


  MapView.GetMapProviders(ProviderVariationsMap.Items);
  MapView.GetMapProviders(ProviderVariationsTiles.Items);
  ReloadLayersList;

  // This is necessary so that the slider does not run away to the side.
  // The problem is in the component, but at the moment it's like a temporary solution.
  ZoomCAreaRange.MinPosition := 6;
  ZoomCAreaRange.MaxPosition := 13;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  BMP_FC := TPicture.Create; BMP_FC.LoadFromFile('img' + PathDelim + 'first_coordinate.png');
  BMP_SC := TPicture.Create; BMP_SC.LoadFromFile('img' + PathDelim + 'second_coordinate.png');
  FPluginCount := 0;
  FPrevAreaIndex.Clear;
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

procedure TfMain.panConcreteZoneResize(Sender: TObject);
begin
  AreaCoordFirstLatitude.Width := Trunc(panConcreteZone.Width / 2) - 3;
  AreaCoordSecondLatitude.Width := Trunc(panConcreteZone.Width / 2) - 2;
end;

procedure TfMain.groupCoordinatesResize(Sender: TObject);
begin
  CoordFirstLatitude.Width := Trunc(FullyOrPartially.Width / 2) - 3;
  CoordSecondLatitude.Width := Trunc(FullyOrPartially.Width / 2) - 2;
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
  i: Integer;
begin
  LPoint.X := X; LPoint.Y := Y;
  LRealPoint := MapView.Engine.ScreenToLatLon(LPoint);
  lblDebugLat.Caption := 'Lat: ' + Format('%13.10f', [LRealPoint.Lat]);
  lblDebugLon.Caption := 'Lon: ' + Format('%14.10f', [LRealPoint.Lon]);

  lblDebugObjects.Caption := 'GpsObjs: ' + MapView.GPSItems.Count.ToString;

  SelectedCoordinates := MapView.ObjsAtScreenPt(X, Y, 10);

  lblDebugObjects.Caption := lblDebugObjects.Caption + ' CursorObjs: ' + (Length(SelectedCoordinates)).ToString;
  if Length(SelectedCoordinates) > 0 then
  begin
    lblDebugObjects.Caption := lblDebugObjects.Caption + ' = ';
    for i := 0 to Length(SelectedCoordinates)-1 do
    begin
      if SelectedCoordinates[i] = FirstCoordinate then
        lblDebugObjects.Caption := lblDebugObjects.Caption + 'FirstCoordinate '
      else
      if SelectedCoordinates[i] = SecondCoordinate then
        lblDebugObjects.Caption := lblDebugObjects.Caption + 'SecondCoordinate'
    end;
  end;
end;

procedure TfMain.MapViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Length(SelectedCoordinates) > 0 then
  if (SelectedCoordinates[0] = FirstCoordinate) or (SelectedCoordinates[0] = SecondCoordinate) then
     fCoordinatesHelp.ShowModal;
end;

procedure TfMain.MapViewZoomChange(Sender: TObject);
begin
  lblDebugZoom.Caption := 'Zoom: ' + MapView.Zoom.ToString;
end;

procedure TfMain.MapViewZoomChanging(Sender: TObject; NewZoom: Integer;
  var Allow: Boolean);
begin


  if chkConcreteZone.Checked then
    Allow := (NewZoom >= ZoomCAreaRange.MinPosition) and (NewZoom <= ZoomCAreaRange.MaxPosition)
  else
    Allow := True;
end;

procedure TfMain.miCoordinatesHelpClick(Sender: TObject);
begin
  fCoordinatesHelp.ShowModal;
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

procedure TfMain.PopupMapViewPopup(Sender: TObject);
begin
  if Length(SelectedCoordinates) > 0 then
    miCoordinatesHelp.Enabled := True
  else
    miCoordinatesHelp.Enabled := False;
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

procedure TfMain.ProviderVariationsMapSelect(Sender: TObject);
begin
  MapView.Active := True;
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

procedure TfMain.OnSelectedAreaBeginChange(Sender: TObject);
var
  LAreaSelectPlugin: TAreaSelectionPlugin;
  LIndex: Integer;
begin
  LAreaSelectPlugin := Sender as TAreaSelectionPlugin;
  LIndex := AreasList.Items.IndexOfObject(LAreaSelectPlugin);
  AreasList.ItemIndex := LIndex;
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

procedure TfMain.SetEnableAreaConrols;
begin
  btnDeleteArea.Enabled := (AreasList.ItemIndex <> -1);
  AreasUpDown.Enabled := btnDeleteArea.Enabled and (AreasList.Count > 1);
  btnExportAreas.Enabled := AreasList.Count > 0;
end;

end.

