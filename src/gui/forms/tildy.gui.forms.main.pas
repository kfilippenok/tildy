unit Tildy.GUI.Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, ActnList, EditBtn, CheckLst, Buttons, Menus, nullable,
  LazFileUtils, IniFiles, Types, System.UITypes, LCLType,
  // Translate
  LCLTranslator, Tildy.GUI.i18n.Runtime, Tildy.GUI.i18n.StrConsts,
  // MapView
  mvMapViewer, mvDLECache, mvEngine, mvTypes, mvDE_BGRA,
  mvDLEFpc, mvPluginCommon, mvAreaSelectionPlugin, mvPlugins,
  mvMapProvider,
  // Brand
  Tildy.GUI.Brand.Colors,
  // Dialogs
  Tildy.GUI.Dialogs.AddLayers, Tildy.GUI.Dialogs.EditAreaName,
  // Plugins
  Tildy.GUI.Plugins.MapViewer.AreaEditor;

type

  { TfMain }

  TfMain = class(TForm, ILocalizableForm)
    actAreasAdd: TAction;
    actAreasDelete: TAction;
    actAreasEditName: TAction;
    actAreasImport: TAction;
    actAreasExport: TAction;
    actAreasUp: TAction;
    actAreasDown: TAction;
    actTileInfo: TAction;
    actZoomIn: TAction;
    actZoomOut: TAction;
    actSelectAll: TAction;
    ActionsCommon: TActionList;
    actLayersNew: TAction;
    actLayersDelete: TAction;
    actLayersDown: TAction;
    actLayersUp: TAction;
    ActionsAreas: TActionList;
    ActionsLayers: TActionList;
    btnNewArea: TSpeedButton;
    btnDownArea: TSpeedButton;
    btnLayersDelete: TSpeedButton;
    btnDeleteArea: TSpeedButton;
    btnImportAreas: TSpeedButton;
    btnLayersDown: TSpeedButton;
    btnZoomOut: TSpeedButton;
    chkCache: TCheckBox;
    ImagesPrimary: TImageList;
    ImagesDark: TImageList;
    lblAreas: TLabel;
    lblProviderMap: TLabel;
    lblCache: TLabel;
    lblLayers: TLabel;
    LayersList: TCheckListBox;
    AreasList: TListBox;
    MainMenu: TMainMenu;
    MapView: TMapView;
    miDeleteArea: TMenuItem;
    miRenameArea: TMenuItem;
    miNewArea: TMenuItem;
    miLangaugeEn: TMenuItem;
    miLangaugeRu: TMenuItem;
    miLanguage: TMenuItem;
    MvBGRADrawingEngine: TMvBGRADrawingEngine;
    MVDEFPC: TMVDEFPC;
    MvPluginManager: TMvPluginManager;
    btnUpArea: TSpeedButton;
    btnLayersUp: TSpeedButton;
    btnZoomIn: TSpeedButton;
    btnTileInfo: TSpeedButton;
    TileInfoPlugin: TTileInfoPlugin;
    dlgOpen: TOpenDialog;
    panAreas: TPanel;
    panOffsetBottom: TPanel;
    panProviderMap: TPanel;
    panCache: TPanel;
    panLayers: TPanel;
    lblDebugZoom: TLabel;
    lblDebugLon: TLabel;
    lblDebugLat: TLabel;
    panDebug: TPanel;
    PopupMapView: TPopupMenu;
    btnLayersAdd: TSpeedButton;
    btnExportAreas: TSpeedButton;
    dlgSave: TSaveDialog;
    btnRenameArea: TSpeedButton;
    DirectoryCache: TDirectoryEdit;
    MVDECache: TMVDECache;
    panMV: TPanel;
    ProviderVariationsMap: TComboBox;
    mvScrollBox: TScrollBox;
    mvSsettings: TSplitter;
    procedure actAreasAddExecute(Sender: TObject);
    procedure actAreasDeleteExecute(Sender: TObject);
    procedure actAreasDownExecute(Sender: TObject);
    procedure actAreasEditNameExecute(Sender: TObject);
    procedure actAreasExportExecute(Sender: TObject);
    procedure actAreasImportExecute(Sender: TObject);
    procedure actAreasUpExecute(Sender: TObject);
    procedure actEditAreaNameExecute(Sender: TObject);
    procedure ActionsAreasUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure ActionsCommonUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure ActionsLayersUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure actLayersNewExecute(Sender: TObject);
    procedure actLayersDeleteExecute(Sender: TObject);
    procedure actLayersDownExecute(Sender: TObject);
    procedure actLayersUpExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actTileInfoExecute(Sender: TObject);
    procedure actZoomInExecute(Sender: TObject);
    procedure actZoomOutExecute(Sender: TObject);
    procedure AreasListDblClick(Sender: TObject);
    procedure AreasListSelectionChange(Sender: TObject; User: boolean);
    procedure AreasUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure chkCacheChange(Sender: TObject);
    procedure DirectoryCacheChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LayersListClickCheck(Sender: TObject);
    procedure LayersListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LayersListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure MapViewCenterMoving(Sender: TObject; var NewCenter: TRealPoint;
      var Allow: Boolean);
    procedure MapViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure MapViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MapViewResize(Sender: TObject);
    procedure MapViewZoomChange(Sender: TObject);
    procedure miLangaugeEnClick(Sender: TObject);
    procedure miLangaugeRuClick(Sender: TObject);
    procedure PopupMapViewPopup(Sender: TObject);
    procedure ProviderVariationsMapChange(Sender: TObject);
    procedure LayersUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure ProviderVariationsMapSelect(Sender: TObject);
    procedure AreaEditorSelectionChanged(Sender: TObject; const OldArea, NewArea: TMapLabeledArea);
  strict private
    FAreaEditor: TAreaEditorPlugin;
    FShowTileInfo: Boolean;
    procedure SetShowTileInfo(AValue: Boolean);
    procedure ReloadLayersList;
    procedure DetectAndApplyTheme;
    procedure UpdateDebugZoom;
    procedure UpdateDebugCursorCoordinate(const X, Y: Integer);
    property ShowTileInfo: Boolean read FShowTileInfo write SetShowTileInfo;
  public
    procedure TranslationChanged;
  end;

var
  fMain: TfMain;

implementation

{$R *.lfm}

const
  ImgIndTileInfo: array[Boolean] of Integer = (12, 13);

{ TfMain }

procedure TfMain.AreasListDblClick(Sender: TObject);
begin
  actAreasEditName.Execute;
end;

procedure TfMain.ActionsAreasUpdate(AAction: TBasicAction; var Handled: Boolean);
var
  IsItemSelected: Boolean;
begin
  IsItemSelected := (AreasList.ItemIndex <> -1) and AreasList.Selected[AreasList.ItemIndex];

  actAreasDelete.Enabled   := IsItemSelected;
  actAreasEditName.Enabled := IsItemSelected;
  actAreasUp.Enabled       := IsItemSelected and (AreasList.ItemIndex > 0);
  actAreasDown.Enabled     := IsItemSelected and (AreasList.ItemIndex < (AreasList.Count - 1));
  actAreasExport.Enabled   := AreasList.Count > 0;

  Handled := True;
end;

procedure TfMain.ActionsCommonUpdate(AAction: TBasicAction; var Handled: Boolean
  );
begin
  actZoomIn.Enabled  := MapView.Zoom <> MapView.ZoomMax;
  actZoomOut.Enabled := MapView.Zoom <> MapView.ZoomMin;

  Handled := True;
end;

procedure TfMain.ActionsLayersUpdate(AAction: TBasicAction; var Handled: Boolean
  );
var
  IsItemSelected: Boolean;
begin
  IsItemSelected := (LayersList.ItemIndex <> -1) and LayersList.Selected[LayersList.ItemIndex];

  actLayersDelete.Enabled := IsItemSelected;
  actLayersUp.Enabled     := IsItemSelected and (LayersList.ItemIndex > 0);
  actLayersDown.Enabled   := IsItemSelected and (LayersList.ItemIndex < (LayersList.Count - 1));

  Handled := True;
end;

procedure TfMain.actLayersNewExecute(Sender: TObject);
begin
  fAddLayers := TfAddLayers.Create(MapView);
  if fAddLayers.ShowModal = mrOK then
  begin
    ReloadLayersList;
  end;
  FreeAndNil(fAddLayers);
end;

procedure TfMain.actLayersDeleteExecute(Sender: TObject);
begin
  MapView.Layers.Delete(LayersList.ItemIndex);
  LayersList.Items.Delete(LayersList.ItemIndex);
end;

procedure TfMain.actLayersDownExecute(Sender: TObject);
var
  NewIndex: Integer;
begin
  if (LayersList.ItemIndex = -1) or (LayersList.ItemIndex = LayersList.Count-1) then Exit;

  NewIndex := LayersList.ItemIndex + 1;
  MapView.Layers.Move(LayersList.ItemIndex, NewIndex);
  LayersList.Items.Move(LayersList.ItemIndex, NewIndex);
  LayersList.Selected[NewIndex] := True;
end;

procedure TfMain.actLayersUpExecute(Sender: TObject);
var
  NewIndex: Integer;
begin
  if LayersList.ItemIndex < 1 then Exit;

  NewIndex := LayersList.ItemIndex - 1;
  MapView.Layers.Move(LayersList.ItemIndex, NewIndex);
  LayersList.Items.Move(LayersList.ItemIndex, NewIndex);
  LayersList.Selected[NewIndex] := True;
end;

procedure TfMain.actSelectAllExecute(Sender: TObject);
begin
  if not AreasList.Focused then Exit;

  AreasList.SelectAll;
end;

procedure TfMain.actTileInfoExecute(Sender: TObject);
begin
  ShowTileInfo := not ShowTileInfo;
end;

procedure TfMain.actZoomInExecute(Sender: TObject);
begin
  MapView.Zoom := MapView.Zoom + 1;
end;

procedure TfMain.actZoomOutExecute(Sender: TObject);
begin
  MapView.Zoom := MapView.Zoom - 1;
end;

procedure TfMain.actAreasImportExecute(Sender: TObject);
const
    _SectionStr = 'Area';
var
  AreaNames: TStringList = nil;
  Section: TStringList = nil;
  IniFile: TMemIniFile = nil;
  MemoryStream: TMemoryStream = nil;
  RealAreaArray: array of TRealArea = nil;
  Area: TMapLabeledArea = nil;
  CountAreas: Integer = 0;
  i: Integer;
  AllValuesExists: Boolean = False;
begin
  if not dlgOpen.Execute then Exit;

  try
    Section := TStringList.Create;
    MemoryStream := TMemoryStream.Create;
    AreaNames := TStringList.Create;
    try
      MemoryStream.LoadFromFile(dlgOpen.FileName);
      IniFile := TMemIniFile.Create(MemoryStream);
      SetLength(RealAreaArray, CountAreas);

      IniFile.ReadSection(_SectionStr, Section);
      while Section.Count > 0 do
      begin
        Inc(CountAreas);
        SetLength(RealAreaArray, CountAreas);
        AllValuesExists :=  IniFile.ValueExists(_SectionStr, 'left')
                         and IniFile.ValueExists(_SectionStr, 'top')
                         and IniFile.ValueExists(_SectionStr, 'right')
                         and IniFile.ValueExists(_SectionStr, 'bottom');
        if not AllValuesExists then
          raise Exception.Create(Format(SAreasErrImport, [dlgOpen.FileName]));

        if IniFile.ValueExists(_SectionStr, 'name') then
          AreaNames.Add(IniFile.ReadString(_SectionStr, 'name', ''))
        else
          AreaNames.Add(String.Empty);
        RealAreaArray[CountAreas-1].TopLeft.Lon := IniFile.ReadFloat(_SectionStr, 'left', 0.0);
        RealAreaArray[CountAreas-1].TopLeft.Lat := IniFile.ReadFloat(_SectionStr, 'top', 0.0);
        RealAreaArray[CountAreas-1].BottomRight.Lon := IniFile.ReadFloat(_SectionStr, 'right', 0.0);
        RealAreaArray[CountAreas-1].BottomRight.Lat := IniFile.ReadFloat(_SectionStr, 'bottom', 0.0);

        IniFile.EraseSection(_SectionStr);
        IniFile.ReadSection(_SectionStr, Section);
      end;

      FAreaEditor.ClearAreas;
      AreasList.Clear;

      if AreaNames.Count > 0 then
      begin
        for i := 0 to AreaNames.Count-1 do
        begin
          Area := FAreaEditor.AddArea;
          Area.Area := RealAreaArray[i];
          if AreaNames[i].IsEmpty then
            Area.Caption := SPluginAreaEditorAreaName + i.ToString
          else
            Area.Caption := AreaNames[i];
          AreasList.AddItem(Area.Caption, nil);
        end;
      end;
    except
      on E: Exception do
        ShowMessage(E.Message);
    end;
  finally
    Section.Free;
    AreaNames.Free;
    IniFile.Free;
    MemoryStream.Free;
  end;
end;

procedure TfMain.actAreasUpExecute(Sender: TObject);
var
  LNewItemIndex: Integer;
begin
  if AreasList.ItemIndex < 1 then Exit;

  LNewItemIndex := AreasList.ItemIndex - 1;
  AreasList.Items.Move(AreasList.ItemIndex, LNewItemIndex);
  AreasList.ItemIndex := LNewItemIndex;
  AreasList.Selected[LNewItemIndex] := True;
  FAreaEditor.MovePrev(AreasList.Items.Objects[LNewItemIndex] as TMapLabeledArea);
end;

procedure TfMain.actEditAreaNameExecute(Sender: TObject);
begin
  fEditAreaName.AreaName := AreasList.Items[AreasList.ItemIndex];
  fEditAreaName.ShowModal;
  if fEditAreaName.ModalResult = mrOK then
  begin
    AreasList.Items[AreasList.ItemIndex] := fEditAreaName.AreaName;
    try
      (AreasList.Items.Objects[AreasList.ItemIndex] as TAreaSelectionPlugin).Caption := fEditAreaName.AreaName;
    finally end;
  end;
end;

procedure TfMain.actAreasExportExecute(Sender: TObject);
const
  _SectionStr = '[Area]';
var
  ConfigStrings: TStringList = nil;
  Area: TMapLabeledArea;
  i: Integer;
begin
  if not dlgSave.Execute then Exit;

  try
    ConfigStrings := TStringList.Create;
    for i := 0 to FAreaEditor.CountAreas - 1 do
    begin
      ConfigStrings.Add(_SectionStr);
      Area := FAreaEditor[i];
      ConfigStrings.Add('name=' + Area.Caption);
      ConfigStrings.Add('left=' + Area.West.ToString);
      ConfigStrings.Add('top=' + Area.North.ToString);
      ConfigStrings.Add('right=' + Area.East.ToString);
      ConfigStrings.Add('bottom=' + Area.South.ToString);
      ConfigStrings.Add('');
    end;
    ConfigStrings.SaveToFile(dlgSave.FileName);
  finally
    ConfigStrings.Free;
  end;
end;

procedure TfMain.actAreasAddExecute(Sender: TObject);
var
  Area: TMapLabeledArea;
begin
  Area := FAreaEditor.AddArea;
  AreasList.AddItem(Area.Caption, Area);
end;

procedure TfMain.actAreasDeleteExecute(Sender: TObject);
var
  i: Integer;
begin

  for i := AreasList.Count - 1 downto 0 do
  begin
    if not AreasList.Selected[i] then Continue;

    FAreaEditor.DeleteArea(i);
    AreasList.Items.Delete(i);
  end;

  MapView.Refresh;
end;

procedure TfMain.actAreasDownExecute(Sender: TObject);
var
  LNewItemIndex: Integer;
begin
  if AreasList.ItemIndex >= (AreasList.Count - 1) then Exit;

  LNewItemIndex := AreasList.ItemIndex + 1;
  AreasList.Items.Move(AreasList.ItemIndex, LNewItemIndex);
  AreasList.ItemIndex := LNewItemIndex;
  AreasList.Selected[LNewItemIndex] := True;
  FAreaEditor.MoveNext(AreasList.Items.Objects[LNewItemIndex] as TMapLabeledArea);
end;

procedure TfMain.actAreasEditNameExecute(Sender: TObject);
begin
  fEditAreaName := TfEditAreaName.Create(MapView);
  fEditAreaName.AreaName := AreasList.Items[AreasList.ItemIndex];
  if fEditAreaName.ShowModal = mrOK then
  begin
    AreasList.Items[AreasList.ItemIndex] := fEditAreaName.AreaName;
    try
      FAreaEditor.Areas[AreasList.ItemIndex].Caption := fEditAreaName.AreaName;
    finally end;
  end;
  FreeAndNil(fEditAreaName);
end;

procedure TfMain.AreasListSelectionChange(Sender: TObject; User: boolean);
begin
  if AreasList.ItemIndex = -1 then Exit;

  FAreaEditor.SelectedArea := FAreaEditor[AreasList.ItemIndex];
end;

procedure TfMain.AreasUpDownClick(Sender: TObject; Button: TUDBtnType);

  procedure _MoveNextAreaSelectionPlugin(AAreaSelectionPlugin: TAreaSelectionPlugin);
  var
    LCurPluginPos: Integer;
    i: Integer;
    NextAreaSelectionPlugin: TAreaSelectionPlugin = nil;
  begin
    LCurPluginPos := MvPluginManager.PluginList.IndexOf(AAreaSelectionPlugin);
    for i := LCurPluginPos to MvPluginManager.PluginList.Count - 1 do
    if MvPluginManager.PluginList[i] is TAreaSelectionPlugin then
    begin
      NextAreaSelectionPlugin := MvPluginManager.PluginList[i] as TAreaSelectionPlugin;
      break;
    end;
    if not Assigned(NextAreaSelectionPlugin) then Exit;
    MvPluginManager.PluginList.Move(LCurPluginPos, i);
  end;

  procedure _MovePrevAreaSelectionPlugin(AAreaSelectionPlugin: TAreaSelectionPlugin);
  var
    LCurPluginPos: Integer;
    i: Integer;
    PrevAreaSelectionPlugin: TAreaSelectionPlugin = nil;
  begin
    LCurPluginPos := MvPluginManager.PluginList.IndexOf(AAreaSelectionPlugin);
    for i := LCurPluginPos downto 0 do
    if MvPluginManager.PluginList[i] is TAreaSelectionPlugin then
    begin
      PrevAreaSelectionPlugin := MvPluginManager.PluginList[i] as TAreaSelectionPlugin;
      break;
    end;
    if not Assigned(PrevAreaSelectionPlugin) then Exit;
    MvPluginManager.PluginList.Move(LCurPluginPos, i);
  end;

var
  LNewStringPos: Integer;
  LAreaSelectionPlugin: TAreaSelectionPlugin;
begin
  if AreasList.ItemIndex = -1 then Exit;

  case Button of
    btPrev:
      begin
        if (AreasList.ItemIndex = (AreasList.Count - 1)) then Exit;
        { Move plugin first }
        LAreaSelectionPlugin := AreasList.Items.Objects[AreasList.ItemIndex] as TAreaSelectionPlugin;
        _MoveNextAreaSelectionPlugin(LAreaSelectionPlugin);
        { Move item in AreasList then }
        LNewStringPos := AreasList.ItemIndex + 1;
        AreasList.Items.Move(AreasList.ItemIndex, LNewStringPos);
        AreasList.ItemIndex := LNewStringPos;
      end;
    btNext:
      begin
        if (AreasList.ItemIndex = 0) then Exit;
        { Move plugin first }
        LAreaSelectionPlugin := AreasList.Items.Objects[AreasList.ItemIndex] as TAreaSelectionPlugin;
        _MovePrevAreaSelectionPlugin(LAreaSelectionPlugin);
        { Move item in AreasList then }
        LNewStringPos := AreasList.ItemIndex - 1;
        AreasList.Items.Move(AreasList.ItemIndex, LNewStringPos);
        AreasList.ItemIndex := LNewStringPos;
      end;
  end;
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

procedure TfMain.DirectoryCacheChange(Sender: TObject);
begin
  MapView.CacheLocation := clCustom;
  MapView.CachePath := DirectoryCache.Text + PathDelim;
end;

procedure TfMain.FormActivate(Sender: TObject);
begin
  MapProvidersToSortedStrings(ProviderVariationsMap.Items);
  ReloadLayersList;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  FormatSettings.DecimalSeparator := '.';
  DetectAndApplyTheme;
  ShowTileInfo := False;
  FAreaEditor := TAreaEditorPlugin.Create(MvPluginManager);
  FAreaEditor.MapView := MapView;
  FAreaEditor.Enabled := True;
  FAreaEditor.OnSelectionChanged := @AreaEditorSelectionChanged;

  TranslationChanged;
end;

procedure TfMain.LayersListClickCheck(Sender: TObject);
begin
  (MapView.Layers.Items[LayersList.ItemIndex] as TMapLayer).Visible := LayersList.Checked[LayersList.ItemIndex];
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

procedure TfMain.MapViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
const
  StepPx = 5;
var
  DX, DY: Integer;
begin
  DX := 0;
  DY := 0;

  case Key of
    VK_LEFT:  DX := -StepPx;
    VK_RIGHT: DX :=  StepPx;
    VK_UP:    DY := -StepPx;
    VK_DOWN:  DY :=  StepPx;
  else
    Exit;
  end;

  // Speed-up
  if ssShift in Shift then
  begin
    DX := DX * StepPx;
    DY := DY * StepPx;
  end;

  FAreaEditor.MoveSelectedByPixels(MapView, DX, DY);
end;


procedure TfMain.MapViewMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  UpdateDebugCursorCoordinate(X, Y);
end;

procedure TfMain.MapViewResize(Sender: TObject);
var
  LDistance: Integer;
begin
  LDistance := ScaleX(20, 144); // 144 - my work design time
  btnZoomIn.Anchors  := [];
  btnZoomOut.Anchors := [];
  btnZoomIn.Left  := MapView.Width - btnZoomIn.Width - LDistance;
  btnZoomOut.Left := btnZoomIn.Left;
  btnZoomIn.Top   := Round(MapView.Height / 2) - Round(LDistance / 2) - btnZoomIn.Height;
  btnZoomOut.Top  := btnZoomIn.Top + LDistance + btnZoomOut.Height;
end;

procedure TfMain.MapViewZoomChange(Sender: TObject);
begin
  UpdateDebugZoom;
end;

procedure TfMain.miLangaugeEnClick(Sender: TObject);
begin
  SetLanguage('en');
end;

procedure TfMain.miLangaugeRuClick(Sender: TObject);
begin
  SetLanguage('ru');
end;

procedure TfMain.PopupMapViewPopup(Sender: TObject);
var
  IsAreaSelected: Boolean;
begin
  IsAreaSelected := Assigned(FAreaEditor.SelectedArea);
  miRenameArea.Enabled := IsAreaSelected;
  miDeleteArea.Enabled := IsAreaSelected;
end;

procedure TfMain.ProviderVariationsMapChange(Sender: TObject);
var
  LMinZoom, LMaxZoom: Integer;
begin
  with ProviderVariationsMap do
    MapView.MapProvider := Items[ItemIndex];
  MapProviderByName(MapView.MapProvider).GetZoomInfos(LMinZoom, LMaxZoom);
  MapView.ZoomMin     := LMinZoom;
  MapView.ZoomMax     := LMaxZoom;
  MapView.OnMouseMove := @MapViewMouseMove;
  MapView.Active      := True;
  MapViewZoomChange(Self);
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

procedure TfMain.ProviderVariationsMapSelect(Sender: TObject);
begin
  MapView.Active := True;
end;

procedure TfMain.AreaEditorSelectionChanged(Sender: TObject; const OldArea, NewArea: TMapLabeledArea);
begin
  if not Assigned(NewArea) then
    AreasList.ClearSelection
  else
  begin
    AreasList.ItemIndex := FAreaEditor.IndexOfArea(NewArea);
    AreasList.Selected[AreasList.ItemIndex] := True;
  end;
end;

procedure TfMain.SetShowTileInfo(AValue: Boolean);
begin
  FShowTileInfo := AValue;

  btnTileInfo.ImageIndex := ImgIndTileInfo[FShowTileInfo];
  TileInfoPlugin.Enabled := FShowTileInfo;
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

procedure TfMain.DetectAndApplyTheme;
begin
  if IsDarkTheme then
    ImagesPrimary.Assign(ImagesDark);
end;

procedure TfMain.UpdateDebugZoom;
begin
  lblDebugZoom.Caption := SZoom + ': ' + MapView.Zoom.ToString;
end;

procedure TfMain.UpdateDebugCursorCoordinate(const X, Y: Integer);
var
  LRealPoint: TRealPoint;
  LPoint: TPoint;
  i: Integer;
begin
  LPoint.X := X; LPoint.Y := Y;
  LRealPoint := MapView.Engine.ScreenToLatLon(LPoint);
  lblDebugLat.Caption := SLat + ': ' + Format('%13.10f', [LRealPoint.Lat]);
  lblDebugLon.Caption := SLon + ': ' + Format('%14.10f', [LRealPoint.Lon]);
end;

procedure TfMain.TranslationChanged;
var
  LPoint: TPoint;
begin
  LPoint := MapView.ScreenToClient(Mouse.CursorPos);

  { Menu }

  // Language
  miLanguage.Caption := SMenuLanguage;
  miLanguage.Hint := SHMenuLanguage;

  { Areas }

  // New
  miNewArea.Caption := SAreasAdd;
  miNewArea.Hint := SHAreasAdd;
  btnNewArea.Hint := SHAreasAdd;

  // Rename
  miRenameArea.Caption := SAreasRename;
  miRenameArea.Hint := SHAreasRename;
  btnRenameArea.Hint := SHAreasRename;

  // Delete
  miDeleteArea.Caption := SAreasDelete;
  miDeleteArea.Hint := SHAreasDelete;
  btnDeleteArea.Hint := SHAreasDelete;

  // Import
  btnImportAreas.Caption := SAreasImport;
  btnImportAreas.Hint := SHAreasImport;

  // Export
  btnExportAreas.Caption := SAreasExport;
  btnExportAreas.Hint := SHAreasExport;

  UpdateDebugZoom;
  UpdateDebugCursorCoordinate(LPoint.X, LPoint.Y)
end;

end.

