unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, ActnList, EditBtn, CheckLst, Buttons, Menus, nullable,
  indSliders, LazFileUtils, IniFiles, Types,
  // MapView
  mvMapViewer, mvDLECache, mvEngine, mvTypes, mvDE_BGRA, mvDrawingEngine,
  mvGpsObj, mvDLEFpc, mvPluginCommon, mvAreaSelectionPlugin, mvMapProvider,
  // Dialogs
  GUI.Dialogs.AddLayers, GUI.Dialogs.EditAreaName;

type

  TNullableInt = specialize TNullable<Integer>;

  { TfMain }

  TfMain = class(TForm)
    actAreasAdd: TAction;
    actAreasDelete: TAction;
    actAreasEditName: TAction;
    actAreasImport: TAction;
    actAreasExport: TAction;
    Action1: TAction;
    ActionsAreas: TActionList;
    ActionList: TActionList;
    btnAddArea: TSpeedButton;
    btnDeleteLayer: TSpeedButton;
    btnDeleteArea: TSpeedButton;
    btnImportAreas: TSpeedButton;
    chkCache: TCheckBox;
    AreasUpDown: TUpDown;
    ImagesCommon: TImageList;
    lblAreas: TLabel;
    lblProviderMap: TLabel;
    lblCache: TLabel;
    lblLayers: TLabel;
    LayersList: TCheckListBox;
    AreasList: TListBox;
    MapView: TMapView;
    MvBGRADrawingEngine: TMvBGRADrawingEngine;
    MVDEFPC: TMVDEFPC;
    MvPluginManager: TMvPluginManager;
    OpenDialog: TOpenDialog;
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
    LayersUpDown: TUpDown;
    btnAddLayer: TSpeedButton;
    btnExportAreas: TSpeedButton;
    SaveDialog: TSaveDialog;
    btnEditArea: TSpeedButton;
    DirectoryCache: TDirectoryEdit;
    groupOther: TGroupBox;
    MVDECache: TMVDECache;
    panMV: TPanel;
    ProviderVariationsMap: TComboBox;
    mvScrollBox: TScrollBox;
    mvSsettings: TSplitter;
    procedure actAreasAddExecute(Sender: TObject);
    procedure actAreasDeleteExecute(Sender: TObject);
    procedure actAreasEditNameExecute(Sender: TObject);
    procedure actAreasExportExecute(Sender: TObject);
    procedure actAreasImportExecute(Sender: TObject);
    procedure actEditAreaNameExecute(Sender: TObject);
    procedure ActionsAreasUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure AreasListDblClick(Sender: TObject);
    procedure AreasListSelectionChange(Sender: TObject; User: boolean);
    procedure AreasUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure btnAddLayerClick(Sender: TObject);
    procedure btnDeleteLayerClick(Sender: TObject);
    procedure chkCacheChange(Sender: TObject);
    procedure DirectoryCacheChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LayersListClick(Sender: TObject);
    procedure LayersListClickCheck(Sender: TObject);
    procedure LayersListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LayersListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure MapViewCenterMoving(Sender: TObject; var NewCenter: TRealPoint;
      var Allow: Boolean);
    procedure MapViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MapViewZoomChange(Sender: TObject);
    procedure ProviderVariationsMapChange(Sender: TObject);
    procedure LayersUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure ProviderVariationsMapSelect(Sender: TObject);
    procedure OnSelectedAreaBeginChange(Sender: TObject);
  strict private
    SelectedCoordinates: TGPSObjarray;
    FConcreteArea: TRealArea;
    FPluginCount: Integer;
    FPrevAreaIndex: TNullableInt;
    procedure ReloadLayersList;
    procedure SetEnableAreaConrols;
  end;

const
  { MapView groups }
  _CLICKED_POINTS_ = 10;

  { Image indexes of buttons }
  ImgIndAdd         = 0;
  ImgIndRemove      = 1;
  ImgIndEdit        = 2;
  ImgIndEditOff     = 3;
  ImgIndFileOpen    = 4;
  ImgIndFileSave    = 5;
  ImgIndFileSaveOff = 6;
  ImgIndFolderOpen  = 7;
  ImgIndApply       = 8;
  ImgIndClose       = 9;

var
  fMain: TfMain;

implementation

{$R *.lfm}

{ TfMain }

procedure TfMain.AreasListDblClick(Sender: TObject);
begin
  actAreasEditName.Execute;
end;

procedure TfMain.ActionsAreasUpdate(AAction: TBasicAction; var Handled: Boolean);
var
  IsItemSelected: Boolean;
begin
  IsItemSelected := (AreasList.ItemIndex <> -1);

  actAreasDelete.Enabled   := IsItemSelected;
  actAreasEditName.Enabled := IsItemSelected;
  AreasUpDown.Enabled      := IsItemSelected and (AreasList.Count > 1);
  actAreasExport.Enabled   := AreasList.Count > 0;

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
  LNameArray: array of String;
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
        SetLength(LRealAreaArray, LCount);
        SetLength(LNameArray    , LCount);
        LAllValuesExists :=  LIniFile.ValueExists(_SectionStr, 'left')
                         and LIniFile.ValueExists(_SectionStr, 'top')
                         and LIniFile.ValueExists(_SectionStr, 'right')
                         and LIniFile.ValueExists(_SectionStr, 'bottom');
        if not LAllValuesExists then
          raise Exception.Create(_SectionStr + ' number ' + LCount.ToString + ' has not all values');

        if LIniFile.ValueExists(_SectionStr, 'name') then
          LStringList.Add(LIniFile.ReadString(_SectionStr, 'name', ''))
        else
          LStringList.Add(String.Empty);
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
          if LStringList[i].IsEmpty then
            LAreaSelectionPlugin.Caption := 'Area' + i.ToString
          else
            LAreaSelectionPlugin.Caption := LStringList[i];
          AreasList.AddItem(LAreaSelectionPlugin.Caption, LAreaSelectionPlugin);
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
      LStringList.Add('name=' + LAreaSelectionPlugin.Caption);
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
  LAreaSelectPlugin.Caption := 'Area' + FPluginCount.ToString;
  AreasList.AddItem(LAreaSelectPlugin.Caption, LAreaSelectPlugin);
end;

procedure TfMain.actAreasDeleteExecute(Sender: TObject);
var
  LMvCustomPlugin: TMvCustomPlugin;
  i: Integer;
begin
  if AreasList.ItemIndex = FPrevAreaIndex.Value then
    FPrevAreaIndex.Clear;

  for i := AreasList.Count - 1 downto 0 do
  begin
    if not AreasList.Selected[i] then Continue;

    LMvCustomPlugin := AreasList.Items.Objects[i] as TMvCustomPlugin;
    MvPluginManager.PluginList.Delete(MvPluginManager.PluginList.IndexOf(LMvCustomPlugin));
    AreasList.Items.Delete(i);
  end;

  MapView.Refresh;
end;

procedure TfMain.actAreasEditNameExecute(Sender: TObject);
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

procedure TfMain.btnAddLayerClick(Sender: TObject);
begin
  fAddLayers := TfAddLayers.CreateEx(MapView);
  if fAddLayers.ShowModal = mrOK then
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
var
  LImages: TImageList;
begin
  FormatSettings.DecimalSeparator := '.';
  FPluginCount := 0;
  FPrevAreaIndex.Clear;
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
end;

procedure TfMain.MapViewZoomChange(Sender: TObject);
begin
  lblDebugZoom.Caption := 'Zoom: ' + MapView.Zoom.ToString;
end;

procedure TfMain.ProviderVariationsMapChange(Sender: TObject);
begin
  with ProviderVariationsMap do
    MapView.MapProvider := Items[ItemIndex];
  MapView.OnMouseMove := @MapViewMouseMove;
  MapView.Active := True;
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
var
  LItemSelected: Boolean;
begin
  LItemSelected := (AreasList.ItemIndex <> -1);

  btnDeleteArea.Enabled := LItemSelected;
  btnEditArea.Enabled   := LItemSelected;
  AreasUpDown.Enabled := btnDeleteArea.Enabled and (AreasList.Count > 1);
end;

end.

