unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, ActnList, EditBtn, mvMapViewer,
  mvDLECache, indSliders;

type

  { TfMain }

  TfMain = class(TForm)
    actStartStop: TAction;
    ActionList: TActionList;
    btnStartDownload: TButton;
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
    groupCoordinates: TGroupBox;
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
    procedure MapViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MapViewZoomChange(Sender: TObject);
    procedure ProviderVariationsMapChange(Sender: TObject);
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
  mvEngine, mvTypes;

{ TfMain }

procedure TfMain.actStartStopExecute(Sender: TObject);
begin
  MapView.Active := not MapView.Active;

  if MapView.Active then
    actStartStop.Caption := 'Stop'
  else
    actStartStop.Caption := 'Start';
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

procedure TfMain.ProviderVariationsMapChange(Sender: TObject);
begin
  with ProviderVariationsMap do
    MapView.MapProvider := Items[ItemIndex];
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

