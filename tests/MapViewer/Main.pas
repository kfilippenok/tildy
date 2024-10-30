unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, ActnList, ValEdit, ColorBox, EditBtn, mvMapViewer,
  mvDLECache;

type

  { TfMain }

  TfMain = class(TForm)
    actStartStop: TAction;
    ActionList: TActionList;
    btnStartDownload: TButton;
    chkProviderName: TCheckBox;
    chkActive: TCheckBox;
    chkCyclic: TCheckBox;
    chkDoubleBuff: TCheckBox;
    chkDebugTiles: TCheckBox;
    chkPreviewTiles: TCheckBox;
    chkUseThreads: TCheckBox;
    chkZoomToCursor: TCheckBox;
    DirectoryCache: TDirectoryEdit;
    groupOther: TGroupBox;
    groupCache: TGroupBox;
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
    Splitter: TSplitter;
    Splitter1: TSplitter;
    procedure actStartStopExecute(Sender: TObject);
    procedure chkActiveChange(Sender: TObject);
    procedure chkCyclicChange(Sender: TObject);
    procedure chkDebugTilesChange(Sender: TObject);
    procedure chkDoubleBuffChange(Sender: TObject);
    procedure chkPreviewTilesChange(Sender: TObject);
    procedure chkUseThreadsChange(Sender: TObject);
    procedure chkZoomToCursorChange(Sender: TObject);
    procedure DirectoryCacheChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ProviderVariationsMapChange(Sender: TObject);
  private

  public

  end;

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

procedure TfMain.chkPreviewTilesChange(Sender: TObject);
begin
  MapView.DrawPreviewTiles := chkPreviewTiles.Checked;
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
end;

procedure TfMain.ProviderVariationsMapChange(Sender: TObject);
begin
  with ProviderVariationsMap do
    MapView.MapProvider := Items[ItemIndex];
end;

end.

