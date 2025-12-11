unit Tildy.GUI.Dialogs.AddLayers;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons,
  // MapView
  mvMapViewer, mvDrawingEngine, mvMapProvider;

type

  { TfAddLayers }

  TfAddLayers = class(TForm)
    ProvidersList: TListBox;
    panControl: TPanel;
    btnAdd: TSpeedButton;
    btnCancel: TSpeedButton;
    procedure btnAddClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure ProvidersListDblClick(Sender: TObject);
    procedure ProvidersListSelectionChange(Sender: TObject; User: boolean);
  private
    MapView: TMapView;
  public
    constructor Create(AMapView: TMapView); reintroduce;
  end;

var
  fAddLayers: TfAddLayers;

implementation

{$R *.lfm}

{ TfAddLayers }

procedure TfAddLayers.btnCancelClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TfAddLayers.ProvidersListDblClick(Sender: TObject);
begin
  if (ProvidersList.ItemIndex = -1) then Exit;

  btnAdd.Click;
end;

procedure TfAddLayers.ProvidersListSelectionChange(Sender: TObject;
  User: boolean);
begin
  btnAdd.Enabled := True;
end;

procedure TfAddLayers.btnAddClick(Sender: TObject);
var
  i: Integer;
  LMapLayer: TMapLayer;
begin
  for i := 0 to ProvidersList.Count-1 do
  begin
    if ProvidersList.Selected[i] then
    begin
      LMapLayer := MapView.Layers.Add as TMapLayer;
      LMapLayer.MapProvider := ProvidersList.Items[ProvidersList.ItemIndex];
      LMapLayer.DrawMode := idmUseSourceAlpha;
      LMapLayer.UseThreads := MapView.UseThreads;
      LMapLayer.Visible := True;
    end;
  end;
  Self.ModalResult := mrOK;
end;

constructor TfAddLayers.Create(AMapView: TMapView);
begin
  inherited Create(Application);

  MapView := AMapView;
  MapProvidersToSortedStrings(ProvidersList.Items);
end;

end.

