unit DlgAddLayers;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, mvMapViewer, mvDrawingEngine;

type

  { TfAddLayers }

  TfAddLayers = class(TForm)
    ProvidersList: TListBox;
    panControl: TPanel;
    btnAdd: TSpeedButton;
    btnCancel: TSpeedButton;
    procedure btnAddClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    MapView: TMapView;
  public
    constructor CreateEx(AMapView: TMapView);
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

constructor TfAddLayers.CreateEx(AMapView: TMapView);
begin
  inherited Create(Application);

  MapView := AMapView;
  MapView.GetMapProviders(ProvidersList.Items);
end;

end.

