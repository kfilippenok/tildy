unit Tildy.GUI.Plugins.MapViewer.DebugOfPosition;

{$mode ObjFPC}{$H+}
{$scopedenums on}
{$modeswitch advancedrecords}

interface

uses
  Classes,
  SysUtils,
  // MapViewer
  mvMapViewer,
  mvPluginCommon,
  mvTypes;

type

  TDebugOfPositionEvent = procedure (Sender: TObject; AMapCoordinate: TRealPoint) of object;

  { TDebugOfPositionPlugin }

  TDebugOfPositionPlugin = class(TMvPlugin)
  private
    FOnDebugOfPosition: TDebugOfPositionEvent;
    procedure SetOnDebugOfPosition(AValue: TDebugOfPositionEvent);
  protected
    procedure MouseMove(AMapView: TMapView; AShift: TShiftState; X, Y: Integer;
      var Handled: Boolean); override;
  published
    property OnDebugOfPosition: TDebugOfPositionEvent read FOnDebugOfPosition write SetOnDebugOfPosition;
  end;

implementation

{ TDebugOfPositionPlugin }

procedure TDebugOfPositionPlugin.SetOnDebugOfPosition(
  AValue: TDebugOfPositionEvent);
begin
  if FOnDebugOfPosition = AValue then Exit;
  FOnDebugOfPosition := AValue;
end;

procedure TDebugOfPositionPlugin.MouseMove(AMapView: TMapView;
  AShift: TShiftState; X, Y: Integer; var Handled: Boolean);
begin
  inherited MouseMove(AMapView, AShift, X, Y, Handled);
end;

end.

