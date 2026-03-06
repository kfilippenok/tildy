unit Tildy.GUI.Plugins.MapViewer.DebugOfPosition;

{$mode ObjFPC}{$H+}
{$scopedenums on}
{$modeswitch advancedrecords}

interface

uses
  Classes,
  SysUtils,
  CustomTimer,
  // MapViewer
  mvMapViewer,
  mvPluginCommon,
  mvTypes,
  // Utilities
  Tildy.Utilities.Throttle;

type

  TDebugOfPositionEvent = procedure (Sender: TObject; const AMapCoordinate: TRealPoint) of object;

  { TDebugOfPositionPlugin }

  TDebugOfPositionPlugin = class(TMvPlugin)
  private
    FMapCoordinate: TRealPoint;
    FOnDebugOfPosition: TDebugOfPositionEvent;
    FThrottle: IThrottle;
    FThrottleInterval: QWord;
    FUseThrottle: Boolean;
    procedure SetThrottleInterval(const AValue: QWord);
    procedure SetUseThrottle(AValue: Boolean);
  protected
    procedure MouseMove(AMapView: TMapView; AShift: TShiftState; X, Y: Integer;
      var Handled: Boolean); override;
  published
    constructor Create(AOwner: TComponent); override;
    property OnDebugOfPosition: TDebugOfPositionEvent read FOnDebugOfPosition write FOnDebugOfPosition;
    property UseThrottle: Boolean read FUseThrottle write SetUseThrottle default False;
    property ThrottleInterval: QWord read FThrottleInterval write SetThrottleInterval default 50;
  end;

implementation

{ TDebugOfPositionPlugin }

procedure TDebugOfPositionPlugin.SetThrottleInterval(const AValue: QWord);
begin
  if FThrottleInterval = AValue then Exit;
  FThrottleInterval := AValue;

  if Assigned(FThrottle) then
    FThrottle.Interval := FThrottleInterval;
end;

procedure TDebugOfPositionPlugin.SetUseThrottle(AValue: Boolean);
begin
  if FUseThrottle = AValue then Exit;
  FUseThrottle := AValue;

  if FUseThrottle then
    FThrottle := TThrottle.Create(FThrottleInterval)
  else
    FThrottle := nil;
end;

procedure TDebugOfPositionPlugin.MouseMove(AMapView: TMapView;
  AShift: TShiftState; X, Y: Integer; var Handled: Boolean);
begin
  FMapCoordinate := AMapView.ScreenToLatLon(Point(X, Y));

  if UseThrottle and not FThrottle.CanExecute then
    Exit;

  if Assigned(FOnDebugOfPosition) then
    FOnDebugOfPosition(Self, FMapCoordinate);
end;

constructor TDebugOfPositionPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThrottleInterval := 50;
  FUseThrottle := False;
end;

end.

