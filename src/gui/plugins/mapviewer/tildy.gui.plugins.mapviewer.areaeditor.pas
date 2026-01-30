unit Tildy.GUI.Plugins.MapViewer.AreaEditor;

{$mode ObjFPC}{$H+}
{$scopedenums on}
{$modeswitch advancedrecords}

interface

uses
  Classes,
  SysUtils,
  Math,
  System.UITypes,
  Graphics,
  FGL,
  LCLType,
  // MapViewer
  mvMapViewer,
  mvDrawingEngine,
  mvPluginCommon,
  mvTypes,
  mvMapProvider,
  // Translation
  Tildy.GUI.i18n.StrConsts;

type

  TResizePosition = (
    Inside,
    Outside,
    EdgeLeft,
    EdgeTop,
    EdgeRight,
    EdgeBottom,
    CornerTopLeft,
    CornerTopRight,
    CornerBottomLeft,
    CornerBottomRight
  );

  { TMapLabeledArea }

  TMapLabeledArea = class(TMapRealArea)
  private
    FIsDragging: Boolean;
  strict private
    FCaption: String;
    function GetIsDragging: Boolean;
    procedure SetCaption(AValue: String);
  public
    constructor Create(AOwner: TComponent);

    procedure BeginDrag;
    procedure EndDrag;
    property IsDragging: Boolean read GetIsDragging;

    property Caption: String read FCaption write SetCaption;
  end;

  TAreaSelectionChangedEvent = procedure(
    Sender: TObject;
    const OldArea, NewArea: TMapLabeledArea
  ) of object;

  {
    TAreaOpacityPolicy

    Opacity(Selected) ≥ Opacity(Hovered) ≥ Opacity(Normal)

  }

  TAreaOpacityPolicy = class
  private const
    // Maximum opacity for a normal area (top layer)
    MaxOpacity = 0.70;

    // Minimum allowed opacity to keep areas visible
    MinAllowedOpacity = 0.15;

    // Opacity for hovered area (mouse over)
    HoveredOpacity = 0.77;

    // Opacity for selected area (active)
    SelectedOpacity = 0.85;

    // Number of areas where opacity starts to degrade to minimum
    DenseAreasThreshold = 10;
  private
    function CalcMinOpacity(Count: Integer): Single;
    function CalcNormalOpacity(Index, Count: Integer): Single;
  public
    function GetOpacity(
      Index: Integer;
      Count: Integer;
      IsHovered: Boolean;
      IsSelected: Boolean
    ): Single;
  end;

  { TAreaEditorPlugin }

  TAreaEditorPlugin = class(TMvDrawPlugin)
  strict private type
    AreaColors = record
    const
      PrimaryLight: TColor = TColor($00E48C3A); // Hex: #3A8CE4; RGB: 58, 140, 228;
      PrimaryDark: TColor = TColor($00B47A37); // Hex: #377AB4; RGB: 55, 122, 180;
      SecondaryLight: TColor = TColor($0041A5F2); // Hex: #F2A541; RGB: 242, 165, 65;
      SecondaryDark: TColor = TColor($001A7AC4); // Hex: #C47A1A; RGB: 196, 122, 26;
    end;
  strict private
    FDraggingArea: TMapLabeledArea;
    FHoveredArea: TMapLabeledArea;
    FSelectedArea: TMapLabeledArea;
    FStartDraggingBorders: TRect;
    FStartDraggingPoint: TPoint;
    FCurrentResizePosition: TResizePosition;
    FDraggingResizePosition: TResizePosition;
    FResizeTolerance: Byte;
    FAreaOpacityPolicy: TAreaOpacityPolicy;
    FOnSelectionChanged: TAreaSelectionChangedEvent;
    function GetInDrag: Boolean;
    procedure SetSelectedArea(AValue: TMapLabeledArea);
    procedure AreaOnChange(Sender: TObject);
  protected
    procedure MouseDown(AMapView: TMapView; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; var Handled: Boolean); override;
    procedure MouseUp(AMapView: TMapView; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; var Handled: Boolean); override;
    procedure MouseMove(AMapView: TMapView; AShift: TShiftState; X, Y: Integer;
      var Handled: Boolean); override;
    procedure MouseWheel(AMapView: TMapView; AShift: TShiftState;
      AWheelDelta: Integer; AMousePos: TPoint; var Handled: Boolean); override;
    procedure AfterDrawObjects(AMapView: TMapView; var Handled: Boolean);
      override;
  strict protected type
    TAreaList = specialize TFPGObjectList<TMapLabeledArea>;
  strict protected
    FAreas: TAreaList;
    function GetArea(AIndex: Integer): TMapLabeledArea;
    function PickArea(const AMapView: TMapView; const APoint: TPoint;
                      out AResizePosition: TResizePosition; out AAreaBorders: TRect): TMapLabeledArea;
    procedure ResizeBordersByDelta(const AStartBorders: TRect; out ResultBorders: TRect;
                      const AResizePosition: TResizePosition; const ADeltaX, ADeltaY: Integer);
    procedure ConstrainAreaToWorld(
                      const AWorldProjection: TProjectionType;
                      const AOriginalRealArea: TRealArea;
                      var ANewRealArea: TRealArea);
    function GetWorldRealArea(AProjection: TProjectionType): TRealArea;
    function GetResizePosition(const APoint: TPoint;
                                const AAreaBorders: TRect): TResizePosition;
    function GetCursorByPosition(const AResizePosition: TResizePosition): TCursor;
    property InDrag: Boolean read GetInDrag;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure MoveSelectedByPixels(const AMapView: TMapView; const ADX, ADY: Integer);
  public
    function AddArea: TMapLabeledArea;
    function AddArea(AArea: TMapLabeledArea): Integer; overload;
    procedure ClearAreas;
    function CountAreas: Integer;
    procedure DeleteArea(AArea: TMapLabeledArea);
    procedure DeleteArea(AIndex: Integer);
    function IndexOfArea(AArea: TMapLabeledArea): Integer;
    procedure MovePrev(AArea: TMapLabeledArea);
    procedure MoveNext(AArea: TMapLabeledArea);
  public
    property Areas[AIndex: Integer]: TMapLabeledArea read GetArea; default;
    property SelectedArea: TMapLabeledArea read FSelectedArea write SetSelectedArea;
  published
    property Font;
    property OnSelectionChanged: TAreaSelectionChangedEvent read FOnSelectionChanged write FOnSelectionChanged;
  end;

implementation

function RectToRealArea(const AMapView: TMapView; const ARect: TRect): TRealArea;
begin
  Result.TopLeft := AMapView.ScreenToLatLon(ARect.TopLeft);
  Result.BottomRight := AMapView.ScreenToLatLon(ARect.BottomRight);
end;

function RealAreaToRect(const AMapView: TMapView; const ARealArea: TRealArea): TRect;
begin
  Result.TopLeft := AMapView.LatLonToScreen(ARealArea.TopLeft);
  Result.BottomRight := AMapView.LatLonToScreen(ARealArea.BottomRight);
end;

{ TMapLabeledArea }

function TMapLabeledArea.GetIsDragging: Boolean;
begin
  Result := FIsDragging;
end;

procedure TMapLabeledArea.SetCaption(AValue: String);
begin
  if FCaption = AValue then Exit;
  FCaption := AValue;
  Update;
end;

constructor TMapLabeledArea.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FCaption := String.Empty;
end;

procedure TMapLabeledArea.BeginDrag;
begin
  FIsDragging := True;
end;

procedure TMapLabeledArea.EndDrag;
begin
  FIsDragging := False;
end;

{ TAreaOpacityPolicy }

{
  Calculates the minimum opacity based on the number of areas.
  As the number of areas increases, the minimum opacity decreases.
}
function TAreaOpacityPolicy.CalcMinOpacity(Count: Integer): Single;
var
  k: Single;
begin
  if Count <= 1 then
    Exit(MaxOpacity);

  k := Count / DenseAreasThreshold;
  if k > 1 then k := 1;

  Result := MaxOpacity - (MaxOpacity - MinAllowedOpacity) * k;
end;

{
  Calculates the normal opacity for a specific area index.
  Areas on top are more opaque, areas below are more transparent.
}
function TAreaOpacityPolicy.CalcNormalOpacity(Index, Count: Integer): Single;
var
  MinOpacity, Step: Single;
begin
  if Count <= 1 then
    Exit(MaxOpacity);

  MinOpacity := CalcMinOpacity(Count);
  Step := (MaxOpacity - MinOpacity) / (Count - 1);

  Result := MaxOpacity - Step * (Count - 1 - Index);

  // Ensure opacity does not go below minimum allowed value
  if Result < MinAllowedOpacity then
    Result := MinAllowedOpacity;
end;

{
  Returns the effective opacity of an area depending on its state.
  - Selected areas override all
  - Hovered areas override normal opacity
  - Normal areas degrade with index and total count
}
function TAreaOpacityPolicy.GetOpacity(
  Index, Count: Integer;
  IsHovered, IsSelected: Boolean
): Single;
begin
  if IsSelected then
    Exit(SelectedOpacity);

  if IsHovered then
    Exit(HoveredOpacity);

  Result := CalcNormalOpacity(Index, Count);
end;

{ TAreaEditorPlugin }

function TAreaEditorPlugin.PickArea(const AMapView: TMapView;
  const APoint: TPoint; out AResizePosition: TResizePosition; out AAreaBorders: TRect
  ): TMapLabeledArea;
var
  i: Integer;
  Area: TMapLabeledArea;
  ExtendedAreaBorders: TRect;
begin
  Result := nil;
  for i := (FAreas.Count - 1) downto 0 do
  begin
    Area := FAreas[i];
    AAreaBorders.Create(
      AMapView.LatLonToScreen(Area.Area.TopLeft),
      AMapView.LatLonToScreen(Area.Area.BottomRight)
    );
    ExtendedAreaBorders := Rect(
      AAreaBorders.Left - FResizeTolerance,
      AAreaBorders.Top - FResizeTolerance,
      AAreaBorders.Right + FResizeTolerance,
      AAreaBorders.Bottom + FResizeTolerance
    );
    if not ExtendedAreaBorders.Contains(APoint) then
    begin
      AResizePosition := TResizePosition.Outside;
      Continue;
    end;
    AResizePosition := GetResizePosition(APoint, AAreaBorders);
    Exit(Area);
  end;
end;

procedure TAreaEditorPlugin.ResizeBordersByDelta(
  const AStartBorders: TRect;
  out ResultBorders: TRect;
  const AResizePosition: TResizePosition;
  const ADeltaX, ADeltaY: Integer
);
begin
  ResultBorders := AStartBorders;

  case AResizePosition of
    TResizePosition.EdgeLeft:
      Inc(ResultBorders.Left, ADeltaX);
    TResizePosition.EdgeRight:
      Inc(ResultBorders.Right, ADeltaX);
    TResizePosition.EdgeTop:
      Inc(ResultBorders.Top, ADeltaY);
    TResizePosition.EdgeBottom:
      Inc(ResultBorders.Bottom, ADeltaY);
    TResizePosition.CornerTopLeft:
      begin
        Inc(ResultBorders.Left, ADeltaX);
        Inc(ResultBorders.Top, ADeltaY);
      end;
    TResizePosition.CornerTopRight:
      begin
        Inc(ResultBorders.Right, ADeltaX);
        Inc(ResultBorders.Top, ADeltaY);
      end;
    TResizePosition.CornerBottomRight:
    begin
      Inc(ResultBorders.Right, ADeltaX);
      Inc(ResultBorders.Bottom, ADeltaY);
    end;
    TResizePosition.CornerBottomLeft:
      begin
        Inc(ResultBorders.Left, ADeltaX);
        Inc(ResultBorders.Bottom, ADeltaY);
      end;
    TResizePosition.Inside:
      begin
        Inc(ResultBorders.Left, ADeltaX);
        Inc(ResultBorders.Right, ADeltaX);
        Inc(ResultBorders.Top, ADeltaY);
        Inc(ResultBorders.Bottom, ADeltaY);
      end;
  end;
end;

procedure TAreaEditorPlugin.ConstrainAreaToWorld(
  const AWorldProjection: TProjectionType;
  const AOriginalRealArea: TRealArea;
  var ANewRealArea: TRealArea
);
const
 LatEpsilon = 1e-8;
var
  AreaWidth, AreaHeight: Double;
  WorldRealArea: TRealArea;
begin
  AreaWidth := AOriginalRealArea.BottomRight.Lon - AOriginalRealArea.TopLeft.Lon;
  AreaHeight := AOriginalRealArea.TopLeft.Lat - AOriginalRealArea.BottomRight.Lat;

  WorldRealArea := GetWorldRealArea(AWorldProjection);

  { Horizontal }

  // Left
  if ANewRealArea.TopLeft.Lon <= WorldRealArea.TopLeft.Lon then
  begin
    ANewRealArea.TopLeft.Lon := WorldRealArea.TopLeft.Lon;
    ANewRealArea.BottomRight.Lon := ANewRealArea.TopLeft.Lon + AreaWidth;
  end;

  // Right
  if ANewRealArea.BottomRight.Lon >= WorldRealArea.BottomRight.Lon then
  begin
    ANewRealArea.BottomRight.Lon := WorldRealArea.BottomRight.Lon;
    ANewRealArea.TopLeft.Lon := ANewRealArea.BottomRight.Lon - AreaWidth;
  end;

  { Vertical }

  // Top
  if ANewRealArea.TopLeft.Lat > WorldRealArea.TopLeft.Lat - LatEpsilon then
  begin
    ANewRealArea.TopLeft.Lat := WorldRealArea.TopLeft.Lat;
    ANewRealArea.BottomRight.Lat := ANewRealArea.TopLeft.Lat - AreaHeight;
  end;

  // Bottom
  if ANewRealArea.BottomRight.Lat < WorldRealArea.BottomRight.Lat + LatEpsilon then
  begin
    ANewRealArea.BottomRight.Lat := WorldRealArea.BottomRight.Lat;
    ANewRealArea.TopLeft.Lat := ANewRealArea.BottomRight.Lat + AreaHeight;
  end;
end;

function TAreaEditorPlugin.GetWorldRealArea(AProjection: TProjectionType
  ): TRealArea;
const
  // TMapViewerEngine.DegreesToPixelsEPSG3857
  WorldAreaEPSG3395: TRealArea = (
    TopLeft: (lon: -180; lat: 80);
    BottomRight: (lon: 180; lat: -84)
  );
  // TMapViewerEngine.DegreesToPixelsEPSG3857
  WorldAreaEPSG3857: TRealArea = (
    TopLeft : (lon: -180; lat: 85.05112878);
    BottomRight : (lon: 180; lat: -85.05112878)
  );
begin
  case AProjection of
    ptEPSG3857: Result := WorldAreaEPSG3857;
    ptEPSG3395: Result := WorldAreaEPSG3395;
  end;
end;

function TAreaEditorPlugin.GetResizePosition(const APoint: TPoint;
  const AAreaBorders: TRect): TResizePosition;
var
  HorizPos, VertPos: TResizePosition;
  ExtendedAreaBorders: TRect;
begin
  ExtendedAreaBorders := Rect(
    AAreaBorders.Left - FResizeTolerance,
    AAreaBorders.Top - FResizeTolerance,
    AAreaBorders.Right + FResizeTolerance,
    AAreaBorders.Bottom + FResizeTolerance
  );

  if ExtendedAreaBorders.Contains(APoint) then
    Result := TResizePosition.Inside
  else
    Exit(TResizePosition.Outside);

  HorizPos := TResizePosition.Inside;
  if InRange(APoint.X, AAreaBorders.Left - FResizeTolerance,
                       AAreaBorders.Left + FResizeTolerance) then
    HorizPos := TResizePosition.EdgeLeft
  else if InRange(APoint.X, AAreaBorders.Right - FResizeTolerance,
                            AAreaBorders.Right + FResizeTolerance) then
    HorizPos := TResizePosition.EdgeRight;

  VertPos := TResizePosition.Inside;
  if InRange(APoint.Y, AAreaBorders.Top - FResizeTolerance,
                       AAreaBorders.Top + FResizeTolerance) then
    VertPos := TResizePosition.EdgeTop
  else if InRange(APoint.Y, AAreaBorders.Bottom - FResizeTolerance,
                            AAreaBorders.Bottom + FResizeTolerance) then
    VertPos := TResizePosition.EdgeBottom;

  if HorizPos = TResizePosition.Inside then
    Exit(VertPos)
  else if VertPos = TResizePosition.Inside then
    Exit(HorizPos);

  if (HorizPos = TResizePosition.EdgeLeft) and (VertPos = TResizePosition.EdgeTop) then
    Exit(TResizePosition.CornerTopLeft)
  else if (HorizPos = TResizePosition.EdgeLeft) and (VertPos = TResizePosition.EdgeBottom) then
    Exit(TResizePosition.CornerBottomLeft);

  if (HorizPos = TResizePosition.EdgeRight) and (VertPos = TResizePosition.EdgeTop) then
    Exit(TResizePosition.CornerTopRight)
  else if (HorizPos = TResizePosition.EdgeRight) and (VertPos = TResizePosition.EdgeBottom) then
    Exit(TResizePosition.CornerBottomRight);
end;

function TAreaEditorPlugin.GetCursorByPosition(
  const AResizePosition: TResizePosition): TCursor;
begin
  case AResizePosition of
    TResizePosition.EdgeLeft, TResizePosition.EdgeRight:
      Result := crSizeWE;
    TResizePosition.EdgeTop, TResizePosition.EdgeBottom:
      Result := crSizeNS;
    TResizePosition.CornerTopLeft, TResizePosition.CornerBottomRight:
      Result := crSizeNWSE;
    TResizePosition.CornerBottomLeft, TResizePosition.CornerTopRight:
      Result := crSizeNESW;
    TResizePosition.Inside:
      if Assigned(FDraggingArea) then
        Result := crSize
      else
        Result := crHandPoint;
  else
    Result := crDefault;
  end;
end;

procedure TAreaEditorPlugin.SetSelectedArea(AValue: TMapLabeledArea);
var
  OldArea: TMapLabeledArea;
begin
  if AValue = FSelectedArea then
    Exit;

  OldArea := FSelectedArea;
  FSelectedArea := AValue;

  if Assigned(FOnSelectionChanged) then
    FOnSelectionChanged(Self, OldArea, FSelectedArea);

  Update;
end;

procedure TAreaEditorPlugin.AreaOnChange(Sender: TObject);
begin
  Update;
end;

function TAreaEditorPlugin.GetInDrag: Boolean;
begin
  Result := Assigned(FDraggingArea);
end;

procedure TAreaEditorPlugin.MouseDown(AMapView: TMapView;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer;
  var Handled: Boolean
);
begin
  if Handled then
  begin
    SelectedArea := nil;
    Exit;
  end;

  if Assigned(FHoveredArea) then
  begin
    FDraggingArea := FHoveredArea;
    FDraggingArea.BeginDrag;
    FStartDraggingBorders.Create(
      AMapView.LatLonToScreen(FDraggingArea.Area.TopLeft),
      AMapView.LatLonToScreen(FDraggingArea.Area.BottomRight)
    );
    FStartDraggingPoint := Point(X, Y);
    FDraggingResizePosition := FCurrenTResizePosition;
    SelectedArea := FHoveredArea;
    Handled := True;
  end
  else
  begin
    SelectedArea := nil;
    FDraggingArea := nil;
  end;

  Update;
end;

procedure TAreaEditorPlugin.MouseUp(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
begin
  if Assigned(FDraggingArea) then
    FDraggingArea.EndDrag;
  FDraggingArea := nil;
end;

procedure TAreaEditorPlugin.MouseMove(
  AMapView: TMapView;
  AShift: TShiftState;
  X, Y: Integer;
  var Handled: Boolean
);
var
  AreaBorders: TRect;
  NewRealArea: TRealArea;
  DX, DY: Integer;
  IsMoved: Boolean = False;
begin
  if Handled then
    Exit;

  IsMoved := FDraggingResizePosition = TResizePosition.Inside;
  FHoveredArea := PickArea(AMapView, Point(X, Y), FCurrentResizePosition, AreaBorders);
  Update;

  if Assigned(FDraggingArea) then
  begin
    MapView.Cursor := GetCursorByPosition(FDraggingResizePosition);
    Handled := True;
  end
  else
  begin
    MapView.Cursor := GetCursorByPosition(FCurrentResizePosition);
    Exit;
  end;

  DX := X - FStartDraggingPoint.X;
  DY := Y - FStartDraggingPoint.Y;

  ResizeBordersByDelta(
    FStartDraggingBorders,
    AreaBorders,
    FDraggingResizePosition,
    DX, DY
  );

  NewRealArea := RectToRealArea(AMapView, AreaBorders);

  if IsMoved then
    ConstrainAreaToWorld(
      MapProviderByName(AMapView.MapProvider).ProjectionType,
      FDraggingArea.Area,
      NewRealArea
    );

  FDraggingArea.Area := NewRealArea;
end;

procedure TAreaEditorPlugin.MouseWheel(AMapView: TMapView; AShift: TShiftState;
  AWheelDelta: Integer; AMousePos: TPoint; var Handled: Boolean);
begin
  Handled := Assigned(FDraggingArea);
end;

procedure TAreaEditorPlugin.AfterDrawObjects(AMapView: TMapView;
  var Handled: Boolean);

  function _GetAreaLightColor(ANumber: Integer): TColor;
  begin
    if (ANumber mod 2) = 0 then
      Result := AreaColors.PrimaryLight
    else
      Result := AreaColors.SecondaryLight;
  end;

  function _GetAreaDarkColor(ANumber: Integer): TColor;
  begin
    if (ANumber mod 2) = 0 then
      Result := AreaColors.PrimaryDark
    else
      Result := AreaColors.SecondaryDark;
  end;

var
  i: Integer;
  Area: TMapLabeledArea;
  DrawingEngine: TMvCustomDrawingEngine;
  AreaBorders: TRect;
  TextHeight, TextWidth: Integer;
  IsSelected, IsHovered: Boolean;
begin
  if CountAreas = 0 then Exit;

  DrawingEngine := AMapView.DrawingEngine;
  DrawingEngine.StoreState;

  for i := 0 to (FAreas.Count - 1) do
  begin
    Area := FAreas[i];
    AreaBorders := RealAreaToRect(AMapView, Area.Area);

    IsSelected := Area = FSelectedArea;
    IsHovered := Area = FHoveredArea;

    DrawingEngine.BrushStyle := bsSolid;
    if IsSelected then
    begin
      DrawingEngine.PenStyle := psSolid;
      DrawingEngine.PenWidth := 1;
      DrawingEngine.PenColor := clBlack;
      DrawingEngine.BrushColor := _GetAreaLightColor(i);
    end
    else
    begin
      DrawingEngine.PenStyle := psClear;
      DrawingEngine.BrushColor := _GetAreaDarkColor(i);
    end;

    DrawingEngine.Opacity :=
      FAreaOpacityPolicy.GetOpacity(
        i,
        FAreas.Count,
        IsHovered and not InDrag,
        IsSelected
      );

    DrawingEngine.Rectangle(AreaBorders.TopLeft.X, AreaBorders.TopLeft.Y,
                           AreaBorders.BottomRight.X, AreaBorders.BottomRight.Y);
    DrawingEngine.SetFont(Font.Name, Font.Size, Font.Style, Font.Color, Font.Orientation);
    TextWidth := DrawingEngine.TextWidth(Area.Caption);
    TextHeight := DrawingEngine.TextHeight(Area.Caption);
    DrawingEngine.BrushStyle := bsClear;
    DrawingEngine.TextOut(
      AreaBorders.Left + Round((AreaBorders.Right - AreaBorders.Left) / 2 - TextWidth / 2),
      AreaBorders.Top + Round((AreaBorders.Bottom - AreaBorders.Top) / 2 - TextHeight / 2),
      Area.Caption
    );
  end;

  DrawingEngine.RestoreState;
end;

function TAreaEditorPlugin.GetArea(AIndex: Integer): TMapLabeledArea;
begin
  Result := FAreas[AIndex];
end;

constructor TAreaEditorPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAreas := TAreaList.Create;
  FCurrentResizePosition := TResizePosition.Outside;
  FDraggingResizePosition := TResizePosition.Outside;
  FResizeTolerance := 5;
  FSelectedArea := nil;
  FAreaOpacityPolicy := TAreaOpacityPolicy.Create;
end;

destructor TAreaEditorPlugin.Destroy;
begin
  FAreas.Free;
  FAreaOpacityPolicy.Free;
  inherited Destroy;
end;

procedure TAreaEditorPlugin.MoveSelectedByPixels(const AMapView: TMapView; const ADX, ADY: Integer);
var
  StartBorders, NewBorders: TRect;
  NewRealArea: TRealArea;
begin
  if not Assigned(AMapView) or not Assigned(FSelectedArea) then
    Exit;

  if FSelectedArea.IsDragging then
    Exit;

  StartBorders := RealAreaToRect(AMapView, FSelectedArea.Area);
  ResizeBordersByDelta(StartBorders, NewBorders, TResizePosition.Inside, ADX, ADY);

  NewRealArea := RectToRealArea(AMapView, NewBorders);

  ConstrainAreaToWorld(
    MapProviderByName(AMapView.MapProvider).ProjectionType,
    FSelectedArea.Area,
    NewRealArea
  );

  FSelectedArea.Area := NewRealArea;
  Update;
end;

function TAreaEditorPlugin.AddArea: TMapLabeledArea;
var
  Area: TMapLabeledArea;
begin
  Area := TMapLabeledArea.Create(Self);
  Area.West := -100;
  Area.North := 50;
  Area.East := 100;
  Area.South := -50;
  Area.OnChange := @AreaOnChange;
  FAreas.Add(Area);
  Area.Caption := SPluginAreaEditorAreaName + FAreas.Count.ToString;
  Result := Area;
  Update;
end;

function TAreaEditorPlugin.AddArea(AArea: TMapLabeledArea): Integer;
begin
  if not Assigned(AArea) then
    raise Exception.Create('Area is not assigned.');

  if not Assigned(AArea.OnChange) then
    AArea.OnChange := @AreaOnChange;
  Result := FAreas.Add(AArea);
  Update;
end;

procedure TAreaEditorPlugin.DeleteArea(AArea: TMapLabeledArea);
begin
  FAreas.Remove(AArea);
  Update;
end;

procedure TAreaEditorPlugin.DeleteArea(AIndex: Integer);
begin
  FAreas.Delete(AIndex);
  Update;
end;

function TAreaEditorPlugin.IndexOfArea(AArea: TMapLabeledArea): Integer;
begin
  Result := FAreas.IndexOf(AArea);
end;

procedure TAreaEditorPlugin.MovePrev(AArea: TMapLabeledArea);
var
  ItemIndex: Integer;
begin
  ItemIndex := IndexOfArea(AArea);

  if ItemIndex <= 0 then
    Exit;

  FAreas.Move(ItemIndex, ItemIndex - 1);
  Update;
end;

procedure TAreaEditorPlugin.MoveNext(AArea: TMapLabeledArea);
var
  ItemIndex: Integer;
begin
  ItemIndex := IndexOfArea(AArea);

  if ItemIndex >= CountAreas - 1 then
    Exit;

  FAreas.Move(ItemIndex, ItemIndex + 1);
  Update;
end;

procedure TAreaEditorPlugin.ClearAreas;
begin
  FAreas.Clear;
end;

function TAreaEditorPlugin.CountAreas: Integer;
begin
  Result := FAreas.Count;
end;

end.

