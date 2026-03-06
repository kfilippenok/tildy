unit Tildy.Utilities.Throttle;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  // Time
  Tildy.Utilities.Time;

type

  IThrottle = interface
    function GetInterval: QWord;
    procedure SetInterval(const AValue: QWord);

    function CanExecute: Boolean;
    procedure Reset;
    property Interval: QWord read GetInterval write SetInterval;
  end;

  { TThrottle }

  TThrottle = class(TInterfacedObject, IThrottle)
  private
    FInterval: QWord;
    FLastTime: QWord;
    function GetInterval: QWord;
    procedure SetInterval(const AValue: QWord);
  public
    constructor Create(Interval: Integer);
    function CanExecute: Boolean;
    procedure Reset;

    property IntervalMs: QWord read GetInterval write SetInterval;
  end;

implementation

function TThrottle.GetInterval: QWord;
begin
  Result := FInterval;
end;

procedure TThrottle.SetInterval(const AValue: QWord);
begin
  FInterval := AValue;
end;

constructor TThrottle.Create(Interval: Integer);
begin
  inherited Create;
  FInterval := Interval;
  FLastTime := 0;
end;

function TThrottle.CanExecute: Boolean;
var
  Now: QWord;
begin
  Now := GetTickCountMS;
  Result := (Now - FLastTime) >= FInterval;
  if Result then
    FLastTime := Now;
end;

procedure TThrottle.Reset;
begin
  FLastTime := 0;
end;

end.

