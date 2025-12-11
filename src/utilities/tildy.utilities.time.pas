unit Tildy.Utilities.Time;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

  function GetTickCountMS: Int64;
  function GetTickCountMCS: Int64;

implementation

{$IFDEF UNIX}
uses
  Unix;

function GetTickCountMS: Int64;
var
  lTimeVal: TTimeVal;
begin
  fpgettimeofday(@lTimeVal, nil);
  Result := lTimeVal.tv_sec * 1000 + Round(lTimeVal.tv_usec / 1000);
end;

function GetTickCountMCS: Int64;
var
  lTimeVal: TTimeVal;
begin
  fpgettimeofday(@lTimeVal, nil);
  Result := lTimeVal.tv_sec * 1000000 + lTimeVal.tv_usec;
end;
{$ENDIF}

{$IFDEF WINDOWS}
uses
  Windows, DateUtils;

var
  QueryPerformanceSupported: Boolean;
  PerformanceCounter: int64;
  PerformanceFrequency: int64; // ticks in second

function GetTickCountMS: Int64;
var
  LTime: TTime;
begin
  if QueryPerformanceSupported then
  begin
    QueryPerformanceCounter(Result);
    Result := Round(Result * (1000 / PerformanceFrequency));
  end
  else
    Result := DateTimeToDosDateTime(Now());
end;

function GetTickCountMCS: Int64;
var
  LTime: TTime;
begin
  if QueryPerformanceSupported then
  begin
    QueryPerformanceCounter(Result);
    Result := Round(Result * (1000000 / PerformanceFrequency));
  end
  else
    Result := DateTimeToDosDateTime(Now());
end;
{$ENDIF}

initialization

{$IFDEF WINDOWS}
  QueryPerformanceSupported := QueryPerformanceFrequency(PerformanceFrequency) and QueryPerformanceCounter(PerformanceCounter);
{$ENDIF}

end.

