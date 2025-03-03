unit TilesManipulations.Utilities;

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

end.

