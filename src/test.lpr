program test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Interfaces, // For BGRABitmap
  {$ENDIF}
  SysUtils, Classes, CustApp,
  TilesManipulations.Classes;

var
  //OptionParameter: array[TOptionKind] of String;
  //glOptions: TOptions;
  FormatSettings: TFormatSettings;

type

  { ATilesDownloader }

  ATilesDownloader = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  procedure ATilesDownloader.DoRun;
  var
    TilesManipulator: TTilesManipulator;
  begin

  end;

  constructor ATilesDownloader.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException:=True;
    FormatSettings.DecimalSeparator := '.';
  end;

  destructor ATilesDownloader.Destroy;
  begin
    inherited Destroy;
  end;

var
  appTilesDownloader: ATilesDownloader;
begin
  appTilesDownloader:=ATilesDownloader.Create(nil);
  appTilesDownloader.Title:='Tiles downloader';
  appTilesDownloader.Run;
  appTilesDownloader.Free;
end.


