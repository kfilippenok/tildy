{
  Copyright (c) 2024-2025 Kirill Filippenok

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License. }

unit Tildy.Threads;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  TilesManipulations.Base;

type

  { TTilesManipulatorAreaThread }

  TTilesManipulatorAreaThread = class(TThread)
  strict private
    FTilesManipulator: TTilesManipulator;
    FArea: TArea;
    FMinZoom, FMaxZoom: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(const ASource: TTilesManipulator; AMinZoom, AMaxZoom: Integer; AArea: TArea);
    destructor Destroy; override;
  public
    property Area: TArea read FArea;
    property MinZoom: Integer read FMinZoom;
    property MaxZoom: Integer read FMaxZoom;
    property TilesManipulator: TTilesManipulator read FTilesManipulator;
  end;

implementation

{ TTilesManipulatorAreaThread }

procedure TTilesManipulatorAreaThread.Execute;
begin
  FTilesManipulator.Download(FMinZoom, FMaxZoom, FArea);
end;

constructor TTilesManipulatorAreaThread.Create(const ASource: TTilesManipulator; AMinZoom, AMaxZoom: Integer; AArea: TArea);
begin
  FTilesManipulator := TTilesManipulator.Create;
  ASource.Assign(FTilesManipulator);
  FMinZoom := AMinZoom;
  FMaxZoom := AMaxZoom;
  FArea := AArea;
end;

destructor TTilesManipulatorAreaThread.Destroy;
begin
  inherited Destroy;
  FTilesManipulator.Free;
end;

end.

