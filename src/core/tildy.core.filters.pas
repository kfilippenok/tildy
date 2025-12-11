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

unit Tildy.Core.Filters;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Tildy.Core.Engine, BGRABitmap;

type

  { TFilterGrayscale }

  TFilterGrayscale = class(TInterfacedObject, IFilter)
  strict private
    procedure Grayscale(var ABGRABitmap: TBGRABitmap);
  public
    procedure Transform(var ABGRABitmap: TBGRABitmap);
  end;

implementation

{ TFilterGrayscale }

procedure TFilterGrayscale.Grayscale(var ABGRABitmap: TBGRABitmap);
var
  LOld: TBGRABitmap;
begin
  LOld := ABGRABitmap;
  ABGRABitmap := ABGRABitmap.FilterGrayscale(true);
  LOld.Free;
end;

procedure TFilterGrayscale.Transform(var ABGRABitmap: TBGRABitmap);
begin
  if Assigned(ABGRABitmap) then
    Grayscale(ABGRABitmap);
end;

end.

