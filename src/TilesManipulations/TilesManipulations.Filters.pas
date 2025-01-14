unit TilesManipulations.Filters;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TilesManipulations.Base, BGRABitmap;

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

