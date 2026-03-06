unit Tildy.Core.Processors.Filters;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl,
  BGRABitmap,
  Tildy.Core.Contracts;

//type
//
  //{ TFilterPipeline - processor with a chain of filters }
  //TFilterPipeline = class(TInterfacedObject, IProcessor)
  //strict protected type
  //  TFilterList = specialize TFPGList<IFilter>;
  //strict protected
  //  FFilters: TFilterList;
  //  function GetName: string;
  //public
  //  constructor Create;
  //  destructor Destroy; override;
  //public
  //  procedure AddFilter(AFilter: IFilter);
  //  procedure ClearFilters;
  //public
  //  procedure Process(const ACoord: TTileCoord; var ATile: TBGRABitmap);
  //end;
  //
  //{ TGrayscaleFilter - shades of grey }
  //TGrayscaleFilter = class(TInterfacedObject, IFilter)
  //strict private
  //  function GetName: string;
  //  function GetDescription: string;
  //public
  //  procedure Apply(var AImage: TBGRABitmap);
  //end;

implementation

{ TFilterPipeline }

//constructor TFilterPipeline.Create;
//begin
//  FFilters := TFilterList.Create;
//end;
//
//destructor TFilterPipeline.Destroy;
//begin
//  FFilters.Free;
//
//  inherited Destroy;
//end;
//
//procedure TFilterPipeline.AddFilter(AFilter: IFilter);
//begin
//  if FFilters.IndexOf(AFilter) <> - 1 then
//    Exit;
//
//  FFilters.Add(AFilter);
//end;
//
//procedure TFilterPipeline.ClearFilters;
//begin
//  FFilters.Clear;
//end;
//
//procedure TFilterPipeline.Process(const ACoord: TTileCoord; var ATile: TBGRABitmap);
//var
//  Filter: IFilter;
//begin
//  // Применяем все фильтры по очереди
//  for Filter in FFilters do
//    Filter.Apply(ATile);
//end;
//
//function TFilterPipeline.GetName: string;
//var
//  i: Integer;
//begin
//  Result := 'FilterPipeline';
//
//  if FFilters.Count = 0 then
//    Exit;
//
//  Result := Result + ': ';
//  for i := 0 to FFilters.Count - 1 do
//  begin
//    Result := Result + FFilters[i].Name;
//    if i <> FFilters.Count - 1 then
//      Result := Result + ', ';
//  end;
//end;
//
//{ TGrayscaleFilter }
//
//function TGrayscaleFilter.GetName: string;
//begin
//  Result := 'Grayscale filter';
//end;
//
//function TGrayscaleFilter.GetDescription: string;
//begin
//  Result := 'Turning all tile colors into shades of gray';
//end;
//
//procedure TGrayscaleFilter.Apply(var AImage: TBGRABitmap);
//var
//  OldImage: TBGRABitmap;
//begin
//  OldImage := AImage;
//  AImage := AImage.FilterGrayscale(true);
//  OldImage.Free;
//end;

end.

