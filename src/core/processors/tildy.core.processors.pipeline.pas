unit Tildy.Core.Processors.Pipeline;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  BGRABitmap,
  Tildy.Core.Contracts.Coordinates,
  Tildy.Core.Contracts.Processor,
  Tildy.Core.Processors.Chain;

type

  { TProcessorPipeline }

  TProcessorPipeline = class(TProcessorChain, IProcessorPipeline)
  public
    procedure Process(const ACoord: TTileCoord; var ATile: TBGRABitmap); override;
  end;

implementation

{ TProcessorPipeline }

procedure TProcessorPipeline.Process(const ACoord: TTileCoord;
  var ATile: TBGRABitmap);
var
  Processor: IProcessor;
  TileClone: TBGRABitmap = nil;
begin
  for Processor in FProcessors do
    try
      TileClone := ATile.Duplicate(True);
      Processor.Process(ACoord, TileClone);
      ATile.Free;
      ATile := TileClone;
    finally
      TileClone.Free;
    end;
end;

end.

