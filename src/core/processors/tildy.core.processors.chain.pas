unit Tildy.Core.Processors.Chain;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fgl,
  BGRABitmap,
  Tildy.Core.Contracts.Coordinates,
  Tildy.Core.Contracts.Processor;

type

  { TProcessorChain }

  TProcessorChain = class(TInterfacedObject, IProcessor, IProcessorChain)
  strict private type
    TProcessorList = specialize TFPGList<IProcessor>;
    TProcessorEnumerator = specialize TFPGListEnumerator<IProcessor>;
  strict protected
    FName: String;
    FProcessors: TProcessorList;
    function GetName: string;
    function GetProcessor(const AIndex: Integer): IProcessor;
    function GetProcessorCount: Integer;
  public
    constructor Create(AName: String = 'ProcessorChain'); virtual; reintroduce;
    procedure BeforeDestruction; override;

    function GetEnumerator: TProcessorEnumerator;

    procedure Process(const ACoord: TTileCoord; var ATile: TBGRABitmap); virtual;
    procedure AddProcessor(AProcessor: IProcessor);
    procedure RemoveProcessor(AProcessor: IProcessor);
    procedure ClearProcessors;

    property Name: string read GetName;
    property ProcessorCount: Integer read GetProcessorCount;
    property Processors[AIndex: Integer]: IProcessor read GetProcessor;
  end;

implementation

{ TProcessorChain }

function TProcessorChain.GetName: string;
begin
  Result := FName;
end;

constructor TProcessorChain.Create(AName: String);
begin
  Inherited Create;

  FName := AName;
  FProcessors := TProcessorList.Create;
end;

procedure TProcessorChain.BeforeDestruction;
begin
  FProcessors.Free;
  inherited BeforeDestruction;
end;

function TProcessorChain.GetEnumerator: TProcessorEnumerator;
begin
  Result := TProcessorEnumerator.Create(FProcessors);
end;

function TProcessorChain.GetProcessorCount: Integer;
begin
  Result := FProcessors.Count;
end;

function TProcessorChain.GetProcessor(const AIndex: Integer): IProcessor;
begin
  Result := FProcessors[AIndex];
end;

procedure TProcessorChain.Process(const ACoord: TTileCoord;
  var ATile: TBGRABitmap);
var
  Procesor: IProcessor;
  TileClone: TBGRABitmap = nil;
begin
  for Procesor in FProcessors do
    try
      TileClone := ATile.Duplicate(True);
      Procesor.Process(ACoord, TileClone);
      ATile.Free;
      ATile := TileClone;
    except
      TileClone.Free;
      raise;
    end;
end;

procedure TProcessorChain.AddProcessor(AProcessor: IProcessor);
begin
  if not Assigned(AProcessor) then
    raise EProcessorError.Create('AProcessor parameter is nil.');

  FProcessors.Add(AProcessor);
end;

procedure TProcessorChain.RemoveProcessor(AProcessor: IProcessor);
begin
  FProcessors.Remove(AProcessor);
end;

procedure TProcessorChain.ClearProcessors;
begin
  FProcessors.Clear;
end;

end.

