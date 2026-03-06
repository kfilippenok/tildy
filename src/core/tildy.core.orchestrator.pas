unit Tildy.Core.Orchestrator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap,
  Tildy.Core.Contracts,
  Tildy.Core.Processors.Pipeline;

type

  { TOrchestrator }

  TOrchestrator = class(TInterfacedObject, IOrchestrator)
  strict private
    FLayerPipeline: TProcessorPipeline;

    function GetCountLayers: Integer;
    function GetLayer(const AIndex: Integer): ILayer;
  public
    constructor Create;

    function AddLayer(ALayer: ILayer): Integer;
    procedure ClearLayers;

    property CountLayers: Integer read GetCountLayers;
    property Layers[AIndex: Integer]: ILayer read GetLayer;
  end;

implementation


{ TOrchestrator }

function TOrchestrator.GetCountLayers: Integer;
begin

end;

function TOrchestrator.GetLayer(const AIndex: Integer): ILayer;
begin

end;

constructor TOrchestrator.Create;
begin

end;

function TOrchestrator.AddLayer(ALayer: ILayer): Integer;
begin

end;

procedure TOrchestrator.ClearLayers;
begin

end;

end.

