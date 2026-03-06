unit Tildy.Core.Contracts.Orchestrator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Tildy.Core.Contracts.Layers;

type

  IOrchestrator = interface
    ['{AF6EED52-515D-4EE9-8682-A3A88165CA32}']

    function GetCountLayers: Integer;
    function GetLayer(const AIndex: Integer): ILayer;

    function AddLayer(ALayer: ILayer): Integer;
    procedure ClearLayers;

    property CountLayers: Integer read GetCountLayers;
    property Layers[AIndex: Integer]: ILayer read GetLayer;
  end;

implementation

end.

