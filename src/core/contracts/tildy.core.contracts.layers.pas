unit Tildy.Core.Contracts.Layers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Tildy.Core.Contracts.Source,
  Tildy.Core.Contracts.Processor;

type

  ILayer = interface
    ['{1D3BEDDD-2B02-4C3A-B6FF-E4DCEEE6B7DB}']

    function GetFilter(const AIndex: Integer): IProcessor;

    function GetName: string;
    procedure SetName(const AValue: string);

    function GetSource: ISource;
    procedure SetSource(AValue: ISource);

    function GetOpacity: Byte;
    procedure SetOpacity(AValue: Byte);

    function GetEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);

    property Filters[AIndex: Integer]: IProcessor read GetFilter;
    property Name: string read GetName write SetName;
    property Opacity: Byte read GetOpacity write SetOpacity;
    property Source: ISource read GetSource;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;


implementation

end.

