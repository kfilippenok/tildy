{ Copyright (c) 2024-2025 Kirill Filippenok

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License. }

unit Tildy.Core.Contracts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  Tildy.Core.Contracts.Orchestrator,
  Tildy.Core.Contracts.Layers,
  Tildy.Core.Contracts.Coordinates,
  Tildy.Core.Contracts.Source,
  Tildy.Core.Contracts.Storage,
  Tildy.Core.Contracts.Processor,
  Tildy.Core.Contracts.Validator,
  Tildy.Core.Contracts.Projection,
  Tildy.Core.Contracts.Scheme,
  Tildy.Core.Contracts.Transform;

type

  // Interfaces
  IOrchestrator = Tildy.Core.Contracts.Orchestrator.IOrchestrator;
  ILayer = Tildy.Core.Contracts.Layers.ILayer;
  ISource = Tildy.Core.Contracts.Source.ISource;
  IStorage = Tildy.Core.Contracts.Storage.IStorage;
  IProcessor = Tildy.Core.Contracts.Processor.IProcessor;
  IProcessorPipeline = Tildy.Core.Contracts.Processor.IProcessorPipeline;
  IValidator = Tildy.Core.Contracts.Validator.IValidator;
  IValidatorChain = Tildy.Core.Contracts.Validator.IValidatorChain;
  IProjection = Tildy.Core.Contracts.Projection.IProjection;
  IScheme = Tildy.Core.Contracts.Scheme.IScheme;
  ICoordinateTransformer = Tildy.Core.Contracts.Transform.ICoordinateTransformer;

  // Coordinates
  TGeoPoint = Tildy.Core.Contracts.Coordinates.TGeoPoint;
  TGeoBounds = Tildy.Core.Contracts.Coordinates.TGeoBounds;
  TProjectedPoint = Tildy.Core.Contracts.Coordinates.TProjectedPoint;
  TProjectedBounds = Tildy.Core.Contracts.Coordinates.TProjectedBounds;
  TPixelCoord = Tildy.Core.Contracts.Coordinates.TPixelCoord;
  TTileCoord = Tildy.Core.Contracts.Coordinates.TTileCoord;
  TTileOrigin = Tildy.Core.Contracts.Coordinates.TTileOrigin;

  // Exceptions
  EStorageError = Tildy.Core.Contracts.Storage.EStorageError;
  ESourceError = Tildy.Core.Contracts.Source.ESourceError;
  EProcessorError = Tildy.Core.Contracts.Processor.EProcessorError;
  EValidatorError = Tildy.Core.Contracts.Validator.EValidatorError;

implementation

end.

