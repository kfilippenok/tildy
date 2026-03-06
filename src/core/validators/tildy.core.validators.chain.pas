unit Tildy.Core.Validators.Chain;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fgl,
  BGRABitmap,
  Tildy.Core.Contracts.Validator,
  Tildy.Core.Contracts.Coordinates;

type

  { TValidatorChain }

  TValidatorChain = class(TInterfacedObject, IValidator, IValidatorChain)
  strict private type
    TValidatorList = specialize TFPGList<IValidator>;
    TValidatorEnumerator = specialize TFPGListEnumerator<IValidator>;
  strict private
    function GetDescription: String;
    function GetName: String;
    function GetValidator(const AIndex: Integer): IValidator;
    function GetValidatorCount: Integer;
  strict protected
    FDescription: String;
    FName: String;
    FValidators: TValidatorList;
  public
    constructor Create(AName: String = 'ValidatorChain';
                       ADescription: String = 'Chain of validators.'); virtual; reintroduce;
    procedure BeforeDestruction; override;

    function GetEnumerator: TValidatorEnumerator;

    function ShouldSkipTile(const ACoord: TTileCoord; ATile: TBGRABitmap): TValidateResult;
    procedure AddValidator(AValidator: IValidator);
    procedure RemoveValidator(AValidator: IValidator);
    procedure ClearValidators;

    property Description: String read GetDescription;
    property Name: String read GetName;
    property ValidatorCount: Integer read GetValidatorCount;
    property Validators[AIndex: Integer]: IValidator read GetValidator;
  end;

implementation

{ TValidatorPipeline }

function TValidatorChain.GetDescription: String;
begin
  Result := FDescription;
end;

function TValidatorChain.GetName: String;
begin
  Result := FName;
end;

constructor TValidatorChain.Create(AName: String; ADescription: String);
begin
  Inherited Create;

  FName := AName;
  FDescription := ADescription;
  FValidators := TValidatorList.Create;
end;

procedure TValidatorChain.BeforeDestruction;
begin
  FValidators.Free;
  inherited BeforeDestruction;
end;

function TValidatorChain.GetEnumerator: TValidatorEnumerator;
begin
  Result := TValidatorEnumerator.Create(FValidators);
end;

function TValidatorChain.GetValidatorCount: Integer;
begin
  Result := FValidators.Count;
end;

function TValidatorChain.GetValidator(const AIndex: Integer): IValidator;
begin
  Result := FValidators[AIndex];
end;

function TValidatorChain.ShouldSkipTile(const ACoord: TTileCoord;
  ATile: TBGRABitmap): TValidateResult;
var
  Validator: IValidator;
begin
  for Validator in FValidators do
  begin
    Result := Validator.ShouldSkipTile(ACoord, ATile);
    if Result.ShouldSkip then Exit;
  end;
end;

procedure TValidatorChain.AddValidator(AValidator: IValidator);
begin
  if not Assigned(AValidator) then
    raise EValidatorError.Create('AValidator parameter is nil.');

  FValidators.Add(AValidator);
end;

procedure TValidatorChain.RemoveValidator(AValidator: IValidator);
begin
  FValidators.Remove(AValidator);
end;

procedure TValidatorChain.ClearValidators;
begin
  FValidators.Clear;
end;

end.



