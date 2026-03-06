unit Tildy.Core.Contracts.Validator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap,
  Tildy.Core.Contracts.Coordinates;

type
  { Результат фильтрации }
  TValidateResult = record
    ShouldSkip: Boolean;     // Нужно ли пропустить тайл
    ValidatorName: String;   // Название валидатора
    Reason: String;          // Причина фильтрации
  end;

  { Контракт фильтра тайлов }
  IValidator = interface
    ['{F1E2D3C4-B5A6-7988-9ABC-DEF012345678}']

    { Проверить тайл }
    function ShouldSkipTile(const ACoord: TTileCoord; ATile: TBGRABitmap): TValidateResult;

    { Метаданные }
    function GetName: String;
    function GetDescription: String;

    property Name: String read GetName;
    property Description: String read GetDescription;
  end;

  { Цепочка фильтров }
  IValidatorChain = interface(IValidator)
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']

    procedure AddValidator(AValidator: IValidator);
    procedure RemoveValidator(AValidator: IValidator);
    procedure ClearValidators;

    function GetValidatorCount: Integer;
    property ValidatorCount: Integer read GetValidatorCount;
  end;

  EValidatorError = class(Exception);

implementation

end.

