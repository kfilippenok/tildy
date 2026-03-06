unit Tildy.Tests.Core.Validators.Base;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  // Core
  Tildy.Core.Contracts.Coordinates,
  Tildy.Core.Contracts.Validator,
  // Tests
  Tildy.Tests.Core.Base;

type

  { ValidatorTestCase }

  TValidatorTestCase = class(TCoreTestCase)
  strict protected
    FValidator: IValidator;
  end;

implementation

end.

