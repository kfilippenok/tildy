{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit tildy_tests;

{$warn 5023 off : no warning about unused units}
interface

uses
  Tildy.Tests.Core.Base, Tildy.Tests.Core.Sources.Base, 
  Tildy.Tests.Core.Sources.FileSystem, Tildy.Tests.Core.Sources.HTTP, 
  Tildy.Tests.Core.Sources, Tildy.Tests.Base, 
  Tildy.Tests.Core.Validators.Monochrome, Tildy.Tests.Core.Validators.Base, 
  Tildy.Tests.Core.Validators, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('tildy_tests', @Register);
end.
