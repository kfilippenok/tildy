unit Tildy.Core.Sources.FpHTTPS;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  openssl,
  opensslsockets,
  Tildy.Core.Sources.FpHTTP;

type


  { TFpHTTPSSource }

  TFpHTTPSSource = class(TFpHTTPSource)
    procedure AfterConstruction; override;
  end;


implementation

{ TFpHTTPSSource }

procedure TFpHTTPSSource.AfterConstruction;
begin
  inherited AfterConstruction;

  InitSSLInterface;
end;

end.

