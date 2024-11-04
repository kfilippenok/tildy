{
  Copyright (c) 2024 Kirill Filippenok

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License. }

unit TilesDownload.Exceptions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TilesDownload.Types;

type

  EMissingOptions = class(Exception)
    constructor Create(AOptions: TOptions); overload;
  end;

  ETileDownload = class(Exception);
  ETDReceive     = class(ETileDownload);
  ETDResample    = class(ETileDownload);
  ETDGetFileName = class(ETileDownload);
  ETDSave        = class(ETileDownload);

implementation

  constructor EMissingOptions.Create(AOptions: TOptions);
  var LMsg: String;
      LOption: TOptionKind;
  begin
    LMsg := 'Options: ';
    for LOption in AOptions do
    begin
      LMsg := LMsg + Format('-%s ', [getOptionName(LOption)]);
    end;
    LMsg := LMsg + 'are missing';

    inherited Create(LMsg);
  end;

end.

