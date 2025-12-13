unit Tildy.GUI.i18n;

{$mode objfpc}{$H+}

interface

uses
  Forms, Classes,
  // i18n
  Tildy.GUI.i18n.Runtime,
  Tildy.GUI.i18n.StrConsts;

  function TranslateFromResource(const ABaseName, ALanguage: string;
    AForm: TCustomForm = nil): Boolean; inline;

  procedure InitDefaultGuiLanguage; inline;

  function SCancel  : String; inline;
  function SApply   : String; inline;
  function SAdd     : String; inline;
  function SZoom    : String; inline;
  function SLanguage: String; inline;
  function SLat     : String; inline;
  function SLon     : String; inline;

var
  DefaultTranslateLanguage: String absolute Tildy.GUI.i18n.Runtime.DefaultTranslateLanguage;

implementation

function TranslateFromResource(const ABaseName, ALanguage: string;
  AForm: TCustomForm = nil): Boolean;
begin
  Result := Tildy.GUI.i18n.Runtime.TranslateFromResource(ABaseName, ALanguage, AForm);
end;

procedure InitDefaultGuiLanguage;
begin
  Tildy.GUI.i18n.Runtime.InitDefaultGuiLanguage;
end;

function SCancel: String;
begin
  Result := Tildy.GUI.i18n.StrConsts.SCancel;
end;

function SApply: String;
begin
  Result := Tildy.GUI.i18n.StrConsts.SApply;
end;

function SAdd: String;
begin
  Result := Tildy.GUI.i18n.StrConsts.SAdd;
end;

function SZoom: String;
begin
  Result := Tildy.GUI.i18n.StrConsts.SZoom;
end;

function SLanguage: String;
begin
  Result := Tildy.GUI.i18n.StrConsts.SLanguage;
end;

function SLat: String;
begin
  Result := Tildy.GUI.i18n.StrConsts.SLat;
end;

function SLon: String;
begin
  Result := Tildy.GUI.i18n.StrConsts.SLon;
end;

end.
