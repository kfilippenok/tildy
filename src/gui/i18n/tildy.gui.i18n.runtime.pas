unit Tildy.GUI.i18n.Runtime;

{$mode objfpc}{$H+}

interface

uses
  Forms, Classes, SysUtils, Controls, LResources,
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  // Translate
  LCLTranslator, Translations;

type
  ILocalizableForm = interface
    ['{3C92427E-D847-4FDC-932F-E4733B1625E2}']
    procedure TranslationChanged;
  end;

var
  DefaultGUILanguageCode: String = 'en';

function SetGUILanguageFromResource(const ABaseName, ALanguageCode: string;
  AForm: TCustomForm = nil): Boolean;
procedure SetGUILanguage(const ALanguageCode: string = '');
procedure SetGUILanguageFromSystem;

implementation

function SetGUILanguageFromResource(const ABaseName, ALanguageCode: string;
  AForm: TCustomForm = nil): Boolean;
var
  ResStream  : TResourceStream = nil;
  PoStream   : TStringStream   = nil;
  PoFile     : TPOFile         = nil;
  Translator : TPOTranslator   = nil;
  LocForm    : ILocalizableForm;
  i          : Integer;
  LangToTry  : String;
  IsLangFound    : Boolean         = False;
  IsResTranslated: Boolean         = False;
begin
  Result := False;

  LangToTry := Trim(ALanguageCode);
  if LangToTry.IsEmpty then
    LangToTry := DefaultGUILanguageCode;

  try
    try
      ResStream := TResourceStream.Create(HInstance,
        ABaseName + '.' + LangToTry, RT_RCDATA);
      IsLangFound := True;
    except
      FreeAndNil(ResStream);
      ResStream := TResourceStream.Create(HInstance,
        ABaseName + '.' + DefaultGUILanguageCode, RT_RCDATA);
    end;

    PoStream := TStringStream.Create('');
    ResStream.SaveToStream(PoStream);

    PoFile := TPOFile.Create(False);
    PoFile.ReadPOText(PoStream.DataString);

    IsResTranslated := TranslateResourceStrings(PoFile);

    if (not IsResTranslated and not IsLangFound) then
      Exception.Create(String.Empty);

    Translator := TPOTranslator.Create(PoFile);
    if Assigned(LRSTranslator) then
      FreeAndNil(LRSTranslator);
    LRSTranslator := Translator;

    if Assigned(AForm) then
      Translator.UpdateTranslation(AForm)
    else
    begin
      for i := 0 to Screen.CustomFormCount - 1 do
      begin
        Translator.UpdateTranslation(Screen.CustomForms[i]);
        if Supports(Screen.CustomForms[I], ILocalizableForm, LocForm) then
          LocForm.TranslationChanged;
      end;
      for i := 0 to Screen.DataModuleCount - 1 do
        Translator.UpdateTranslation(Screen.DataModules[i]);
    end;

    Result := True;
  finally
    if Translator = nil then
    begin
      FreeAndNil(PoFile);
      if Assigned(LRSTranslator) then
      begin
        LRSTranslator.Free;
        LRSTranslator := nil;
      end;
    end;

    FreeAndNil(PoStream);
    FreeAndNil(ResStream);
  end;
end;

procedure SetGUILanguage(const ALanguageCode: string);
begin
  if ALanguageCode.IsEmpty then
    SetGUILanguageFromResource('tildy_gui', DefaultGUILanguageCode)
  else
    SetGUILanguageFromResource('tildy_gui', ALanguageCode);
end;

procedure SetGUILanguageFromSystem;
begin
  SetGUILanguage(GetLanguageID.LanguageCode);
end;

end.
