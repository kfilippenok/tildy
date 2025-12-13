unit Tildy.GUI.Dialogs.EditAreaName;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  // Translate
  LCLTranslator, Tildy.GUI.i18n.Runtime, Tildy.GUI.i18n.StrConsts;

type

  { TfEditAreaName }

  TfEditAreaName = class(TForm, ILocalizableForm)
    edAreaName: TEdit;
    btnApply: TSpeedButton;
    btnCancel: TSpeedButton;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure edAreaNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  strict private
    FAreaName: String;
    procedure SetAreaName(AAreaName: String);
  public
    procedure ApplyLanguage;
    property AreaName: String read FAreaName write SetAreaName;
  end;

var
  fEditAreaName: TfEditAreaName;

implementation

{$R *.lfm}

{ TfEditAreaName }

procedure TfEditAreaName.edAreaNameChange(Sender: TObject);
begin
  FAreaName := edAreaName.Text;
end;

procedure TfEditAreaName.FormCreate(Sender: TObject);
begin
  ApplyLanguage;
end;

procedure TfEditAreaName.btnCancelClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TfEditAreaName.btnApplyClick(Sender: TObject);
begin
  Self.ModalResult := mrOK;
end;

procedure TfEditAreaName.SetAreaName(AAreaName: String);
begin
  if FAreaName = AAreaName then Exit;

  FAreaName := AAreaName;
  edAreaName.Text := FAreaName;
end;

procedure TfEditAreaName.ApplyLanguage;
begin
  btnApply.Caption  := SAdd;
  btnCancel.Caption := SCancel;
end;

end.

