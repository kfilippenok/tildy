unit Tildy.GUI.Dialogs.EditAreaName;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TfEditAreaName }

  TfEditAreaName = class(TForm)
    edAreaName: TEdit;
    btnApply: TSpeedButton;
    btnClose: TSpeedButton;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure edAreaNameChange(Sender: TObject);
  strict private
    FAreaName: String;
    procedure SetAreaName(AAreaName: String);
  public
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

procedure TfEditAreaName.btnCloseClick(Sender: TObject);
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

end.

