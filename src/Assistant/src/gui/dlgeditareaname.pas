unit DlgEditAreaName;

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
    procedure FormCreate(Sender: TObject);
  strict private
    FAreaName: String;
    procedure SetAreaName(AAreaName: String);
  public
    property AreaName: String read FAreaName write SetAreaName;
  end;

var
  fEditAreaName: TfEditAreaName;

implementation

uses
  Main;

{$R *.lfm}

{ TfEditAreaName }

procedure TfEditAreaName.edAreaNameChange(Sender: TObject);
begin
  FAreaName := edAreaName.Text;
end;

procedure TfEditAreaName.FormCreate(Sender: TObject);
var
  LImages: TImageList;
begin
  LImages := fMain.ImagesCurrent;
  btnApply.Images := LImages;
  btnClose.Images := LImages;
end;

procedure TfEditAreaName.btnCloseClick(Sender: TObject);
begin
  fEditAreaName.Close;
end;

procedure TfEditAreaName.btnApplyClick(Sender: TObject);
begin
  fEditAreaName.Close;
  ModalResult := mrOK;
end;

procedure TfEditAreaName.SetAreaName(AAreaName: String);
begin
  if FAreaName = AAreaName then Exit;

  FAreaName := AAreaName;
  edAreaName.Text := FAreaName;
end;

end.

