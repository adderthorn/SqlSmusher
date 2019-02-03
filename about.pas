unit about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ButtonPanel, lclintf;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    ButtonPanel: TButtonPanel;
    LabelWebsite: TLabel;
    LabelAuthor: TLabel;
    LabelVersion: TLabel;
    LabelAppName: TLabel;
    Memo1: TMemo;
    PanelInfo: TPanel;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LabelWebsiteClick(Sender: TObject);
  private

  public

  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.lfm}

{ TFormAbout }

procedure TFormAbout.LabelWebsiteClick(Sender: TObject);
begin
  TLabel(Sender).Font.Color:=clPurple;
  OpenUrl('https://www.noahw.org/');
end;

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  LabelAppName.Caption:=Application.Title;
end;

procedure TFormAbout.CloseButtonClick(Sender: TObject);
begin
  Self.Close;
end;

end.

