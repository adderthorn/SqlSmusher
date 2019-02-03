unit OptionsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ButtonPanel, StdCtrls, ActnList, Buttons, Utilities;

type

  { TFormOptions }

  TFormOptions = class(TForm)
    BtnBrowse: TBitBtn;
    CheckBoxFilter: TCheckBox;
    CheckBoxLineBreak: TCheckBox;
    ComboBoxFilter: TComboBox;
    EditExternalApp: TEdit;
    FileClose: TAction;
    FileSave: TAction;
    FileReset: TAction;
    ActionList1: TActionList;
    ButtonPanel1: TButtonPanel;
    ComboBoxSmush: TComboBox;
    EditEndText: TEdit;
    LabelAddFolder: TLabel;
    LabelExternalApp: TLabel;
    LabelSmush: TLabel;
    LabelGoText: TLabel;
    MainMenu: TMainMenu;
    MenuItemSep2: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    MenuItemDel: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemCut: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemClose: TMenuItem;
    MenuItemSep1: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItemReset: TMenuItem;
    MenuItemFile: TMenuItem;
    OpenAppDlg: TOpenDialog;
    PanelMain: TPanel;
    procedure BtnBrowseClick(Sender: TObject);
    procedure CheckBoxFilterChange(Sender: TObject);
    procedure FileCloseExecute(Sender: TObject);
    procedure FileResetExecute(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    AppSettings: TAppSettings;
    procedure SaveSettings;
    constructor Create(AnOwner: TComponent; AnAppSettings: TAppSettings);
      reintroduce;
  end;

var
  FormOptions: TFormOptions;

implementation

{$R *.lfm}

procedure TFormOptions.FileResetExecute(Sender: TObject);
begin
  FormCreate(Sender);
end;

procedure TFormOptions.FileCloseExecute(Sender: TObject);
begin
  Self.Close;
end;

procedure TFormOptions.BtnBrowseClick(Sender: TObject);
begin
  if OpenAppDlg.Execute then
    EditExternalApp.Text:=OpenAppDlg.FileName;
end;

procedure TFormOptions.CheckBoxFilterChange(Sender: TObject);
begin
  ComboBoxFilter.Enabled:=TCheckBox(Sender).Checked;
end;

procedure TFormOptions.FileSaveExecute(Sender: TObject);
begin
  SaveSettings;
  Self.Close;
end;

procedure TFormOptions.FormCreate(Sender: TObject);
begin
  with AppSettings do
  begin
    EditEndText.Text:=GoText;
    ComboBoxSmush.ItemIndex:=ExecuteOption;
    CheckBoxLineBreak.Checked:=IncludeLineBreak;
    EditExternalApp.Text:=ExternalApp;
    CheckBoxFilter.Checked:=FilterFolder;
    ComboBoxFilter.Text:=FilterExt;
  end;
  CheckBoxFilterChange(CheckBoxFilter);
end;

procedure TFormOptions.SaveSettings;
begin
  with AppSettings do
  begin
    GoText:=EditEndText.Text;
    ExecuteOption:=ComboBoxSmush.ItemIndex;
    IncludeLineBreak:=CheckBoxLineBreak.Checked;
    ExternalApp:=EditExternalApp.Text;
    FilterFolder:=CheckBoxFilter.Checked;
    FilterExt:=ComboBoxFilter.Text;
  end;
end;

constructor TFormOptions.Create(AnOwner: TComponent;
  AnAppSettings: TAppSettings);
begin
  inherited Create(AnOwner);
  AppSettings:=AnAppSettings;
end;

end.

