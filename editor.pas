unit Editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ActnList, StdActns, Menus;

type

  { TFormEditor }

  TFormEditor = class(TForm)
    FontDialog: TFontDialog;
    MenuItemAbout: TMenuItem;
    MenuItemPopupSelectAll: TMenuItem;
    MenuItemPopupSep1: TMenuItem;
    MenuItemPopupCopy: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemDefaultZoom: TMenuItem;
    MenuItemZoomOut: TMenuItem;
    MenuItemZoomIn: TMenuItem;
    MenuItemView: TMenuItem;
    MenuItemFont: TMenuItem;
    MenuItemWordWrap: TMenuItem;
    MenuItemFormat: TMenuItem;
    PopupMenuEditor: TPopupMenu;
    SaveDialog: TSaveDialog;
    ViewZoomDefault: TAction;
    ViewZoomOut: TAction;
    ViewZoomIn: TAction;
    FormatFont: TAction;
    FormatWordWrap: TAction;
    EditCopy: TEditCopy;
    EditSelectAll: TEditSelectAll;
    FileClose: TAction;
    FilePrint: TAction;
    FilePageSetup: TAction;
    FIleSave: TAction;
    ActionListEditor: TActionList;
    MainMenu: TMainMenu;
    MemoDocument: TMemo;
    MenuItemSelectAll: TMenuItem;
    MenuItemEditSep1: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemClose: TMenuItem;
    MenuItemFileSep2: TMenuItem;
    MenuItemPrint: TMenuItem;
    MenuItemPageSetup: TMenuItem;
    MenuItemFileSep1: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItemFile: TMenuItem;
    procedure FileCloseExecute(Sender: TObject);
    procedure FIleSaveExecute(Sender: TObject);
    procedure FormatFontExecute(Sender: TObject);
    procedure FormatWordWrapExecute(Sender: TObject);
    procedure ViewZoomDefaultExecute(Sender: TObject);
    procedure ViewZoomInExecute(Sender: TObject);
    procedure ViewZoomOutExecute(Sender: TObject);
    procedure ViewZoomOutUpdate(Sender: TObject);
  private
    FDocument: string;
    procedure SetDocument(Value: string);
  public
    property Document: string read FDocument write SetDocument;
    constructor Create(AnOwner: TComponent; ADocument: string); overload;
  end;

const
  DEF_FONT_SZ = 10;
  DEF_TITLE = ' - SQL Smusher';

var
  FormEditor: TFormEditor;

implementation

{$R *.lfm}

{ TFormEditor }

procedure TFormEditor.FileCloseExecute(Sender: TObject);
begin
  Self.Close;
end;

procedure TFormEditor.FIleSaveExecute(Sender: TObject);
var
  FileOut: TextFile;
begin
  if SaveDialog.Execute then
  begin
    AssignFile(FileOut, SaveDialog.FileName);
    ReWrite(FileOut);
    Write(FileOut, Document);
    CloseFile(FileOut);
    Self.Caption:=SaveDialog.FileName + DEF_TITLE;
  end;
end;

procedure TFormEditor.FormatFontExecute(Sender: TObject);
begin
  FontDialog.Font:=MemoDocument.Font;
  if FontDialog.Execute then
  begin
    MemoDocument.Font:=FontDialog.Font;
  end;
end;

procedure TFormEditor.FormatWordWrapExecute(Sender: TObject);
begin
  MemoDocument.WordWrap:=TAction(Sender).Checked;
end;

procedure TFormEditor.ViewZoomDefaultExecute(Sender: TObject);
begin
  MemoDocument.Font.Size:=DEF_FONT_SZ;
end;

procedure TFormEditor.ViewZoomInExecute(Sender: TObject);
begin
  MemoDocument.Font.Size:=MemoDocument.Font.Size + 1;
end;

procedure TFormEditor.ViewZoomOutExecute(Sender: TObject);
begin
  MemoDocument.Font.Size:=MemoDocument.Font.Size - 1;
end;

procedure TFormEditor.ViewZoomOutUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=(MemoDocument.Font.Size > 0);
end;

procedure TFormEditor.SetDocument(Value: string);
begin
  FDocument:=Value;
  MemoDocument.Text:=Value;
end;

constructor TFormEditor.Create(AnOwner: TComponent; ADocument: string);
begin
  inherited Create(AnOwner);
  Document:=ADocument;
end;

end.

