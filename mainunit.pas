unit MainUnit;

{$mode objfpc}{$H+}
{$MACRO ON}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, StdActns, ComCtrls, ExtCtrls, Buttons, StdCtrls, Clipbrd,
  OptionsUnit, IniFiles, Utilities, fileinfo, About, Editor;

{$IF defined(Windows)}
{$Define NotPosix}
{$ENDIF}

type

  { TFormMain }

  TFormMain = class(TForm)
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemView: TMenuItem;
    SaveDialog: TSaveDialog;
    ScrollBox1: TScrollBox;
    ViewFileOnly: TAction;
    ViewFullPath: TAction;
    FileSmush: TAction;
    FileRemoveAll: TAction;
    FileMoveDown: TAction;
    FileMoveUp: TAction;
    BtnExit: TBitBtn;
    BtnAddFiles: TBitBtn;
    BtnAddFolder: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BtnRemoveFile: TBitBtn;
    BtnRemoveAll: TBitBtn;
    BtnSmush: TBitBtn;
    FileRemove: TAction;
    HelpChangelog: TAction;
    HelpAbout: TAction;
    ListBoxFiles: TListBox;
    MenuItemSep7: TMenuItem;
    MenuItemSmush: TMenuItem;
    MenuItemSep6: TMenuItem;
    MenuItemRemoveAllFiles: TMenuItem;
    MenuItemSep5: TMenuItem;
    MenuItemMoveDown: TMenuItem;
    MenuItemMoveUp: TMenuItem;
    MenuItemMoveFile: TMenuItem;
    MenuItemContextDel: TMenuItem;
    MenuItemContextPaste: TMenuItem;
    MenuItemContextCopy: TMenuItem;
    MenuItemContextCut: TMenuItem;
    MenuItemRemoveFile: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemSep4: TMenuItem;
    MenuItemChangelog: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemToolsOptions: TMenuItem;
    MenuItemTools: TMenuItem;
    MenuItemDel: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemCut: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemSep1: TMenuItem;
    MenuItemAddFolder: TMenuItem;
    MenuItemAddFiles: TMenuItem;
    OpenFileDlg: TOpenDialog;
    PanelUpDown: TPanel;
    PanelLeft: TPanel;
    PanelRight: TPanel;
    PopupMenuMain: TPopupMenu;
    OpenDirDlg: TSelectDirectoryDialog;
    StatusBar: TStatusBar;
    ToolsOptions: TAction;
    EditCopy: TEditCopy;
    EditCut: TEditCut;
    EditDelete: TEditDelete;
    EditPaste: TEditPaste;
    EditSelectAll: TEditSelectAll;
    FileExit: TAction;
    FileAddFolders: TAction;
    FileAddFiles: TAction;
    FileNew: TAction;
    ActionList: TActionList;
    MainMenu: TMainMenu;
    MenuItemNew: TMenuItem;
    MenuItemFile: TMenuItem;
    procedure EditCutExecute(Sender: TObject);
    procedure EditCopyExecute(Sender: TObject);
    procedure EditDeleteExecute(Sender: TObject);
    procedure EditUpdate(Sender: TObject);
    procedure EditDelUpdate(Sender: TObject);
    procedure FileAddFilesExecute(Sender: TObject);
    procedure FileAddFoldersExecute(Sender: TObject);
    procedure FileExitExecute(Sender: TObject);
    procedure FileMoveDownExecute(Sender: TObject);
    procedure FileMoveUpExecute(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FileRemoveAllExecute(Sender: TObject);
    procedure FileRemoveExecute(Sender: TObject);
    procedure FileSmushExecute(Sender: TObject);
    procedure FileSmushUpdate(Sender: TObject);
    procedure FileUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure ToolsOptionsExecute(Sender: TObject);
    procedure ViewPathExecute(Sender: TObject);
  private
    procedure ClearAllFiles;
    procedure MoveAndSelect(NewIndex: integer);
    procedure WriteFile(FileName: string; FileText: string);
    function CombineFiles(ListBox: TListBox): string;
  public
    AppSettings: TAppSettings;
  end;

const
  NO_FILES = 'No Files Added...';
  SEP_CHAR = {$IFDEF NotPosix} '\' {$ELSE} '/'{$ENDIF};
  INI_FILE = 'settings.ini';
  INI_SECT = 'Settings';
  INI_GOTEXT = 'StatementEndText';
  INI_LINE_BREAK = 'StatementEndLineBreak';
  INI_EXECUTE = 'ExecuteOption';
  INI_EXTERNAL_APP = 'ExternalApplication';
  INI_FILTER = 'FilterAddFolderByType';
  INI_FILTER_EXT = 'FilterAddFolderExtension';
  DEF_GOTEXT = 'GO';
  DEF_FILTER = '*.sql';

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FileNewExecute(Sender: TObject);
begin
  ShowMessage('Hello!');
end;

procedure TFormMain.FileExitExecute(Sender: TObject);
begin
  Self.Close;
end;

procedure TFormMain.FileMoveDownExecute(Sender: TObject);
var
  NewIndex: integer;
begin
  NewIndex:=ListBoxFiles.ItemIndex + 1;
  if NewIndex >= ListBoxFiles.Items.Count then exit;
  MoveAndSelect(NewIndex);
end;

procedure TFormMain.FileMoveUpExecute(Sender: TObject);
var
  NewIndex: integer;
begin
  NewIndex:=ListBoxFiles.ItemIndex - 1;
  if NewIndex = -1 then exit;
  MoveAndSelect(NewIndex);
end;

procedure TFormMain.FileAddFilesExecute(Sender: TObject);
var
  i: integer;
  FileInfo: TFileInfo;
begin
  if OpenFileDlg.Execute then
  begin
    if ListBoxFiles.Items.IndexOf(NO_FILES) > -1 then
    begin
      ListBoxFiles.Items.Delete(0);
    end;
    for i:=0 to OpenFileDlg.Files.Count - 1 do
    begin
      FileInfo:=TFileInfo.Create(OpenFileDlg.Files[i]);
      ListBoxFiles.AddItem(FileInfo.FilePath, FileInfo);
    end;
  end;
end;

procedure TFormMain.EditCutExecute(Sender: TObject);
begin
  EditCopyExecute(Sender);
  ListBoxFiles.Items.Delete(ListBoxFiles.ItemIndex);
end;

procedure TFormMain.EditUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=false;
  if ListBoxFiles.ItemIndex > -1 then
  begin
    TAction(Sender).Enabled:=true;
  end;
end;

procedure TFormMain.EditCopyExecute(Sender: TObject);
begin
  Clipboard.AsText:=ListBoxFiles.GetSelectedText;
end;

procedure TFormMain.EditDeleteExecute(Sender: TObject);
begin
  if (Self.ActiveControl is TListBox) then
  begin
    FileRemoveExecute(Sender);
  end;
end;

procedure TFormMain.EditDelUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=false;
  if (ListBoxFiles.ItemIndex > -1) and
     (ListBoxFiles.GetSelectedText <> NO_FILES) then
  begin
    TAction(Sender).Enabled:=true;
  end;
end;

procedure TFormMain.FileAddFoldersExecute(Sender: TObject);
var
  Sr: TSearchRec;
  Mask, DirPath: string;
  AddedOne: boolean;
  FileInfo: TFileInfo;
begin
  if OpenDirDlg.Execute then
  begin
    Mask:='*.*';
    if AppSettings.FilterFolder then
    begin
      Mask:=AppSettings.FilterExt;
    end;
    DirPath:=OpenDirDlg.FileName.TrimEnd(SEP_CHAR) + SEP_CHAR;
    if FindFirst(DirPath + Mask, faAnyFile, Sr) = 0 then
    begin
      repeat
      begin
        if (Sr.Name <> '.') and (Sr.Name <> '..') then
        begin
          FileInfo:=TFileInfo.Create(DirPath + Sr.Name);
          ListBoxFiles.AddItem(FileInfo.FilePath, FileInfo);
          AddedOne:=true;
        end;
      end;
      until FindNext(sr) <> 0;
      if AddedOne and (ListBoxFiles.Items.IndexOf(NO_FILES) > -1) then
      begin
        ListBoxFiles.Items.Delete(ListBoxFiles.Items.IndexOf(NO_FILES));
      end;
    end;
  end;
end;

procedure TFormMain.FileRemoveAllExecute(Sender: TObject);
begin
  ClearAllFiles;
end;

procedure TFormMain.FileRemoveExecute(Sender: TObject);
var
  NewIndex: integer;
begin
  NewIndex:=ListBoxFiles.ItemIndex;
  ListBoxFiles.Items.Objects[NewIndex].Free;
  ListBoxFiles.Items.Delete(NewIndex);
  if NewIndex >= ListBoxFiles.Items.Count then
    Dec(NewIndex);
  ListBoxFiles.ItemIndex:=NewIndex;
  if ListBoxFiles.Items.Count < 1 then
    ListBoxFiles.AddItem(NO_FILES, nil);
end;

procedure TFormMain.FileSmushExecute(Sender: TObject);
var
  ResultStr, FilePath: string;
  FormEditor: TFormEditor;
begin
  ResultStr:=CombineFiles(ListBoxFiles);
  case AppSettings.ExecuteOption of
    0: // Save File
    begin
      if SaveDialog.Execute then WriteFile(SaveDialog.FileName, ResultStr);
    end;
    1: // Show Editor
    begin
      FormEditor:=TFormEditor.Create(Self, ResultStr);
      FormEditor.Show;
    end;
    2: // Open External
    begin
      FilePath:=GetTempFileName;
      WriteFile(FilePath, ResultStr);
      ExecuteProcess(AppSettings.ExternalApp, FilePath, []);
    end;
  end;
end;

procedure TFormMain.FileSmushUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=(ListBoxFiles.Items.Count > 1);
end;

procedure TFormMain.FileUpdate(Sender: TObject);
begin
  if (ListBoxFiles.GetSelectedText <> '') and
     (ListBoxFiles.GetSelectedText <> NO_FILES) then
  begin
    TAction(Sender).Enabled:=true;
  end
  else
  begin
    TAction(Sender).Enabled:=false;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  Ini: TIniFile;
begin
  Ini:=TIniFile.Create(INI_FILE);
  try
    with AppSettings do
    begin
      GoText:=Ini.ReadString(INI_SECT, INI_GOTEXT, DEF_GOTEXT);
      ExecuteOption:=Ini.ReadInteger(INI_SECT, INI_EXECUTE, 0);
      IncludeLineBreak:=Ini.ReadBool(INI_SECT, INI_LINE_BREAK, true);
      ExternalApp:=Ini.ReadString(INI_SECT, INI_EXTERNAL_APP, '');
      FilterFolder:=Ini.ReadBool(INI_SECT, INI_FILTER, false);
      FilterExt:=Ini.ReadString(INI_SECT, INI_FILTER_EXT, DEF_FILTER);
    end;
  finally
    Ini.Free;
  end;
  ClearAllFiles;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
var
  Ini: TIniFile;
begin
  Ini:=TIniFile.Create(INI_FILE);
  try
    with AppSettings do
    begin
      Ini.WriteString(INI_SECT, INI_GOTEXT, GoText);
      Ini.WriteInteger(INI_SECT, INI_EXECUTE, ExecuteOption);
      Ini.WriteBool(INI_SECT, INI_LINE_BREAK, IncludeLineBreak);
      Ini.WriteString(INI_SECT, INI_EXTERNAL_APP, ExternalApp);
      Ini.WriteBool(INI_SECT, INI_FILTER, FilterFolder);
      Ini.WriteString(INI_SECT, INI_FILTER_EXT, FilterExt);
    end;
  finally
    Ini.Free;
  end;
  ClearAllFiles;
end;

procedure TFormMain.HelpAboutExecute(Sender: TObject);
var
  FormAbout: TFormAbout;
begin
  FormAbout:=TFormAbout.Create(Self);
  FormAbout.ShowModal;
end;

procedure TFormMain.ToolsOptionsExecute(Sender: TObject);
var
  OptionsDlg: TFormOptions;
begin
  OptionsDlg:=TFormOptions.Create(Self, AppSettings);
  OptionsDlg.ShowModal;
  AppSettings:=OptionsDlg.AppSettings;
  FreeAndNil(OptionsDlg);
end;

procedure TFormMain.ViewPathExecute(Sender: TObject);
var
  i: integer;
  NewStr: string;
begin
  if Sender = ViewFullPath then
  begin
    ViewFullPath.Checked:=(not ViewFullPath.Checked);
    ViewFileOnly.Checked:=(not ViewFullPath.Checked);
  end
  else if Sender = ViewFileOnly then
  begin
    ViewFileOnly.Checked:=(not ViewFileOnly.Checked);
    ViewFullPath.Checked:=(not ViewFileOnly.Checked);
  end;
  for i:=0 to ListBoxFiles.Items.Count - 1 do
  begin
    if ListBoxFiles.Items.Strings[i] = NO_FILES then
      continue;
    NewStr:='';
    if ViewFullPath.Checked then
    begin
      NewStr:=TFileInfo(ListBoxFiles.Items.Objects[i]).FilePath;
    end
    else if ViewFileOnly.Checked then
    begin
      NewStr:=TFileInfo(ListBoxFiles.Items.Objects[i]).FileName;
    end;
    ListBoxFiles.Items.Strings[i]:=NewStr;
  end;
end;

procedure TFormMain.ClearAllFiles;
var
  i: integer;
begin
  for i:=0 to ListBoxFiles.Items.Count - 1 do
  begin
    ListBoxFiles.Items.Objects[i].Free;
  end;
  ListBoxFiles.Clear;
  ListBoxFiles.AddItem(NO_FILES, nil);
end;

procedure TFormMain.MoveAndSelect(NewIndex: integer);
begin
  ListBoxFiles.Items.Move(ListBoxFiles.ItemIndex, NewIndex);
  ListBoxFiles.ItemIndex:=NewIndex;
end;

function TFormMain.CombineFiles(ListBox: TListBox): string;
var
  ErrorFiles: TStringList;
  FileStr, TotalStr, OneLine: string;
  TheFile: TextFile;
  i: integer;
begin
  ErrorFiles:=TStringList.Create;
  TotalStr:='';
  for i:=0 to ListBox.Count - 1 do
  begin
    AssignFile(TheFile, TFileInfo(ListBox.Items.Objects[i]).FilePath);
    Reset(TheFile);
    FileStr:='';
    while not Eof(TheFile) do
    begin
      ReadLn(TheFile, OneLine);
      OneLine:=OneLine.Trim;
      if OneLine.EndsWith(AppSettings.GoText, true) then
      begin
        OneLine:=OneLine.Substring(0,
          OneLine.Length - AppSettings.GoText.Length);
      end;
      FileStr+=OneLine+LineEnding;
    end;
    CloseFile(TheFile);
    FileStr:=FileStr.Trim;
    TotalStr+=FileStr;
    if AppSettings.IncludeLineBreak then
    begin
      TotalStr+=LineEnding+LineEnding;
    end;
    TotalStr+=AppSettings.GoText;
    if AppSettings.IncludeLineBreak then
    begin
      TotalStr+=LineEnding+LineEnding;
    end;
  end;
  FreeAndNil(ErrorFiles);
  Result:=TotalStr.Trim;
end;

procedure TFormMain.WriteFile(FileName: string; FileText: string);
var
  FileOut: TextFile;
begin
  AssignFile(FileOut, FileName);
  ReWrite(FileOut);
  Write(FileOut, FileText);
  CloseFile(FileOut);
end;

end.

