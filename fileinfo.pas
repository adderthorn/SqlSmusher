unit fileinfo;

{$mode objfpc}{$H+}
{$MACRO ON}

interface

uses
  Classes, SysUtils;

{$IF defined(Windows)}
{$Define NotPosix}
{$ENDIF}

type
 
  TFileInfo = class(TObject)
  private
    FFilePath: string;
    function GetFileName: string;
    procedure SetFilePath(FilePath: string);
  public
    property FilePath: string read FFilePath write SetFilePath;
    property FileName: string read GetFileName;
    constructor Create(AFilePath: string);
  end;

const
  SEP_CHAR = {$IFDEF NotPosix} '\' {$ELSE} '/'{$ENDIF};

implementation

  procedure TFileInfo.SetFilePath(FilePath: string);
  begin
    FFilePath:=FilePath;
  end;

  function TFileInfo.GetFileName: string;
  begin
    result:=FFilePath.Substring(FFilePath.LastIndexOf(SEP_CHAR) + 1);
  end;

  constructor TFileInfo.Create(AFilePath: string);
  begin
    FFilePath:=AFilePath;
  end;

end.
