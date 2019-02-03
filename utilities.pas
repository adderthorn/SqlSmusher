unit Utilities;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
    TAppSettings = record
    GoText, ExternalApp, FilterExt: string;
    ExecuteOption: integer;
    IncludeLineBreak, FilterFolder: boolean;
  end;

implementation

end.

