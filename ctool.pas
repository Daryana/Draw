unit CTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
type
    { TTool }

  TTool = class
  public
    procedure MouseDown(x, y: integer);  virtual; Abstract;
    procedure MouseMove(x, y: integer);  virtual; Abstract;
  end;

  ClassOfTTool = class of TTool;
  ArrOfTTool = array of TTool;

  { TCTool }

  TCTool = class
  private
    arr: array of TTool;
  public
    procedure ToolRegister(t: ClassOfTTool);
    property tool: ArrOfTTool read arr;
  end;

  var
    ConstTool: TCTool;
    PLineFlag: boolean;


implementation

{ TCTool }

procedure TCTool.ToolRegister(t: ClassOfTTool);
begin
  SetLength(arr, Length(arr)+1);
  arr[High(arr)] := t.Create;
end;

end.

