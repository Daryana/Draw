unit WorldPole;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Clipbrd, ExtCtrls, Forms;
type

  { Pole }

  TPole = class
  public
    function Local(x, y: extended; k: TPoint; n: extended): TPoint;
    function World(x: integer; k: integer; n: Extended): Extended;
  end;
var
  Pole: TPole;
  PoleShift: TPoint;
  PoleZoom: extended;
implementation

{ Pole }

function TPole.Local(x, y: extended; k: TPoint; n: Extended): TPoint;
begin
  Result.x := Trunc((x + k.x)*n);
  Result.y := Trunc((y + k.y)*n);
end;

function  TPole.World(x: integer; k: integer; n: Extended): Extended;
begin
  Result := (x - k)/n;
end;

end.

