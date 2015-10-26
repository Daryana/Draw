unit WorldPole;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Clipbrd, ExtCtrls, UTypes;
type


  { TPole }

  TPole = class
  public
    function WorldToLocal(po: TFloatPoint): TPoint;
    function LocalToWorld(po: TPoint): TFloatPoint;
  end;
var
  Pole: TPole;
  PoleShift: TPoint;
  PoleZoom: extended;
implementation


function TPole.WorldToLocal(po: TFloatPoint): TPoint;
begin
  Result := Point(Trunc((po.x + PoleShift.x)*PoleZoom), Trunc((po.y + PoleShift.y)*PoleZoom));
end;

function TPole.LocalToWorld(po: TPoint): TFloatPoint;
begin
  Result := ToFloatPoint((po.x - PoleShift.x)/PoleZoom, (po.y - PoleShift.y)/PoleZoom);
end;

end.

