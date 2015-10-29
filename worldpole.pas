unit WorldPole;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Clipbrd, ExtCtrls, UTypes;
type


  { TPole }

  TPole = class
  public
    Shift: TFloatPoint;
    Zoom: extended;
    PRect: TRect;
    constructor Create(w, h: Extended);
    procedure PorMLoupe(s: string);
    procedure loupeRect(w, h: integer);
    function WorldToLocal(po: TFloatPoint): TPoint;
    function LocalToWorld(po: TPoint): TFloatPoint;
  end;
var
  Pole: TPole;

implementation

constructor TPole.Create(w, h: Extended);
begin
  Shift := ToFloatPoint(0, 0);
  Zoom := 1;
end;
procedure TPole.PorMLoupe(s: string);
begin
  if (s = '+') and (Pole.Zoom <= 100) then
    if Pole.Zoom >= 1 then
      Pole.Zoom := Pole.Zoom + 1
    else
      Pole.Zoom := Pole.Zoom * 2;
  if (s = '-') and (Pole.Zoom >= 0.125) then
    if Pole.Zoom > 1 then
      Pole.Zoom := Pole.Zoom - 1
    else
      Pole.Zoom := Pole.Zoom / 2;
end;

procedure TPole.loupeRect(w, h: integer);
var
  t: integer;
  d: Extended;
begin
  if Pole.PRect.BottomRight.x < Pole.PRect.TopLeft.x then
    begin
      t :=  Pole.PRect.BottomRight.x;
      Pole.PRect.BottomRight.x := Pole.PRect.TopLeft.x;
      Pole.PRect.TopLeft.x := t;
    end;
    if Pole.PRect.BottomRight.y < Pole.PRect.TopLeft.y then
    begin
      t :=  Pole.PRect.BottomRight.y;
      Pole.PRect.BottomRight.y := Pole.PRect.TopLeft.y;
      Pole.PRect.TopLeft.y := t;
    end;
  if ((Pole.PRect.Bottom <> Pole.PRect.Top) and (Pole.PRect.Left <> Pole.PRect.Right)) then
  begin
    if (W / (Pole.PRect.BottomRight.x - Pole.PRect.TopLeft.x)) < (H / (Pole.PRect.BottomRight.y - Pole.PRect.TopLeft.y)) then
    begin
        d := W / (Pole.PRect.BottomRight.x - Pole.PRect.TopLeft.x);
        if (Pole.Zoom <= 100) then Pole.Zoom *= W / (Pole.PRect.BottomRight.x - Pole.PRect.TopLeft.x);
        Pole.Shift.x += (-1) * (PRect.TopLeft.x);
        //Pole.Shift.y += (((h - (PRect.BottomRight.y - PRect.TopLeft.y) * Pole.Zoom) / 2) - PRect.TopLeft.y * Pole.Zoom) / Pole.Zoom;
        Pole.Shift.y += (-1) * (PRect.TopLeft.y - ((H / (2 * d)) - ((PRect.BottomRight.y - PRect.TopLeft.y) / 2)));
    end
    else
    begin
        d := W / (Pole.PRect.BottomRight.y - Pole.PRect.TopLeft.y);
        if (Pole.Zoom <= 100) then Pole.Zoom *=  H / (Pole.PRect.BottomRight.y - Pole.PRect.TopLeft.y);
        Pole.Shift.y += (-1) * (PRect.TopLeft.y);
        //Pole.Shift.x += (((w - (PRect.BottomRight.x - PRect.TopLeft.x) * Pole.Zoom) / 2) - PRect.TopLeft.x * Pole.Zoom) / Pole.Zoom;
        Pole.Shift.x += (-1) * (PRect.TopLeft.x - ((W / (2 * d)) - ((PRect.BottomRight.x - PRect.TopLeft.x) / 2)));
    end;
    end;
    //Pole.Shift.x += (((w / 2) - ((((Pole.PRect.BottomRight.x - Pole.PRect.TopLeft.x) * Pole.Zoom) / 2) + Pole.PRect.TopLeft.x * (Pole.Zoom))) / (Pole.Zoom));
    //Pole.Shift.y += (((h / 2) - ((((Pole.PRect.BottomRight.y - Pole.PRect.TopLeft.y) * Pole.Zoom) / 2) + Pole.PRect.TopLeft.y * (Pole.Zoom))) / (Pole.Zoom));
  end;

function TPole.WorldToLocal(po: TFloatPoint): TPoint;
begin
  Result := Point(Trunc((po.x + Pole.Shift.x) * Pole.Zoom), Trunc((po.y + Pole.Shift.y) * Pole.Zoom));
end;

function TPole.LocalToWorld(po: TPoint): TFloatPoint;
begin
  Result := ToFloatPoint((po.x / Pole.Zoom) - Pole.Shift.x, (po.y / Pole.Zoom) - Pole.Shift.y);
end;


end.

