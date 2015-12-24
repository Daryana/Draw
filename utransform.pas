unit UTransform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Clipbrd, ExtCtrls, UTypes;
type


  { TTransform }

  TTransform = class
  public
    Shift: TFloatPoint;
    Zoom: extended;
    PRect: TRect;
    constructor Create(w, h: Extended);
    procedure PluseLoupe;
    procedure MinuseLoupe;
    procedure loupeRect(w, h: integer; RRect: TRect);
    function WorldToLocal(po: TFloatPoint): TPoint;
    function LocalToWorld(po: TPoint): TFloatPoint;
  end;
var
  Field: TTransform;
  MouseRect: TFloatRect;

implementation

constructor TTransform.Create(w, h: Extended);
begin
  Shift := ToFloatPoint(0, 0);
  Zoom := 1;
end;

procedure TTransform.PluseLoupe;
begin
  if (Zoom <= 100) then
    if Zoom >= 1 then
        Zoom := Zoom + 1
    else
      Zoom := Zoom * 2;
end;

procedure TTransform.MinuseLoupe;
begin
  if Zoom >= 0.125 then
    if Zoom > 1 then
      Zoom := Zoom - 1
    else
      Zoom := Zoom / 2;
end;

procedure TTransform.loupeRect(w, h: integer; RRect: TRect);
var
  t: integer;
  d: Extended;
begin
  if RRect.BottomRight.x < RRect.TopLeft.x then
    begin
      t :=  RRect.BottomRight.x;
      RRect.BottomRight.x := RRect.TopLeft.x;
      RRect.TopLeft.x := t;
    end;
    if RRect.BottomRight.y < RRect.TopLeft.y then
    begin
      t :=  RRect.BottomRight.y;
      RRect.BottomRight.y := RRect.TopLeft.y;
      RRect.TopLeft.y := t;
    end;
  if ((RRect.Bottom <> RRect.Top) and (RRect.Left <> RRect.Right)) then
  begin
    if (W / (RRect.BottomRight.x - RRect.TopLeft.x)) < (H / (RRect.BottomRight.y - RRect.TopLeft.y)) then
    begin
        d := W / (RRect.BottomRight.x - RRect.TopLeft.x);
    end
    else
    begin
        d :=  H / (RRect.BottomRight.y - RRect.TopLeft.y);
    end;
    if (Zoom <= 100) then Zoom *=  d;
    Shift.x += ((w / 2) - ((((RRect.BottomRight.x - RRect.TopLeft.x) * d) / 2) + RRect.TopLeft.x * d)) / Zoom;
    Shift.y += ((h / 2) - ((((RRect.BottomRight.y - RRect.TopLeft.y) * d) / 2) + RRect.TopLeft.y * d)) / Zoom;
  end;
end;

function TTransform.WorldToLocal(po: TFloatPoint): TPoint;
begin
  Result := Point(Trunc((po.x + Field.Shift.x) * Zoom), Trunc((po.y + Field.Shift.y) * Zoom));
end;

function TTransform.LocalToWorld(po: TPoint): TFloatPoint;
begin
  Result := ToFloatPoint((po.x / Field.Zoom) - Field.Shift.x, (po.y / Field.Zoom) - Field.Shift.y);
end;


end.

