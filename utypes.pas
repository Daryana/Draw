unit UTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, math;
type

  { TFloatPoint }

  TFloatPoint = record
    X, Y: Extended;
  end;

    { TRectFloat }

  TFloatRect = record
    Top, Bottom: TFloatPoint;
  end;


  TFloatPointArr = array of TFloatPoint;

  function ToFloatPoint(p1, p2: Extended): TFloatPoint;
  function ToFloatPoint(p: TPoint): TFloatPoint;
  function ToFloatRect(p1, p2: TFloatPoint): TFloatRect;
  function ToRect(p1, p2: TPoint): TRect;
  function ToRect(r: TFloatRect): TRect;
  function MaxPoint(a, b: TFloatPoint): TFloatPoint;
  function MinPoint(a, b: TFloatPoint): TFloatPoint;
  operator +(a, b: TFloatPoint): TFloatPoint;
  operator -(a, b: TFloatPoint): TFloatPoint;
  operator +(a, b: TPoint): TPoint;
  operator -(a, b: TPoint): TPoint;
  operator *(a: TPoint; b: Extended): TFloatPoint;
  operator /(a: TPoint; b: Extended): TFloatPoint;
  operator +(a, b: TFloatRect): TFloatRect;
  operator -(a, b: TFloatRect): TFloatRect;
  operator >(a, b: TFloatPoint): boolean;
  operator <(a, b: TFloatPoint): boolean;
  operator >=(a, b: TFloatPoint): boolean;
  operator <=(a, b: TFloatPoint): boolean;
  operator =(a, b: TFloatPoint): boolean;
  operator /(a: TFloatPoint; b: Extended): TFloatPoint;
  operator *(a: TFloatPoint; b: Extended): TFloatPoint;
  function swaprect(a: TFloatRect): TFloatRect;
  function insideFigure(a, b: TFloatRect): boolean;
  function intersectFigure(a, b: TFloatRect): boolean;

  var
    Hist: procedure of Object;

implementation

function ToFloatPoint(p1, p2: Extended): TFloatPoint;
begin
  Result.x := p1;
  Result.y := p2;
end;

function ToFloatPoint(p: TPoint): TFloatPoint;
begin
  Result.x := p.x;
  Result.y := p.y;
end;

function ToFloatRect(p1, p2: TFloatPoint): TFloatRect;
begin
  Result.Top := p1;
  Result.Bottom := p2;
end;

function ToRect(p1, p2: TPoint): TRect;
begin
  Result.TopLeft := p1;
  Result.BottomRight := p2;
end;

function ToRect(r: TFloatRect): TRect;
begin
  Result.Left := trunc(r.Top.x);
  Result.Top := trunc(r.Top.y);
  Result.Right := trunc(r.Bottom.x);
  Result.Bottom := trunc(r.Bottom.y);
end;

function MaxPoint(a, b: TFloatPoint): TFloatPoint;
begin
  Result.x := max(a.x, b.x);
  Result.y := max(a.y, b.y);
end;

function MinPoint(a, b: TFloatPoint): TFloatPoint;
begin
  Result.x := min(a.x, b.x);
  Result.y := min(a.y, b.y);
end;

operator+(a, b: TFloatPoint): TFloatPoint;
begin
  result.x := a.x + b.x;
  result.y := a.y + b.y;
end;

operator-(a, b: TFloatPoint): TFloatPoint;
begin
  result.x := a.x - b.x;
  result.y := a.y - b.y;
end;

operator+(a, b: TPoint): TPoint;
begin
  result.x := a.x + b.x;
  result.y := a.y + b.y;
end;

operator-(a, b: TPoint): TPoint;
begin
  result.x := a.x - b.x;
  result.y := a.y - b.y;
end;

operator*(a: TPoint; b: Extended): TFloatPoint;
begin
  result.x := a.x * b;
  result.y := a.y * b;
end;

operator/(a: TPoint; b: Extended): TFloatPoint;
begin
  result.x := a.x / b;
  result.y := a.y / b;
end;

operator+(a, b: TFloatRect): TFloatRect;
begin
  result.Top := a.Top + b.Top;
  result.Bottom := a.Bottom + b.Bottom;
end;

operator-(a, b: TFloatRect): TFloatRect;
begin
  result.Top := a.Top - b.Top;
  result.Bottom := a.Bottom - b.Bottom;
end;

operator>(a, b: TFloatPoint): boolean;
begin
  result := (a.x > b.x) and (a.y > b.y);
end;

operator<(a, b: TFloatPoint): boolean;
begin
  result := (a.x < b.x) and (a.y < b.y);
end;

operator>=(a, b: TFloatPoint): boolean;
begin
  result := (a.x >= b.x) and (a.y >= b.y);
end;

operator<=(a, b: TFloatPoint): boolean;
begin
  result := (a.x <= b.x) and (a.y <= b.y);
end;

operator=(a, b: TFloatPoint): boolean;
begin
  result := (a.x = b.x) and (a.y = b.y);
end;

operator/(a: TFloatPoint; b: Extended): TFloatPoint;
begin
  result.x := a.x / b;
  result.y := a.y / b;
end;

operator*(a: TFloatPoint; b: Extended): TFloatPoint;
begin
  result.x := a.x * b;
  result.y := a.y * b;
end;

function swaprect(a: TFloatRect): TFloatRect;
var
  t: Extended;
begin
  if (a.Top.x > a.Bottom.x) then
  begin
    t := a.Top.x;
    a.Top.x := a.Bottom.x;
    a.Bottom.x := t;
  end;
  if (a.Top.y > a.Bottom.y) then
  begin
    t := a.Top.y;
    a.Top.y := a.Bottom.y;
    a.Bottom.y := t;
  end;
  Result := a;
end;

function insideFigure(a, b: TFloatRect): boolean;
begin
  a := swaprect(a);
  b := swaprect(b);
  result := (b.Bottom < (a.Bottom - ToFloatPoint(5, 5))) and (b.Top > (a.Top + ToFloatPoint(5, 5)));
end;

function intersectFigure(a, b: TFloatRect): boolean;
begin
  a := swaprect(a);
  b := swaprect(b);
  result := (b.Top.x > a.Bottom.x + 5) or (b.Bottom.x < a.Top.x - 5) or (b.Top.y > a.Bottom.y + 5) or (b.Bottom.y < a.Top.y - 5);
end;



end.
