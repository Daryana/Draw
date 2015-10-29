unit UTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes;
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
  function ToFloatRect(p1, p2: TFloatPoint):TFloatRect;
  function ToRect(p1, p2: TPoint):TRect;
  function ToRect(r: TFloatRect):TRect;
  operator +(a, b: TFloatPoint):TFloatPoint;
  operator -(a, b: TFloatPoint):TFloatPoint;
  operator +(a, b: TPoint):TPoint;
  operator -(a, b: TPoint):TPoint;
  operator +(a, b: TFloatRect):TFloatRect;
  operator -(a, b: TFloatRect):TFloatRect;
  operator >(a, b: TFloatPoint):boolean;
  operator <(a, b: TFloatPoint):boolean;
  operator >=(a, b: TFloatPoint):boolean;
  operator <=(a, b: TFloatPoint):boolean;
  operator =(a, b: TFloatPoint):boolean;
  operator /(a: TFloatPoint; b: Extended):TFloatPoint;
  operator *(a: TFloatPoint; b: Extended):TFloatPoint;

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

operator+(a, b: TFloatPoint): TFloatPoint;
begin
  result.x:= a.x + b.x;
  result.y:= a.y + b.y;
end;

operator-(a, b: TFloatPoint): TFloatPoint;
begin
  result.x:= a.x - b.x;
  result.y:= a.y - b.y;
end;

operator+(a, b: TPoint): TPoint;
begin
  result.x:= a.x + b.x;
  result.y:= a.y + b.y;
end;

operator-(a, b: TPoint): TPoint;
begin
  result.x:= a.x - b.x;
  result.x:= a.y - b.y;
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
  result:= (a.x > b.x) and (a.y > b.y);
end;

operator<(a, b: TFloatPoint): boolean;
begin
  result:= (a.x < b.x) and (a.y < b.y);
end;

operator>=(a, b: TFloatPoint): boolean;
begin
  result:= (a.x >= b.x) and (a.y >= b.y);
end;

operator<=(a, b: TFloatPoint): boolean;
begin
  result:= (a.x <= b.x) and (a.y <= b.y);
end;

operator=(a, b: TFloatPoint): boolean;
begin
  result:= (a.x = b.x) and (a.y = b.y);
end;

operator/(a: TFloatPoint; b: Extended): TFloatPoint;
begin
  result.x := a.x / b;
  result.y := a.y / b;
end;

operator*(a: TFloatPoint; b: Extended): TFloatPoint;
begin
  result.x := a.x*b;
  result.y := a.y*b;
end;

end.
