unit Figure;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, crt, Graph, WorldPole;

type
   TFigure = class
   public
     ColorFigure: TColor;
     procedure Draw(canvas: TCanvas); virtual; Abstract;
     procedure Finish(x, y: integer); virtual; Abstract;
   end;

   { TRectangle }

   TRectangle = class(TFigure)
   private
     r: array[1..4] of extended;
   public
     constructor Create(x, y: integer);
     procedure Draw(canvas: TCanvas); override;
     procedure Finish(x, y: integer); override;
   end;

   { TEllipse }

   TEllipse = class(TFigure)
   private
     r: array[1..4] of extended;
   public
     constructor Create(x, y: integer);
     procedure Draw(canvas: TCanvas); override;
     procedure Finish(x, y: integer); override;
   end;


   { TLine }

   TLine = class(TFigure)
   private
     r: array[1..4] of extended;
   public
     constructor Create(x, y: integer);
     procedure Draw(canvas: TCanvas); override;
     procedure Finish(x, y: integer); override;
   end;

   { TRoundRectangle }

   TRoundRectangle = class(TFigure)
   private
     r: array[1..4] of extended;
   const
     ConstRound = 15;
   public
     constructor Create(x, y: integer);
     procedure Draw(canvas: TCanvas); override;
     procedure Finish(x, y: integer); override;
   end;

   { TPolyLine }

   TPolyLine = class(TFigure)
   private
     p: array of Extended;
   public
     constructor Create(x, y: integer);
     procedure Draw(canvas: TCanvas); override;
     procedure Finish(x, y: integer); override;
     procedure FreeLine(x, y: integer);
   end;
implementation

{ TRectangle }

constructor TRectangle.Create(x, y: integer);
begin
  r[1] := Pole.World(x, PoleShift.x, PoleZoom);
  r[2] := Pole.World(y, PoleShift.y, PoleZoom);
  r[3] := Pole.World(x, PoleShift.x, PoleZoom);
  r[4] := Pole.World(y, PoleShift.y, PoleZoom);
end;

procedure TRectangle.Draw(canvas: TCanvas);
begin
  canvas.Rectangle(Pole.Local(r[1], r[2], PoleShift, PoleZoom).x, Pole.Local(r[1], r[2], PoleShift, PoleZoom).y, Pole.Local(r[3], r[4], PoleShift, PoleZoom).x, Pole.Local(r[3], r[4], PoleShift, PoleZoom).y);
end;

procedure TRectangle.Finish(x, y: integer);
begin
  r[3] := Pole.World(x, PoleShift.x, PoleZoom);
  r[4] := Pole.World(y, PoleShift.y, PoleZoom);
end;

{ TEllipse }

constructor TEllipse.Create(x, y: integer);
begin
  r[1] := Pole.World(x, PoleShift.x, PoleZoom);
  r[2] := Pole.World(y, PoleShift.y, PoleZoom);
  r[3] := Pole.World(x, PoleShift.x, PoleZoom);
  r[4] := Pole.World(y, PoleShift.y, PoleZoom);
end;

procedure TEllipse.Draw(canvas: TCanvas);
begin
  canvas.Ellipse(Pole.Local(r[1], r[2], PoleShift, PoleZoom).x, Pole.Local(r[1], r[2], PoleShift, PoleZoom).y, Pole.Local(r[3], r[4], PoleShift, PoleZoom).x, Pole.Local(r[3], r[4], PoleShift, PoleZoom).y);
end;

procedure TEllipse.Finish(x, y: integer);
begin
  r[3] := Pole.World(x, PoleShift.x, PoleZoom);
  r[4] := Pole.World(y, PoleShift.y, PoleZoom);
end;


{ TLine }

constructor TLine.Create(x, y: integer);
begin
  r[1] := Pole.World(x, PoleShift.x, PoleZoom);
  r[2] := Pole.World(y, PoleShift.y, PoleZoom);
  r[3] := Pole.World(x, PoleShift.x, PoleZoom);
  r[4] := Pole.World(y, PoleShift.y, PoleZoom);
end;

procedure TLine.Draw(canvas: TCanvas);
begin
  canvas.Line(Pole.Local(r[1], r[2], PoleShift, PoleZoom), Pole.Local(r[3], r[4], PoleShift, PoleZoom));
end;

procedure TLine.Finish(x, y: integer);
begin
  r[3] := Pole.World(x, PoleShift.x, PoleZoom);
  r[4] := Pole.World(y, PoleShift.y, PoleZoom);
end;

{ TRoundRectangle }

constructor TRoundRectangle.Create(x, y: integer);
begin
  r[1] := Pole.World(x, PoleShift.x, PoleZoom);
  r[2] := Pole.World(y, PoleShift.y, PoleZoom);
  r[3] := Pole.World(x, PoleShift.x, PoleZoom);
  r[4] := Pole.World(y, PoleShift.y, PoleZoom);
end;

procedure TRoundRectangle.Draw(canvas: TCanvas);
begin
  canvas.RoundRect(Pole.Local(r[1], r[2], PoleShift, PoleZoom).x, Pole.Local(r[1], r[2], PoleShift, PoleZoom).y, Pole.Local(r[3], r[4], PoleShift, PoleZoom).x, Pole.Local(r[3], r[4], PoleShift, PoleZoom).y, ConstRound, ConstRound);
end;

procedure TRoundRectangle.Finish(x, y: integer);
begin
  r[3] := Pole.World(x, PoleShift.x, PoleZoom);
  r[4] := Pole.World(y, PoleShift.y, PoleZoom);
end;

{ TPolyLine }

constructor TPolyLine.Create(x, y: integer);
begin
  SetLength(p, 4);
  p[0] := Pole.World(x, PoleShift.x, PoleZoom);
  p[1] := Pole.World(y, PoleShift.y, PoleZoom);
  p[2] := Pole.World(x, PoleShift.x, PoleZoom);
  p[3] := Pole.World(y, PoleShift.y, PoleZoom);
end;

procedure TPolyLine.Draw(canvas: TCanvas);
var
  i: integer;
begin
  for i := 0 to High(p) - 3 do
    if (i mod 2 = 0) then
    begin
      canvas.Line(Pole.Local(p[i], p[i + 1], PoleShift, PoleZoom), Pole.Local(p[i + 2], p[i + 3], PoleShift, PoleZoom));
    end;
end;

procedure TPolyLine.Finish(x, y: integer);
begin
  p[High(p) - 1] := Pole.World(x, PoleShift.x, PoleZoom);
  p[High(p)] := Pole.World(y, PoleShift.y, PoleZoom);
end;

procedure TPolyLine.FreeLine(x, y: integer);
begin
  SetLength(p, Length(p) + 2);
  p[High(p) - 1] := Pole.World(x, PoleShift.x, PoleZoom);
  p[High(p)] := Pole.World(y, PoleShift.y, PoleZoom);
end;

end.

