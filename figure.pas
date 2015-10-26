unit Figure;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, crt, Graph, WorldPole, UTypes;

type

   { TFigure }

   TFigure = class
   public
     ColorFigure: TColor;
     WidthFigure: Integer;
     StyleFigure: TPenStyle;
     procedure Draw(canvas: TCanvas); virtual; Abstract;
     procedure Finish(po: TPoint); virtual; Abstract;
   end;

   { TRectangle }

   TRectangle = class(TFigure)
   private
     r: TFloatRect;
   public
     constructor Create(po: TPoint);
     procedure Draw(canvas: TCanvas); override;
     procedure Finish(po: TPoint); override;
   end;

   { TEllipse }

   TEllipse = class(TFigure)
   private
     r: TFloatRect;
   public
     constructor Create(po: TPoint);
     procedure Draw(canvas: TCanvas); override;
     procedure Finish(po: TPoint); override;
   end;


   { TLine }

   TLine = class(TFigure)
   private
     r: TFloatRect;
   public
     constructor Create(po: TPoint);
     procedure Draw(canvas: TCanvas); override;
     procedure Finish(po: TPoint); override;
   end;

   { TRoundRectangle }

   TRoundRectangle = class(TFigure)
   private
     r: TFloatRect;
   public
     ConstRound: integer;
     constructor Create(po: TPoint);
     procedure Draw(canvas: TCanvas); override;
     procedure Finish(po: TPoint); override;
   end;

   { TPolyLine }

   TPolyLine = class(TFigure)
   private
     p: array of TFloatPoint;
   public
     constructor Create(po: TPoint);
     procedure Draw(canvas: TCanvas); override;
     procedure Finish(po: TPoint); override;
     procedure FreeLine(po: TPoint);
   end;
implementation

{ TRectangle }

constructor TRectangle.Create(po: TPoint);
begin
  r  := ToFloatRect(Pole.LocalToWorld(po), Pole.LocalToWorld(po));
end;

procedure TRectangle.Draw(canvas: TCanvas);
begin
  canvas.Rectangle(ToRect(Pole.WorldToLocal(r.Top), Pole.WorldToLocal(r.Bottom)));
end;

procedure TRectangle.Finish(po: TPoint);
begin
  r.Bottom := Pole.LocalToWorld(po);
end;

{ TEllipse }

constructor TEllipse.Create(po: TPoint);
begin
  r  := ToFloatRect(Pole.LocalToWorld(po), Pole.LocalToWorld(po));
end;

procedure TEllipse.Draw(canvas: TCanvas);
begin
  canvas.Ellipse(ToRect(Pole.WorldToLocal(r.Top), Pole.WorldToLocal(r.Bottom)));
end;

procedure TEllipse.Finish(po: TPoint);
begin
  r.Bottom := Pole.LocalToWorld(po);
end;


{ TLine }

constructor TLine.Create(po: TPoint);
begin
  r  := ToFloatRect(Pole.LocalToWorld(po), Pole.LocalToWorld(po));
end;

procedure TLine.Draw(canvas: TCanvas);
begin
  canvas.Line(Pole.WorldToLocal(r.Top), Pole.WorldToLocal(r.Bottom));
end;

procedure TLine.Finish(po: TPoint);
begin
  r.Bottom := Pole.LocalToWorld(po);
end;

{ TRoundRectangle }

constructor TRoundRectangle.Create(po: TPoint);
begin
  r  := ToFloatRect(Pole.LocalToWorld(po), Pole.LocalToWorld(po));
end;

procedure TRoundRectangle.Draw(canvas: TCanvas);
begin
  canvas.RoundRect(ToRect(Pole.WorldToLocal(r.Top), Pole.WorldToLocal(r.Bottom)), ConstRound, ConstRound);
end;

procedure TRoundRectangle.Finish(po: TPoint);
begin
  r.Bottom := Pole.LocalToWorld(po);
end;

{ TPolyLine }

constructor TPolyLine.Create(po: TPoint);
begin
  SetLength(p, 2);
  p[0] := Pole.LocalToWorld(po);
  p[1] := Pole.LocalToWorld(po);
end;

procedure TPolyLine.Draw(canvas: TCanvas);
var
  i: integer;
begin
  for i := 0 to High(p) - 1 do
    canvas.Line(Pole.WorldToLocal(p[i]), Pole.WorldToLocal(p[i + 1]));
end;

procedure TPolyLine.Finish(po: TPoint);
begin
  p[High(p)] := Pole.LocalToWorld(po);
end;

procedure TPolyLine.FreeLine(po: TPoint);
begin
  SetLength(p, Length(p) + 1);
  p[High(p)] := Pole.LocalToWorld(po);
end;

end.

