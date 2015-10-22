unit Figure;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, crt;

type
   TFigure = class
   public
     procedure Draw(canvas: TCanvas); virtual; Abstract;
     procedure Finish(po: TPoint); virtual; Abstract;
   end;

   { TRectangle }

   TRectangle = class(TFigure)
   private
     r: TRect;
   public
     constructor Create(po: TPoint);
     procedure Draw(canvas: TCanvas); override;
     procedure Finish(po: TPoint); override;
   end;

   { TEllipse }

   TEllipse = class(TFigure)
   private
     r: TRect;
   public
     constructor Create(po: TPoint);
     procedure Draw(canvas: TCanvas); override;
     procedure Finish(po: TPoint); override;
   end;


   { TLine }

   TLine = class(TFigure)
   private
     r: TRect;
   public
     constructor Create(po: TPoint);
     procedure Draw(canvas: TCanvas); override;
     procedure Finish(po: TPoint); override;
   end;

   { TRoundRectangle }

   TRoundRectangle = class(TFigure)
   private
     r: TRect;
   const
     a = 15;
   public
     constructor Create(po: TPoint);
     procedure Draw(canvas: TCanvas); override;
     procedure Finish(po: TPoint); override;
   end;

   { TPolyLine }

   TPolyLine = class(TFigure)
   private
     p: array of TPoint;
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
  r.TopLeft := po;
  r.BottomRight := po;
end;

procedure TRectangle.Draw(canvas: TCanvas);
begin
  canvas.Rectangle(r);
end;

procedure TRectangle.Finish(po: TPoint);
begin
  r.BottomRight := po;
end;

{ TEllipse }

constructor TEllipse.Create(po: TPoint);
begin
  r.TopLeft := po;
  r.BottomRight := po;
end;

procedure TEllipse.Draw(canvas: TCanvas);
begin
  canvas.Ellipse(r);
end;

procedure TEllipse.Finish(po: TPoint);
begin
  r.BottomRight := po;
end;


{ TLine }

constructor TLine.Create(po: TPoint);
begin
  r.TopLeft := po;
  r.BottomRight := po;
end;

procedure TLine.Draw(canvas: TCanvas);
begin
  canvas.Line(r);
end;

procedure TLine.Finish(po: TPoint);
begin
  r.BottomRight := po;
end;

{ TRoundRectangle }

constructor TRoundRectangle.Create(po: TPoint);
begin
  r.TopLeft := po;
  r.BottomRight := po;
end;

procedure TRoundRectangle.Draw(canvas: TCanvas);
begin
  canvas.RoundRect(r, a, a);
end;

procedure TRoundRectangle.Finish(po: TPoint);
begin
  r.BottomRight := po;
end;

{ TPolyLine }

constructor TPolyLine.Create(po: TPoint);
begin
  SetLength(p, 2);
  p[0] := po;
  p[1] := po;
end;

procedure TPolyLine.Draw(canvas: TCanvas);
var
  i: integer;
begin
  for i := 0 to High(p)-1 do
  canvas.Line(p[i], p[i+1]);
end;

procedure TPolyLine.Finish(po: TPoint);
begin
  p[High(p)] := po;
end;

procedure TPolyLine.FreeLine(po: TPoint);
begin
  SetLength(p, Length(p)+1);
  p[High(p)] := po;
end;

end.

