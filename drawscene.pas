unit DrawScene;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Figure, Graphics, crt, Graph, UTypes, WorldPole;
type

  { TScene }

  TScene= class
  public
    procedure PBDraw(canvas: TCanvas);
    procedure PBClear();
    procedure Centre(W, H: integer);
end;
 var
   Scene: TScene;
   Figures: array of TFigure;
   WidthLine: Integer;
   DWidthLine: Integer;
   ColorLine: TColor;
   StyleLine: array[0..4] of TPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot);
   StyleNumber: integer;
   FillBrush: array[0..7] of TBrushStyle = (bsClear, bsSolid, bsVertical, bsHorizontal, bsBDiagonal, bsFDiagonal, bsCross, bsDiagCross);
   FillNumber: integer;
   ColorBrush: TColor;
   ConstRectRound: Integer;
implementation

{ TScene }


procedure TScene.PBDraw(canvas: TCanvas);
var
 figure: TFigure;
begin
  for figure in figures do
  begin
    canvas.Pen.Color := figure.ColorFigure;
    canvas.Pen.Width := Figure.WidthFigure + DWidthLine;
    canvas.Pen.Style := Figure.StyleFigure;
    canvas.Brush.Color := figure.ColorFill;
    canvas.Brush.Style := Figure.FillFigure;
    figure.Draw(Canvas);
  end;
end;

procedure TScene.PBClear();
var
  i:integer;
begin
  for i := 0 to High(figures) do figures[i].free;
    SetLength(figures, 0);
end;

procedure TScene.Centre(W, H: integer);
var
  d: Extended;
  p: TRect;
begin
  p.BottomRight := Pole.WorldToLocal(PMax);
  p.TopLeft := Pole.WorldToLocal(PMin);
  if ((P.Bottom <> P.Top) and (P.Left <> P.Right)) then
  begin
    if (W / (p.BottomRight.x - p.TopLeft.x)) < (H / (p.BottomRight.y - p.TopLeft.y)) then
    begin
      d := W / (p.BottomRight.x - p.TopLeft.x);
      if (Pole.Zoom <= 100) then Pole.Zoom *= W / (p.BottomRight.x - p.TopLeft.x);
    end
    else
    begin
      d := H / (p.BottomRight.y - p.TopLeft.y);
      if (Pole.Zoom <= 100) then Pole.Zoom *=  H / (p.BottomRight.y - p.TopLeft.y);
    end;
    Pole.Shift.x += (((w / 2) - ((((p.BottomRight.x - p.TopLeft.x) * d) / 2) + p.TopLeft.x * (d))) / (Pole.Zoom));
    Pole.Shift.y += (((h / 2) - ((((p.BottomRight.y - p.TopLeft.y) * d) / 2) + p.TopLeft.y * (d))) / (Pole.Zoom));
  end;
end;


end.

