unit DrawScene;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Figure, Graphics, crt, Graph, UTypes;
type

  { TScene }

  TScene= class
  public
    procedure PBDraw(canvas: TCanvas);
    procedure PBClear();
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


end.

