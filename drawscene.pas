unit DrawScene;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Figure, Graphics, crt, Graph, WorldPole;
type

  { TScene }

  TScene = class
  public
    procedure PBDraw(canvas: TCanvas);
    procedure PBClear();
    procedure PBLoupe(s: string);
end;
 var
   Scene: TScene;
   Figures: array of TFigure;
   WidthLine: Integer;
   ColorLine: TColor;
   StyleLine: array[0..4] of TPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot);
   StyleNumber: integer;
   //FillLine:
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
    canvas.Pen.Width := Figure.WidthFigure;
    canvas.Pen.Style := Figure.StyleFigure;
    figure.Draw(Canvas);
  end;
end;

procedure TScene.PBClear();
var
  i:integer;
begin
  for i := 0 to High(figures) do figures[i].free;
    SetLength(figures, 0);
  PoleZoom := 1;
  PoleShift := Point(0, 0);
end;

procedure TScene.PBLoupe(s: string);
begin
    if (s = '+') and (PoleZoom <= 100) then
      if PoleZoom >= 1 then
        PoleZoom := PoleZoom + 1
      else
        PoleZoom := PoleZoom * 2;
    if (s = '-') and (PoleZoom >= 0.125) then
      if PoleZoom > 1 then
        PoleZoom := PoleZoom - 1
      else
        PoleZoom := PoleZoom / 2;
end;

end.

