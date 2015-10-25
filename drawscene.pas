unit DrawScene;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Figure, Graphics, crt, Graph;
type

  { TScene }

  TScene = class
  public
    procedure PBDraw(canvas: TCanvas);
    procedure PBClear;
  private
end;
 var
   Scene: TScene;
   Figures: array of TFigure;
   ColorLine: TColor;
implementation

{ TScene }

procedure TScene.PBDraw(canvas: TCanvas);
var
 figure: TFigure;
begin
  for figure in figures do
  begin
    canvas.Pen.Color := figure.ColorFigure;
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

