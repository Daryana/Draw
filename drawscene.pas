unit DrawScene;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Figure, Graphics, crt;
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

implementation

{ TScene }

procedure TScene.PBDraw(canvas: TCanvas);
var
 figure: TFigure;
begin
for figure in figures do
  figure.Draw(Canvas);
end;

procedure TScene.PBClear();
var
  i:integer;
begin
  for i := 0 to High(figures) do figures[i].free;
  SetLength(figures, 0);
end;

end.

