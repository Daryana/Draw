unit Tool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Figure, Graphics, crt, CTool, DrawScene, WorldPole, UTypes;
type
  { TToolPen }

  TToolPen = class (TTool)
  public
    procedure MouseDown(po: TPoint);  override;
    procedure MouseMove(po: TPoint);  override;
  end;

  { TToolRectangle }

  TToolRectangle = class (TTool)
  public
     procedure MouseDown(po: TPoint);  override;
     procedure MouseMove(po: TPoint);  override;
   end;

  { TToolEllipse }

  TToolEllipse = class (TTool)
   public
     procedure MouseDown(po: TPoint);  override;
     procedure MouseMove(po: TPoint);  override;
  end;

   { TToolLine }

   TToolLine = class (TTool)
   public
     procedure MouseDown(po: TPoint);  override;
     procedure MouseMove(po: TPoint);  override;
   end;


  { TToolRoundRectangle }

  TToolRoundRectangle = class (TTool)
  public
    procedure MouseDown(po: TPoint);  override;
    procedure MouseMove(po: TPoint);  override;
  end;

  { TToolPolyLine }

  TToolPolyLine  = class (TTool)
  public
    procedure MouseDown(po: TPoint);  override;
    procedure MouseMove(po: TPoint);  override;
  end;

  { TToolHand }

  TToolHand = class (TTool)
  private
    t: TFloatPoint;
  public
    procedure MouseDown(po: TPoint);  override;
    procedure MouseMove(po: TPoint);  override;
  end;

  { TToolLoupe }

  TToolLoupe = class (TTool)
  public
    procedure MouseDown(po: TPoint);  override;
    procedure MouseMove(po: TPoint);  override;
  end;

implementation

{ TToolLoupe }

procedure TToolLoupe.MouseDown(po: TPoint);
begin
  Pole.PRect.TopLeft := po;
end;

procedure TToolLoupe.MouseMove(po: TPoint);
begin
  Pole.PRect.BottomRight := po;
end;

{ TToolHand }

procedure TToolHand.MouseDown(po: TPoint);
begin
  t := ToFloatPoint(po);
end;

procedure TToolHand.MouseMove(po: TPoint);
begin
  Pole.Shift += (ToFloatPoint(po) - t) / Pole.Zoom;
  t := ToFloatPoint(po);
end;

{ TToolPolyLine }

procedure TToolPolyLine.MouseDown(po: TPoint);
begin
  if not PLineFlag then
    begin
      SetLength(figures, Length(figures)+1);
      figures[High(figures)] := TPolyLine.Create(po);
      figures[High(figures)].ColorFigure := ColorLine;
      figures[High(figures)].WidthFigure := WidthLine;
      figures[High(figures)].StyleFigure := StyleLine[StyleNumber];
      PLineFlag := true;
    end
    else
    begin
      tpolyline(figures[High(figures)]).FreeLine(po);
    end;
end;

procedure TToolPolyLine.MouseMove(po: TPoint);
begin
  figures[High(figures)].Finish(po);
end;

{ TToolRoundRectangle }

procedure TToolRoundRectangle.MouseDown(po: TPoint);
begin
  SetLength(figures, Length(figures)+1);
  figures[High(figures)] := TRoundRectangle.Create(po);
  figures[High(figures)].ColorFigure := ColorLine;
  figures[High(figures)].WidthFigure := WidthLine;
  figures[High(figures)].StyleFigure := StyleLine[StyleNumber];
  figures[High(figures)].FillFigure := FillBrush[FillNumber];
  figures[High(figures)].ColorFill:= ColorBrush;
  TRoundRectangle(figures[High(figures)]).ConstRound := ConstRectRound;
end;

procedure TToolRoundRectangle.MouseMove(po: TPoint);
begin
  figures[High(figures)].Finish(po);
end;

{ TToolLine }

procedure TToolLine.MouseDown(po: TPoint);
begin
  SetLength(figures, Length(figures)+1);
  figures[High(figures)] := TLine.Create(po);
  figures[High(figures)].ColorFigure := ColorLine;
  figures[High(figures)].WidthFigure := WidthLine;
  figures[High(figures)].StyleFigure := StyleLine[StyleNumber];
end;

procedure TToolLine.MouseMove(po: TPoint);
begin
  figures[High(figures)].Finish(po);
end;

{ TToolEllipse }


procedure TToolEllipse.MouseDown(po: TPoint);
begin
  SetLength(figures, Length(figures)+1);
  figures[High(figures)] := TEllipse.Create(po);
  figures[High(figures)].ColorFigure := ColorLine;
  figures[High(figures)].WidthFigure := WidthLine;
  figures[High(figures)].StyleFigure := StyleLine[StyleNumber];
  figures[High(figures)].FillFigure := FillBrush[FillNumber];
  figures[High(figures)].ColorFill:= ColorBrush;
end;

procedure TToolEllipse.MouseMove(po: TPoint);
begin
  figures[High(figures)].Finish(po);
end;

{ TToolRectangle }


procedure TToolRectangle.MouseDown(po: TPoint);
begin
  SetLength(figures, Length(figures)+1);
  figures[High(figures)] := TRectangle.Create(po);
  figures[High(figures)].ColorFigure := ColorLine;
  figures[High(figures)].WidthFigure := WidthLine;
  figures[High(figures)].StyleFigure := StyleLine[StyleNumber];
  figures[High(figures)].FillFigure := FillBrush[FillNumber];
  figures[High(figures)].ColorFill:= ColorBrush;
end;

procedure TToolRectangle.MouseMove(po: TPoint);
begin
  figures[High(figures)].Finish(po);
end;

{ TToolPen }


procedure TToolPen.MouseDown(po: TPoint);
begin
  SetLength(figures, Length(figures)+1);
  figures[High(figures)] := TPolyLine.Create(po);
  figures[High(figures)].ColorFigure := ColorLine;
  figures[High(figures)].WidthFigure := WidthLine;
  figures[High(figures)].StyleFigure := StyleLine[StyleNumber];
end;

procedure TToolPen.MouseMove(po: TPoint);
begin
  TPolyLine(figures[High(figures)]).FreeLine(po);
end;

initialization
 ConstTool := TCTool.Create;
 ConstTool.ToolRegister(TToolPen);
 ConstTool.ToolRegister(TToolLine);
 ConstTool.ToolRegister(TToolPolyLine);
 ConstTool.ToolRegister(TToolEllipse);
 ConstTool.ToolRegister(TToolRectangle);
 ConstTool.ToolRegister(TToolRoundRectangle);
 ConstTool.ToolRegister(TToolHand);
 ConstTool.ToolRegister(TToolLoupe);
finalization
 ConstTool.free;

end.

