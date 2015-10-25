unit Tool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Figure, Graphics, crt, CTool, DrawScene, WorldPole;
type
  { TToolPen }

  TToolPen = class (TTool)
  public
    procedure MouseDown(x, y: integer);  override;
    procedure MouseMove(x, y: integer);  override;
  end;

  { TToolRectangle }

  TToolRectangle = class (TTool)
  public
     procedure MouseDown(x, y: integer);  override;
     procedure MouseMove(x, y: integer);  override;
   end;

  { TToolEllipse }

  TToolEllipse = class (TTool)
   public
     procedure MouseDown(x, y: integer);  override;
     procedure MouseMove(x, y: integer);  override;
  end;

   { TToolLine }

   TToolLine = class (TTool)
   public
     procedure MouseDown(x, y: integer);  override;
     procedure MouseMove(x, y: integer);  override;
   end;


  { TToolRoundRectangle }

  TToolRoundRectangle = class (TTool)
  public
    procedure MouseDown(x, y: integer);  override;
    procedure MouseMove(x, y: integer);  override;
  end;

  { TToolPolyLine }

  TToolPolyLine  = class (TTool)
  public
    procedure MouseDown(x, y: integer);  override;
    procedure MouseMove(x, y: integer);  override;
  end;

  { TToolHand }

  TToolHand = class (TTool)
  private
    tx, ty: Extended;
  public
    procedure MouseDown(x, y: integer);  override;
    procedure MouseMove(x, y: integer);  override;
  end;

  { TToolLoupePlus }

  TToolLoupePlus = class (TTool)
  private
    const
      a = 2;
  public
    procedure MouseDown(x, y: integer);  override;
    procedure MouseMove(x, y: integer);  override;
  end;

{ TToolLoupeMinus }

TToolLoupeMinus = class (TTool)
  private
    const
      a = 2;
  public
    procedure MouseDown(x, y: integer);  override;
    procedure MouseMove(x, y: integer);  override;
  end;

implementation

{ TToolLoupeMinus }

procedure TToolLoupeMinus.MouseDown(x, y: integer);
begin
  PoleZoom := PoleZoom / a;
end;

procedure TToolLoupeMinus.MouseMove(x, y: integer);
begin
end;

{ TToolLoupePlus }

procedure TToolLoupePlus.MouseDown(x, y: integer);
begin
  PoleZoom := PoleZoom * a;
end;

procedure TToolLoupePlus.MouseMove(x, y: integer);
begin
end;

{ TToolHand }

procedure TToolHand.MouseDown(x, y: integer);
begin
  tx := x;
  ty := y;
end;

procedure TToolHand.MouseMove(x, y: integer);
begin
  PoleShift.X += trunc(x - tx);
  PoleShift.Y += trunc(y - ty);
  tx := x;
  ty := y;
end;

{ TToolPolyLine }

procedure TToolPolyLine.MouseDown(x, y: integer);
begin
  if not PLineFlag then
    begin
      SetLength(figures, Length(figures)+1);
      figures[High(figures)] := TPolyLine.Create(x, y);
      figures[High(figures)].ColorFigure := ColorLine;
      PLineFlag := true;
    end
    else
    begin
      tpolyline(figures[High(figures)]).FreeLine(x, y);
    end;
end;

procedure TToolPolyLine.MouseMove(x, y: integer);
begin
  figures[High(figures)].Finish(x, y);
end;

{ TToolRoundRectangle }

procedure TToolRoundRectangle.MouseDown(x, y: integer);
begin
  SetLength(figures, Length(figures)+1);
  figures[High(figures)] := TRoundRectangle.Create(x, y);
  figures[High(figures)].ColorFigure := ColorLine;
end;

procedure TToolRoundRectangle.MouseMove(x, y: integer);
begin
  figures[High(figures)].Finish(x, y);
end;

{ TToolLine }

procedure TToolLine.MouseDown(x, y: integer);
begin
  SetLength(figures, Length(figures)+1);
  figures[High(figures)] := TLine.Create(x, y);
  figures[High(figures)].ColorFigure := ColorLine;
end;

procedure TToolLine.MouseMove(x, y: integer);
begin
  figures[High(figures)].Finish(x, y);
end;

{ TToolEllipse }


procedure TToolEllipse.MouseDown(x, y: integer);
begin
  SetLength(figures, Length(figures)+1);
  figures[High(figures)] := TEllipse.Create(x, y);
  figures[High(figures)].ColorFigure := ColorLine;
end;

procedure TToolEllipse.MouseMove(x, y: integer);
begin
  figures[High(figures)].Finish(x, y);
end;

{ TToolRectangle }


procedure TToolRectangle.MouseDown(x, y: integer);
begin
  SetLength(figures, Length(figures)+1);
  figures[High(figures)] := TRectangle.Create(x, y);
  figures[High(figures)].ColorFigure := ColorLine;
end;

procedure TToolRectangle.MouseMove(x, y: integer);
begin
  figures[High(figures)].Finish(x, y);
end;

{ TToolPen }


procedure TToolPen.MouseDown(x, y: integer);
begin
  SetLength(figures, Length(figures)+1);
  figures[High(figures)] := TPolyLine.Create(x, y);
  figures[High(figures)].ColorFigure := ColorLine;
end;

procedure TToolPen.MouseMove(x, y: integer);
begin
  TPolyLine(figures[High(figures)]).FreeLine(x, y);
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
 ConstTool.ToolRegister(TToolLoupePlus);
 ConstTool.ToolRegister(TToolLoupeMinus);
finalization
 ConstTool.free;

end.

