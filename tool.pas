unit Tool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Figure, Graphics, crt, CTool, DrawScene;
type
  { TToolPen }

  TToolPen = class (TTool)
  public
    procedure MouseDown(po:TPoint);  override;
    procedure MouseMove(po:TPoint);  override;
  end;

  { TToolRectangle }

  TToolRectangle = class (TTool)
  public
     procedure MouseDown(po:TPoint);  override;
     procedure MouseMove(po:TPoint);  override;
   end;

  { TToolEllipse }

  TToolEllipse = class (TTool)
   public
     procedure MouseDown(po:TPoint);  override;
     procedure MouseMove(po:TPoint);  override;
  end;

   { TToolLine }

   TToolLine = class (TTool)
   public
     procedure MouseDown(po:TPoint);  override;
     procedure MouseMove(po:TPoint);  override;
   end;


  { TToolRoundRectangle }

  TToolRoundRectangle = class (TTool)
  public
    procedure MouseDown(po:TPoint);  override;
    procedure MouseMove(po:TPoint);  override;
  end;

  { TToolPolyLine }

  TToolPolyLine  = class (TTool)
  public
    procedure MouseDown(po:TPoint);  override;
    procedure MouseMove(po:TPoint);  override;
  end;

implementation

{ TToolPolyLine }

procedure TToolPolyLine.MouseDown(po: TPoint);
begin
  if not PLineFlag then
    begin
      SetLength(figures, Length(figures)+1);
      figures[High(figures)] := TPolyLine.Create(Po);
      PLineFlag := true;
    end
    else
    begin
      tpolyline(figures[High(figures)]).FreeLine(Po);
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
finalization
 ConstTool.free;

end.

