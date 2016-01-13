unit Tool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Figure, Graphics, crt, UTransform, UTypes, EditCreate, DrawScene;
type

  { TTool }

  TTool = class
  private
    WFigure: TFigure;
  public
    function CreateParamObj:TPersistent; virtual;
    procedure MouseDown(po: TPoint);  virtual; Abstract;
    procedure MouseMove(po: TPoint);  virtual; Abstract;
    procedure MouseUp(PB: TPoint); virtual;
  end;

  { TToolPen }

  TToolPen = class (TTool)
  public
    function CreateParamObj:TPersistent;  override;
    procedure MouseDown(po: TPoint);  override;
    procedure MouseMove(po: TPoint);  override;
    procedure MouseUp(PB: TPoint); override;
  end;

  { TToolRectangle }

  TToolRectangle = class (TTool)
  public
    function CreateParamObj:TPersistent;  override;
    procedure MouseDown(po: TPoint);  override;
    procedure MouseMove(po: TPoint);  override;
    procedure MouseUp(PB: TPoint); override;
  end;

  { TToolEllipse }

  TToolEllipse = class (TTool)
   public
     function CreateParamObj:TPersistent;  override;
     procedure MouseDown(po: TPoint);  override;
     procedure MouseMove(po: TPoint);  override;
     procedure MouseUp(PB: TPoint); override;
  end;

   { TToolLine }

   TToolLine = class (TTool)
   public
     function CreateParamObj:TPersistent;  override;
     procedure MouseDown(po: TPoint);  override;
     procedure MouseMove(po: TPoint);  override;
     procedure MouseUp(PB: TPoint); override;
   end;


  { TToolRoundRectangle }

  TToolRoundRectangle = class (TTool)
  public
    function CreateParamObj:TPersistent;  override;
    procedure MouseDown(po: TPoint);  override;
    procedure MouseMove(po: TPoint);  override;
    procedure MouseUp(PB: TPoint); override;
  end;

  { TToolPolyLine }

  TToolPolyLine  = class (TTool)
  public
    function CreateParamObj:TPersistent;  override;
    procedure MouseDown(po: TPoint);  override;
    procedure MouseMove(po: TPoint);  override;
    procedure MouseUp(PB: TPoint); override;
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
    procedure MouseUp(PB: TPoint); override;
  end;

  { TToolMouse }

  TToolMouse = class(TTool)
  private
    sh: TFloatRect;
  public
    procedure MouseDown(po: TPoint);  override;
    procedure MouseMove(po: TPoint);  override;
    procedure MouseUp(PB: TPoint); override;
  end;

  { TToolText }

  TToolText = class(TTool)
  public
    function CreateParamObj:TPersistent;  override;
    procedure MouseDown(po: TPoint);  override;
    procedure MouseMove(po: TPoint);  override;
    procedure MouseUp(PB: TPoint); override;
  end;

  ClassOfTTool = class of TTool;
  ArrOfTTool = array of TTool;

  { TCTool }

  TCTool = class
  private
    arr: array of TTool;
  public
    procedure ToolRegister(t: ClassOfTTool);
    property tool: ArrOfTTool read arr;
  end;


var
  ConstTool: TCTool;
  PLineFlag: boolean;
  ShiftFlag: Boolean;
implementation

{ TToolText }

function TToolText.CreateParamObj: TPersistent;
begin
  WFigure := TTextRectengl.Create;
  Result:=inherited CreateParamObj;
end;

procedure TToolText.MouseDown(po: TPoint);
var
  p: TFloatPoint;
begin
  SetLength(figures, Length(figures) + 1);
  figures[High(figures)] := WFigure;
  p := Field.LocalToWorld(po);
  WFigure.Next(p);
end;

procedure TToolText.MouseMove(po: TPoint);
var
  p: TFloatPoint;
begin
  p := Field.LocalToWorld(po);
  WFigure.Finish(p);
end;

procedure TToolText.MouseUp(PB: TPoint);
begin
  inherited MouseUp(PB);
  Scene.SelectEdit;
end;

{ TToolMouse }

procedure TToolMouse.MouseDown(po: TPoint);
var
  figure: TFigure;
begin
  MouseRect.Bottom := Field.LocalToWorld(po);
  MouseRect.Top := Field.LocalToWorld(po);
  if ShiftFlag then
  begin
    sh.Top := Field.LocalToWorld(po);
    sh.Bottom := Field.LocalToWorld(po);
  end
  else
  begin
    for figure in Figures do
      if not figure.Del then
        figure.MouseMark(MouseRect);
  end;
end;

procedure TToolMouse.MouseMove(po: TPoint);
var
  figure: TFigure;
begin
  if ShiftFlag then
  begin
    sh.Bottom := Field.LocalToWorld(po);
    for figure in Figures do
      if figure.isSelected then
      begin
        Figure.FSX -= sh.Top.x - sh.Bottom.x;
        Figure.FSY -= sh.Top.y - sh.Bottom.y;
      end;
    sh.Top := Field.LocalToWorld(po);
  end;
  MouseRect.Bottom := Field.LocalToWorld(po);
end;

procedure TToolMouse.MouseUp(PB: TPoint);
var
  figure: TFigure;
begin
  if not ShiftFlag then
  begin
    for figure in Figures do
      if not figure.Del then
        figure.MouseMark(MouseRect);
    Scene.SelectEdit;
  end;
end;


{ TTool }

function TTool.CreateParamObj: TPersistent;
begin
  Result := TPersistent(WFigure);
end;

procedure TTool.MouseUp(PB: TPoint);
begin
  FigureEditor.EditChoiceOne(CreateParamObj);
end;

{ TCTool }

procedure TCTool.ToolRegister(t: ClassOfTTool);
begin
  SetLength(arr, Length(arr) + 1);
  arr[High(arr)] := t.Create;
end;


{ TToolLoupe }

procedure TToolLoupe.MouseDown(po: TPoint);
begin
  Field.PRect.TopLeft := po;
end;

procedure TToolLoupe.MouseMove(po: TPoint);
begin
  Field.PRect.BottomRight := po;
end;

procedure TToolLoupe.MouseUp(PB: TPoint);
begin
  Field.loupeRect(PB.x, PB.y, Field.PRect);
  actualSave := false;
end;

{ TToolHand }

procedure TToolHand.MouseDown(po: TPoint);
begin
  t := ToFloatPoint(po);
end;

procedure TToolHand.MouseMove(po: TPoint);
begin
  Field.Shift += (ToFloatPoint(po) - t) / Field.Zoom;
  t := ToFloatPoint(po);
end;

{ TToolPolyLine }

function TToolPolyLine.CreateParamObj: TPersistent;
begin
  WFigure := TPolyLine.Create;
  Result := inherited CreateParamObj;
end;

procedure TToolPolyLine.MouseDown(po: TPoint);
var
  p: TFloatPoint;
begin
  p := Field.LocalToWorld(po);
  if not PLineFlag then
    begin
      SetLength(figures, Length(figures) + 1);
      figures[High(figures)] := WFigure;
      WFigure.Next(p);
      PLineFlag := true;
    end
    else
    begin
      tpolyline(WFigure).FreeLine(p);
    end;
end;

procedure TToolPolyLine.MouseMove(po: TPoint);
var
  p: TFloatPoint;
begin
  p := Field.LocalToWorld(po);
  WFigure.Finish(p);
end;

procedure TToolPolyLine.MouseUp(PB: TPoint);
begin
  if not PLineFlag then
    inherited MouseUp(pb);
end;

{ TToolRoundRectangle }

function TToolRoundRectangle.CreateParamObj: TPersistent;
begin
  WFigure := TRoundRectangle.Create;
  Result:=inherited CreateParamObj;
end;

procedure TToolRoundRectangle.MouseDown(po: TPoint);
var
  p: TFloatPoint;
begin
  SetLength(figures, Length(figures) + 1);
  figures[High(figures)] := WFigure;
  p := Field.LocalToWorld(po);
  WFigure.Next(p);
end;

procedure TToolRoundRectangle.MouseMove(po: TPoint);
var
  p: TFloatPoint;
begin
  p := Field.LocalToWorld(po);
  WFigure.Finish(p);
end;

procedure TToolRoundRectangle.MouseUp(PB: TPoint);
begin
  inherited MouseUp(pb);
end;

{ TToolLine }

function TToolLine.CreateParamObj: TPersistent;
begin
  WFigure := TPolyLine.Create;
  Result := inherited CreateParamObj;
end;

procedure TToolLine.MouseDown(po: TPoint);
var
  p: TFloatPoint;
begin
  SetLength(figures, Length(figures) + 1);
  figures[High(figures)] := WFigure;
  p := Field.LocalToWorld(po);
  WFigure.Next(p);
end;

procedure TToolLine.MouseMove(po: TPoint);
var
  p: TFloatPoint;
begin
  p := Field.LocalToWorld(po);
  WFigure.Finish(p);
end;

procedure TToolLine.MouseUp(PB: TPoint);
begin
  inherited MouseUp(pb);
end;

{ TToolEllipse }

function TToolEllipse.CreateParamObj: TPersistent;
begin
  WFigure := TEllipse.Create;
  Result := inherited CreateParamObj;
end;

procedure TToolEllipse.MouseDown(po: TPoint);
var
  p: TFloatPoint;
begin
  SetLength(figures, Length(figures) + 1);
  figures[High(figures)] := WFigure;
  p := Field.LocalToWorld(po);
  WFigure.Next(p);
end;

procedure TToolEllipse.MouseMove(po: TPoint);
var
  p: TFloatPoint;
begin
  p := Field.LocalToWorld(po);
  WFigure.Finish(p);
end;

procedure TToolEllipse.MouseUp(PB: TPoint);
begin
  inherited MouseUp(pb);
end;

{ TToolRectangle }

function TToolRectangle.CreateParamObj: TPersistent;
begin
  WFigure := TRectangle.Create;
  Result := inherited CreateParamObj;
end;

procedure TToolRectangle.MouseDown(po: TPoint);
var
  p: TFloatPoint;
begin
  SetLength(figures, Length(figures) + 1);
  figures[High(figures)] := WFigure;
  p := Field.LocalToWorld(po);
  WFigure.Next(p);
end;

procedure TToolRectangle.MouseMove(po: TPoint);
var
  p: TFloatPoint;
begin
  p := Field.LocalToWorld(po);
  WFigure.Finish(p);
end;

procedure TToolRectangle.MouseUp(PB: TPoint);
begin
  inherited MouseUp(pb);
end;

{ TToolPen }


function TToolPen.CreateParamObj: TPersistent;
begin
  WFigure := TPolyLine.Create;
  Result := inherited CreateParamObj;
end;

procedure TToolPen.MouseDown(po: TPoint);
var
  p: TFloatPoint;
begin
  SetLength(figures, Length(figures) + 1);
  figures[High(figures)] := WFigure;
  p := Field.LocalToWorld(po);
  WFigure.Next(p);
end;

procedure TToolPen.MouseMove(po: TPoint);
var
  p: TFloatPoint;
begin
  p := Field.LocalToWorld(po);
  TPolyLine(WFigure).FreeLine(p);
end;

procedure TToolPen.MouseUp(PB: TPoint);
begin
  inherited MouseUp(pb);
end;

initialization
 ConstTool := TCTool.Create;
 ConstTool.ToolRegister(TToolPen);
 ConstTool.ToolRegister(TToolLine);
 ConstTool.ToolRegister(TToolPolyLine);
 ConstTool.ToolRegister(TToolEllipse);
 ConstTool.ToolRegister(TToolRectangle);
 ConstTool.ToolRegister(TToolRoundRectangle);
 ConstTool.ToolRegister(TToolText);
 ConstTool.ToolRegister(TToolHand);
 ConstTool.ToolRegister(TToolLoupe);
 ConstTool.ToolRegister(TToolMouse);
finalization
 ConstTool.free;

end.

