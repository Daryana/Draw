unit Figure;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, Graphics, crt, Graph, UTransform, UTypes, FPCanvas, typinfo, XMLWrite, DOM, Dialogs;

type

   { TFigure }

   TFigure = class(TPersistent)
   private
     FPW: LongInt;
     FPS: TFPPenStyle;
     FPC: TColor;
     CCC: boolean;
     arrStr: TStringList;
     function SVGColor(c: TColor): string;
     procedure SVGStrokParam(Stroke: TDOMElement);
     function StrokeStyleSVG: String;
   public
     StrState: string;
     FSX: Extended;
     FSY: Extended;
     isSelected: Boolean;
     Del: Boolean;
     fmax, fmin: TFloatPoint;
     procedure Next(po: TFloatPoint); virtual; Abstract;
     procedure Draw(canvas: TCanvas); virtual; Abstract;
     procedure DrawM(canvas: TCanvas); virtual; Abstract;
     procedure Finish(po: TFloatPoint); virtual; Abstract;
     procedure MouseMark(t: TFloatRect); virtual; Abstract;
     procedure Save; virtual; Abstract;
     procedure OpenF(s: string); virtual; Abstract;
     procedure ExportSVG(node: TDOMElement; svg: TXMLDocument; minp: TFloatPoint); virtual; Abstract;
   published
     property WidthFigure: integer read FPW write FPW;
     property FPenColor: TColor read FPC write FPC;
   end;


   { TRemRectFigure }

   TRemRectFigure = class(TFigure)
   private
     r: TFloatRect;
     FBS: TFPBrushStyle;
     FBC: TColor;
     procedure SVGFillParam(SVGDoc: TXMLDocument; node, figure: TDOMElement);
     procedure AddPattern(SVGDoc: TXMLDocument; node: TDOMElement);
   public
     procedure Next(po: TFloatPoint); override;
     procedure Draw(canvas: TCanvas); override;
     procedure DrawM(canvas: TCanvas); override;
     procedure Finish(po: TFloatPoint); override;
     procedure MouseMark(t: TFloatRect); override;
     procedure Save; override;
     procedure OpenF(s: string); override;
   published
     property FBrushColor: TColor read FBC write FBC;
   end;

   { TRectangle }

   TRectangle = class(TRemRectFigure)
   public
     constructor Create;
     procedure Draw(canvas: TCanvas); override;
     procedure DrawM(canvas: TCanvas); override;
     procedure Save; override;
     procedure OpenF(s: string); override;
     procedure ExportSVG(node: TDOMElement; svg: TXMLDocument; minp: TFloatPoint); override;
   published
     property FPenStyle: TFPPenStyle read FPS write FPS;
     property FBrushStyle: TFPBrushStyle read FBS write FBS;
   end;

   { TEllipse }

   TEllipse = class(TRemRectFigure)
   public
     constructor Create;
     procedure Draw(canvas: TCanvas); override;
     procedure DrawM(canvas: TCanvas); override;
     procedure save; override;
     procedure OpenF(s: string); override;
     procedure ExportSVG(node: TDOMElement; svg: TXMLDocument; minp: TFloatPoint); override;
   published
     property FPenStyle: TFPPenStyle read FPS write FPS;
     property FBrushStyle: TFPBrushStyle read FBS write FBS;
   end;

   { TRoundRectangle }

   TRoundRectangle = class(TRemRectFigure)
   private
     FRR: LongInt;
   public
     constructor Create;
     procedure Draw(canvas: TCanvas); override;
     procedure DrawM(canvas: TCanvas); override;
     procedure Save; override;
     procedure OpenF(s: string); override;
     procedure ExportSVG(node: TDOMElement; svg: TXMLDocument; minp: TFloatPoint); override;
   published
     property FPenStyle: TFPPenStyle read FPS write FPS;
     property FRoundRect: LongInt read FRR write FRR;
     property FBrushStyle: TFPBrushStyle read FBS write FBS;
   end;

   { TTextRectengl }

   TTextRectengl = class(TRemRectFigure)
   private
     FTR: AnsiString;
     FTS: AnsiString;
   public
     constructor Create;
     procedure Draw(canvas: TCanvas); override;
     procedure DrawM(canvas: TCanvas); override;
     procedure Save; override;
     procedure OpenF(s: string); override;
     procedure ExportSVG(node: TDOMElement; svg: TXMLDocument; minp: TFloatPoint); override;
   published
     property FTextRect: AnsiString read FTR write FTR;
     property FTextName: AnsiString read FTS write FTS;
   end;

   { TPolyLine }

   TPolyLine = class(TFigure)
   private
     p: array of TFloatPoint;
   public
     constructor Create;
     procedure Next(po: TFloatPoint); override;
     procedure Draw(canvas: TCanvas); override;
     procedure DrawM(canvas: TCanvas); override;
     procedure Finish(po: TFloatPoint); override;
     procedure FreeLine(po: TFloatPoint);
     procedure MouseMark(t: TFloatRect); override;
     procedure Save; override;
     procedure OpenF(s: string); override;
     procedure ExportSVG(node: TDOMElement; svg: TXMLDocument; minp: TFloatPoint); override;
   published
     property FPenStyle: TFPPenStyle read FPS write FPS;
   end;

implementation

{ TTextRectengl }

constructor TTextRectengl.Create;
begin
  FPW := 1;
  FPS := psSolid;
  FBS := bsClear;
  FBC := clWhite;
  isSelected := True;
  FTR := ' ';
  FTS := 'Times New Roman';
  Del := False;
end;

procedure TTextRectengl.Draw(canvas: TCanvas);
var
  x, y: integer;
  style: TTextStyle;
begin
  style := Canvas.TextStyle;
  r.Top += ToFloatPoint(FSX, FSY);
  r.Bottom += ToFloatPoint(FSX, FSY);
  FMax := maxPoint(r.Top,  r.Bottom);
  FMin := minPoint(r.Top,  r.Bottom);
  FSX := 0;
  FSY := 0;
  x := Field.WorldToLocal(swaprect(r).Top).x + 3;
  y := Field.WorldToLocal(swaprect(r).Top).y + TextHeight(FTR);
  style.Wordbreak := True;
  style.SingleLine := False;
  style.Clipping := False;
  style.Opaque := True;
  canvas.Brush.Color := FBC;
  canvas.Font.Color := FPC;
  canvas.Font.Size := FPW * 3;
  canvas.Font.Name := FTS;
  canvas.TextRect(ToRect(Field.WorldToLocal(r.Top), Field.WorldToLocal(r.Bottom)), x, y, FTR, style);
end;

procedure TTextRectengl.DrawM(canvas: TCanvas);
begin
  inherited DrawM(canvas);
  canvas.Rectangle(ToRect(Field.WorldToLocal(r.Top), Field.WorldToLocal(r.Bottom)));
end;

procedure TTextRectengl.Save;
begin
  StrState := 'TTextRectangl' + '*';
  inherited Save;
  StrState += FTR + '*';
end;

procedure TTextRectengl.OpenF(s: string);
begin
  inherited OpenF(s);
  FTR := arrStr[10];
  arrStr.Free;
end;

procedure TTextRectengl.ExportSVG(node: TDOMElement; svg: TXMLDocument;
  minp: TFloatPoint);
begin

end;

{ TFigure }

function TFigure.SVGColor(c: TColor): string;
var
  s: string;
begin
  s := IntToHex(ColorToRGB(c), 6);
  Result := '#' + s[5] + s[6] + s[3] + s[4] + s[1] + s[2];
end;

procedure TFigure.SVGStrokParam(Stroke: TDOMElement);
begin
  with Stroke do
  begin
    SetAttribute('stroke', SVGColor(FPC));
    SetAttribute('stroke-width', IntToStr(trunc(FPW * Field.Zoom) + 1));
    SetAttribute('stroke-linecap', 'round');
    if FPS <> psSolid then
    begin
      SetAttribute('stroke-dasharray', StrokeStyleSVG);
    end;
  end;
end;

function TFigure.StrokeStyleSVG: String;
var
  DashL, DashG, DotL, DotG: integer;
begin
  DashL := 2 * FPW + 1 + FPW mod 2;
  DashG := 2 * FPW + 1 - FPW mod 2;
  DotL := 1;
  DotG := 2 * FPW + FPW mod 2;

  case TPenStyle(FPS) of
    psDash:      if FPW = 1 then
                   Result := '18, 6'
                 else
                   Result := Format('%d, %d', [DashL, DashG]);
    psDot:       if FPW = 1 then
                   Result := '3, 3'
                 else
                 Result := Format('%d, %d', [DotL, DotG]);
    psDashDot:   if FPW = 1 then
                    Result := '9, 6, 3, 6'
                  else
                    Result := Format('%d, %d, %d, %d', [DashL, DotG, DotL, DotG]);
    psDashDotDot: if FPW = 1 then
                    Result := '9, 3, 3, 3, 3, 3'
                  else
                    Result := Format('%d, %d, %d, %d, %d, %d', [DashL, DotG, DotL, DotG, DotL, DotG]);
  else
    Result := '1';
  end;
end;

{ TRemRectFigure }

procedure TRemRectFigure.SVGFillParam(SVGDoc: TXMLDocument; node,
  figure: TDOMElement);
var
  FillValue: string;

  function PatternExists(PatternEl: string; node: TDOMElement
    ): boolean;
  var
    PatternList: TDOMNodeList;
    PatternParam: TDOMElement;
    i: integer;
  begin
    Result := false;
    PatternList := node.GetElementsByTagName('pattern');
    for i := 0 to PatternList.Count - 1 do
    begin
      PatternParam := TDOMElement(PatternList.Item[i]);
      Result := PatternParam.GetAttribute('id') = PatternEl;
      if Result then exit;
    end;
  end;

begin
  case FBS of
    bsSolid: FillValue := SVGColor(FBC);
    bsClear: FillValue := 'none';
    else
    begin
      if not PatternExists(Format('pattern%d-%d', [FBS, FBC]), node) then
        AddPattern(SVGDoc, node);
      FillValue := Format('url(#%s)', [Format('pattern%d-%d', [FBS, FBC])]);
    end;
  end;
  figure.SetAttribute('fill', FillValue);
end;


procedure TRemRectFigure.AddPattern(SVGDoc: TXMLDocument; node: TDOMElement
  );
const
  HOR = 'M 0 4 H 8 Z';
  VER = 'M 4 0 V 8 Z';
  FDIAG = 'M 0 0 L 8 8 Z';
  BDIAG = 'M 8 0 L 0 8 Z';
var
  PatternEl: TDOMElement;
  PatternParam: string;

procedure PathToPattern(AXMLDoc: TXMLDocument;
  APatternTag: TDOMElement; ADValue, AStrokeValue: string);
var
  PathTag: TDOMElement;
begin
  PathTag := AXMLDoc.CreateElement('path');
  with PathTag do
  begin
    SetAttribute('d', ADValue);
    SetAttribute('stroke', AStrokeValue);
  end;
  APatternTag.AppendChild(PathTag);
end;

begin
  PatternParam := Format('pattern%d-%d', [FBS, FBC]);
  PatternEl := SVGDoc.CreateElement('pattern');
  with PatternEl do begin
    SetAttribute('id', PatternParam);
    SetAttribute('width', '8');
    SetAttribute('height', '8');
    SetAttribute('viewbox', '0 0 8 8');
    SetAttribute('x', '0');
    SetAttribute('y', '0');
    SetAttribute('patternUnits', 'userSpaceOnUse');
  end;
  node.AppendChild(PatternEl);
  case FBS of
    bsHorizontal: PathToPattern(SVGDoc, PatternEl, HOR, SVGColor(FBC));
    bsVertical:   PathToPattern(SVGDoc, PatternEl, VER, SVGColor(FBC));
    bsCross:      begin
                    PathToPattern(SVGDoc, PatternEl, HOR, SVGColor(FBC));
                    PathToPattern(SVGDoc, PatternEl, VER, SVGColor(FBC));
                  end;
    bsFDiagonal:  PathToPattern(SVGDoc, PatternEl, FDIAG, SVGColor(FBC));
    bsBDiagonal:  PathToPattern(SVGDoc, PatternEl, BDIAG, SVGColor(FBC));
    bsDiagCross:  begin
                    PathToPattern(SVGDoc, PatternEl, FDIAG, SVGColor(FBC));
                    PathToPattern(SVGDoc, PatternEl, BDIAG, SVGColor(FBC));
                  end;
  end;
end;

procedure TRemRectFigure.Next(po: TFloatPoint);
begin
  r  := ToFloatRect(po, po);
end;

procedure TRemRectFigure.Draw(canvas: TCanvas);
begin
  canvas.Pen.Width := Trunc(FPW * Field.Zoom);
  canvas.Pen.Style := FPS;
  canvas.Brush.Style := FBS;
  r.Top += ToFloatPoint(FSX, FSY);
  r.Bottom += ToFloatPoint(FSX, FSY);
  if FBS <> bsClear then canvas.Brush.Color := FBC;
  canvas.Pen.Color := FPC;
  FMax := maxPoint(r.Top,  r.Bottom);
  FMin := minPoint(r.Top,  r.Bottom);
  FSX := 0;
  FSY := 0;
end;

procedure TRemRectFigure.DrawM(canvas: TCanvas);
begin
  CCC := not CCC;
  canvas.Pen.Width := 5;
  canvas.Pen.Style := psDot;
  canvas.Brush.Style := bsClear;
  if ccc then canvas.Pen.Color := clRed else  canvas.Pen.Color := clBlue;
end;

procedure TRemRectFigure.Finish(po: TFloatPoint);
begin
  r.Bottom := po;
end;

procedure TRemRectFigure.MouseMark(t: TFloatRect);
begin
  if not ((((insideFigure(r, t)) and (abs(r.Top.x - r.Bottom.x) > 10) and (abs(r.Top.y - r.Bottom.y) > 10)) and (FBS <> bsSolid)) or (intersectFigure(r, t))) then
    isSelected := True;
end;

procedure TRemRectFigure.Save;
begin
  StrState += ColorToString(FPC) + '|' + ColorToString(FBC) + '|' +  IntToStr(FPW) + '|' + GetEnumName(TypeInfo(TFPPenStyle), ord(FPS)) + '|' + GetEnumName(TypeInfo(TFPBrushStyle), ord(FBS)) + '|' + floattostr(round(r.Top.X)) + '|' + floattostr(round(r.Top.y)) + '|' + floattostr(round(r.Bottom.X)) + '|' + floattostr(round(r.Bottom.y)) + '|';
end;

procedure TRemRectFigure.OpenF(s: string);
var
  i: integer;
  o: string;
begin
  o := '';
  Del := False;
  arrStr := TStringList.Create;
  arrStr.Delimiter := '|';
  arrStr.DelimitedText := s;
  FPC := StringToColor(arrStr[1]);
  FBC := StringToColor(arrStr[2]);
  FPW := StrToInt(arrStr[3]);
  FPS := TFPPenStyle(GetEnumValue(TypeInfo(TFPPenStyle), arrStr[4]));
  FBS := TFPBrushStyle(GetEnumValue(TypeInfo(TFPBrushStyle), arrStr[5]));
  r.Top.X := StrToFloat(arrStr[6]);
  r.Top.y := StrToFloat(arrStr[7]);
  r.Bottom.x := StrToFloat(arrStr[8]);
  r.Bottom.y := StrToFloat(arrStr[9]);
end;

{ TRectangle }

constructor TRectangle.Create;
begin
  FPW := 1;
  FPS := psSolid;
  FBS := bsClear;
  FBC := clWhite;
  isSelected := False;
  Del := False;
end;

procedure TRectangle.Draw(canvas: TCanvas);
begin
  inherited Draw(canvas);
  canvas.Rectangle(ToRect(Field.WorldToLocal(r.Top), Field.WorldToLocal(r.Bottom)));
end;

procedure TRectangle.DrawM(canvas: TCanvas);
begin
  inherited DrawM(canvas);
  canvas.Rectangle(ToRect(Field.WorldToLocal(r.Top), Field.WorldToLocal(r.Bottom)));
end;

procedure TRectangle.Save;
begin
  StrState := 'TRectangle' + '|';
  inherited Save;
end;

procedure TRectangle.OpenF(s: string);
begin
  inherited OpenF(s);
  arrStr.Free;
end;

procedure TRectangle.ExportSVG(node: TDOMElement; svg: TXMLDocument;
  minp: TFloatPoint);
var
  figure: TDOMElement;
begin
  figure := svg.CreateElement('rect');
  r := swaprect(r);
  with figure do
  begin
    SetAttribute('x', FloatToStr(r.top.x - minp.x));
    SetAttribute('y', FloatToStr(r.top.y - minp.y));
    SetAttribute('width', FloatToStr(r.Bottom.x - r.top.x));
    SetAttribute('height', FloatToStr(r.Bottom.y - r.top.y));
  end;
  SVGStrokParam(figure);
  SVGFillParam(svg, node, figure);
  node.AppendChild(figure);
end;

{ TEllipse }

constructor TEllipse.Create;
begin
  FPW := 1;
  FPS := psSolid;
  FBS := bsClear;
  FBC := clWhite;
  Del := False;
  isSelected := False;
end;

procedure TEllipse.Draw(canvas: TCanvas);
begin
  inherited Draw(canvas);
  canvas.Ellipse(ToRect(Field.WorldToLocal(r.Top), Field.WorldToLocal(r.Bottom)));
end;

procedure TEllipse.DrawM(canvas: TCanvas);
begin
  inherited DrawM(canvas);
  canvas.Ellipse(ToRect(Field.WorldToLocal(r.Top), Field.WorldToLocal(r.Bottom)));
end;

procedure TEllipse.save;
begin
  StrState := 'TEllipse' + '|';
  inherited Save;
end;

procedure TEllipse.OpenF(s: string);
begin
  inherited OpenF(s);
  arrStr.Free;
end;

procedure TEllipse.ExportSVG(node: TDOMElement; svg: TXMLDocument;
  minp: TFloatPoint);
var
  figure: TDOMElement;
begin
  r := swaprect(r);
  figure := svg.CreateElement('ellipse');
  with figure do
  begin
    SetAttribute('cx', FloatToStr(r.top.x - minp.x + round((r.Bottom.x - r.top.x) / 2)));
    SetAttribute('cy', FloatToStr(r.top.y - minp.y + round((r.Bottom.y - r.top.y) / 2)));
    SetAttribute('rx', FloatToStr(round((r.Bottom.x - r.top.x) / 2)));
    SetAttribute('ry', FloatToStr(round((r.Bottom.y - r.top.y) / 2)));
  end;
  SVGStrokParam(figure);
  SVGFillParam(svg, node, figure);
  node.AppendChild(figure);
end;


{ TRoundRectangle }

constructor TRoundRectangle.Create;
begin
  FPW := 1;
  FPS := psSolid;
  FBS := bsClear;
  FRR := 1;
  FBC := clWhite;
  Del := False;
  isSelected := False;
end;

procedure TRoundRectangle.Draw(canvas: TCanvas);
begin
  inherited Draw(canvas);
  canvas.RoundRect(ToRect(Field.WorldToLocal(r.Top), Field.WorldToLocal(r.Bottom)), FRR, FRR);
end;

procedure TRoundRectangle.DrawM(canvas: TCanvas);
begin
  inherited DrawM(canvas);
  canvas.RoundRect(ToRect(Field.WorldToLocal(r.Top), Field.WorldToLocal(r.Bottom)), FRR, FRR);
  end;

procedure TRoundRectangle.Save;
begin
  StrState := 'TRoundRectangle' + '|';
  inherited Save;
  StrState += IntToStr(FRR) + '|';
end;

procedure TRoundRectangle.OpenF(s: string);
begin
  inherited OpenF(s);
  FRR := StrToInt(arrStr[10]);
  arrStr.Free;
end;

procedure TRoundRectangle.ExportSVG(node: TDOMElement; svg: TXMLDocument;
  minp: TFloatPoint);
var
    figure: TDOMElement;
begin
  figure := svg.CreateElement('rect');
  r := swaprect(r);
  with figure do
  begin
    SetAttribute('x', FloatToStr(r.top.x - minp.x));
    SetAttribute('y', FloatToStr(r.top.y - minp.y));
    SetAttribute('width', FloatToStr(r.Bottom.x - r.top.x));
    SetAttribute('height', FloatToStr(r.Bottom.y - r.top.y));
    SetAttribute('rx', IntToStr(FRR));
  end;
  SVGStrokParam(figure);
  SVGFillParam(svg, node, figure);
  node.AppendChild(figure);
end;

{ TPolyLine }

constructor TPolyLine.Create;
begin
  FPW := 1;
  FPS := psSolid;
  Del := False;
  isSelected := False;
end;

procedure TPolyLine.Next(po: TFloatPoint);
begin
  SetLength(p, 2);
  p[0] := po;
  p[1] := po;
end;

procedure TPolyLine.Draw(canvas: TCanvas);
var
  i: integer;
begin
  fmax := ToFloatPoint(0, 0);
  fmin := ToFloatPoint(0, 0);
  canvas.Pen.Width := Trunc(FPW * Field.Zoom);
  canvas.Pen.Style := FPS;
  canvas.Pen.Color := FPC;
  for i := 0 to High(p) do
    p[i] += ToFloatPoint(FSX, FSY);
  FSX := 0;
  FSY := 0;
  for i := 0 to High(p) - 1 do
  begin
    canvas.Line(Field.WorldToLocal(p[i]), Field.WorldToLocal(p[i + 1]));
    FMax := maxPoint(p[i],  fmax);
    FMin := minPoint(p[i],  fmin);
    FMax := maxPoint(p[i + 1], fmax);
    FMin := minPoint(p[i + 1], fmin);
  end;

end;

procedure TPolyLine.DrawM(canvas: TCanvas);
var
  i: integer;
begin
  canvas.Pen.Width := 3;
  canvas.Pen.Style := psDot;
  canvas.Brush.Style := bsClear;
  if ccc then canvas.Pen.Color := clRed else  canvas.Pen.Color := clBlue;
  for i := 0 to High(p) - 1 do
    canvas.Line(Field.WorldToLocal(p[i]), Field.WorldToLocal(p[i + 1]));
  CCC := not CCC;
end;

procedure TPolyLine.Finish(po: TFloatPoint);
begin
  p[High(p)] := po;
end;

procedure TPolyLine.FreeLine(po: TFloatPoint);
begin
  SetLength(p, Length(p) + 1);
  p[High(p)] := po;
end;

procedure TPolyLine.MouseMark(t: TFloatRect);
var
  i: integer;
begin
  for i := 0 to High(p) - 1 do
  begin
    if not  (intersectFigure(ToFloatRect(p[i], p[i + 1]), t)) then
    isSelected := True;
  end;
end;

procedure TPolyLine.Save;
var
  s: string;
  i: integer;
begin
  StrState := 'TPolyLine' + '|' + ColorToString(FPC) + '|' +  IntToStr(FPW) + '|' + GetEnumName(TypeInfo(TFPPenStyle), ord(FPS)) + '|';
  for i := 0 to High(p) do
    StrState += FloatToStr(round(p[i].x)) + '|' + FloatToStr(round(p[i].y)) + '|';
end;

procedure TPolyLine.OpenF(s: string);
var
  i: integer;
  o: string;
begin
  o := '';
  Del := False;
  arrStr := TStringList.Create;
  arrStr.Delimiter := '|';
  arrStr.DelimitedText := s;
  FPC := StringToColor(arrStr[1]);
  FPW := StrToInt(arrStr[2]);
  FPS := TFPPenStyle(GetEnumValue(TypeInfo(TFPPenStyle), arrStr[3]));
  SetLength(p, 0);
  for i := 4 to arrStr.Count - 1 do
  begin
    if i mod 2 = 0 then
    begin
      SetLength(p, Length(p) + 1);
      p[High(p)].x := StrToFloat(arrStr[i]);
    end
    else
      p[High(p)].y := StrToFloat(arrStr[i]);
  end;
  arrStr.Free;
end;

procedure TPolyLine.ExportSVG(node: TDOMElement; svg: TXMLDocument;
  minp: TFloatPoint);
var
  figure: TDOMElement;
  s: string;
  i: integer;
begin
  figure := svg.CreateElement('polyline');
  s := '';
  for i := 0 to High(p) do
    s += FloattoStr(p[i].x) + ',' + FloattoStr(p[i].y) + ' ';
  figure.SetAttribute('points', s);
  figure.SetAttribute('fill', 'none');
  SVGStrokParam(figure);
  node.AppendChild(figure);
end;


initialization
  RegisterClass(TPolyLine);
  RegisterClass(TRectangle);
  RegisterClass(TRoundRectangle);
  RegisterClass(TEllipse);
end.

