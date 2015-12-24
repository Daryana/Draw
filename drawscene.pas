unit DrawScene;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Figure, Graphics, crt, Graph, UTypes, UTransform, EditCreate, typinfo, Clipbrd, XMLWrite, DOM;

const
  formatIndex = '358yam7h_daryana\*';


type

  { TScene }

  TScene = class
  public
    procedure SelectEdit;
    procedure PBDraw(canvas: TCanvas);
    procedure MouseMarkNul;
    procedure DeleteFigure;
    procedure SaveFigure;
    procedure OpenFigure;
    procedure DrawLayerUp;
    procedure DrawLayerDown;
    procedure PBClear;
    procedure History;
    procedure CopyFigure;
    procedure PasteFigure;
    procedure ExportSVGFile;
end;
 var
   Scene: TScene;
   Figures: array of TFigure;
   WidthLine: Integer;
   DWidthLine: Integer;
   PoleMax: TFloatPoint;
   PoleMin: TFloatPoint;
   fileSaveYHD: Text;
   HistoryArray: array of array of string;
   indexHistory: integer;
   actualSave: boolean;
   onFileName: string;

implementation

{ TScene }


procedure TScene.PBDraw(canvas: TCanvas);
var
  i: integer;
begin
  for i := 0 to High(figures) do
  begin
    if i = 0 then
    begin
      polemax := figures[0].fmax;
      polemin := figures[0].fmin;
    end
    else
    begin
      polemax := maxPoint(polemax,  figures[i].fmax);
      polemin := minPoint(polemin,  figures[i].fmin);
    end;
    if not figures[i].Del then
      figures[i].Draw(Canvas);
    if (figures[i].isSelected) and (not figures[i].Del) then
      figures[i].DrawM(Canvas);
  end;
end;

procedure TScene.SelectEdit;
var
  i: integer;
  EditerArrey: array of TPersistent;
begin
  setLength(EditerArrey, 0);
  for i := 0 to High(Figures) do
    if Figures[i].isSelected then
    begin
      setLength(EditerArrey, length(EditerArrey) + 1);
      EditerArrey[high(EditerArrey)] := TPersistent(Figures[i]);
    end;
  FigureEditor.EditChoice(EditerArrey);
end;

procedure TScene.MouseMarkNul;
var
 figure: TFigure;
begin
  for figure in Figures do
    figure.isSelected := False;
  Scene.SelectEdit;
end;

procedure TScene.DeleteFigure;
var
 figure: TFigure;
begin
  for figure in Figures do
    if figure.isSelected then
      figure.del := true;
  History;
end;

procedure TScene.SaveFigure;
var
 figure: TFigure;
begin
  writeln(fileSaveYHD, formatIndex);
  writeln(fileSaveYHD,
    FloatToStr(round(Field.Shift.x)) + ' ' + FloatToStr(round(Field.Shift.y)) + ' ' + FloatToStr(Field.Zoom));
  for figure in figures do
    if not figure.del then
    begin
      figure.Save;
      writeln(fileSaveYHD, figure.StrState);
    end;
  writeln(fileSaveYHD, '*');
end;

procedure TScene.OpenFigure;
var
 i, k: integer;
 s: string;
 FileSaveText: string;
begin
  SetLength(Figures, 0);
  for i := 0 to High(HistoryArray[indexHistory]) do
  begin
    s := '';
    k := 1;
    FileSaveText := HistoryArray[indexHistory][i];
    while FileSaveText[k] <> ' ' do
    begin
      s += FileSaveText[k];
      inc(k);
    end;
    SetLength(Figures, Length(Figures) + 1);
    Figures[high(Figures)] := TFigure(GetClass(s).Create);
    Figures[high(Figures)].OpenF(FileSaveText);
  end;
end;

procedure TScene.DrawLayerUp;
var
  i, j, k:integer;
  n: array of integer;
  f: array of TFigure;
begin
  SetLength(f, 0);
  SetLength(n, 0);
  for i := 0 to High(figures) do
    if (figures[i].isSelected)  then
    begin
      SetLength(f, length(f) + 1);
      SetLength(n, length(n) + 1);
      f[high(f)] := figures[i];
      n[high(n)] := i;
    end;
  if length(f) = 0 then Exit;
  j := 0;
  k := 0;
  for i := 0 to High(figures) do
    if n[j] = i then
      inc(j)
    else
    begin
      figures[k] := Figures[i];
      inc(k);
    end;
  j := 0;
  for i := High(figures) - High(f) to High(figures) do
  begin
    figures[i] := f[j];
    inc(j);
  end;
  History;
end;

procedure TScene.DrawLayerDown;
var
  i, j:integer;
  n: array of integer;
  f: array of TFigure;
begin
  History;
  SetLength(f, 0);
  SetLength(n, 0);
  for i := 0 to High(figures) do
    if figures[i].isSelected then
    begin
      SetLength(f, length(f) + 1);
      SetLength(n, length(n) + 1);
      f[high(f)] := figures[i];
      n[high(n)] := i;
    end;
  if length(f) = 0 then Exit;
  j := high(f);
  for i := High(figures) downto 0 do
    if (n[j] <> i) or (j < 0) then
      figures[i - j + High(f)] := Figures[i]
    else
      dec(j);
  for i := 0 to High(f) do
end;

procedure TScene.PBClear();
var
  i:integer;
begin
  for i := 0 to High(figures) do figures[i].free;
    SetLength(figures, 0);
end;

procedure TScene.History;
var
  i: integer;
  s: array of string;
function addent(a, b: array of String): boolean;
var
  i, j: integer;
begin
  Result := False;
  if Length(a) = Length(b) then
  begin
    Result := true;
    for i := 0 to high(a) do
    begin
      if Length(a[i]) <> Length(b[i]) then Result := false
      else
        for j := 0 to Length(a[i]) do
          if a[i][j] <> b[i][j] then Result := false;
    end;
  end;
end;
begin
  SetLength(s, 0);
  for i := 0 to High(Figures) do
  begin
    SetLength(s, Length(s) + 1);
    Figures[i].Save;
    s[High(s)] := Figures[i].StrState;
  end;
  if not addent(HistoryArray[indexHistory], s) then
    begin
      inc(indexHistory);
      SetLength(HistoryArray, indexHistory + 1);
      SetLength(HistoryArray[indexHistory], Length(s));
      for i := 0 to High(s) do
        HistoryArray[indexHistory][i] := s[i];
      actualSave := false;
    end;
end;

procedure TScene.CopyFigure;
var
  f: TFigure;
  str: string;
begin
  str := formatIndex + '|';
  for f in Figures do
    if f.isSelected then
    begin
      f.Save;
      str += f.StrState + '|';
    end;
  ClipBoard.AsText := str;
end;

procedure TScene.PasteFigure;
var
  i: integer;
  str: string;
  StrList: TStringList;
begin
  str := Clipboard.AsText;
  if Pos(str, formatIndex)  = 1 then
  begin
    Delete(str, 1, Length(formatIndex));
    StrList := TStringList.Create;
    StrList.Delimiter := '|';
    StrList.DelimitedText := str;
    for i := 0 to StrList.Count - 1 do
    begin
        SetLength(HistoryArray[indexHistory], Length(HistoryArray[indexHistory]) + 1);
        HistoryArray[indexHistory][High(HistoryArray[indexHistory])] := StrList[i];
    end;
  end;
  StrList.Free;
  OpenFigure;
end;

procedure TScene.ExportSVGFile;
var
  xml: TXMLDocument;
  node: TDOMElement;
  figure: TFigure;
begin
  PBInd;
  xml := TXMLDocument.Create;
  node := xml.CreateElement('svg');
  node.SetAttribute('version', '1.1');
  node.SetAttribute('baseProfile', 'full');
  node.SetAttribute('xmlns', 'http://www.w3.org/2000/svg');
  node.SetAttribute('xmlns:xlink', 'http://www.w3.org/1999/xlink');
  node.SetAttribute('xmlns:ev', 'http://www.w3.org/2001/xml-events');
  node.SetAttribute('height', inttostr(trunc(PoleMax.Y - PoleMin.y) + 21) + 'px');
  node.SetAttribute('width', inttostr(trunc(PoleMax.x - PoleMin.x) + 21) + 'px');
  xml.AppendChild(node);
  for figure in Figures do
    figure.ExportSVG(node, xml, PoleMin - ToFloatPoint(10, 10));

  WriteXMLFile(xml, onFileName + '.svg');
end;

end.

