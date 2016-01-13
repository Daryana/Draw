unit paint;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Menus, StdCtrls, Grids, typinfo, tool,
  DrawScene, UTransform, UTypes, types, FPCanvas, EditCreate, LCLType, ColorBox;
type

  { TMyForm }

  TMyForm = class(TForm)
    ColorDialog: TColorDialog;
    MenuDrawLayerUp: TMenuItem;
    MenuDrawLayerDown: TMenuItem;
    MenuAgo: TMenuItem;
    MenuDelete: TMenuItem;
    MenuClear: TMenuItem;
    MenuExport: TMenuItem;
    MenuNext: TMenuItem;
    MenuCopy: TMenuItem;
    MenuPaste: TMenuItem;
    MenuNew: TMenuItem;
    MenuShape: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveAs: TMenuItem;
    Save: TMenuItem;
    Open: TMenuItem;
    PaletteGrid: TDrawGrid;
    EditZoom: TEdit;
    LabelZoom: TLabel;
    MenuShift: TMenuItem;
    MenuZoom: TMenuItem;
    PanelZoom: TPanel;
    PanelPaint: TPanel;
    HScrollBar: TScrollBar;
    BrushColor: TShape;
    PenColor: TShape;
    SaveDialog: TSaveDialog;
    VScrollBar: TScrollBar;
    LoupePlus: TBitBtn;
    LoupeMinus: TBitBtn;
    TToolClear: TBitBtn;
    MainMenu: TMainMenu;
    MenuItemFile: TMenuItem;
    MenuItemFAQ: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemAutor: TMenuItem;
    PaintBox: TPaintBox;
    PanelTools: TPanel;
    PanelEdit: TPanel;
    procedure BrushColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MenuAgoClick(Sender: TObject);
    procedure MenuClearClick(Sender: TObject);
    procedure MenuCopyClick(Sender: TObject);
    procedure MenuDeleteClick(Sender: TObject);
    procedure MenuDrawLayerDownClick(Sender: TObject);
    procedure MenuDrawLayerUpClick(Sender: TObject);
    procedure MenuExportClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuNewClick(Sender: TObject);
    procedure MenuNextClick(Sender: TObject);
    procedure MenuPasteClick(Sender: TObject);
    procedure OpenClick(Sender: TObject);
    procedure PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PaletteGridDblClick(Sender: TObject);
    procedure PaletteGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure PaletteGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PenColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SaveAsClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure Scroll;
    procedure HScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure MenuZoomClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ToolLoupePlusClick(Sender: TObject);
    procedure ToolLoupeMinusClick(Sender: TObject);
    procedure MenuItemAutorClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure ButtonToolClick(Sender: TObject);
    procedure TToolClearClick(Sender: TObject);
    procedure VScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
  private
    DownMouseFlag: boolean;
    ToolKod: integer;
    procedure ClickExport(Sender: TObject);
  public
    { public declarations }
  end;

var
  MyForm: TMyForm;
  PaletteColors: array of TColor;
  paletteNumber: TPoint;
  PMax: TFloatPoint;
  PMin: TFloatPoint;
  HSbPosition, VSbPosition: Integer;
  onSave: boolean;

implementation

{$R *.lfm}

{ TMyForm }

procedure TMyForm.MenuItemAutorClick(Sender: TObject);
begin
  ShowMessage('Автор: Терехина Дарьяна, группа Б8103а1Б 2015г.');
end;

procedure TMyForm.FormCreate(Sender: TObject);
const
  a = 35;
var
    i, j, p: integer;
    b: TBitBtn;
    bpng: TPortableNetworkGraphic;
begin
  for i := High(consttool.tool) downto 0 do
  begin
    bpng := TPortableNetworkGraphic.Create;
    bpng.LoadFromFile('icons/' + consttool.tool[i].ClassName + '.png');
    b := TBitBtn.Create(PanelTools);
    b.Parent:= PanelTools;
    b.Width := a;
    b.Height := a;
    b.Glyph.Assign(bpng);
    b.Align := alLeft;
    b.tag := i;
    b.OnClick :=@ButtonToolClick;
  end;
  setLength(PaletteColors, PaletteGrid.ColCount * PaletteGrid.RowCount);
  for i := 0 to high(PaletteColors) do PaletteColors[i] := clWhite;
  p := PaletteGrid.ColCount;
  for i := 0 to 3 do
     for j := 0 to 11 do
       begin
         PaletteColors[p] := RGBToColor((11 - j) * 23, 85 * i, j * 23);
         inc(p);
       end;
  for i := 0 to 11 do
    PaletteColors[i + 60]:= RGBToColor(i * 23, i * 23, i * 23);

  indexHistory := 0;
  SetLength(HistoryArray, 1);
  SetLength(HistoryArray[0], 0);
  PBInd := @MyForm.PaintBox.Invalidate;
  Hist := @Scene.History;
  FigureEditor := TEditRun.Create(PanelEdit);
  ShiftFlag := False;
  FigureEditor.EditChoiceOne(TPersistent(consttool.tool[ToolKod].CreateParamObj));
  FigureEditor.SetBrushColor(BrushColor.Brush.Color);
  Field := TTransform.Create(PaintBox.Width, PaintBox.Height);
  onSave := False;
  actualSave := True;
  onFileName := 'NoName';
  PaintBox.Invalidate;
end;

procedure TMyForm.ToolLoupePlusClick(Sender: TObject);
begin
  Field.PluseLoupe;
  PLineFlag := False;
  actualSave := false;
  EditZoom.Text := FloatToStr(Field.Zoom * 100);
  paintbox.Invalidate;
end;

procedure TMyForm.ToolLoupeMinusClick(Sender: TObject);
begin
  Field.MinuseLoupe;
  PLineFlag := False;
  actualSave := false;
  EditZoom.Text := FloatToStr(Field.Zoom * 100);
  paintbox.Invalidate;
end;

procedure TMyForm.HScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  PaintBox.Invalidate;
end;

procedure TMyForm.MenuZoomClick(Sender: TObject);
var
  r : TRect;
begin
  r := ToRect(Field.WorldToLocal(polemin), Field.WorldToLocal(polemax));
  Field.loupeRect(PaintBox.Width, PaintBox.Height, r);
  PLineFlag := False;
  EditZoom.Text := FloatToStr(Field.Zoom * 100);
  actualSave := false;
  paintbox.Invalidate;
end;

procedure TMyForm.Scroll;
var
  Poligon: TFloatRect;
  PSize: TFloatPoint;
  WTop, WBottom: TFloatPoint;
  delta: integer;
begin
  Poligon := ToFloatRect(polemin, polemax);

  WTop := Field.LocalToWorld(Point(0, 0));
  if Poligon.Top.X > WTop.x then Poligon.top.x := WTop.x;
  if Poligon.Top.y > WTop.y then Poligon.Top.y := WTop.y;
  WBottom := Field.LocalToWorld(Point(PaintBox.Width, PaintBox.Height));
  if Poligon.Bottom.x < WBottom.x then Poligon.Bottom.x := WBottom.x;
  if Poligon.Bottom.y < WBottom.y then Poligon.Bottom.y := WBottom.y;
  PSize.X := Poligon.Bottom.x - Poligon.Top.x;
  PSize.Y := Poligon.Bottom.y - Poligon.Top.y;
  if PSize.x * PSize.y = 0 then exit;

  delta := HScrollBar.Max - HScrollBar.Min;
  HScrollBar.PageSize := round(PaintBox.Width / (PSize.X * Field.Zoom) * delta);
  HScrollBar.Visible := HScrollBar.PageSize < delta;
  if HScrollBar.PageSize < delta then
  begin
    if (HSbPosition = HScrollBar.Position) then
      HScrollBar.Position := round(((-1) * (Field.Shift.X + Poligon.Top.x)) / PSize.X * delta)
    else
      Field.Shift.X := (-1) * (HScrollBar.Position / delta * PSize.X + Poligon.Top.x);
    HSbPosition := HScrollBar.Position;
  end;

  delta := VScrollBar.Max - VScrollBar.Min;
  VScrollBar.PageSize := round(PaintBox.Height / (PSize.Y * Field.Zoom) * delta);
  VScrollBar.Visible := VScrollBar.PageSize < delta;
  if VScrollBar.PageSize < delta then
  begin
    if (VSbPosition = VScrollBar.Position) then
      VScrollBar.Position := round(((-1) * (Field.Shift.Y + Poligon.Top.y)) / PSize.Y *delta)
    else
      Field.Shift.Y := (-1) * (VScrollBar.Position / delta * PSize.Y + Poligon.Top.y);
    VSbPosition := VScrollBar.Position;
  end;
end;


procedure TMyForm.PaletteGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  PaletteGrid.Canvas.Brush.Color := paletteColors[aRow*PaletteGrid.ColCount + aCol];
  PaletteGrid.Canvas.FillRect(aRect);
end;

procedure TMyForm.PaletteGridDblClick(Sender: TObject);
var
  p: integer;
begin
  p := PaletteGrid.ColCount * paletteNumber.y + paletteNumber.x;
  if not (p in [0..PaletteGrid.ColCount - 1]) Then exit;
  ColorDialog.Color := paletteColors[p];
  if not ColorDialog.Execute then exit;
  paletteColors[p] := ColorDialog.Color;
  PenColor.Brush.Color := paletteColors[p];
  FigureEditor.SetPenColor(PenColor.Brush.Color);
  PaletteGrid.InvalidateCell(paletteNumber.x, paletteNumber.y);
end;

procedure TMyForm.BrushColorMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color := BrushColor.Brush.Color;
  if ColorDialog.Execute then
  begin
    BrushColor.Brush.Color := ColorDialog.Color;
    FigureEditor.SetBrushColor(BrushColor.Brush.Color);
  end;
end;

procedure TMyForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  ans: integer;
begin
  CanClose := true;
  if not actualSave then begin
    ans:= MessageDlg('Сохранить изменения в '+ onFileName + '.yhd ?', mtInformation, [mbYes, mbNo, mbCancel], 0);
    if ans = mrCancel Then CanClose := false;
    if ans = mrYes Then SaveClick(Sender);
  end;
end;

procedure TMyForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if (key = vk_back) or (key = VK_DELETE) then
  begin
    Scene.DeleteFigure();
    PaintBox.Invalidate;
  end;
  if not ((Key in [vk_0..vk_9]) or (key = vk_down) or (key = VK_UP) or (key = vk_back) or (key = VK_DELETE)) then Key := VK_UNKNOWN;
end;

procedure TMyForm.MenuAgoClick(Sender: TObject);
begin
  If indexHistory > 0 then dec(indexHistory);
  Scene.OpenFigure;
  PaintBox.Invalidate;
end;

procedure TMyForm.MenuClearClick(Sender: TObject);
begin
  TToolClearClick(nil);
end;

procedure TMyForm.MenuCopyClick(Sender: TObject);
begin
  Scene.CopyFigure;
end;

procedure TMyForm.MenuDeleteClick(Sender: TObject);
begin
  Scene.DeleteFigure();
  PaintBox.Invalidate;
end;

procedure TMyForm.MenuDrawLayerDownClick(Sender: TObject);
begin
  Scene.DrawLayerDown;
  PaintBox.Invalidate;
end;

procedure TMyForm.MenuDrawLayerUpClick(Sender: TObject);
begin
  Scene.DrawLayerUp;
  PaintBox.Invalidate;
end;

procedure TMyForm.MenuExportClick(Sender: TObject);
var
  i: integer;
  ans: array[1..5] of string = ('BMP','PNG', 'JPG', 'SVG', 'Cancel');
  f: TForm;
  b: TButton;
  l: TLabel;
  p: TPanel;
begin
  f := TForm.Create(MyForm);
  f.Width := 400;
  f.Height := 50;
  f.Top := MyForm.Top + MyForm.Height div 2 - 25;
  f.Left := MyForm.Left + MyForm.Width div 2 - 200;
  l := TLabel.Create(f);
  l.Parent := f;
  l.Align := alTop;
  l.Caption := 'Экспортировать '+ onFileName + ' в';
  p := TPanel.Create(f);
  p.Parent := f;
  p.Align := alClient;
  for i := 1 to 5 do
  Begin
    b := TButton.Create(f);
    b.Caption := ans[i];
    b.Tag := i;
    b.Align := alLeft;
    b.BorderSpacing.Around := 3;
    b.Parent := p;
    b.OnClick := @ClickExport;
  end;
  PLineFlag := false;
  f.Show;
end;

procedure TMyForm.MenuItemExitClick(Sender: TObject);
begin
  MyForm.Close;
end;

procedure TMyForm.MenuNewClick(Sender: TObject);
var
  ans: integer;
begin
  if not actualSave then begin
    ans := MessageDlg('Сохранить изменения в '+ onFileName + '.yhd ?', mtInformation, [mbYes, mbNo, mbCancel], 0);
    if ans = mrCancel Then Exit;
    if ans = mrYes Then SaveClick(nil);
  end;
  TToolClear.Click;
  onSave := False;
  actualSave := True;
  MyForm.Caption := 'Редактор';
  onFileName := 'NoName';
end;

procedure TMyForm.MenuNextClick(Sender: TObject);
begin
  If indexHistory < high(HistoryArray) then
  begin
    inc(indexHistory);
    Scene.OpenFigure;
    PaintBox.Invalidate;
  end;
end;

procedure TMyForm.MenuPasteClick(Sender: TObject);
begin
  Scene.PasteFigure;
  PaintBox.Invalidate;
end;

procedure TMyForm.OpenClick(Sender: TObject);
var
  s, o: string;
  ans, i: integer;
begin
  if not actualSave then
  begin
    ans := MessageDlg('Сохранить изменения в '+ onFileName + '.yhd ?', mtInformation, [mbYes, mbNo, mbCancel], 0);
    if ans = mrCancel Then Exit;
    if ans = mrYes Then SaveClick(nil);
  end;
  if not OpenDialog.Execute Then exit;
  AssignFile(fileSaveYHD, OpenDialog.FileName);
  Reset(fileSaveYHD);
  onFileName := ExtractFileName(OpenDialog.FileName);
  Delete(onFileName, Length(onFileName) - 3, 4);
  onSave := True;
  SetLength(HistoryArray, 1);
  SetLength(HistoryArray[0], 0);
  indexHistory := 0;
  readln(fileSaveYHD, s);
  if s <> formatIndex then Exit;
  read(fileSaveYHD, Field.Shift.x);
  read(fileSaveYHD, Field.Shift.y);
  read(fileSaveYHD, s);
  Field.Zoom :=StrToFloat(s);
  Readln(fileSaveYHD);
  readln(fileSaveYHD, s);
  while s <> '*' do
  begin
    SetLength(HistoryArray[0], Length(HistoryArray[0]) + 1);
    HistoryArray[0][High(HistoryArray[0])] := s;
    readln(fileSaveYHD, s);
  end;
  Scene.OpenFigure;
  MyForm.Caption := onFileName + '.yhd';
  PLineFlag := false;
  CloseFile(fileSaveYHD);
  PaintBox.Invalidate;
end;


procedure TMyForm.PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  delta: TFloatPoint;
begin
  delta := Field.LocalToWorld(MousePos);
  if WheelDelta > 0 then
    Field.PluseLoupe;
  if WheelDelta < 0 then
    Field.MinuseLoupe;
  Field.Shift += Field.LocalToWorld(MousePos) - delta;
  PLineFlag := False;
  EditZoom.Text := FloatToStr(Field.Zoom * 100);
  paintbox.Invalidate;
end;

procedure TMyForm.PaletteGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  gCol, gRow: longint;
  p: integer;
begin
  PaletteGrid.MouseToCell(x, y, gCol, gRow);
  paletteNumber.x := gCol;
  paletteNumber.y := gRow;
  p:= PaletteGrid.ColCount * gRow + gCol;
  if Button = mbLeft then
  begin
    PenColor.Brush.Color := paletteColors[p];
    FigureEditor.SetPenColor(PenColor.Brush.Color);
   end;
  if Button = mbRight then
  begin
    BrushColor.Brush.Color := paletteColors[p];
    FigureEditor.SetBrushColor(BrushColor.Brush.Color);
  end;
end;

procedure TMyForm.PenColorMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color := PenColor.Brush.Color;
  if ColorDialog.Execute then
  begin
    PenColor.Brush.Color := ColorDialog.Color;
    FigureEditor.SetPenColor(PenColor.Brush.Color);
  end;
end;

procedure TMyForm.SaveAsClick(Sender: TObject);
begin
  SaveDialog.FileName := onFileName;
  if not SaveDialog.Execute Then exit;
  AssignFile(fileSaveYHD, SaveDialog.FileName);
  rewrite(fileSaveYHD);
  onSave := True;
  Scene.SaveFigure;
  onFileName := ExtractFileName(SaveDialog.FileName);
  Delete(onFileName, Length(onFileName) - 3, 4);
  MyForm.Caption := onFileName + '.yhd';
  PLineFlag := false;
  CloseFile(fileSaveYHD);
  actualSave := True;
end;

procedure TMyForm.SaveClick(Sender: TObject);
begin
 if onSave then
 begin
   rewrite(fileSaveYHD);
   Scene.SaveFigure;
   PLineFlag := false;
   if onFileName[Length(onFileName)] = '*' then Delete(onFileName, Length(onFileName), 1);
   MyForm.Caption := onFileName + '.yhd';
   CloseFile(fileSaveYHD);
   actualSave := True;
 end
 else SaveAsClick(Sender);
end;


procedure TMyForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  If Button = mbLeft then
  begin
    if not ((ssCtrl in Shift) or (ssShift in Shift)) then
      Scene.MouseMarkNul;
    ShiftFlag := (ssShift in Shift) and (ToolKod = 9);
    DownMouseFlag := True;
    consttool.tool[ToolKod].MouseDown(Point(x, y));
  end
  else
    PLineFlag := False;
  paintbox.Invalidate;
end;

procedure TMyForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: integer);
begin
  if DownMouseFlag then
    consttool.tool[ToolKod].MouseMove(Point(x, y));
  paintbox.Invalidate;
end;

procedure TMyForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  DownMouseFlag := false;
  consttool.tool[ToolKod].MouseUp(Point(PaintBox.Width, PaintBox.Height));
  EditZoom.Text := FloatToStr(Field.Zoom * 100);
  Scene.History;
  paintbox.Invalidate;
end;

procedure TMyForm.PaintBoxPaint(Sender: TObject);
begin
  if not actualSave then
    MyForm.Caption := onFileName + '*.yhd';
  Field.Zoom := StrToFloat(EditZoom.Text) / 100;
  Scene.PBDraw(PaintBox.Canvas);
  if ((ToolKod = 8) or (ToolKod = 9)) and DownMouseFlag then
  begin
    PaintBox.canvas.Pen.Style := psDot;
    PaintBox.canvas.Brush.Style := bsClear;
    PaintBox.canvas.Pen.Width := 3;
    PaintBox.canvas.Pen.Color := clGray;
    if ToolKod = 8 then PaintBox.Canvas.Rectangle(Field.PRect);
    if (ToolKod = 9) and (not ShiftFlag) then PaintBox.Canvas.Rectangle(ToRect(Field.WorldToLocal(MouseRect.Bottom), Field.WorldToLocal(MouseRect.Top)));
  end;
  MyForm.Scroll;
end;

procedure TMyForm.ButtonToolClick(Sender: TObject);
begin
  ToolKod := TBitBtn(sender).tag;
  PLineFlag := False;
  Scene.MouseMarkNul;
  FigureEditor.EditChoiceOne(TPersistent(consttool.tool[ToolKod].CreateParamObj));
  PaintBox.Invalidate;
end;

procedure TMyForm.TToolClearClick(Sender: TObject);
begin
  ToolKod := 0;
  Scene.PBClear;
  DWidthLine := 0;
  PLineFlag := False;
  Field.Zoom := 1;
  Field.Shift := ToFloatPoint(0, 0);
  EditZoom.Text := '100';
  polemax := ToFloatPoint(0, 0);
  polemin := ToFloatPoint(0, 0);
  ShiftFlag := False;
  actualSave := false;
  FigureEditor.EditChoiceOne(TPersistent(consttool.tool[ToolKod].CreateParamObj));
  paintbox.Invalidate;
end;

procedure TMyForm.VScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  PaintBox.Invalidate;
end;

procedure TMyForm.ClickExport(Sender: TObject);
var
  Img: TImage;
  f: TFloatPoint;
begin
  if (TButton(Sender).Tag = 1) or (TButton(Sender).Tag = 2) or (TButton(Sender).Tag = 3) Then
  begin
    SaveClick(nil);
    f := Field.Shift;
    Field.Shift := ToFloatPoint(0, 0) - PoleMin;
    Img := TImage.Create(PanelPaint);
    Img.Visible := False;
    Img.Width := trunc(PoleMax.x - PoleMin.x + 10);
    Img.Height := trunc(PoleMax.y - PoleMin.y + 10);
    Img.canvas.Brush.Style := bsSolid;
    Img.canvas.Pen.Color := clWhite;
    Img.canvas.Brush.Color := clWhite;
    Img.canvas.Rectangle(0, 0, Img.Width, Img.Height);
    Scene.PBDraw(Img.Canvas);
    if TButton(Sender).Tag = 1 Then
      Img.Picture.Bitmap.SaveToFile(onFileName + '.bmp');
    if TButton(Sender).Tag = 2 Then
      Img.Picture.PNG.SaveToFile(onFileName + '.png');
    if TButton(Sender).Tag = 3 Then
      Img.Picture.Jpeg.SaveToFile(onFileName + '.jpeg');
    Img.Free;
    Field.Shift := f;
  end;
  if TButton(Sender).Tag = 4 Then
  begin
    Scene.ExportSVGFile;
    SaveClick(nil);
  end;
  TForm(TButton(Sender).Parent.Parent).Close;
end;

end.

