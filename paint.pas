unit paint;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Menus, StdCtrls, CheckLst, ColorBox, ComCtrls, typinfo, ctool,
  DrawScene, WorldPole, UTypes, Figure;
type

  { TMyForm }

  TMyForm = class(TForm)
    BrushColorBox: TColorBox;
    EditZoom: TEdit;
    LabelZoom: TLabel;
    MenuShift: TMenuItem;
    MenuZoom: TMenuItem;
    PanelZoom: TPanel;
    PanelPaint: TPanel;
    HScrollBar: TScrollBar;
    VScrollBar: TScrollBar;
    StyleBox: TComboBox;
    FillBox: TComboBox;
    RoundListBox: TEdit;
    ScaleListBox: TEdit;
    LoupePlus: TBitBtn;
    LoupeMinus: TBitBtn;
    PenColorBox: TColorBox;
    UpDownScale: TUpDown;
    UpDownRound: TUpDown;
    TToolClear: TBitBtn;
    MainMenu: TMainMenu;
    MenuItemFile: TMenuItem;
    MenuItemFAQ: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemAutor: TMenuItem;
    PaintBox: TPaintBox;
    PanelTools: TPanel;
    Panel: TPanel;
    procedure BrushColorBoxSelect(Sender: TObject);
    procedure FillBoxSelect(Sender: TObject);
    procedure HScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure MenuZoomClick(Sender: TObject);
    procedure PenColorBoxSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BoxKeyPress(Sender: TObject; var Key: char);
    procedure StyleBoxSelect(Sender: TObject);
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
    procedure UpDownRoundChanging(Sender: TObject; var AllowChange: Boolean);
    procedure UpDownScaleChanging(Sender: TObject; var AllowChange: Boolean);
    Procedure Param;
    procedure VScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
  private
    DownMouseFlag: boolean;
    ToolKod: integer;
  public
    { public declarations }
  end;

var
  MyForm: TMyForm;
  k : boolean;
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
    i: integer;
    b: TBitBtn;
    bpng: TPortableNetworkGraphic;
begin
  for i := High(consttool.tool) downto 0 do
  begin
    bpng := TPortableNetworkGraphic.Create;
    bpng.LoadFromFile('icons/' +consttool.tool[i].ClassName + '.png');
    b := TBitBtn.Create(PanelTools);
    b.Parent:= PanelTools;
    b.Width := a;
    b.Height := a;
    b.Glyph.Assign(bpng);
    b.Align := alLeft;
    b.tag := i;
    b.OnClick:=@ButtonToolClick;
  end;
  Pole := TPole.Create(PaintBox.Width, PaintBox.Height);
  EditZoom.Text := FloatToStr(Pole.Zoom * 100);
  MyForm.Param;
  PMin := ToFloatPoint(10000, 10000);
end;

procedure TMyForm.BoxKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9']) then Key := #0;
end;

procedure TMyForm.StyleBoxSelect(Sender: TObject);
begin
  StyleNumber := StyleBox.ItemIndex;
end;

procedure TMyForm.ToolLoupePlusClick(Sender: TObject);
begin
  Pole.PorMLoupe('+');
  PLineFlag := False;
  EditZoom.Text := FloatToStr(Pole.Zoom * 100);
  paintbox.Invalidate;
end;

procedure TMyForm.ToolLoupeMinusClick(Sender: TObject);
begin
  Pole.PorMLoupe('-');
  PLineFlag := False;
  EditZoom.Text := FloatToStr(Pole.Zoom * 100);
  paintbox.Invalidate;
end;

procedure TMyForm.PenColorBoxSelect(Sender: TObject);
begin
  ColorLine := PenColorBox.Selected;
end;

procedure TMyForm.FillBoxSelect(Sender: TObject);
begin
  FillNumber := FillBox.ItemIndex;
end;

procedure TMyForm.HScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  Pole.Shift.x -= HScrollBar.Position;
end;

procedure TMyForm.MenuZoomClick(Sender: TObject);
begin
  Scene.Centre(PaintBox.Width, PaintBox.Height);
  PaintBox.Invalidate;
end;

procedure TMyForm.BrushColorBoxSelect(Sender: TObject);
begin
  ColorBrush := BrushColorBox.Selected;
end;

procedure TMyForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  If Button = mbLeft then
  begin
    DownMouseFlag := True;
    consttool.tool[ToolKod].MouseDown(Point(x, y));
    paintbox.Invalidate;
  end
  else
    PLineFlag := False;
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
  DownMouseFlag:= false;
  if (ToolKod = 7) then
  begin
    Pole.loupeRect(PaintBox.Width, PaintBox.Height);
    EditZoom.Text := FloatToStr(Pole.Zoom * 100);
  end;
  paintbox.Invalidate;
end;

procedure TMyForm.PaintBoxPaint(Sender: TObject);
begin
  UpDownScale.Position := trunc(pole.Zoom);
  if UpDownScale.Position = 0 then UpDownScale.Position := 1;
    DWidthLine := UpDownScale.Position;
  pole.Zoom := StrToFloat(EditZoom.Text) /100;
  Scene.PBDraw(PaintBox.Canvas);
  if (ToolKod = 7) and DownMouseFlag then
  begin
    PaintBox.canvas.Pen.Style := psDot;
    PaintBox.canvas.Brush.Style := bsClear;
    PaintBox.canvas.Pen.Color := clGray;
    PaintBox.Canvas.Rectangle(Pole.PRect);
  end;
  {if ((HScrollBar.max - HScrollBar.min) <> 0) and (Pmin <> ToFloatPoint(10000, 10000)) then HScrollBar.Visible := True else HScrollBar.Visible := False;
  if ((VScrollBar.max - VScrollBar.min) <> 0) then VScrollBar.Visible := True else VScrollBar.Visible := False;
  if (Pmin.x < Pole.LocalToWorld(Point(0, 0)).x) and (Pmin <> ToFloatPoint(10000, 10000)) then
    HScrollBar.Min := HScrollBar.Min - trunc(PMin.x - Pole.LocalToWorld(Point(0, 0)).x);
  if (Pmin.y < Pole.LocalToWorld(Point(0, 0)).y) and (Pmin <> ToFloatPoint(10000, 10000)) then
    VScrollBar.Min := VScrollBar.Min - trunc(PMin.y - Pole.LocalToWorld(Point(0, 0)).y);
  if (PMax.x > Pole.LocalToWorld(Point(PaintBox.Width, PaintBox.Height)).x) then
    HScrollBar.Max := HScrollBar.Max + trunc(PMax.x - Pole.LocalToWorld(Point(PaintBox.Width, PaintBox.Height)).x);
  if (PMax.y > Pole.LocalToWorld(Point(PaintBox.Width, PaintBox.Height)).y) then
    VScrollBar.Max := VScrollBar.Max + trunc(PMax.y - Pole.LocalToWorld(Point(PaintBox.Width, PaintBox.Height)).y);    }
end;

procedure TMyForm.ButtonToolClick(Sender: TObject);
begin
  ToolKod := TBitBtn(sender).tag;
  PLineFlag := False;
  MyForm.Param;
end;

procedure TMyForm.TToolClearClick(Sender: TObject);
begin
  Scene.PBClear();
  DWidthLine := 0;
  UpDownScale.Position := 1;
  PLineFlag := False;
  Pole.Zoom := 1;
  Pole.Shift := ToFloatPoint(0, 0);
  EditZoom.Text := '100';
  PMin := ToFloatPoint(10000, 10000);
  PMax := ToFloatPoint(0, 0);
  paintbox.Invalidate;
end;

procedure TMyForm.UpDownRoundChanging(Sender: TObject; var AllowChange: Boolean
  );
begin
  ConstRectRound := UpDownRound.Position;
end;

procedure TMyForm.UpDownScaleChanging(Sender: TObject; var AllowChange: Boolean);
begin
  WidthLine := UpDownScale.Position;
end;

procedure TMyForm.Param;
begin
  PenColorBox.Visible := ToolKod in [0..5];
  ScaleListBox.Visible := ToolKod in [0..5];
  StyleBox.Visible := ToolKod in [0..5];
  FillBox.Visible := ToolKod in [3..5];
  BrushColorBox.Visible := ToolKod in [3..5];
  RoundListBox.Visible := ToolKod = 5;
end;

procedure TMyForm.VScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  Pole.Shift.y -= VScrollBar.Position;
end;




end.

