unit paint;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Menus, StdCtrls, CheckLst, ColorBox, ComCtrls, typinfo, ctool,
  DrawScene, WorldPole;
type

  { TMyForm }

  TMyForm = class(TForm)
    StyleBox: TComboBox;
    FillBox: TComboBox;
    RoundListBox: TEdit;
    ScaleListBox: TEdit;
    LoupePlus: TBitBtn;
    LoupeMinus: TBitBtn;
    ColorBox: TColorBox;
    PaintPanel: TPanel;
    HorizontalSB: TScrollBar;
    UpDownScale: TUpDown;
    UpDownRound: TUpDown;
    VerticalSB: TScrollBar;
    TToolClear: TBitBtn;
    MainMenu: TMainMenu;
    MenuItemFile: TMenuItem;
    MenuItemFAQ: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemAutor: TMenuItem;
    PaintBox: TPaintBox;
    PanelTools: TPanel;
    Panel: TPanel;
    procedure ColorBoxSelect(Sender: TObject);
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
  PoleZoom := 1;
  MyForm.Param;
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
  Scene.PBLoupe('+');
  PLineFlag := False;
  paintbox.Invalidate;
end;

procedure TMyForm.ToolLoupeMinusClick(Sender: TObject);
begin
  Scene.PBLoupe('-');
  PLineFlag := False;
  paintbox.Invalidate;
end;

procedure TMyForm.ColorBoxSelect(Sender: TObject);
begin
  ColorLine := ColorBox.Selected;
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
end;

procedure TMyForm.PaintBoxPaint(Sender: TObject);
begin
  Scene.PBDraw(PaintBox.Canvas);
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
  PLineFlag := False;
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
  ColorBox.Visible := ToolKod in [0..5];
  ScaleListBox.Visible := ToolKod in [0..5];
  StyleBox.Visible := ToolKod in [0..5];
  FillBox.Visible := ToolKod in [3..5];
  RoundListBox.Visible := ToolKod = 5;
end;




end.

