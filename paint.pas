unit paint;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Menus, StdCtrls, CheckLst, ColorBox, typinfo, ctool, DrawScene, WorldPole;
type

  { TMyForm }

  TMyForm = class(TForm)
    ColorBox: TColorBox;
    PaintPanel: TPanel;
    HorizontalSB: TScrollBar;
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
  private
    DownMouseFlag: boolean;
    ToolKod: integer;
  public
    { public declarations }
  end;

var
  MyForm: TMyForm;
  k : boolean;
  xx, yy: integer;
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
  //c = 5;
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
    //b.Top := c;
    //b.left := (i + 1)*a + c*(i + 1);
    b.Align := alLeft;
    b.tag := i;
    b.OnClick:=@ButtonToolClick;
  end;
  PoleZoom := 1;
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
    consttool.tool[ToolKod].MouseDown(x, y);
    paintbox.Invalidate;
  end
  else
    PLineFlag := False;
end;

procedure TMyForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: integer);
begin
  if DownMouseFlag then
      consttool.tool[ToolKod].MouseMove(x, y);
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
end;

procedure TMyForm.TToolClearClick(Sender: TObject);
begin
  Scene.PBClear();
  PLineFlag := False;
  paintbox.Invalidate;
end;


end.

