unit EditCreate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, Graphics, ExtCtrls, StdCtrls, Controls, spin, LCLType, FPCanvas, UTypes, Dialogs;

type

  { TFigureEdit }

  TFigureEdit = class
  private
    lbl: TLabel;
    obj: array of TPersistent;
    prop: PPropInfo;
    procedure Change(Sender: TObject); virtual;
  public
    destructor Destroy; override;
    constructor Create(pObj: array of TPersistent; pProp: PPropInfo; panel: TPanel; defaultProp: boolean); virtual;
    procedure Refresh; virtual; abstract;
  end;

  { TEditPenStyle }

  TEditPenStyle = class(TFigureEdit)
  private
    cmbbox: TComboBox;
    procedure cmbboxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure Change(Sender: TObject); override;
  public
    constructor Create(pObj: array of TPersistent; pProp: PPropInfo; panel: TPanel; defaultProp: boolean); override;
    destructor Destroy; override;
    procedure Refresh; override;
  end;

  { TEditStr }

  TEditStr = class(TFigureEdit)
  private
    ed: TEdit;
    procedure Change(Sender: TObject); override;
  public
    constructor Create(pObj: array of TPersistent; pProp: PPropInfo; panel: TPanel; defaultProp: boolean); override;
    destructor Destroy; override;
    procedure Refresh; override;
  end;

  { TEditeTextStyle }

  TEditeTextStyle = class(TFigureEdit)
  private
    but: TButton;
    procedure Change(Sender: TObject); override;
  public
    constructor Create(pObj: array of TPersistent; pProp: PPropInfo; panel: TPanel; defaultProp: boolean); override;
    destructor Destroy; override;
    procedure Refresh; override;
  end;

  { TEditInt }

  TEditInt = class(TFigureEdit)
  private
    spin: TSpinEdit;
    onChange: Boolean;
    procedure Change(Sender: TObject); override;
  public
    constructor Create(pObj: array of TPersistent; pProp: PPropInfo; panel: TPanel; defaultProp: boolean); override;
    destructor Destroy; override;
    procedure Refresh; override;
  end;



  { TEditBrushStyle }

  TEditBrushStyle = class(TFigureEdit)
  private
    cmbbox: TComboBox;
    procedure cmbboxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure Change(Sender: TObject); override;
  public
    constructor Create(pObj: array of TPersistent; pProp: PPropInfo; panel: TPanel; defaultProp: boolean); override;
    destructor Destroy; override;
    procedure Refresh; override;
  end;


  { TEditRun }

  TEditRun = class
  private
   obj: array of TPersistent;
   editors: array of TFigureEdit;
   defaultProp: boolean;
   panel: TPanel;
   FPenColor, FBrushColor: TColor;
   procedure SetColor(s: string; c: TColor);
 public
   constructor Create(pPanel: TPanel);
   procedure EditChoiceOne(pObj: TPersistent);
   procedure EditChoice(pObj: array of TPersistent);
   procedure Refresh;
   procedure EClear;
   procedure SetPenColor(color: TColor);
   procedure SetBrushColor(color: TColor);
  end;


  ClassOfEdit = class of TFigureEdit;
  ArrOfEdit = array of record
    item: ClassOfEdit;
    sstr: ShortString;
  end;

  { TFEditorContainer }

  TFEditorContainer = class
  private
    eArr: ArrOfEdit;
  public
    procedure addTool(e: ClassOfEdit; psstr: ShortString);
    property editor: ArrOfEdit read eArr;
  end;

var
  PropValues: TStringList;
  FigureEditor: TEditRun;
  PropNames: TStringList;
  EditorContainer: TFEditorContainer;
  PBInd: procedure of Object;

implementation

{ TEditeTextStyle }


procedure TEditeTextStyle.Change(Sender: TObject);
var
  FD: TFontDialog;
  i: integer;
begin
  FD := TFontDialog.Create(nil);
  if FD.Execute then
  but.Caption := FD.Font.Name;
  for i := 0 to high(obj) do
    SetStrProp(obj[i], prop, but.Caption);
  PropValues.Values[prop^.Name] := but.Caption;
  inherited Change(Sender);
  FD.destroy;
end;

constructor TEditeTextStyle.Create(pObj: array of TPersistent;
  pProp: PPropInfo; panel: TPanel; defaultProp: boolean);
var
  i: integer;
begin
  but := TButton.Create(nil);
  but.parent := panel;
  but.Left := trunc(panel.width * 0.5) + 2;
  but.width := trunc(panel.Width * 0.5) - 4;
  but.Caption := 'Times New Roman';
  but.OnClick := @Change;
  but.Top := panel.tag;
  inherited Create(pObj, pProp, panel, defaultProp);
  if defaultProp and (PropValues.Values[prop^.Name] <> '') Then
    for i := 0 to high(obj) do
      SetStrProp(obj[i], prop, PropValues.Values[prop^.Name]);
  Refresh;
end;

destructor TEditeTextStyle.Destroy;
begin
  but.destroy;
  inherited Destroy;
end;

procedure TEditeTextStyle.Refresh;
var
  i: integer;
  s: string;
begin
  s := GetStrProp(obj[0], prop);
  for i := 0 to high(obj) do
    if GetStrProp(obj[i], prop) <> s then
    begin
      s := GetStrProp(obj[0], prop);
      break;
    end;
  but.Caption := s;
end;

{ TEditStr }

procedure TEditStr.Change(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to high(obj) do
    SetStrProp(obj[i], prop, ed.text);
  PropValues.Values[prop^.Name] := Ed.Text;
  inherited Change(Sender);
end;

constructor TEditStr.Create(pObj: array of TPersistent; pProp: PPropInfo;
  panel: TPanel; defaultProp: boolean);
var
  i: integer;
begin
  ed := TEdit.Create(nil);
  ed.parent := panel;
  ed.Left := trunc(panel.width * 0.5) + 2;
  ed.width := trunc(panel.Width * 0.5) - 4;
  ed.OnChange := @Change;
  ed.Top := panel.tag;
  inherited Create(pObj, pProp, panel, defaultProp);
  if defaultProp and (PropValues.Values[prop^.Name] <> '') Then
    for i := 0 to high(obj) do
      SetStrProp(obj[i], prop, PropValues.Values[prop^.Name]);
  Refresh;
end;

destructor TEditStr.Destroy;
begin
  ed.destroy;
  inherited Destroy;
end;

procedure TEditStr.Refresh;
var
  i: integer;
  s: string;
begin
  s := GetStrProp(obj[0], prop);
  for i := 0 to high(obj) do
    if GetStrProp(obj[i], prop) <> s then
    begin
      s := GetStrProp(obj[0], prop);
      break;
    end;
  ed.text := s;
end;

{ TEditRun }

procedure TEditRun.SetColor(s: string; c: TColor);
var
  list: PPropList;
  i, j, k: integer;
begin
  if obj = nil then exit;
  for k := 0 to high(obj) do
  begin
    j := GetPropList(obj[k], list);
    for i := 0 to j - 1 do
      if list^[i]^.Name = s then SetInt64Prop(obj[k], list^[i], c);
  end;
  PBInd;
  Hist;
end;

constructor TEditRun.Create(pPanel: TPanel);
begin
  panel := pPanel;
end;

procedure TEditRun.EditChoiceOne(pObj: TPersistent);
var
  arr: array of TPersistent;
begin
  if pObj = nil then
  begin
    EClear;
    exit;
  end;
  defaultProp := true;
  setLength(arr, 1);
  arr[0] := pObj;
  EditChoice(arr);
end;

procedure TEditRun.EditChoice(pObj: array of TPersistent);
function CheckObj(aobj: TObject; prop: PPropInfo): boolean;
var
  i, j: integer;
  list: PPropList;
begin
  j := GetPropList(aobj, list);
  result := false;
  for i := 0 to j - 1 do
    if list^[i] = prop then
    begin
      result := true;
      exit;
    end;
end;
var
  list: PPropList;
  i, j: integer;
  b: boolean;
begin
  EClear;
  setLength(obj, length(pObj));
  if length(pObj) = 0 then exit;
  for i := 0 to high(pObj) do
    obj[i] := pObj[i];
  panel.caption := '';
  panel.tag := 0;
  j := GetPropList(obj[0], list);
  for i := 0 to j - 1 do
    begin
    b := true;
    for j := 1 to high(obj) do
      if not CheckObj(obj[j], list^[i]) then
      begin
        b := false;
        break;
      end;
    if b then
    begin
      for j := 0 to high(EditorContainer.editor) do
        if list^[i]^.Name = EditorContainer.editor[j].sstr then
        begin
          SetLength(editors, length(editors) + 1);
          editors[high(editors)] := EditorContainer.editor[j].item.Create(obj, list^[i], panel, defaultProp);
          break;
        end;
    end;
  end;
  if defaultProp then
  begin
    SetPenColor(FPenColor);
    SetBrushColor(FBrushColor);
  end;
  defaultProp := false;
end;

procedure TEditRun.Refresh;
var
  i: integer;
begin
  for i := 0 to high(editors) do
    editors[i].Refresh;
end;

procedure TEditRun.EClear;
var
  i: integer;
begin
  for i := 0 to high(editors) do
    editors[i].Destroy;
  setLength(editors, 0);
end;

procedure TEditRun.SetPenColor(color: TColor);
begin
  FPenColor := color;
  SetColor('FPenColor', color);
end;

procedure TEditRun.SetBrushColor(color: TColor);
begin
  FBrushColor := color;
  SetColor('FBrushColor', color);
end;


{ TFEditorContainer }

procedure TFEditorContainer.addTool(e: ClassOfEdit; psstr: ShortString);
begin
  setLength(eArr, length(eArr) + 1);
  eArr[high(eArr)].item := e;
  eArr[high(eArr)].sstr := psstr;
end;

{ TEditBrushStyle }

procedure TEditBrushStyle.cmbboxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  cbox: TCombobox;
begin
  cbox := TCombobox(Control);
  cbox.Canvas.Brush.Color := clWhite;
  If odFocused in State Then cbox.Canvas.Brush.Color:= cl3DLight;
  cbox.Canvas.FillRect(ARect);
  cbox.Canvas.Brush.Color := clBlack;
  cbox.Canvas.Brush.Style := TFPBrushStyle(cbox.Items.Objects[Index]);
  cbox.Canvas.FillRect(ARect.Left + 1, (ARect.Bottom+ARect.Top) div 2 - 5 , ARect.Right - 1, (ARect.Bottom + ARect.Top) div 2 + 5);
end;

procedure TEditBrushStyle.Change(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to high(obj) do
    SetInt64Prop(obj[i], prop, TCombobox(sender).ItemIndex);
  PropValues.Values[prop^.Name] := intToStr(TCombobox(sender).ItemIndex);
  inherited Change(Sender);
end;

constructor TEditBrushStyle.Create(pObj: array of TPersistent; pProp: PPropInfo;
  panel: TPanel; defaultProp: boolean);
var
  b: TFPBrushStyle;
  i: integer;
begin
  cmbbox := TComboBox.Create(nil);
  cmbbox.parent := panel;
  cmbbox.Left := trunc(panel.width * 0.5) + 2;
  cmbbox.width := trunc(panel.Width * 0.5) - 4;
  for b in TFPBrushStyle do
    if not (b in [bsImage, bsPattern]) then cmbbox.AddItem('',TObject(b));
  cmbbox.OnDrawItem := @cmbboxDrawItem;
  cmbbox.OnChange := @Change;
  cmbbox.Style := csOwnerDrawFixed;
  cmbbox.ReadOnly := True;
  cmbbox.Top := panel.tag;
  inherited Create(pObj, pProp, panel, defaultProp);
  if defaultProp and (PropValues.Values[prop^.Name] <> '') Then
    for i := 0 to high(obj) do
      SetInt64Prop(obj[i], prop, StrToInt64(PropValues.Values[prop^.Name]));
  Refresh;
end;

destructor TEditBrushStyle.Destroy;
begin
  cmbbox.Destroy;
  inherited Destroy;
end;

procedure TEditBrushStyle.Refresh;
var
  i, j: integer;
begin
  j := GetInt64Prop(obj[0], prop);
  for i := 1 to high(obj) do
    if GetInt64Prop(obj[i], prop) <> j then
    begin
      j := integer(bsClear);
      break;
    end;
  for i := 0 to cmbbox.Items.Count  do
    if integer(cmbbox.Items.Objects[i]) = j then
    begin
      cmbbox.ItemIndex := i;
      exit;
    end;
end;


{ TEditPenStyle }

procedure TEditPenStyle.cmbboxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  cbox: TCombobox;
begin
  cbox := TCombobox(Control);
  cbox.Canvas.Brush.Color := clWhite;
  cbox.Canvas.FillRect(ARect);
  cbox.Canvas.Pen.Color := clBlack;
  cbox.Canvas.Pen.Style := TFPPenStyle(cbox.Items.Objects[Index]);
  cbox.Canvas.Line(ARect.Left, (ARect.Bottom + ARect.Top) div 2, ARect.Right, (ARect.Bottom + ARect.Top) div 2);
end;

procedure TEditPenStyle.Change(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to high(obj) do
    SetInt64Prop(obj[i], prop, cmbbox.ItemIndex);
  PropValues.Values[prop^.Name] := intToStr(cmbbox.ItemIndex);
  inherited Change(Sender);
end;

constructor TEditPenStyle.Create(pObj: array of TPersistent; pProp: PPropInfo;
  panel: TPanel; defaultProp: boolean);
var
  i: TFPPenStyle;
  j: integer;
begin
  cmbbox := TComboBox.Create(nil);
  cmbbox.parent := panel;
  cmbbox.Left := trunc(panel.width * 0.5) + 2;
  cmbbox.width := trunc(panel.Width * 0.5) - 4;
  for i in TFPPenStyle do
    if not (i in [psinsideFrame,psPattern,psClear]) then
      cmbbox.AddItem('',TObject(i));
  cmbbox.OnDrawItem := @cmbboxDrawItem;
  cmbbox.OnChange := @Change;
  cmbbox.Style := csOwnerDrawFixed;
  cmbbox.ReadOnly := True;
  cmbbox.Top := panel.tag;
  inherited Create(pObj, pProp, panel, defaultProp);
  if defaultProp and (PropValues.Values[prop^.Name] <> '') then
    for j := 0 to high(obj) do
      SetInt64Prop(obj[j], prop, StrToInt64(PropValues.Values[prop^.Name]));
  Refresh;
end;

destructor TEditPenStyle.Destroy;
begin
  cmbbox.Destroy;
  inherited Destroy;
end;

procedure TEditPenStyle.Refresh;
var
  i, j: integer;
begin
  j := GetInt64Prop(obj[0], prop);
  for i := 1 to high(obj) do
    if GetInt64Prop(obj[i], prop) <> j then
    begin
      j := integer(psSolid);
      break;
    end;
  for i := 0 to cmbbox.Items.Count - 1 do
  begin
    if integer(cmbbox.Items.Objects[i]) = j Then
    begin
      cmbbox.ItemIndex := i;
      exit;
    end;
  end;
end;

{ TEditInt }

procedure TEditInt.Change(Sender: TObject);
var
  i: integer;
begin
  if onChange then
  begin
    onChange := false;
    exit;
  end;
  for i := 0 to high(obj) do
    SetInt64Prop(obj[i], prop, TSpinEdit(Sender).Value);
  PropValues.Values[prop^.Name] := intToStr(TSpinEdit(Sender).Value);
  inherited Change(Sender);
end;

constructor TEditInt.Create(pObj: array of TPersistent; pProp: PPropInfo;
  panel: TPanel; defaultProp: boolean);
var
  i: integer;
begin
  spin := TSpinEdit.Create(nil);
  spin.MinValue := 1;
  spin.MaxValue := 100;
  spin.parent := panel;
  spin.Left := trunc(panel.width * 0.5) + 2;
  spin.width := trunc(panel.Width * 0.5) - 4;
  spin.OnChange := @Change;
  spin.Top := panel.tag;
  inherited Create(pObj, pProp, panel, defaultProp);
  if defaultProp and (PropValues.Values[prop^.Name] <> '') Then
    for i := 0 to high(obj) do
      SetInt64Prop(obj[i], prop, StrToInt64(PropValues.Values[prop^.Name]));
  Refresh;
end;

destructor TEditInt.Destroy;
begin
  spin.destroy;
  inherited Destroy;
end;

procedure TEditInt.Refresh;
var j: Int64; i: integer;
begin
  j := GetInt64Prop(obj[0], prop);
  for i := 0 to high(obj) do
    if GetInt64Prop(obj[i], prop) <> j then
    begin
      j := GetInt64Prop(obj[i], prop);
      onChange := true;
      break;
    end;
  spin.value := j;
end;

{ TFigureEdit }

procedure TFigureEdit.Change(Sender: TObject);
begin
  PBInd;
  Hist;
end;

destructor TFigureEdit.Destroy;
begin
  lbl.Destroy;
end;

constructor TFigureEdit.Create(pObj: array of TPersistent; pProp: PPropInfo;
  panel: TPanel; defaultProp: boolean);
var
  i: integer;
begin
  setLength(obj, length(pObj));
  for i := 0 to high(pObj) do
    obj[i] := pObj[i];
  prop := pProp;
  lbl := TLabel.Create(nil);
  lbl.Parent := panel;
  lbl.Caption := PropNames.Values[pProp^.Name];
  lbl.Left := 2;
  lbl.Width := trunc(panel.width * 0.5) - 4;
  lbl.Top := panel.tag + 4;
  if lbl.Caption = '' Then lbl.Caption := pProp^.Name;
  panel.tag := panel.tag + 25;
end;


initialization

EditorContainer := TFEditorContainer.create;
EditorContainer.addTool(TEditStr, 'FTextRect');
EditorContainer.addTool(TEditeTextStyle,'FTextName');
EditorContainer.addTool(TEditInt, 'WidthFigure');
EditorContainer.addTool(TEditPenStyle, 'FPenStyle');
EditorContainer.addTool(TEditBrushStyle, 'FBrushStyle');
EditorContainer.addTool(TEditInt, 'FRoundRect');

PropValues := TStringList.Create;
PropValues.Values['FTextRect'] := ' ';
PropValues.Values['WidthFigure'] := '3';
PropValues.Values['FPenStyle'] := '0';
PropValues.Values['FBrushStyle'] := '0';
PropValues.Values['FRoundRect'] := '15';

PropNames := TStringList.Create;
PropNames.Values['FTextRect'] := 'Текст';
PropNames.Values['FTextName'] := 'Стиль';
PropNames.Values['WidthFigure'] := 'Толщина пера';
PropNames.Values['FPenStyle'] :='Стиль линии';
PropNames.Values['FBrushStyle'] := 'Вид заливки';
PropNames.Values['FRoundRect'] := 'Радиус скругления';
PropNames.Values['FPenColor'] := 'Цвет пера';
PropNames.Values['FBrushColor'] := 'Цвет кисти';
end.


