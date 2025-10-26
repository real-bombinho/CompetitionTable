unit CompetitionMain;

// MIT License
//
// Copyright (c) 2022 real-bombinho
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Grids, StdCtrls,
  About, Settings, Math, Types;

type

  TEntry = record
    value: single;
    name: string;
  end;

  { TCompetitionList }

  TCompetitionList = class
  private
    entries: array of TEntry;
    Fprecision: integer;
    function getName(index: integer): string;
    function getValue(index: integer): string;
    procedure setName(index: integer; AValue: string);
    procedure Setprecision(AValue: integer);
    procedure setValue(index: integer; AValue: string);
    procedure sort;
    function isExisting(const name: string): boolean;
  public
    function Add(const Name: string; const value: single): boolean;
    function Count: integer;
    function SumTop(const cnt: integer): string;
    procedure Clear;
    property Precision: integer read Fprecision write Setprecision;
    property Name[index: integer] : string read getName write setName;
    property Value[index: integer]: string read getValue write setValue;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    ButtonAdd: TButton;
    EditName: TEdit;
    EditResult: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    StringGrid1: TStringGrid;
    procedure ButtonAddClick(Sender: TObject);
    procedure EditResultKeyPress(Sender: TObject; var Key: char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItemNewClick(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure StringGrid1EditingDone(Sender: TObject);
    procedure StringGrid1SelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    CompetitionList: TCompetitionList;
    FDataAltered: boolean;
    FCaption: string;
    saveFileName: string;
    procedure SetDataAltered(AValue: boolean);
    function ShowCentredModal(const form: TForm): integer;
    procedure displayTable;
    procedure displayHTMLtable(const zeroString: string);
    procedure displayTextTable(const zeroString: string);
    function saveFile: boolean;
    function loadFile: boolean;
  public
     EntryCount: integer;
     property DataAltered: boolean read FDataAltered write SetDataAltered;
  end;

var
  OctopusForm: TForm1;

implementation

{$R *.lfm}

{ TCompetitionList }

function TCompetitionList.Add(const Name: string; const value: single): boolean;
var i: integer;
begin
  if (not isNAN(value)) and (not isExisting(Name)) then
  begin
    i := length(entries);
    setLength(entries, i + 1);
    entries[i].value := value;
    entries[i].name := Name;
    sort;
    result := true;
  end
  else result := false;
end;

function TCompetitionList.Count: integer;
begin
  result := length(entries);
end;

function TCompetitionList.SumTop(const cnt: integer): string;
var i: integer;
    r: single;
begin
  r := 0;
  for i := 0 to cnt - 1 do
    if i < length(entries) then r := r + entries[i].value;
  result := floatToStrF(r, ffFixed, 3, FPrecision);
end;

procedure TCompetitionList.Clear;
begin
  setLength(entries, 0);
end;

procedure TCompetitionList.sort;
var i: integer;
    n: boolean;

  procedure swap(const i1, i2: integer);
  var e: TEntry;
  begin
    e.name := entries[i1].name;
    e.value := entries[i1].value;
    entries[i1].name := entries[i2].name;
    entries[i1].value := entries[i2].value;
    entries[i2].name := e.name;
    entries[i2].value := e.value;
    n := false;
  end;

begin
  if length(entries) > 1 then
  begin
    repeat
      n := true;
      for i := 1 to length(entries) - 1 do
        if entries[i - 1].value < entries[i].value then swap(i-1, i);
    until n;
  end;
end;

function TCompetitionList.getName(index: integer): string;
begin
  result := entries[index].name;
end;

function TCompetitionList.getValue(index: integer): string;
begin
  result := floatToStrF(entries[index].value, ffFixed, 3, FPrecision);
end;

procedure TCompetitionList.setName(index: integer; AValue: string);
begin
  if entries[index].name <> aValue then
  begin
    entries[index].name := aValue;

  end;

end;

procedure TCompetitionList.Setprecision(AValue: integer);
begin
  if Fprecision = AValue then Exit;
  Fprecision := AValue;
end;

procedure TCompetitionList.setValue(index: integer; AValue: string);
var f: single;
begin
  f := strtofloatDef(aValue, NaN);
  if not IsNan(f) then
  begin
    if entries[index].value <> f then
    begin
      entries[index].value := f;
      OctopusForm.DataAltered := true;
    end;
  end;
end;

function TCompetitionList.isExisting(const name: string): boolean;
var i: integer;
begin
  result := false;
  if length(entries) < 1 then exit;
  for i := 0 to length(entries) - 1 do
    if uppercase(entries[i].name) = uppercase(name) then
    begin
      result := true;
      break;
    end;
end;

{ TForm1 }

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  ShowCentredModal(AboutForm);
end;

procedure TForm1.MenuItem5Click(Sender: TObject);
begin
  saveFile;
end;

procedure TForm1.MenuItem6Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    SaveFileName := SaveDialog1.FileName;
    if fileExists(SaveFileName) then
      if MessageDlg('Question', 'Do you wish to overwrite?', mtConfirmation,
         [mbYes, mbNo],0) = mrNo
      then
        exit;
    saveFile;
  end;
end;

procedure TForm1.MenuItem7Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    if fileExists(OpenDialog1.FileName) then
    begin
      saveFileName := OpenDialog1.FileName;
      loadFile;
    end
    else
      showmessage('No valid file selected');
  end;
end;

procedure TForm1.MenuItemNewClick(Sender: TObject);
begin
  saveFileName := '';
  StringGrid1.Clear;
  StringGrid1.RowCount := 6;
  CompetitionList.Clear;
  displayTable;
  dataAltered := false;
end;

procedure TForm1.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var f: single;
begin
  if aCol = 2 then
  begin
    if StringGrid1.Cells[2, aRow] <> '' then
    begin
      f := strtofloatDef(StringGrid1.Cells[2, aRow], NaN);
      if isNaN(f) then
      begin
        StringGrid1.Canvas.Font.Color := clWhite;
        StringGrid1.Canvas.Brush.Color := clRed;
      end
      else
      begin
        StringGrid1.Canvas.Font.Color := clBlack;
        StringGrid1.Canvas.Brush.Color := clWindow;
      end;
      StringGrid1.Canvas.FillRect(ARect);
      StringGrid1.Canvas.TextOut(ARect.Left + 2, ARect.Top + 2, StringGrid1.Cells[ACol, ARow]);
    end;
  end;
end;

procedure TForm1.StringGrid1EditingDone(Sender: TObject);
begin
  displayTable;
end;

procedure TForm1.StringGrid1SelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);
begin
  if StringGrid1.Cells[0, aRow] <> '' then
    Editor := StringGrid1.EditorByStyle(cbsAuto) // allow editing
  else
  begin
    Editor := StringGrid1.EditorByStyle(cbsNone);
  end;
end;

procedure TForm1.StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
var f: single;
begin
  if aCol = 1 then
  begin
    CompetitionList.Name[aRow - 1]  := StringGrid1.Cells[1, aRow];
  end;
  if aCol = 2 then
  begin
    f := strtofloatDef(StringGrid1.Cells[2, aRow], NaN);
    if isNaN(f) then
    begin

    end;
    CompetitionList.Value[aRow - 1]  := StringGrid1.Cells[2, aRow];
    //StringGrid1.Cells[2, i] := CompetitionList.Value[i - 1];
  end;
end;

function TForm1.ShowCentredModal(const form: TForm): integer;
begin
  form.Left := OctopusForm.Left + (OctopusForm.Width div 2) - (form.Width div 2);
  form.Top := OctopusForm.Top + (OctopusForm.Height div 2) - (form.Height div 2);
  result := form.ShowModal;
end;

procedure TForm1.SetDataAltered(AValue: boolean);
begin
  if FDataAltered = AValue then Exit;
  FDataAltered := AValue;
  if FDataAltered then
  begin
    if pos(' *', Caption) = 0 then
      Caption := FCaption + ' *';
  end
  else
  begin
    if pos(' *', Caption) <> (length(Caption) - 2) then
      Caption := FCaption;
  end;
end;

procedure TForm1.displayTable;
var zs: string;
begin
  Memo1.Clear;
  case SettingsForm.precision of
    0: zs := '0';
    3: zs := '0.000';
  end;
  case SettingsForm.Style of
    TTableStyle.tsHTML: displayHTMLTable(zs);
    TTableStyle.tsText: displayTextTable(zs);
  end;
end;

procedure TForm1.displayHTMLtable(const zeroString: string);
var i: integer;
begin
  Memo1.Append('<table>');
  Memo1.Append('<tr><th>Rank<th>Participant<th>Result</tr>');
  i := 0;
  while (i < EntryCount) do
  begin
    if (i <= CompetitionList.Count -1) then
      Memo1.Append('<tr><td>' + inttostr(i + 1) + '.<td>@' +
        CompetitionList.Name[i] + '<td>' +
        CompetitionList.Value[i] + SettingsForm.units + '</tr>')
    else
      Memo1.Append('<tr><td>' + inttostr(i + 1) + '.<td>@<td>' + zeroString +
        SettingsForm.units + '</tr>');
    inc(i);
  end;
  Memo1.Append('<tr><th><th>TOTAL Top' + inttostr(EntryCount) + '<th>' +
    CompetitionList.SumTop(EntryCount) + SettingsForm.units + '</tr>');
  Memo1.Append('</table>');
end;

procedure TForm1.displayTextTable(const zeroString: string);
var i: integer;
begin
  Memo1.Append('|Rank|Participant|Result|');
  Memo1.Append('| --- | --- | --- |');
  i := 0;
  while (i < EntryCount) do
  begin
    if (i <= CompetitionList.Count -1) then
      Memo1.Append('|' + inttostr(i + 1) + '.|@' +
        CompetitionList.Name[i] + '|' +
        CompetitionList.Value[i] + SettingsForm.units + '|')
    else
      Memo1.Append('|' + inttostr(i + 1) + '.| @ |' + zeroString +
        SettingsForm.units + '|');
    inc(i);
  end;
  Memo1.Append('||TOTAL Top' + inttostr(EntryCount) + '|' +
    CompetitionList.SumTop(EntryCount) + SettingsForm.units + '|');
end;

function TForm1.saveFile: boolean;
var i: integer;
    f: TextFile;
begin
  result := false;
  begin
    AssignFile(f, SaveFileName);
    try
      rewrite(f);
      for i := 0 to CompetitionList.Count - 1 do
      writeln(f, CompetitionList.Name[i] + ',' + CompetitionList.Value[i]);
      CloseFile(f);
      DataAltered := false;
      result := true;
    except
      on E: EInOutError do
        writeln('File handling error occurred. Details: ', E.ClassName, '/', E.Message);
    end;
  end;
  if not fileExists(SaveFileName) then result := false;
end;

function TForm1.loadFile: boolean;
var i: integer;
    f: TextFile;
    s: string;
    p: integer;
begin
  result := false;
  AssignFile(f, SaveFileName);
  try
    Reset(f);
    CompetitionList.Clear;
    i := 1;
    while not eof(f) do
    begin
      readln(f, s);
      p := pos(',', s);
      CompetitionList.Add(copy(s, 1, p - 1), strtofloatDef(copy(s, p + 1, 7), NaN) );
      if StringGrid1.RowCount < (i + 1) then
        StringGrid1.RowCount := i + 1;
      StringGrid1.Cells[0, i] := inttostr(i) + '.';
      StringGrid1.Cells[1, i] := CompetitionList.Name[i - 1];
      StringGrid1.Cells[2, i] := CompetitionList.Value[i - 1];
      inc(i);
    end;
    CloseFile(f);
    result := true;
  except
    on E: EInOutError do
    writeln('File handling error occurred. Details: ', E.ClassName, '/', E.Message);
  end;
  displayTable;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  SettingsForm.DoSave := false;
  SettingsForm.Edit1.Text := inttostr(EntryCount);
  ShowCentredModal(SettingsForm);
  if SettingsForm.DoSave then
  begin
    EntryCount := strtointDef(SettingsForm.Edit1.Text,5);
    competitionList.Precision := SettingsForm.Precision;
    displayTable;
  end;
end;

procedure TForm1.ButtonAddClick(Sender: TObject);
var i: integer;
    f: single;
begin
  f := strtofloatDef(EditResult.Text, NaN);
  if CompetitionList.Add(EditName.Text, f) then
  begin
    if CompetitionList.Count > 5 then
      StringGrid1.RowCount := CompetitionList.Count + 1;
    for i := 1 to CompetitionList.Count do
    begin
      StringGrid1.Cells[0, i] := inttostr(i)+'.';
      StringGrid1.Cells[1, i] := CompetitionList.Name[i - 1];
      StringGrid1.Cells[2, i] := CompetitionList.Value[i - 1];
    end;
    displayTable;
    DataAltered := true;
  end;
end;

procedure TForm1.EditResultKeyPress(Sender: TObject; var Key: char);
begin
  if Key in ['0'..'9', DefaultFormatSettings.DecimalSeparator, #9, #8, '-'] then
  else Key := #0;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if DataAltered then
    if MessageDlg('Warning', 'Unsaved progress, do you really want to leave?', mtConfirmation,
         [mbYes, mbNo],0) = mrNo
      then
        CanClose := false;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CompetitionList := TCompetitionList.Create;
  SaveFileName := 'default.csv';
  DataAltered := false;
  EntryCount := 5;
  FCaption := Caption;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Label1.Top := Height - 68;
  Label2.Top := Height - 68;
  EditName.Top := Height - 73;
  EditResult.Top := Height - 73;
  ButtonAdd.Top := Height - 73;
  StringGrid1.Height := Height - 142;
  Memo1.Height := Height - 142;
end;

end.

