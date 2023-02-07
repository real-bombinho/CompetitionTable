unit Settings;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  TTableStyle = (tsHTML, tsText);

  { TForm3 }   // orphaned stub only at the moment

  TForm3 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    function getPrecision: integer;
    function getStyle: TTableStyle;
    function getUnits: string;
    procedure SetStyle(AValue: TTableStyle);

  public
    DoSave: boolean;
    property Units: string read getUnits;
    property Precision: integer read getPrecision;
    property Style: TTableStyle read getStyle write setStyle;
  end;

var
  SettingsForm: TForm3;

implementation

{$R *.lfm}

const
  unitPrecision: array[0..1] of integer = (0, 3);


{ TForm3 }

procedure TForm3.Button2Click(Sender: TObject);
begin
  DoSave := false;
  Close;
end;

function TForm3.getPrecision: integer;
begin
  if comboBox1.ItemIndex = -1 then
    raise exception.Create('no unit selected');
  result := unitPrecision[comboBox1.ItemIndex]
end;

function TForm3.getStyle: TTableStyle;
begin
  case ComboBox2.ItemIndex of
    0: result := tsHTML;
    1: result := tsText;
  end;
end;

function TForm3.getUnits: string;
begin
  case ComboBox2.ItemIndex of
    0: result := ' ' + comboBox1.Text + '</tr>';
    1: result := ' ' + comboBox1.Text + '|';
  end;
end;


procedure TForm3.SetStyle(AValue: TTableStyle);
begin
  case AValue of
    tsHTML: comboBox2.ItemIndex := 0;
    tsText: comboBox2.ItemIndex := 1;
  end;
end;

procedure TForm3.Button1Click(Sender: TObject);
begin
  DoSave := true;
  Close;
end;

end.

