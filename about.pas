unit About;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TAboutForm1 }

  TAboutForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  AboutForm1: TAboutForm1;

implementation

{$R *.lfm}

{ TAboutForm1 }

procedure TAboutForm1.Button1Click(Sender: TObject);
begin
  Close;
end;

end.

