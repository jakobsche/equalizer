unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses FormEx, FctForm;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FormAdjust(Self)
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  if not Assigned(FunctionForm) then FunctionForm.Create(Application);
  FunctionForm.Show;
  FunctionForm.BringToFront;
end;

end.

