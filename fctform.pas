unit FctForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, TAGraph, TASeries,
  BandPass;

type

  { TFunctionForm }

  TFunctionForm = class(TForm)
    Chart1: TChart;
    VoltageTransfer: TLineSeries;
    VoltageTransferDQ: TLineSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  public
    LowPassFilter: TLowPassFilter;
  end;

var
  FunctionForm: TFunctionForm;

implementation

{$R *.lfm}

{ TFunctionForm }

procedure TFunctionForm.FormCreate(Sender: TObject);
const
  N = 1000;
  MIN = 10;
  MAX = 1000;
var
  i: Integer;
  f, f0, U, U0, DQ: Double;
begin
  LowPassFilter := TLowPassFilter.Create;
  LowPassFilter.InstanceName := 'LowPassFilter';
  for i := 0 to N - 1 do begin
    f := MIN + (MAX - MIN) * i / (N - 1);
    U := LowPassFilter.VoltageAmplitudeTransfer(f);
    VoltageTransfer.AddXY(f, U);
    {WriteLn(i, f, LowPassFilter.VoltageAmplitudeTransfer(f),
      LowPassFilter.ComplexVoltageTransfer.Re,
      LowPassFilter.ComplexVoltageTransfer.Im);}
    if i = 0 then begin
      f0 := 0;
      U0 := U
    end
    else begin
      DQ := (U - U0) / (f - f0);
      VoltageTransferDQ.AddXY(f, DQ);
      f := f0; U := U0
    end;
  end;
end;

procedure TFunctionForm.FormDestroy(Sender: TObject);
begin
  LowPassFilter.Free
end;

end.

