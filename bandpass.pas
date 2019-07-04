unit BandPass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LinNetwk, ComplexMath;

type

  { TLowPassFilter }

  TLowPassFilter = class(TSerialDipoleCollection)
  private
    FComplexVoltageTransfer: TComplexQuotient;
    function GetComplexVoltageTransfer: TComplexQuotient;
  private
    R1, C1, RD, R2, R3a, R3b, C3: TStaticLinearDipole;
    Z1, Z3a: TParallelDipoleCollection;
    Z3aX: TSerialDipoleCollection;
  public
    constructor Create;
    function VoltageAmplitudeTransfer(AFrequency: Extended): Extended;
    property ComplexVoltageTransfer: TComplexQuotient
      read GetComplexVoltageTransfer;
  end;

  { TBandPassFilter }

  TBandPassFilter = class(TSerialDipoleCollection)
  private
    R3a, R3b, C3, R2, C2, R1, C1: TStaticLinearDipole;
    RC1, RC2, Z3a: TParallelDipoleCollection;
    Z3aX: TSerialDipoleCollection;
  public
    constructor Create;
  end;

implementation

{ TLowPassFilter }

function TLowPassFilter.GetComplexVoltageTransfer: TComplexQuotient;
begin
  if not Assigned(FComplexVoltageTransfer) then
    FComplexVoltageTransfer := TComplexQuotient.Create
  else FComplexVoltageTransfer.RemoveOperands;
  Z1.Frequency := Frequency;
  WriteLn(Z1.InstanceName, '.Impedance = (', Z1.Impedance.Re, ', ', Z1.Impedance.Im, ')');
  WriteLn('Impedance = (', Impedance.Re, ', ', Impedance.Im, ')');
  FComplexVoltageTransfer.AddOperand(Z1.Impedance);
  FComplexVoltageTransfer.AddOperand(Impedance);
  FComplexVoltageTransfer.Operate;
  Result := FComplexVoltageTransfer;
end;

constructor TLowPassFilter.Create;
begin
  inherited Create;
  R3b := TStaticLinearDipole.Create;
  R3b.InstanceName := 'R3b';
  R3b.Resistance := 11E3;
  AddDipole(R3b);
  Z3a := TParallelDipoleCollection.Create;
  Z3a.InstanceName := 'Z3a';
  R3a := TStaticLinearDipole.Create;
  R3a.InstanceName := 'R3a';
  R3a.Resistance := 11E3;
  Z3a.AddDipole(R3a);
  Z3aX := TSerialDipoleCollection.Create;
  Z3aX.InstanceName := 'Z3aX';
  C3 := TStaticLinearDipole.Create;
  C3.InstanceName := 'C3';
  C3.Capacitance := 100E-6;
  Z3aX.AddDipole(C3);
  R2 := TStaticLinearDipole.Create;
  R2.InstanceName := 'R2';
  R2.Resistance := 100E3;
  Z3aX.AddDipole(R2);
  Z1 := TParallelDipoleCollection.Create;
  Z1.InstanceName := 'Z1';
  R1 := TStaticLinearDipole.Create;
  R1.InstanceName := 'R1';
  R1.Resistance := 100E3;
  Z1.AddDipole(R1);
  RD := TStaticLinearDipole.Create;
  RD.InstanceName := 'RD';
  RD.Resistance := 12E3;
  Z1.AddDipole(RD);
  C1 := TStaticLinearDipole.Create;
  C1.InstanceName := 'C1';
  C1.Capacitance := 14E-8;
  Z1.AddDipole(C1);
  Z3aX.AddDipole(Z1);
  Z3a.AddDipole(Z3aX);
  AddDipole(Z3a)
end;

function TLowPassFilter.VoltageAmplitudeTransfer(AFrequency: Extended): Extended;
begin
  Frequency := AFrequency;
  Result := ComplexVoltageTransfer.Abs;
  WriteLn(InstanceName + '.ComplexVoltageTransfer.Abs = ', Result);
end;

{ TBandPassFilter }

constructor TBandPassFilter.Create;
begin
  inherited Create;
  R3b := TStaticLinearDipole.Create;
  R3b.Resistance := 11E3; {50% of R3 = 22E3}
  AddDipole(R3b);
  Z3a := TParallelDipoleCollection.Create;
  R3a := TStaticLinearDipole.Create;
  R3a.Resistance := 11E3; {50% of R3 = 22E3}
  Z3a.AddDipole(R3a);
  Z3aX := TSerialDipoleCollection.Create;
  C3 := TStaticLinearDipole.Create;
  C3.Capacitance := 100E-6;
  Z3aX.AddDipole(C3);
  RC2 := TParallelDipoleCollection.Create;
  R2 := TStaticLinearDipole.Create;
  R2.Resistance := 100E3;
  RC2.AddDipole(R2);
  {C2 := TStaticLinearDipole.Create;
  C2.Capacitance := 1E-6;
  RC2.AddDipole(C2); not for low pass }
  Z3aX.AddDipole(RC2);
  RC1 := TParallelDipoleCollection.Create;
  R1 := TStaticLinearDipole.Create;
  R1.Resistance := 100E3;
  RC1.AddDipole(R1);
  C1 := TStaticLinearDipole.Create;
  C1.Capacitance := 10E-6;
  RC1.AddDipole(C1);
  Z3aX.AddDipole(RC1);
  Z3a.AddDipole(Z3aX);
  AddDipole(Z3a);
end;

end.

