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
    constructor Create; {creates the software model}
    function VoltageAmplitudeTransfer(AFrequency: Extended): Extended; {
      absoulte amplitude based on property ComplexVoltageTransfer }
    property ComplexVoltageTransfer: TComplexQuotient
      read GetComplexVoltageTransfer; { frequency response function (amplitude-
      frequency-function) }
  end;

  { TBandPassFilter }

  TBandPassFilter = class(TLowPassFilter)
  private
    C2: TStaticLinearDipole;
    RC2: TParallelDipoleCollection;
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
  FComplexVoltageTransfer.AddOperand(Z1.Impedance);
  FComplexVoltageTransfer.AddOperand(Impedance);
  FComplexVoltageTransfer.Operate;
  Result := FComplexVoltageTransfer;
end;

constructor TLowPassFilter.Create;
begin
  inherited Create;
{ Modelling the circuit as a software model, field names related to the circuit
  diagram }
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
end;

{ TBandPassFilter }

constructor TBandPassFilter.Create;
begin
  inherited Create; {based on TLowPassFilter}
{ adding C2 to the model }
  RC2 := TParallelDipoleCollection.Create; {creating a parallel circuit}
  RC2.InstanceName := 'RC2';
  Z3aX.RemoveDipole(R2); {moving R2 from the previous place ...}
  RC2.AddDipole(R2); {... to the new parallel circuit}
  C2 := TStaticLinearDipole.Create; {creating, ...}
  C2.Capacitance := 1E-6; {... configuring, ...}
  RC2.AddDipole(C2); {... and adding C2}
  Z3aX.AddDipole(RC2); {adding RC2 to the model in the previous R2 position}
end;

end.

