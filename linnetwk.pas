unit LinNetwk;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComplexMath;

type

  { TLinearDipole }

  TLinearDipole = class(TObject)
  private
    FFrequency: Extended;
    FInstanceName: string;
    function GetAngularFrequency: Extended;
    function GetCapacitiveReactance: Extended; virtual; abstract;
    function GetInductiveReactance: Extended; virtual; abstract;
    procedure SetAngularFrequency(AValue: Extended); virtual;
    procedure SetCapacitiveReactance(AValue: Extended);
    procedure SetImpedance(AValue: TComplexNumber);
    procedure SetInductiveReactance(AValue: Extended);
  private
    function GetAdmittance: TComplexNumber; virtual; abstract;
    function GetCapacitance: Extended; virtual; abstract;
    function GetCurrent: TComplexNumber; virtual; abstract;
    function GetReactance: Extended; virtual;
    function GetResistance: Extended; virtual; abstract;
    function GetInductance: Extended; virtual; abstract;
    function GetVoltage: TComplexNumber; virtual; abstract;
    procedure SetCapacitance(AValue: Extended); virtual; abstract;
    procedure SetCurrent(AValue: TComplexNumber); virtual; abstract;
    procedure SetFrequency(AValue: Extended); virtual;
    procedure SetResistance(AValue: Extended); virtual; abstract;
    procedure SetInductance(AValue: Extended); virtual; abstract;
    procedure SetVoltage(AValue: TComplexNumber); virtual; abstract;
    property AngularFrequency: Extended read GetAngularFrequency write SetAngularFrequency;
    property Voltage: TComplexNumber read GetVoltage write SetVoltage;
    property Current: TComplexNumber read GetCurrent write SetCurrent;
    property Inductance: Extended read GetInductance write SetInductance;
    property InductiveReactance: Extended read GetInductiveReactance
      write SetInductiveReactance;
    property CapacitiveReactance: Extended read GetCapacitiveReactance
      write SetCapacitiveReactance;
  protected
    function GetImpedance: TComplexNumber; virtual; abstract;
  public
    property Admittance: TComplexNumber read GetAdmittance;
    property Frequency: Extended read FFrequency write SetFrequency;
    property Impedance: TComplexNumber read GetImpedance write SetImpedance;
    property Capacitance: Extended read GetCapacitance write SetCapacitance;
    property InstanceName: string read FInstanceName write FInstanceName;
    property Reactance: Extended read GetReactance;
    property Resistance: Extended read GetResistance
      write SetResistance;
  end;

  { TStaticLinearDipole }

  TStaticLinearDipole = class(TLinearDipole)
  private
    FAdmittance, FImpedance: TComplexNumber;
    FCapacitance, FInductance, FResistance: Extended;
    function GetAdmittance: TComplexNumber; override;
    function GetCapacitance: Extended; override;
    function GetCapacitiveReactance: Extended; override;
    function GetInductance: Extended; override;
    function GetInductiveReactance: Extended; override;
    function GetResistance: Extended; override;
    procedure SetCapacitance(AValue: Extended); override;
    procedure SetResistance(AValue: Extended); override;
    procedure SetInductance(AValue: Extended); override;
  protected
    function GetImpedance: TComplexNumber; override;
  public
    destructor Destroy; override;
  end;

  { TLinearDipoleCollection }

  TLinearDipoleCollection = class(TLinearDipole)
  private
    FAdmittance: TComplexNumber;
    FImpedance: TComplexNumber;
    function GetDipoleList: TList;
  private
    FDipoleList: TList;
    function GetCapacitance: Extended; override;
    function GetCapacitiveReactance: Extended; override;
    function GetDipoleCount: Integer;
    function GetDipoles(AnIndex: Integer): TLinearDipole;
    function GetInductance: Extended; override;
    function GetInductiveReactance: Extended; override;
    function GetResistance: Extended; override;
    procedure SetFrequency(AValue: Extended); override;
    property DipoleList: TList read GetDipoleList;
  public
    destructor Destroy; override;
    procedure AddDipole(ADipole: TLinearDipole);
    property DipoleCount: Integer read GetDipoleCount;
    property Dipoles[AnIndex: Integer]: TLinearDipole read GetDipoles;
  end;

  { TSerialDipoleCollection }

  TSerialDipoleCollection = class(TLinearDipoleCollection)
  private
    function GetAdmittance: TComplexNumber; override;
  protected
    function GetImpedance: TComplexNumber; override;
  end;

  { TParallelDipoleCollection }

  TParallelDipoleCollection = class(TLinearDipoleCollection)
  private
    function GetAdmittance: TComplexNumber; override;
  protected
    function GetImpedance: TComplexNumber; override;
  public

  end;

implementation

{ TSerialDipoleCollection }

function TSerialDipoleCollection.GetAdmittance: TComplexNumber;
begin
  Result := Impedance.Reciprocal(FAdmittance)
end;

function TSerialDipoleCollection.GetImpedance: TComplexNumber;
var
  i: Integer;
begin
  if not Assigned(FImpedance) then FImpedance := TComplexSum.Create
  else (FImpedance as TComplexSum).RemoveOperands;
  for i := 0 to DipoleCount - 1 do begin
    WriteLn(InstanceName, '.', Dipoles[i].InstanceName, '.Impedance = (', Dipoles[i].Impedance.Re, ', ', Dipoles[i].Impedance.Im, '), class = ', Dipoles[i].ClassName);
    (FImpedance as TComplexSum).AddOperand(Dipoles[i].Impedance);
  end;
  (FImpedance as TComplexSum).Operate;
  Result := FImpedance
end;

{ TParallelDipoleCollection }

function TParallelDipoleCollection.GetAdmittance: TComplexNumber;
var
  i: Integer;
begin
  if not Assigned(FAdmittance) then FAdmittance := TComplexSum.Create
  else (FAdmittance as TComplexSum).RemoveOperands;
  for i := 0 to DipoleCount - 1 do
      (FAdmittance as TComplexSum).AddOperand(Dipoles[i].Admittance);
  (FAdmittance as TComplexSum).Operate;
  Result := FAdmittance
end;

function TParallelDipoleCollection.GetImpedance: TComplexNumber;
begin
  Result := Admittance.Reciprocal(FImpedance) as TComplexNumber;
end;

{ TStaticLinearDipole }

function TStaticLinearDipole.GetAdmittance: TComplexNumber;
begin
  Result := Impedance.Reciprocal(FAdmittance);
end;

function TStaticLinearDipole.GetCapacitance: Extended;
begin
  Result := FCapacitance
end;

function TStaticLinearDipole.GetCapacitiveReactance: Extended;
begin
  if Capacitance = 0 then Result := 0
  else Result := 1 / AngularFrequency / Capacitance
end;

function TStaticLinearDipole.GetImpedance: TComplexNumber;
begin
  if not Assigned(FImpedance) then FImpedance := TComplexNumber.Create;
  FImpedance.Re := Resistance;
  FImpedance.Im := Reactance;
  Result := FImpedance
end;

function TStaticLinearDipole.GetInductance: Extended;
begin
  Result := FInductance
end;

function TStaticLinearDipole.GetInductiveReactance: Extended;
begin
  Result := AngularFrequency * Inductance
end;

function TStaticLinearDipole.GetResistance: Extended;
begin
  Result := FResistance
end;

procedure TStaticLinearDipole.SetCapacitance(AValue: Extended);
begin
  FCapacitance := AValue;
end;

procedure TStaticLinearDipole.SetResistance(AValue: Extended);
begin
  FResistance := AValue
end;

procedure TStaticLinearDipole.SetInductance(AValue: Extended);
begin
  FInductance := AValue
end;

destructor TStaticLinearDipole.Destroy;
begin
  FAdmittance.Free;
  FImpedance.Free;
  inherited Destroy;
end;

{ TLinearDipoleCollection }

function TLinearDipoleCollection.GetDipoleList: TList;
begin
  if not Assigned(FDipoleList) then FDipoleList := TList.Create;
  Result := FDipoleList;
end;

function TLinearDipoleCollection.GetCapacitance: Extended;
begin
  if CapacitiveReactance > 0 then Result := 1 / CapacitiveReactance / AngularFrequency
  else Result := 0
end;

function TLinearDipoleCollection.GetCapacitiveReactance: Extended;
begin
  with Impedance do if Im < 0 then Result := -Im else Result := 0
end;

function TLinearDipoleCollection.GetDipoles(AnIndex: Integer): TLinearDipole;
begin
  Pointer(Result) := DipoleList[AnIndex]
end;

function TLinearDipoleCollection.GetInductance: Extended;
begin
  with Impedance do
    if Im > 0 then Result := Im / AngularFrequency
    else Result := 0
end;

function TLinearDipoleCollection.GetInductiveReactance: Extended;
begin
  with Impedance do if Im > 0 then Result := Im else Result := 0
end;

function TLinearDipoleCollection.GetResistance: Extended;
begin
  Result := Impedance.Re
end;

procedure TLinearDipoleCollection.SetFrequency(AValue: Extended);
var
  i: Integer;
begin
  inherited SetFrequency(AValue);
  for i := 0 to DipoleCount - 1 do Dipoles[i].Frequency := AValue;
end;

function TLinearDipoleCollection.GetDipoleCount: Integer;
begin
  if Assigned(FDipoleList) then Result := FDipoleList.Count
  else Result := 0
end;

destructor TLinearDipoleCollection.Destroy;
var
  i: Integer;
begin
  FAdmittance.Free;
  FImpedance.Free;
  for i := 0 to DipoleCount - 1 do Dipoles[i].Free;
  FDipoleList.Free;
  inherited Destroy;
end;

procedure TLinearDipoleCollection.AddDipole(ADipole: TLinearDipole);
begin
  ADipole.Frequency := FFrequency;
  DipoleList.Add(ADipole);
end;

{ TLinearDipole }

function TLinearDipole.GetAngularFrequency: Extended;
begin
  Result := 2 * Pi * Frequency
end;

procedure TLinearDipole.SetAngularFrequency(AValue: Extended);
begin
  Frequency := AValue / 2 / Pi
end;

procedure TLinearDipole.SetCapacitiveReactance(AValue: Extended);
begin
  Capacitance := 1 / AngularFrequency / AValue
end;

procedure TLinearDipole.SetImpedance(AValue: TComplexNumber);
begin
  Resistance := AValue.Re;
  if AValue.Im < 0 then begin
    CapacitiveReactance := -AValue.Im;
    InductiveReactance := 0
  end
  else begin
    CapacitiveReactance := 0;
    InductiveReactance := AValue.Im
  end;
end;

procedure TLinearDipole.SetInductiveReactance(AValue: Extended);
begin
  Inductance := AValue / AngularFrequency
end;

function TLinearDipole.GetReactance: Extended;
begin
  Result := InductiveReactance - CapacitiveReactance;
end;

procedure TLinearDipole.SetFrequency(AValue: Extended);
begin
  FFrequency := AValue
end;

end.

