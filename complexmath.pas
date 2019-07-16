{ Classes to store and calculate with complex numbers }
unit ComplexMath;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TComplexNumber = class;

  { TComplexNumberHandler }

  { TComplexAbstract }
  { Class template (base class) for complex numbers and complex operations in
    this library }
  TComplexAbstract = class(TPersistent)
  private
    FInstanceName: string;
    function GetAbs: Extended; virtual; abstract;
    function GetArg: Extended; virtual; abstract;
    function GetIm: Extended; virtual; abstract;
    function GetRe: Extended; virtual; abstract;
    procedure SetAbs(AValue: Extended); virtual; abstract;
    procedure SetArg(AValue: Extended); virtual; abstract;
    procedure SetIm(AValue: Extended); virtual; abstract;
    procedure SetRe(AValue: Extended); virtual; abstract;
    procedure SetPolar(AnAbs, AnArg: Extended); virtual; abstract;
  public
    function Reciprocal(var ResultPointer: TComplexNumber): TComplexNumber; virtual;
    property Re: Extended read GetRe write SetRe;
    property Im: Extended read GetIm write SetIm;
    property Abs: Extended read GetAbs write SetAbs;
    property Arg: Extended read GetArg write SetArg;
    property InstanceName: string read FInstanceName write FInstanceName; {use
      it for a unique Id to find the instance during debugging}
  end;

  TComplexNumber = class(TComplexAbstract)
  private
    FRe, FIm: Extended;
    function GetRe: Extended; override;
    procedure SetRe(AValue: Extended); override;
    function GetIm: Extended; override;
    procedure SetIm(AValue: Extended); override;
    function GetAbs: Extended; override;
    procedure SetAbs(AValue: Extended); override;
    function GetArg: Extended; override;
    procedure SetArg(AValue: Extended); override;
    procedure SetPolar(AnAbs, AnArg: Extended); override;
  public
  end;

  { TComplexOperation }
  { template for classes that operate on a collection of complex numbers
    providing the result of the operation }
  TComplexOperation = class(TComplexNumber)
  private
    FOperandList: TList;
    function GetOperandList: TList;
    function GetOperandCount: Integer;
    function GetOperands(AnIndex: Integer): TComplexAbstract;
    function GetRe: Extended; override;
    function GetIm: Extended; override;
    procedure Operate; virtual; abstract;
    procedure SetOperands(AnIndex: Integer; AValue: TComplexAbstract);
    property OperandList: TList read GetOperandList;
  public
    destructor Destroy; override;
    procedure AddOperand(AnOperand: TComplexAbstract);
    procedure RemoveOperands;
    property OperandCount: Integer read GetOperandCount;
    property Operands[AnIndex: Integer]: TComplexAbstract read GetOperands write SetOperands;
  end;

  { TComplexSum }
  { sum of complex numbers }
  TComplexSum = class(TComplexOperation)
  public
    procedure Operate; override;
  end;

  { TComplexProduct }
  { product of complex numbers }
  TComplexProduct = class(TComplexOperation)
  private
    procedure Operate; override;
  public

  end;

  { TComplexQuotient }
  { quotient of complex numbers }
  TComplexQuotient = class(TComplexOperation)
  public
    procedure Operate; override;
  end;

implementation

uses Math;

{ TComplexSum }

procedure TComplexSum.Operate;
var
  i: Integer;
begin
  FRe := 0; FIm := 0;
  for i := 0 to OperandCount - 1 do begin
    FRe := FRe + Operands[i].Re;
    FIm := FIm + Operands[i].Im
  end;
end;

{ TComplexQuotient }

procedure TComplexQuotient.Operate;
var
  x, y: Extended;
  i: Integer;
begin
  x := Operands[0].Abs;
  y := Operands[0].Arg;
  for i := 1 to OperandCount - 1 do begin
    x := x / Operands[i].Abs;
    y := y - Operands[i].Arg;
  end;
  SetPolar(x, y)
end;

{ TComplexNumber }

function TComplexNumber.GetRe: Extended;
begin
  Result := FRe
end;

procedure TComplexNumber.SetRe(AValue: Extended);
begin
  FRe := AValue
end;

function TComplexNumber.GetIm: Extended;
begin
  Result := FIm
end;

procedure TComplexNumber.SetIm(AValue: Extended);
begin
  FIm := AValue
end;

function TComplexNumber.GetAbs: Extended;
begin
  Result := Sqrt(Sqr(Re) + Sqr(Im))
end;

procedure TComplexNumber.SetAbs(AValue: Extended);
var
  a: Extended;
begin
  a := Arg;
  Re := AValue * Cos(a);
  Im := AValue * Sin(a)
end;

function TComplexNumber.GetArg: Extended;
begin
  Result := arctan2(Im, Re);
end;

procedure TComplexNumber.SetArg(AValue: Extended);
var
  a: Extended;
begin
  a := Abs;
  Re := a * cos(AValue);
  Im := a * sin(AValue);
end;

procedure TComplexNumber.SetPolar(AnAbs, AnArg: Extended);
begin
  Re := AnAbs * cos(AnArg);
  Im := AnAbs * sin(AnArg)
end;

{ TComplexProduct }

procedure TComplexProduct.Operate;
var
  x, y: Extended;
  i: Integer;
begin
  x := 1; y := 0;
  for i := 0 to OperandCount -1 do begin
    x := x * Operands[i].Abs;
    y := y + Operands[i].Arg;
  end;
  SetPolar(x, y);
end;

{ TComplexOperation }

function TComplexOperation.GetOperandList: TList;
begin
  if not Assigned(FOperandList) then FOperandList := TList.Create;
  Result := FOperandList
end;

function TComplexOperation.GetOperandCount: Integer;
begin
  if Assigned(FOperandList) then Result := FOperandList.Count
  else Result := 0;
end;

function TComplexOperation.GetOperands(AnIndex: Integer): TComplexAbstract;
begin
  Pointer(Result) := OperandList[AnIndex]
end;

function TComplexOperation.GetRe: Extended;
begin
  Operate;
  Result := inherited GetRe;
end;

function TComplexOperation.GetIm: Extended;
begin
  Operate;
  Result := inherited GetIm
end;

procedure TComplexOperation.SetOperands(AnIndex: Integer;
  AValue: TComplexAbstract);
begin
  OperandList[AnIndex] := AValue
end;

destructor TComplexOperation.Destroy;
var
  i: Integer;
begin
  FOperandList.Free;
  inherited Destroy;
end;

procedure TComplexOperation.AddOperand(AnOperand: TComplexAbstract);
begin
  OperandList.Add(AnOperand);
end;

procedure TComplexOperation.RemoveOperands;
begin
  if not Assigned(FOperandList) then Exit;
  if FOperandList.Count = 0 then Exit;
  FOperandList.Clear;
end;

{ TComplexAbstract }

function TComplexAbstract.Reciprocal(var ResultPointer: TComplexNumber
  ): TComplexNumber;
begin
  if not Assigned(ResultPointer) then ResultPointer := TComplexNumber.Create;
  ResultPointer.Abs := 1 / Abs;
  ResultPointer.Arg := -Arg;
  Result := ResultPointer;
end;

end.

