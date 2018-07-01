unit class_bolzano;
interface
uses
  Classes, SysUtils, Dialogs, ParseMath;
{$mode objfpc}{$H+}


type
TOCMethods = class
public
    b: Real;
    e: Real;
    X0: Real;
    Top: Real;
    ErrorAllowed: Real;
    FunctionList: TStringList;
    FunctionType: Integer;
    MethodType: Integer;
    CalculateModeList: TStringList;
    CalculateMode: Integer;

    sequenceXn, sequenceError: TStringList;
    parserFx, parserGx : TParseMath;

    function Execute(): Real;
    procedure setExpressionFx(expr: String);
    procedure setExpressionGx(expr: String);

    constructor create;
    function f(x: Double): Double;
    function df(x: Double): Double;
    function g(x: Double): Double;
private
    function modeNormal(): Real;
    function modeBestRoot(): Real;
    function modeIntersectFunctions(): Real;

    function calculateBisection():Real;
    function calculateFalsePos():Real;
    function calculateIntersectFunctions():Real;
    function calculateNewtonRaphson(): Real;
    function calculateSecant(): Real;
    function calculateBisSec():real;

    function isGoodParameters(): Boolean;
    function getByFunctionType(): Real;

    procedure checkOpenMethodXn(xn: Real);
end;


const
  isBiseccion = 1;
  isFalsePos = 2;
  isNewtonRaphson = 3;
  isSecant = 4;
  isBisAndSecant=5;

  isModeNormal = 0;
  isModeBestRoot = 1;
  isModeIntersect = 2;

  isClosedMethod = 0;
  isOpenMethod = 1;

implementation

function Power( b: Real; n: Integer ): Extended;
var i: Integer;
begin
   Result:= 1;
   for i:= 1 to n do
      Result:= Result * b;
end;

constructor TOCMethods.create;
begin
    Top := 1000;
    ErrorAllowed  := 0.0000001;
    FunctionList  := TStringList.create;
    sequenceXn    := TStringList.create;
    sequenceError := TStringList.create;
    parserFx        := TParseMath.create;
    parserFx.addVariable('x',0);
    parserGx        := TParseMath.create;
    parserGx.addVariable('x',0);

    FunctionList.addObject('Biseccion', TObject(isBiseccion));
    FunctionList.addObject('False Position', TObject(isFalsePos));
    FunctionList.addObject('Newton Raphson', TObject(isNewtonRaphson));
    FunctionList.addObject('Secant', TObject(isSecant));
    FunctionList.AddObject('Biseccion y secante', TObject(isBisAndSecant));
    CalculateModeList := TStringList.create;
    CalculateModeList.addObject('Normal', TObject(isModeNormal));
    CalculateModeList.addObject('Best root', TObject(isModeBestRoot));
    CalculateModeList.addObject('Functions Intersect', TObject(isModeIntersect));
end;

function TOCMethods.Execute(): Real;
begin
     sequenceXn.Clear;
     sequenceError.Clear;
     case FunctionType of
          isBiseccion, isFalsePos, isBisAndSecant:   MethodType :=  isClosedMethod;
          isNewtonRaphson, isSecant: MethodType := isOpenMethod;
     end;
     case CalculateMode of
         isModeNormal: Result    := ModeNormal();
         isModeBestRoot: Result  := ModeBestRoot();
         isModeIntersect: Result := ModeIntersectFunctions();
     end;
end;

function TOCMethods.ModeNormal(): Real;
begin
    if not isGoodParameters() then begin
        raise Exception.Create('No se cumple el teorema de bolzano en este intervalo!');
        Result := -1234.5;
        exit();
    end;
    Result := getByFunctionType();
end;

function TOCMethods.ModeBestRoot(): Real;
var
  temp_e, x1, x2: Real;
begin
    temp_e := e;
    e := (e + b)/2;
    if not isGoodParameters() then begin
        Result := -1234.5;
        exit();
    end;
    x1 := getByFunctionType();
    b := e;
    e := temp_e;
    if not isGoodParameters() then begin
        Result := -1234.5;
        exit();
    end;
    x2 := getByFunctionType();
    if x1 > x2 then
      Result := x1
    else
      Result := x2;
end;

function TOCMethods.ModeIntersectFunctions(): Real;
begin
    if b >= e then begin
        Result := -1234;
        raise Exception.Create('El intervalo izquierdo debe ser inferior al intervalo derecho!');
        exit();
    end;
    Result := calculateIntersectFunctions();
end;

function TOCMethods.calculateBisection(): Real;
var
  n: Integer;
  xn, prevXn, Error: Real;
begin
    n := 0;
    Error := Top;
    xn := -123;
    repeat
       prevXn := xn;
       xn := (b+e)/2;
       //sequenceXn.add(FloatToStr(Xn));
       if f(xn) = 0 then
          break
       else if f(b)*f(xn) < 0 then
          e := xn
       else
          b := xn;
       if n > 0 then
          Error := abs(xn - prevXn);
       //sequenceError.add(FloatToStr(Error));
       n := n + 1;
    until(Error < ErrorAllowed) or (n >= Top);
    Result := xn;
end;

function TOCMethods.calculateFalsePos(): Real;
var
  Error:Real;
  n:Integer;
  Signo:real;
  Xn:Real;
  prev_Xn:Real;
begin
   n:=0;
   Error := Top;
   Xn:= X0;
   repeat
      prev_Xn:=Xn;
      Xn:= b - f(b) * ( (e-b)/(f(e)-f(b)));
      sequenceXn.add(FloatToStr(Xn));
      Signo:= f(b)*f(Xn);
      if Signo<0 then
         e := Xn
      else
         b := Xn;
      if n > 0 then
        Error:= abs( Xn - prev_Xn);
      sequenceError.add(FloatToStr(Error));
    n:= n + 1;

   until ( Error <= ErrorAllowed ) or ( n >= Top ) ;
   Result := Xn;
end;

function TOCMethods.calculateIntersectFunctions():Real;
var
  n: Integer;
  prevXn, xn, Error: Real;
begin
    n := 0;
    Error := Top;
    xn := -123;
    repeat
       prevXn := xn;
       xn := (e+b)/2;
       sequenceXn.add(FloatToStr(Xn));
       if f(xn) = f(xn) then
          break
       else if ((f(b) < g(b)) and (f(xn) > g(xn)))
       or  ((f(b) > g(b)) and (f(xn) < g(xn)))  then
          e := abs(f(xn)-g(xn))
       else
          b := abs(f(xn)-g(xn));
       if n > 0 then
          Error := abs(xn - prevXn);
       sequenceError.add(FloatToStr(Error));
       n := n + 1;
    until(Error < ErrorAllowed) or (n >= Top);
    Result := xn;
end;

function TOCMethods.calculateNewtonRaphson(): Real;
var
  n: Integer;
  xn, prevXn, Error: Real;
begin
    n := 0;
    Error := Top;
    xn := X0;
    repeat
       prevXn := xn;
       try
           xn := xn - f(xn)/dF(xn);
       except
           on E:Exception do
              raise Exception.Create('La sucesion generada tiende a cero');
       end;
       sequenceXn.add(FloatToStr(Xn));
       if n > 0 then
          Error := abs(xn - prevXn);
       sequenceError.add(FloatToStr(Error));
       n := n + 1;
   until(Error < ErrorAllowed) or (n >= Top);
   checkOpenMethodXn(xn);
   Result := xn;
end;

function TOCMethods.calculateSecant(): Real;
var
  n: Integer;
  xn, prevXn, Error, h: Real;
begin
    n := 0;
    Error := Top;
    xn := X0;
    h := ErrorAllowed/10;
    repeat
       prevXn := xn;
       xn := xn - (2*h * f(xn))/(f(xn+h)-f(xn-h));
       sequenceXn.add(FloatToStr(Xn));
       if n > 0 then
          Error := abs(xn - prevXn);
       sequenceError.add(FloatToStr(Error));
       n := n + 1;
   until(Error < ErrorAllowed) or (n >= Top);
   checkOpenMethodXn(xn);
   Result := xn;
end;
 function TOCmethods.calculateBisSec():Real;
 var
  p:Real;
  error:Real;
  n: integer;
begin
   X0 := calculateBisection();
   Result := calculateSecant();
end;
function TOCMethods.isGoodParameters(): Boolean;
begin
    Result := True;
    if MethodType = isOpenMethod then
       exit();

    if b >= e then begin
        Result := False;
    end;
    if f(b)*f(e) >= 0 then begin
        Result := False;
    end;
end;

function TOCMethods.getByFunctionType(): Real;
begin
   case FunctionType of
          isBiseccion:      Result := calculateBisection();
          IsFalsePos:       Result := calculateFalsePos();
          isNewtonRaphson:  Result := calculateNewtonRaphson();
          isSecant:         Result := calculateSecant();
          isBisAndSecant:   Result := calculateBisSec();
          else raise Exception.Create('No se encontro que tipo de metodo ejecutar');
   end;
end;

procedure TOCMethods.checkOpenMethodXn(xn: Real);
begin
   if abs(xn) > ErrorAllowed then
      raise Exception.Create('No se encontro la raiz usando un metodo abierto');
end;

procedure TOCMethods.setExpressionFx(expr: String);
begin
   parserFx.Expression := expr;
end;

procedure TOCMethods.setExpressionGx(expr: String);
begin
   parserGx.Expression := expr;
end;

function TOCMethods.f(x: Double): Double;
begin
   parserFx.newValue('x',x);
   f := parserFx.Evaluate;
end;
function TOCMethods.df(x: Double): Double;
const
  h = 1e-6;
begin
   df := (f(x+h)-f(x))/h;
end;
function TOCMethods.g(x: Double): Double;
begin
   parserGx.newValue('x',x);
   g := parserGx.Evaluate;
end;

end.

