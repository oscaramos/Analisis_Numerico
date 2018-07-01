unit class_bolzano;
interface
uses
  Classes, SysUtils, Dialogs, Table;
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

    function Execute(): Real;

    constructor create;
private
    function modeNormal(): Real;
    function modeBestRoot(): Real;
    function modeIntersectFunctions(): Real;

    function calculateBisection():Real;
    function calculateFalsePos():Real;
    function calculateIntersectFunctions():Real;
    function calculateNewtonRaphson(): Real;
    function calculateSecant(): Real;
    function puntofijo():real;

    function isGoodParameters(): Boolean;
    function getByFunctionType(): Real;
end;


const
  isBiseccion = 0;
  isFalsePos = 1;
  isNewtonRaphson = 2;
  isSecant = 3;
  isPuntofijo=4;

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

(*Unused*)function Factorial( n: Integer ): Extended;
begin

     if n > 1 then
        Result:= n * Factorial( n -1 )

     else if n >= 0 then
        Result:= 1

     else
        Result:= 0;
end;

(*Unused*)function Combinatory(a: Integer; b: Integer): Extended;
begin
     Result := Trunc(Factorial(a)/(Factorial(a-b)*Factorial(b)));
end;
{*
function fx1(x: Real): Real;  //x0 = 0.333333...
begin
   //Result := x*sin(power(x,3));
   Result := 1/x;
end;

function dvfx1(x: Real): Real;
begin
   //Result := sin(power(x,3)) + x*cos(power(x,3))*(2*power(x,2));
   Result := -1/(x*x);
end;

function fx2(x: Real): Real;
begin
   Result := -1/(x*x);
end;
*}

constructor TOCMethods.create;
begin
    Top := 1000;
    ErrorAllowed  := 0.0000001;
    FunctionList  := TStringList.create;
    sequenceXn    := TStringList.create;
    sequenceError := TStringList.create;

    FunctionList.addObject('Biseccion', TObject(isBiseccion));
    FunctionList.addObject('False Position', TObject(isFalsePos));
    FunctionList.addObject('Newton Raphson', TObject(isNewtonRaphson));
    FunctionList.addObject('Secant', TObject(isSecant));
    FunctionList.AddObject('Punto Fijo', TObject(ispuntofijo));
    CalculateModeList := TStringList.create;
    CalculateModeList.addObject('Normal', TObject(isModeNormal));
    CalculateModeList.addObject('Best root', TObject(isModeBestRoot));
    CalculateModeList.addObject('Functions Intersect', TObject(isModeIntersect));

end;

function TOCMethods.Execute(): Real;
begin
     case FunctionType of
          isBiseccion, isFalsePos:   MethodType :=  isClosedMethod;
          isNewtonRaphson, isSecant, ispuntofijo: MethodType := isOpenMethod;
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
        ShowMessage('El intervalo izquierdo debe ser inferior al intervalo derecho!');
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
       sequenceXn.add(FloatToStr(Xn));
       if Fx(xn) = 0 then
          break
       else if Fx(b)*Fx(xn) < 0 then
          e := xn
       else
          b := xn;
       if n > 0 then
          Error := abs(xn - prevXn);
       sequenceError.add(FloatToStr(Error));
       n := n + 1;
    until(Error < ErrorAllowed) or (n >= Top);
    Result := xn;
end;

function TOCMethods.calculateFalsePos(): Real;
var

  Xn:Real;
  n:Integer;
  S:real;
  Mid:Real;
  prev_res:Real;
begin
   n:=0;
   xn := Top;
   mid:= -123;
   repeat
   if b >= e then
    begin
         Result:=0;
         ShowMessage('El intervalo izquierdo debe ser inferior al derecho');
         exit();
    end;
  { if fx(b)*fx(e) > 0 then begin
        result := -1;
        showmessage('no se cumple');
        exit();
        end;     }
      prev_res:=mid;    //prev_res = result;,,,,, error=xn;
      mid:= b - fx(b) * ( (e-b)/(fx(e)-fx(b)));
      sequenceXn.add(FloatToStr(mid));
      s:= fx(b)*fx(mid);
      if(s<0)then
         e := mid
      else
         b := mid;
      if n > 0 then
        xn:= abs( mid - prev_res);
      sequenceError.add(FloatToStr(xn));
    n:= n + 1;

   until ( xn <= ErrorAllowed ) or ( n >= Top ) ;
   Result := mid;
   showMessage(floattostr(xn));
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
       if Fx(xn) = Fx(xn) then
          break
       else if ((Fx(b) < Gx(b)) and (Fx(xn) > Gx(xn)))
       or  ((Fx(b) > Gx(b)) and (Fx(xn) < Gx(xn)))  then
          e := abs(fx(xn)-gx(xn))
       else
          b := abs(fx(xn)-gx(xn));
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
             xn := xn - Fx(xn)/dFx(xn);
         except
             on E:Exception do
                showMessage('La sucesion generada tiende a cero');
         end;

         exit();

       sequenceXn.add(FloatToStr(Xn));
       if n > 0 then
          Error := abs(xn - prevXn);
       sequenceError.add(FloatToStr(Error));
       n := n + 1;
   until(Error < ErrorAllowed) or (n >= Top);
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
    h := Error/10;
    repeat
       prevXn := xn;
       xn := xn - (2*h * Fx(xn))/(Fx(xn+h)-Fx(xn-h));
       sequenceXn.add(FloatToStr(Xn));
       if n > 0 then
          Error := abs(xn - prevXn);
       sequenceError.add(FloatToStr(Error));
       n := n + 1;
   until(Error < ErrorAllowed) or (n >= Top);
   Result := xn;
end;
 function TOCmethods.PuntoFijo( ):Real;
 var
  p:Real;
  error:Real;
  n: integer;
begin
   n:=0;
   error:=top;
   p:=0;
   while n < top do
   begin
     p := Fx(x0);

     if n > 0 then
       error:= abs(p - x0);
     if( abs(p-x0) < ErrorAllowed ) then
     begin
         result:= p;
         showmessage('Punto Fijo: ' + floattostr(result));
         exit();
     end;
    n:= n + 1;
    x0:=p;
   end;
   ShowMessage('fracaso');
end;
function TOCMethods.isGoodParameters(): Boolean;
begin
    Result := True;
    if MethodType = isOpenMethod then
       exit();

    if b >= e then begin
        Result := False;
        ShowMessage('El intervalo izquierdo debe ser inferior al intervalo derecho!');
    end;
    if Fx(b)*Fx(e) >= 0 then begin
        Result := False;
        ShowMessage('No se cumple el teorema de bolzano en ese intervalo!');;
    end;
end;

function TOCMethods.getByFunctionType(): Real;
begin
   case FunctionType of
          isBiseccion:      Result := calculateBisection();
          IsFalsePos:       Result := calculateFalsePos();
          isNewtonRaphson:  Result := calculateNewtonRaphson();
          isSecant:         Result := calculateSecant();
          ispuntofijo:      result:= puntofijo();
          else ShowMessage('No se encontro que tipo de metodo ejecutar');
   end;
end;

end.

