unit class_bolzano;
interface
uses
  Classes, SysUtils, Dialogs;
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

    function calculateBisection():Real;
    function calculateFalsePos():Real;

    function isGoodParameters(): Boolean;
end;


const
  isBiseccion = 0;
  isFalsePos = 1;

  isModeNormal = 0;

  isClosedMethod = 0;

implementation
uses mainInterpolation;

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

constructor TOCMethods.create;
begin
    Top := 100;
    ErrorAllowed  := 0.0000001;
    FunctionList  := TStringList.create;
    sequenceXn    := TStringList.create;
    sequenceError := TStringList.create;

    FunctionList.addObject('Biseccion', TObject(isBiseccion));
    FunctionList.addObject('False Position', TObject(isFalsePos));;
    CalculateModeList := TStringList.create;
    CalculateModeList.addObject('Normal', TObject(isModeNormal));

    sequenceXn.add('Xn');
    sequenceError.add('Error');

end;

function TOCMethods.Execute(): Real;
begin
   case FunctionType of
          isBiseccion:      Result := calculateBisection();
          IsFalsePos:       Result := calculateFalsePos();
          else ShowMessage('No se encontro que tipo de metodo ejecutar');
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
       if Fx3(xn) = 0 then
          break
       else if Fx3(b)*Fx3(xn) < 0 then
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
  n: Integer;
  xn, prevXn, Error: Real;
begin
    n := 0;
    Error := Top;
    xn := -123;
    repeat
       prevXn := xn;
       xn := b - Fx3(b) * (e - b)/(Fx3(e) - Fx3(b)); //Unico cambio
       sequenceXn.add(FloatToStr(Xn));
       if Fx3(xn) = 0 then
          break
       else if Fx3(b)*Fx3(xn) < 0 then
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

function TOCMethods.isGoodParameters(): Boolean;
begin
    Result := True;

    if b >= e then begin
        Result := False;
        ShowMessage('El intervalo izquierdo debe ser inferior al intervalo derecho!');
    end;
    if Fx3(b)*Fx3(e) >= 0 then begin
        Result := False;
        ShowMessage('No se cumple el teorema de bolzano en ese intervalo!');;
    end;
end;


end.

