unit integrales;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, ParseMath;

type
  TMethodIntegral = class
    a,b: Double;
    n: Integer;
    MethodType: Integer;
    FindArea: Boolean;
    procedure setExpr(s: String);
    function Evaluate(): Double;
    constructor create();
  private
    function f(x: Double): Double;
    function MethodTrapecio(): Double;
    function MethodSimpson13(): Double;
    function MethodSimpson38(): Double;
  private
    parse: TParseMath;
  end;

const
  Trapecio  = 1;
  Simpson13 = 2;
  Simpson38 = 3;

implementation

constructor TMethodIntegral.create();
begin
   parse := TParseMath.create;
   parse.AddVariable('x',0);
   FindArea := False; //por default
end;


function TMethodIntegral.Evaluate(): Double;
begin
    case MethodType of
      Trapecio: Result := MethodTrapecio();
      Simpson13: Result := MethodSimpson13();
      Simpson38: Result := MethodSimpson38();
      else raise Exception.Create('TMethodIntegral.Evaluate(), MethodType no valido');
    end;
end;

function TMethodIntegral.MethodTrapecio(): Double;
var
  i: Integer;
  h: double;
begin
  h := (b-a)/n;
  Result := (f(a)+f(b))/2;
  for i:=1 to n-1 do
     Result := Result + f(a+i*h);
  Result := h*Result;
end;
//Integral por simpson1/3
//integral(f(x) dx) = h/3*(f(a) + f(b) +
//                    2*sum(from i=1 to n-1, f(X(2i)))) +
//                    4*sum(from i=0 to n-1, f(X(2i+1)) )
function TMethodIntegral.MethodSimpson13(): Double;
var
  i: Integer;
  h: Double;
begin
  h := (b-a)/(2*n);
  Result := f(a) + f(b);
  for i:=1 to n-1 do
     Result := Result + 2*f(a+(2*i)*h);
  for i:=0 to n-1 do
     Result := Result + 4*f(a+(2*i+1)*h);
  Result := h/3 * Result;
end;

//Integral por simpson3/8
//integral(f(x) dx) = 3*h/8 * (f(a) + f(b) +
//                   3*sum(from i=1 to n, f(X(3i-2)) )
//                   3*sum(from i=1 to n, f(X(3i-1)) )
//                   2*sum(from i=1 to n-1, f(X(3i)))

function TMethodIntegral.MethodSimpson38(): Double;
var
  i: Integer;
  h: Double;
begin
  h := (b-a)/(3*n);
  Result := f(a) + f(b);
  for i:=1 to n do begin
     Result := Result + 3*f(a+(3*i - 1)*h);
     Result := Result + 3*f(a+(3*i - 2)*h);
  end;
  for i:=0 to n-1 do
     Result := Result + 2*f(a+(3*i)*h);
  Result := (3*h)/8 * Result;
end;

function TMethodIntegral.f(x: Double): Double;
begin
    parse.NewValue('x',x);
    f := Parse.evaluate;
    if FindArea then
        f := abs(f); //En vez de calcular una integral, calculo una area.
end;

procedure TMethodIntegral.setExpr(s: String);
begin
    parse.Expression := s;
end;

end.

