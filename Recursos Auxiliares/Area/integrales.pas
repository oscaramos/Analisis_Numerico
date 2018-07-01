unit integrales;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

type
  TMethodIntegral = class
    a,b: Double;
    n: Integer;
    function Evaluate(): Double;
  end;

implementation
uses main;
function afx(x: Double): Double;
begin
  Result := abs(frmGraficadora.f(x));
end;
function TMethodIntegral.Evaluate(): Double; //metodo del trapecio
var
  i: Integer;
  h: double;
begin
  h := (b-a)/n;
  Result := (afx(a)+afx(b))/2;
  for i:=1 to n-1 do
     Result := Result + afx(a+i*h);
  Result := h*Result;
end;

end.

