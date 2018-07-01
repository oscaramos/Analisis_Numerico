unit integrales;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, ParseMath;

type
  TMethodIntegral = class
    a,b: Double;
    n: Integer;
    func: TParseMath;
    constructor create();
    function Evaluate(): Double;
    function fx(x: Double): Double;
    function afx(x: Double): Double;
    procedure setExpr(s: String);
  end;

implementation

constructor TMethodIntegral.create();
begin
    func := TParseMath.create;
    func.AddVariable('x',0);
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

function TMethodIntegral.fx(x: Double): Double;
begin
   func.NewValue('x',x);
   Result := func.Evaluate();
end;

function TMethodIntegral.afx(x: Double): Double;
begin
  Result := abs(fx(x));
end;

procedure TMethodIntegral.setExpr(s: String);
begin
  func.Expression:=s;
end;

end.

