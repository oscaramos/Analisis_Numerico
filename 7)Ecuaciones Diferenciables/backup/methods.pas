unit Methods;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParseMath;

type
  TMethodsDif = class
    a,b: Double;
    n: Integer;
  public
    constructor create();
    procedure setExpression(expr: String);
    function f(x: Double): Double;
  private
    parse: TParseMath;
  end;

implementation

constructor TMethodsDif.create();
begin
  parse := TParseMath.create;
  parse.AddVariable('x',0);
end;

procedure TMethodsDif.setExpression(expr: String);
begin
  parse.Expression := expr;
end;

function TMethodsDif.f(x: Double): Double;
begin
  parse.NewValue('x', x);
  f := parse.Evaluate;
end;

end.

