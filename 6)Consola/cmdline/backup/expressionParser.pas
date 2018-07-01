unit assignSys;

{$mode objfpc}

interface

uses
  Classes, SysUtils, SuperExpressionParser;

type
  TAssignSys = class
    //Objetivo: expression --(TAssignSys)--> value.
    //         f(x) + 5*g(x)   -------->       y
    public
      constructor create();
      function evaluate(const varname: String; expression: String): String;
      function getValue(): String;
    private
      value: String;
      spexprparser :TSuperExpressionParser;


  end;

implementation
constructor TAssignSys.create();
begin
  spexprparser := TSuperExpressionParser.Create;
end;

function TAssignSys.evaluate(const varname: String; expression: String): String;
begin
  spexprparser.Expression := expression;
  value := FloatToStr(spexprparser.Evaluate());
  Result := 'Asignación exitosa';
end;

function TAssignSys.getValue(): String;
begin
  Result := value;
end;

end.

