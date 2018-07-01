unit StatementParser;

{$mode objfpc}

interface

uses
  Classes, SysUtils, Regexpr;

type
  TStatementParser = class
    private
      isFail: Boolean;
      varname: String;
      expression: String;

    public
      procedure parser(statement: String);
      function getVarName(): String;
      function getExpression(): String;
      function fails(): Boolean;

  end;

implementation
//statement
//[var] = [literal | function | operator ]+
//varname = expression
//Also
//expression
//The final value of the expression is stored in Ans
procedure TStatementParser.parser(statement: String);
var
  idx_equal: Integer;
begin
    isFail := False;
    try
      idx_equal := Pos('=', statement);

      if idx_equal = 0 then raise Exception.create('No es una sentencia valida');
      varname := Copy(statement, 1, idx_equal-1);
      varname := Trim(varname);
      expression := Copy(statement, idx_equal+1, Length(statement)-idx_equal);
      expression := TrimRight(TrimLeft(expression)); //Remueve los espacios en blanco antes y despues de la expresion

      if (varname = '') or (expression = '') then raise Exception.create('varname o expression no pueden estar vacios');
    except
      on E:Exception do
          isFail := True;
    end;

end;

function TStatementParser.getVarName(): String;
begin
    Result := varname;
end;

function TStatementParser.getExpression(): String;
begin
    Result := Expression;
end;

function TStatementParser.fails(): Boolean;
begin
  Result := isFail;
end;

end.

