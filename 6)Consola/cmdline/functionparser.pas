unit functionParser;

{$mode objfpc}

interface

uses
  Classes, SysUtils;

type
  TFunctionParser = class
  private
    funcName: String;
    list_parameters: TStringList;
    isGood: Boolean;
  public
    procedure parser(expr: String);
    function getFunctName(): String;
    function getList_Parameters(): TStringList;
    function fails(): Boolean;
  end;

const
  separator = ',';
  trashchr = '*';

implementation

//Example: expr = 'MyFunc(sen(x)+1, -5, 5)'
//Atencion: no reconoce los parametros correctamente si dentro de ellos hay un separator en medio de ellos, como: raiz(power(x,2), ...).
//                                                                                                                            ^
procedure TFunctionParser.parser(expr: String);
var
   nameIdx, i, commaIdx: Integer;
   par: String;
begin
   try
       list_parameters := TStringList.Create;
       isGood := True;
       nameIdx := Pos('(',expr);
       self.funcName := Copy(expr,1,nameIdx -1); //Toma 'MyFunc'
       i := nameIdx+1; //inicia despues del '('
       while(True) do begin
           commaIdx := Pos(separator, expr);
           par := Copy(expr, i, commaIdx - i);
           if par <> '' then
             list_parameters.Add(par)
           else break;

           expr[commaIdx] := trashchr; //MyFunc(x+1* -5, 5) para que busque el siguiente separator
           i := commaIdx+1;
       end;
       par := Copy(expr, i, LastDelimiter(')', expr) - i);
       if par <> '' then begin
         list_parameters.Add(par);
       end else
         raise Exception.create('Error en TFunctionParser.parser(expr: String)');
   except
     on E: Exception do
       isGood := False;
   end;

end;

function TFunctionParser.getFunctName(): String;
begin
   Result := self.funcName;
end;

function TFunctionParser.getList_Parameters(): TStringList;
begin
   Result := self.list_parameters;
end;

function TFunctionParser.fails(): Boolean;
begin
   Result := not isGood;
end;

end.

