unit PreSuperParser;

{$mode objfpc}

interface

uses
  Classes, SysUtils, RegExpr, adaptors, functionParser, dialogs ,
  variableSys(*Solo para obtener function getType(s: String): String;*),
  character;

type
  TPreSuperParser = class
  public
    constructor create();
    function preParse(expr: String): String;

  private
    function parseFunction(exprfunc: String): String;
    function miniparser(arg: String): String;

  private
    adaptorsys: TAdaptorSys;
    funcparser: TFunctionParser;
    _expression: String;
  end;

implementation
uses wnmainform;

constructor TPreSuperParser.create();
begin
    adaptorsys := TAdaptorSys.Create;
    funcparser := TFunctionParser.Create;

end;


function TPreSuperParser.preParse(expr: String): String;

      function getEndFunction(idx: Integer): Integer;
      var
        ParCount: Integer; //Parenthesis count
      begin
        while _expression[idx] <> '(' do begin
             idx := idx+1;
        end;
        ParCount := 1;
        idx := idx+1;
        repeat
            if _expression[idx] = '(' then ParCount := ParCount + 1
            else if _expression[idx] = ')' then ParCount := ParCount - 1;
            idx := idx+1;
        until ParCount = 0;
        Result := idx;
      end;

      procedure evaluateFunction(idx: Integer);
      var
        idx2: Integer;
        f, res: String;
        tp: String;
      begin
          idx2 := getEndFunction(idx);
          f := Copy(_expression, idx, idx2-idx);
          res := ParseFunction(f);
          tp := getType(res);
          if (tp = 'Matrix') or (tp='String') then res := '''' + res + ''''; //Para que en la fase de parseo no haya problemas con reconocer una matriz o un string.
          Delete(_expression, idx, idx2-idx);
          Insert(res, _expression, idx);
      end;

      function findByFunctionName(const s: String): Boolean;
      var
        i: Integer;
      begin
        i := Pos(s, _expression);
        if i <> 0 then begin
          evaluateFunction(i);
          Result := True;
        end
        else
          Result := False;
      end;

var
  ThereAvailableFunctionNames: Boolean;

begin
  _expression := expr;
  if getType(_expression) = 'Matrix' then _expression := '''' + _expression + '''';
  repeat
      ThereAvailableFunctionNames := False;
      if findByFunctionName(fn_raiz)        then ThereAvailableFunctionNames := True;
      if findByFunctionName(fn_senl)        then ThereAvailableFunctionNames := True;
      if findByFunctionName(fn_lagrange)    then ThereAvailableFunctionNames := True;
      if findByFunctionName(fn_integral)    then ThereAvailableFunctionNames := True;
      if findByFunctionName(fn_area)        then ThereAvailableFunctionNames := True;
      if findByFunctionName(fn_edo)         then ThereAvailableFunctionNames := True;
      if findByFunctionName(fn_plot2d)      then ThereAvailableFunctionNames := True;
      if findByFunctionName(fn_func2d)      then ThereAvailableFunctionNames := True;
      if findByFunctionName(fn_clearplot)   then ThereAvailableFunctionNames := True;
      if findByFunctionName(fn_intersection)then ThereAvailableFunctionNames := True;
      if findByFunctionName(fn_sedo)        then ThereAvailableFunctionNames := True;
      if findByFunctionName(fn_proportional)then ThereAvailableFunctionNames := True;
  until not ThereAvailableFunctionNames;

  Result := _expression;
end;

function TPreSuperParser.parseFunction(exprfunc: String): String;
var
  evaluated: String;
  listPar: TStringList;
  funcname: String;
  i: Integer;
begin
  funcparser.parser(exprfunc);
  funcname := funcparser.getFunctName();
  listPar := funcparser.getList_Parameters();

  for i:=0 to listPar.Count-1 do begin
       listPar[i] := miniparser(  TrimLeft( TrimRight (listPar[i] ) )  );
  end;
  evaluated := adaptorsys.evaluate(funcname,listPar);

  Result := evaluated;
end;

function TPreSuperParser.miniparser(arg: String): String;
var
  i, j: Integer;
  s, name, val: String;
  ListVarNames, ListVarValues: TStringList;

     procedure minireplace(i, j: Integer);
     var k: Integer;
     begin
         s := Copy(arg, i, j-i+1);
         for k := 0 to  WMainForm.sys_variable.getNumValues()-1 do begin
           name := ListVarNames [k];
           val  := ListVarValues[k];
           if name = s then begin
             Delete(arg,i,j-i);
             Insert(val, arg, i);
           end;
         end;
     end;

  begin
  ListVarNames  := WMainForm.sys_variable.getColVarName();
  ListVarValues := WMainForm.sys_variable.getColVarValue();
  i := 1;
  while i <= Length(arg) do begin
      if character.IsLetterOrDigit(arg[i]) then begin
        j := i;
        while character.IsLetterOrDigit(arg[j+1]) do begin
            j := j + 1;
        end;
        minireplace(i, j);
        i := j;
      end;
      i := i+1;
  end;
  Result := arg;
  end;



end.

