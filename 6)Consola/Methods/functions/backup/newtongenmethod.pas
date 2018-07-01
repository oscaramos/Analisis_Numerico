unit NewtonGenMethod ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, matrix, newtongen_matrix, ParseMath, Dialogs;

type
  TNewtonGen = class
    numFunc, numVar : Integer;
    Xo : TMatrix;
    listXi, listError : TStringList;

  public
    constructor create(_numFunc, _numVar: Integer);
    function calculate(): TMatrix;
  private
    function evalFx(Xi: TMatrix): TMatrix;
    function evalJFx(Xi: TMatrix): TMatrix;
  end;
procedure initArrFunctions(numFunc, numVar: Integer);
procedure initVariables(out numVar: Integer; rawVar: String);

var
 arrFunctions : array of TParseMath;
 arrVariables : array of Char;

const
  Top = 1000;
  MaxError = 1e-6;
  h = 1e-4;  //Precision de derivadas

implementation

function MatrixToStr(mat: TMatrix): String;
var
 i: Integer;
 tst: TStringList;
begin
  if mat.getNumColumns <> 1 then
     raise Exception.create('MatrixToStr :La matriz no es Stringificable');
  tst := TStringList.create();
  for i:=0 to  mat.getNumRows-1 do
     tst.add(FloatToStr(StrToFLoat(formatDouble(mat.getElement(i,0))))); //Doble conversion para eliminar comillas
  Result := '(' + tst.CommaText + ')';
end;


procedure initArrFunctions(numFunc, numVar: Integer);
var
  i, j : Integer;
begin
   SetLength(arrFunctions, numFunc );
   for i:=0 to numFunc-1 do begin
       arrFunctions[i] := TParseMath.create;
       for j:=0 to numVar-1 do
           arrFunctions[i].AddVariable(arrVariables[j], 0);
   end;
end;

procedure initVariables(out numVar: Integer; rawVar: String);
var
  i,j: Integer;
  c: Char;
begin
   SetLength(arrVariables, rawVar.Length);
   i := 0;
   for j:=1 to rawVar.Length do begin
     c := rawVar[j];
     if c <> ' ' then begin
        arrVariables[i] := c;
        i := i + 1;
     end;
   end;
   numVar := i;
   SetLength(arrVariables, numVar);
end;

constructor TNewtonGen.create(_numFunc, _numVar: Integer);
begin
   self.numFunc := _numFunc;
   self.numVar := _numVar;
   listXi := TStringList.create;
   listError := TStringList.create;
   Xo := TMatrix.create(_numFunc, 1);
end;

function TNewtonGen.calculate(): TMatrix;
var
  Fx, JFx : TMatrix;
  Xi, Xprev: TMatrix;
  i: Integer;
  Error: Double;
begin
   i := 0;
   Xi := Xo;
   Error := Top;
   listXi.Clear; listError.Clear;
   listXi.Add('Xi');
   listError.Add('Error');
   repeat
      Xprev := Xi;
      Fx := evalFx(Xi);
      JFx := evalJFx(Xi);
      Xi := Xi.Sub(JFx.inverse().MultMat(Fx)); // Xi = Xi - JFx**-1 * Fx
      listXi.add(MatrixToStr(Xi));
      if i <> 0 then begin
          Error := (Xi.sub(Xprev)).norm(); // Error = Norma(Xi+1 - Xi)
      end;
      listError.add(FloatToStr(Error));
      i := i + 1;
   until (i > Top) or (Error < MaxError);
   Result := Xi;
end;

function TNewtonGen.evalFx(Xi: TMatrix): TMatrix;
var
  i,j: Integer;
begin
   Result := TMatrix.create(numFunc,1);
   for i:=0 to numFunc-1 do begin
       for j:=0 to length(arrVariables)-1 do
           arrFunctions[i].newValue(arrVariables[j], Xi.getElement(j,0));
       Result.setElement(i,0, arrFunctions[i].Evaluate());
   end;
end;

function TNewtonGen.evalJFx(Xi: TMatrix): TMatrix;
var
  i,j,k: Integer;
  Fx: TMatrix;
begin
  Result := TMatrix.create(numFunc,numVar);
  Fx := evalFx(Xi);
  for i:=0 to numFunc-1 do begin
    for j:=0 to numVar-1 do begin
      for k:=0 to length(arrVariables)-1 do begin
        if k <> j then
          arrFunctions[i].newValue(arrVariables[k], Xi.getElement(k,0))
        else
          arrFunctions[i].newValue(arrVariables[k], Xi.getElement(k,0) + h);
      end;
      Result.setElement(i,j,(arrFunctions[i].Evaluate() - Fx.getElement(i,0))/h); //f(x1,x2,...,xi+h,...,xn) - f(x1,x2,...,xn)
    end;
  end;
end;

end.

