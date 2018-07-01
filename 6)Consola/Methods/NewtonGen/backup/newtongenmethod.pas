unit NewtonGenMethod ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, newton_gen_matrix, ParseMath, Dialogs;

type
  TNewtonGen = class
    numFunc, numVar : Integer;
    Xo : TMatrix;
    listXi, listError : TStringList;

  public
    constructor create(_numFunc, _numVar: Integer);
    function calculate(): TMatrix;
    procedure initVariables(rawVar: String);
    procedure initArrFunctions();
    procedure setExpression(expr: String; i: Integer);
    procedure setElementX0(e: Double; i: Integer);

  private
    function evalFx(Xi: TMatrix): TMatrix;
    function evalJFx(Xi: TMatrix): TMatrix;
  private
    arrFunctions : array of TParseMath;
    arrVariables : array of Char;
  end;


function MatrixToStr(mat: TMatrix): String;



const
  Top = 1000;
  MaxError = 1e-6;
  h = 1e-4;  //Precision de derivadas

implementation

//obj matrix to '[a b c; d e f; ...; x y z]'
function MatrixToStr(mat: TMatrix): String;
var
 i, j: Integer;
 row, str_matrix: TStringList;
      function RemoverComillas(const d: Double): String;
      begin
        Result := FloatToStr(StrToFLoat(formatDouble(d)));
      end;

begin
  str_matrix := TStringList.Create();
  str_matrix.Delimiter := ';';
  row := TStringList.Create();
  row.Delimiter := ' ';
  for i:=0 to  mat.getNumRows-1 do begin
      row.Clear;
      for j:=0 to mat.getNumColumns-1 do
          row.Add(RemoverComillas(mat.getElement(i,j)));
      str_matrix.Add(row.DelimitedText);
  end;
  Result := '[' + str_matrix.DelimitedText + ']';
end;


procedure TNewtonGen.initArrFunctions();
var
  i, j : Integer;
begin
   SetLength(arrFunctions, numFunc);
   for i:=0 to numFunc-1 do begin
       arrFunctions[i] := TParseMath.create;
       for j:=0 to numVar-1 do
           arrFunctions[i].AddVariable(arrVariables[j], 0);
   end;
end;

procedure TNewtonGen.initVariables(rawVar: String);
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

procedure TNewtonGen.setExpression(expr: String; i: Integer);
begin
  arrFunctions[i].Expression := expr;
end;

procedure TNewtonGen.setElementX0(e: Double; i: Integer);
begin
  Xo.setElement(i,0,e);
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

