unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, newtongenmethod, ParseMath, Matrix;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnSetGlobals: TButton;
    cmbXo: TComboBox;
    EdiRawVars: TEdit;
    Label6: TLabel;
    btnSetFunction: TButton;
    btnSetXo: TButton;
    btnCalculate: TButton;
    cmbFunctions: TComboBox;
    ediFunction: TEdit;
    ediNumFunc: TEdit;
    ediXo: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    stgDebug: TStringGrid;
    stgResults: TStringGrid;
    stgShowFx: TStringGrid;
    stgShowXo: TStringGrid;


    procedure FormCreate(Sender: TObject);
    procedure btnCalculateClick(Sender: TObject);
    procedure setStgShowResult(mat: TMatrix);
    procedure btnSetFunctionClick(Sender: TObject);
    procedure btnSetXoClick(Sender: TObject);
    procedure btnSetGlobalsClick(Sender: TObject);

    procedure initcmbFunctions();
    procedure initcmbX0();
    procedure initStgShowFx();
    procedure initStgShowResults();
    procedure initStgXo();

    procedure fillStgDebug();

  private
    newtonGen : TNewtonGen;
    isInit: Boolean;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
   isInit := False;
   ediFunction.enabled := False;
   ediXo.enabled := False;
end;

procedure TForm1.btnCalculateClick(Sender: TObject);
var
  m : TMatrix;
begin
   if isInit = False then begin
     raise Exception.create('Initial variables have not initialized');
     exit();
   end;
   //Falta checkear si todos los campos fueron rellenados
   try
     try
       m := newtonGen.calculate();
       setStgShowResult(m);
     except
       on E: Exception do begin
           raise Exception.create('Error durante la ejecucion del algoritmo de newton generalizado');
       end;
     end;
   finally
     fillStgDebug();
   end;


end;
procedure TForm1.setStgShowResult(mat: TMatrix);
var
  i: Integer;
begin
   for i:=1 to NewtonGen.numVar do
      stgResults.Cells[1,i] := FloatToStr(mat.getElement(i-1,0));
end;

procedure TForm1.btnSetFunctionClick(Sender: TObject);
var
  i : Integer;
  strFx : String;
begin
   i := cmbFunctions.itemIndex;
   if i = -1 then begin //Si no se ejecuto brnSetGlobalsClick
       ShowMessage('Primero selecciones las variables iniciales');
       exit();
   end;
   strFx := ediFunction.text;
   arrFunctions[i].Expression:= strFx;
   stgShowFx.Cells[0,i+1] := strFx;


end;

procedure TForm1.btnSetXoClick(Sender: TObject);
var
   xi: Real;
   f : Integer;
begin
   xi := StrToFloat(ediXo.text);
   f := cmbXo.itemIndex;
   if f = -1 then begin
      ShowMessage('Primero selecciones las variables iniciales');
      exit();
   end;
   newtonGen.Xo.setElement(f,0,xi);
   stgShowXo.Cells[0,f+1] := FloatToStr(xi);
end;

procedure TForm1.btnSetGlobalsClick(Sender: TObject);
var
  numFunc, numVar : Integer;
begin

  numFunc := strToInt(ediNumFunc.text);
  initVariables(numVar, ediRawVars.text);
  newtonGen := TNewtonGen.create(numFunc, numVar);

  initArrFunctions(numFunc, numVar);
  initcmbFunctions;
  initcmbX0;
  initStgShowFx;
  initStgShowResults;
  initStgXo;

  isInit := True;
  ediFunction.enabled := True;
  ediXo.enabled := True;
end;



procedure TForm1.initcmbFunctions();
var
  i, lim: Integer;
  options: TStringList;
begin
  options := TStringList.create;
  lim := newtonGen.numFunc;
  for i:=1 to lim do
     options.addObject(IntToStr(i),TObject(0));;
  cmbFunctions.Items.assign(options);
  cmbFunctions.ItemIndex := 0;
  options.Destroy;
end;


procedure TForm1.initcmbX0();
var
  i: Integer;
  options: TStringList;
begin
  options := TStringList.create;
  for i:=0 to newtonGen.numVar-1 do
     options.addObject(arrVariables[i],TObject(0));
  cmbXo.Items.assign(options);
  cmbXo.ItemIndex := 0;
  options.Destroy;
end;

procedure TForm1.initStgShowFx();
begin
  stgShowFx.RowCount := newtonGen.numFunc + 1;
end;

procedure TForm1.initStgShowResults();
var
  i: Integer;
begin
  stgResults.RowCount := NewtonGen.numVar + 1;
  for i:=1 to NewtonGen.numVar do begin
     stgResults.Cells[0,i] := arrVariables[i-1];
  end;

end;

procedure TForm1.initStgXo();
begin
  stgShowXo.RowCount := NewtonGen.numVar + 1;
end;

procedure TForm1.fillStgDebug();
var
  i, rowCount: Integer;
begin
  rowCount := NewtonGen.listXi.Count;
  stgDebug.RowCount:= rowCount;
  for i:=1 to rowCount-1 do
    stgDebug.cells[0,i] := IntToStr(i);
  stgDebug.cols[1].Assign(NewtonGen.listXi);
  stgDebug.cols[2].Assign(newtonGen.listError);
end;

end.

