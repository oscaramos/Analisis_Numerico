unit mainDiferenciables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TAFuncSeries, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Grids, ExtCtrls, methodsDif;

type

  { TDiferenciablefrm }

  TDiferenciablefrm = class(TForm)
    btnSolve: TButton;
    btnSolveAll: TButton;
    chrDiferenciable: TChart;
    chrDiferenciablePloteo1: TLineSeries;
    chrDiferenciableXAxis: TConstantLine;
    chrDiferenciableYAxis: TConstantLine;
    ediFunction: TEdit;
    ediH: TEdit;
    EdiX0: TEdit;
    EdiXn: TEdit;
    EdiY0: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblRpta: TLabel;
    Label7: TLabel;
    rdgMethods: TRadioGroup;
    stgResults: TStringGrid;
    procedure btnSolveAllClick(Sender: TObject);
    procedure btnSolveClick(Sender: TObject);
    procedure chrDiferenciableFuncDiferenciableCalculate(const AX: Double; out
      AY: Double);
    procedure FormCreate(Sender: TObject);
  private
    methodsD: TMethodsDif;
  public

  end;

var
  Diferenciablefrm: TDiferenciablefrm;

implementation

{$R *.lfm}

{ TDiferenciablefrm }

procedure TDiferenciablefrm.btnSolveClick(Sender: TObject);
var
  X0, Y0, Xn, h: Double;
  isReversed: Boolean;
  //Procedures
   procedure setStg();
   var
     i, j, sizeListYn, s: Integer;//s es signo
     X0: Double;
   const
     nCol = 0; XnCol = 1;
   begin
      with stgResults do begin
         Clear;
         rowCount := 1;
         colCount := 2 + methodsD.num_cols_table;
         Cells[nCol ,0] := 'n';
         Cells[XnCol,0] := 'Xn';
         for j:=0 to methodsD.num_cols_table - 1 do
            Cells[XnCol+1+j, 0] := methodsD.table[j][0];
      end;

      X0 := methodsD.X0;
      sizeListYn := methodsD.table[0].Count;
      stgResults.RowCount := sizeListYn;
      if not isReversed then s := 1 else s := -1;
      for i:=0 to sizeListYn-2 do begin
         stgResults.Cells[nCol ,i+1] := IntToStr(i);        //n
         stgResults.Cells[XnCol,i+1] := FloatToStr(X0+i*h*s); //Xn
         for j:=0 to methodsD.num_cols_table - 1 do
            stgResults.Cells[XnCol+1+j, i+1] := methodsD.table[j][i+1];
      end;
   end;

   procedure showchr();
   var
     XNew, YNew: Double;
     i, n, s: Integer; //s es signo
   begin
     chrDiferenciablePloteo1.Clear;
     XNew := X0;
     n := methodsD.n;
     if not isReversed then s := 1 else s := -1;
     for i:=1 to n+1 do begin
        YNew := StrToFloat(methodsD.table[0][i]);
        chrDiferenciablePloteo1.AddXY(XNew, YNew);
        XNew := XNew + h*s;
     end;
   end;
   //end procedures

begin
   X0 := StrToFloat(EdiX0.Text);
   Y0 := StrToFloat(EdiY0.Text);
   Xn := StrToFloat(EdiXn.Text);
   h  := StrToFloat(EdiH.Text);
   isReversed := Xn < X0;

   methodsD := TMethodsDif.Create;
   methodsD.X0 := X0;
   methodsD.Y0 := Y0;
   methodsD.n := Trunc(abs(Xn-X0)/h);
   methodsD.h := h;
   methodsD.isReversed := isReversed;
   methodsD.setExpression(EdiFunction.Text);
   case rdgMethods.itemIndex of
     Euler:         methodsD.methodType := Euler;
     ImprovedEuler: methodsD.methodType := ImprovedEuler;
     RungeKutta3:    methodsD.methodType := RungeKutta3;
     RungeKutta4:    methodsD.methodType := RungeKutta4;
     DormandPrice:  methodsD.methodType := DormandPrice;
   end;
   lblRpta.Caption := Copy( FloatToStr(methodsD.Execute()), 1, 12); //like substr(x, 0, 12)
   chrDiferenciablePloteo1.Active := True;
   setStg;
   showchr;
   methodsD.Free;

end;

procedure TDiferenciablefrm.btnSolveAllClick(Sender: TObject);
const
  numMethods = 4;
  methodCol = 0; resultCol = 1;

  procedure initStg();
  begin
      with stgResults do begin
         Clear;
         RowCount := numMethods+1;
         ColCount := 2;
         Cells[methodCol,0] := 'Métodos';
         Cells[resultCol,0] := 'Resultados';
      end;
  end;

var
  X0, Y0, Xn, h: Double;
  mtname, res: String;
  i: Integer;
begin //SOLVE ALL CLICK != SOLVE CLICK
   initStg;
   chrDiferenciablePloteo1.Clear;
   X0 := StrToFloat(EdiX0.Text);
   Y0 := StrToFloat(EdiY0.Text);
   Xn := StrToFloat(EdiXn.Text);
   h  := StrToFloat(EdiH.Text);

   methodsD := TMethodsDif.Create;
   methodsD.X0 := X0;
   methodsD.Y0 := Y0;
   methodsD.n := Trunc(abs(Xn-X0)/h);
   methodsD.h := h;
   methodsD.setExpression(EdiFunction.Text);
   for i:=0 to numMethods-1 do begin
      case i of
         Euler:         mtname := 'Euler';
         ImprovedEuler: mtname := 'Euler Mejorado';
         RungeKutta3:    mtname := 'RungeKutta3';
         RungeKutta4:    mtname := 'RungeKutta4';
         DormandPrice:  mtname := 'DormandPrice';
      end;
      methodsD.methodType:=i;
      res := Copy( FloatToStr(methodsD.Execute()), 1, 12);
      with stgResults do begin
         Cells[methodCol, i+1] := mtname;
         Cells[resultCol, i+1] := res;
      end;
   end;

end;

procedure TDiferenciablefrm.chrDiferenciableFuncDiferenciableCalculate(
  const AX: Double; out AY: Double);
begin
  AY := AX;
end;

procedure TDiferenciablefrm.FormCreate(Sender: TObject);
begin

end;

end.

