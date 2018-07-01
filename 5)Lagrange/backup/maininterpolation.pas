unit mainInterpolation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TAFuncSeries, TASeries, TATools, Forms,
  Controls, Graphics, Dialogs, StdCtrls, Grids, LogicInterpolation, ParseMath, class_bolzano;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnSetTotalPoints: TButton;
    btnCalculate: TButton;
    btnAddPoint: TButton;
    btnIntersect: TButton;
    ChartToolset1: TChartToolset;
    ChartToolset1ZoomMouseWheelTool1: TZoomMouseWheelTool;
    chrFunctionResult: TChart;
    chrFunctionResultConstantLine1: TConstantLine;
    chrFunctionResultConstantLine2: TConstantLine;
    chrFunctionResultConstantLine3: TConstantLine;
    chrFunctionResultConstantLine4: TConstantLine;
    chrFunctionResultConstantLine5: TConstantLine;
    chrFunctionResultConstantLine6: TConstantLine;
    chrFunctionResultFuncSeries1: TFuncSeries;
    chrFunctionResultFuncSeries2: TFuncSeries;
    chrFunctionResultLineSeries1: TLineSeries;
    ediPolinomy1: TEdit;
    ediPolinomy2: TEdit;
    ediTotalPoints: TEdit;
    edix: TEdit;
    ediy: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    stgPoints: TStringGrid;
    procedure btnAddPointClick(Sender: TObject);
    procedure btnCalculateClick(Sender: TObject);
    procedure btnIntersectClick(Sender: TObject);
    procedure btnSetTotalPointsClick(Sender: TObject);
    procedure chrFunctionResultFuncSeries1Calculate(const AX: Double; out
      AY: Double);
    procedure chrFunctionResultFuncSeries2Calculate(const AX: Double; out
      AY: Double);
    procedure FormCreate(Sender: TObject);
    procedure showIntersect(); //Intersección en el ambito más reducido, no el más amplio.
  private
    interpolation: TInterpolation;
    OCMethods: TOCMethods;
    calcFx1 : Boolean;
  public

  end;
function Fx1(x: double): double;
function Fx2(x: double): double;
function Fx3(x: double): double;

var
  Form1: TForm1;
  numPoints : Integer;
  rawFx1, rawFx2: TParseMath;

implementation

{$R *.lfm}

{ TForm1 }
function Fx1(x: double): double;
begin
    rawFx1.newValue('x',x);
    Result := rawFx1.evaluate();
end;

function Fx2(x: double): double;
begin
    rawFx2.newValue('x',x);
    Result := rawFx2.evaluate();
end;

function Fx3(x: double): double;
begin
    rawFx1.newValue('x',x);
    rawFx2.newValue('x',x);
    Result := rawFx1.evaluate() - rawFx2.evaluate();
end;

procedure TForm1.btnSetTotalPointsClick(Sender: TObject);
var
  n, i : Integer;
begin
  n := StrToInt(ediTotalPoints.text);
  if n < 1 then
     raise Exception.create('El numero de puntos no es valido');

  interpolation := TInterpolation.create(n);
  stgPoints.RowCount := n + 1;
  for i:=1 to n do
      stgPoints.cells[0,i] := IntToStr(i);
  numPoints := 0;
  ediX.Enabled := True;
  ediY.Enabled := True;

end;

procedure TForm1.chrFunctionResultFuncSeries1Calculate(const AX: Double; out
  AY: Double);
begin
  Ay := Fx1(AX);
end;

procedure TForm1.chrFunctionResultFuncSeries2Calculate(const AX: Double; out
  AY: Double);
begin
  Ay := Fx2(AX);
end;

procedure TForm1.btnAddPointClick(Sender: TObject);
var
  x, y : Real;
begin
  if numPoints = interpolation.tp then exit();
  x := StrToFloat(edix.text);
  y := StrToFloat(ediy.text);
  interpolation.arrPoints[numPoints].x := x;
  interpolation.arrPoints[numPoints].y := y;
  stgPoints.cells[1,numPoints+1] := FloatToStr(x);
  stgPoints.cells[2,numPoints+1] := FloatToStr(y);


  numPoints := numPoints + 1;

end;

procedure TForm1.btnCalculateClick(Sender: TObject);
var
  s: String;
  i: integer;
begin
  s := interpolation.getPolinomy();
  if calcFx1 then begin
    ediPolinomy1.text := s;
    rawFx1.Expression:=s;
    chrFunctionResultFuncSeries1.Active:= True;
    chrFunctionResultConstantLine3.Position := interpolation.limInferior();
    chrFunctionResultConstantLine4.Position := interpolation.limSuperior();
  end
  else begin
    ediPolinomy2.text := s;
    rawFx2.Expression:=s;
    chrFunctionResultFuncSeries2.Active:= True;
    chrFunctionResultConstantLine5.Position := interpolation.limInferior();
    chrFunctionResultConstantLine6.Position := interpolation.limSuperior();
    showIntersect();
  end;
end;

procedure TForm1.btnIntersectClick(Sender: TObject);
begin
  stgPoints.RowCount:=1; //Limpia el StringGrid
  ediX.text := '';
  ediY.text := '';
  ediX.Enabled := False;
  ediY.Enabled := False;
  ediPolinomy2.Enabled := True;
  calcFx1 := False;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    interpolation := TInterpolation.create(0);
    OCMethods := TOCMethods.create();
    rawFx1 := TParseMath.create();
    rawFx1.AddVariable('x',0);
    rawFx2 := TParseMath.create();
    rawFx2.AddVariable('x',0);
    calcFx1 := True;
end;

procedure TForm1.showIntersect();
var
  li1, li2, ls1, ls2: Double; //limites inferiores/superiores
  gli, lls: Double; //greatest limit inferior, learest limit superior
  x: Double;
begin
  li1 := chrFunctionResultConstantLine3.Position;
  ls1 := chrFunctionResultConstantLine4.Position;
  li2 := chrFunctionResultConstantLine5.Position;
  ls2 := chrFunctionResultConstantLine6.Position;
  if li1 > li2 then gli := li1 else gli := li2;
  if ls1 < ls2 then lls := ls1 else lls := ls2;
  ocMethods.b := gli;
  ocMethods.e := lls;
  ocMethods.FunctionType := isBiseccion;
  x := ocMethods.execute();
  chrFunctionResultLineSeries1.AddXY(x,Fx1(x));

end;

end.

