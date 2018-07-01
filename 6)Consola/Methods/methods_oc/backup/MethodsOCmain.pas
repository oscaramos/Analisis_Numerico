unit MethodsOCmain;

{$mode objfpc}{$H+}

interface
(*
-Atencion, la linea de texto dFx no es necesario ingresar la derivada de F(x)
Basta con poner un valor cualquiera para que se grafique
*)
uses
  Classes, SysUtils, FileUtil, TAGraph, TAFuncSeries, TASeries,
  TATransformations, TAChartCombos, TAStyles, TAIntervalSources, TATools, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Menus,
  class_bolzano, Table;

type
  { TMethodsOCfrm }

  TMethodsOCfrm = class(TForm)
    btnPoint: TButton;
    btn_calculate: TButton;
    chartGraphics: TChart;
    chartGraphicsConstantLine1: TConstantLine;
    chartGraphicsConstantLine2: TConstantLine;
    chartGraphicsFuncSeries1: TFuncSeries;
    chartGraphicsFuncSeries2: TFuncSeries;
    chartGraphicsFuncSeries3: TFuncSeries;
    chartGraphicsLineSeries1: TLineSeries;
    chkProportional: TCheckBox;
    cmb_biseccion: TComboBox;
    cmb_mode: TComboBox;
    ediPointX: TEdit;
    ediPointY: TEdit;
    EdiBeginInterval: TEdit;
    EdiEndInterval: TEdit;
    ediError: TEdit;
    ediRpta: TEdit;
    ediX0: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbl_Interval: TLabel;
    pnlRight: TPanel;
    trbMax: TTrackBar;
    trbMin: TTrackBar;
    procedure btnGraphClick(Sender: TObject);
    procedure btnPointClick(Sender: TObject);
    procedure btnGraph2Click(Sender: TObject);
    procedure btn_calculateClick(Sender: TObject);
    procedure chartGraphicsFuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure chartGraphicsFuncSeries2Calculate(const AX: Double; out AY: Double);
    procedure chartGraphicsFuncSeries3Calculate(const AX: Double; out AY: Double);
    procedure chkProportionalChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure trbMaxChange(Sender: TObject);
    procedure trbMinChange(Sender: TObject);
  public
      Bolzano : TOCMethods;
  private
    function F(x: Double): Double;
    function dF(x: Double): Double;
    function G(x: Double): Double;
  end;
var
  MethodsOCfrm: TMethodsOCfrm;


implementation



{$R *.lfm}
{ TMethodsOCfrm }

procedure TMethodsOCfrm.btnGraphClick(Sender: TObject);
begin
  chartGraphicsFuncSeries1.Pen.Color:= clBlue;
  chartGraphicsFuncSeries1.Active:= True;
end;

procedure TMethodsOCfrm.btnGraph2Click(Sender: TObject);
begin
  chartGraphicsFuncSeries2.Pen.Color:= clRed;
  chartGraphicsFuncSeries2.Active:= True;
end;



procedure TMethodsOCfrm.btnPointClick(Sender: TObject);
var x, y: Real;
begin
  x:= StrToFloat(ediPointX.Text);
  y:= StrToFloat(ediPointY.Text);
  chartGraphicsLineSeries1.AddXY( x, y );
end;

procedure TMethodsOCfrm.btn_calculateClick(Sender: TObject);
var
  x: Real;
begin
  MethodsOCfrm.show;
  with bolzano do begin
      b := StrToFloat(EdiBeginInterval.Text);
      e := StrToFloat(EdiEndInterval.Text);
      X0 :=  StrTofLoat(EdiX0.Text);
      Top := 1000;
      setExpressionFx(FormResults.ediFx.Text);
      setExpressionGx(FormResults.ediGx.Text);
      ErrorAllowed := StrTofLoat(EdiError.Text);
      FunctionType := cmb_biseccion.Itemindex;
      calculateMode := cmb_mode.ItemIndex;
      x := Execute();
  end;

  if Bolzano.calculateMode = isModeIntersect then
     chartGraphicsLineSeries1.AddXY( x, f(x))
  else
     chartGraphicsLineSeries1.AddXY( x, 0);
  ediRpta.Text := FloatToStr(x);
  FormResults.sequenceXn    := Bolzano.sequenceXn;
  FormResults.sequenceError := Bolzano.sequenceError;
  FormResults.showResults();
end;

procedure TMethodsOCfrm.chartGraphicsFuncSeries1Calculate(const AX: Double; out AY: Double);
begin
  AY:= ( F(AX) );
end;

procedure TMethodsOCfrm.chartGraphicsFuncSeries2Calculate(const AX: Double; out AY: Double);
begin
  AY:= ( dF(AX) );
end;

procedure TMethodsOCfrm.chartGraphicsFuncSeries3Calculate(const AX: Double; out
  AY: Double);
begin
  AY:= ( G(AX) );
end;

procedure TMethodsOCfrm.chkProportionalChange(Sender: TObject);
begin
  chartGraphics.Proportional:= not chartGraphics.Proportional;
end;


procedure TMethodsOCfrm.FormCreate(Sender: TObject);
begin
  chartGraphics.Extent.UseXMax:= true;
  chartGraphics.Extent.UseXMin:= true;
  chartGraphicsLineSeries1.ShowPoints:= True;
  chartGraphicsLineSeries1.ShowLines:= False;
  Bolzano := TOCMethods.create;
  cmb_biseccion.items.assign(Bolzano.FunctionList);
  cmb_biseccion.itemindex := 0;
  cmb_mode.items.assign(Bolzano.CalculateModeList);
  cmb_mode.itemindex := 0;
  chartGraphicsFuncSeries1.Pen.Color:= clBlue;
  chartGraphicsFuncSeries2.Pen.Color:= clGreen;
  chartGraphicsFuncSeries3.Pen.Color:= clRed;
end;

procedure TMethodsOCfrm.trbMaxChange(Sender: TObject);
begin
  chartGraphics.Extent.XMax:= trbMax.Position;
end;

procedure TMethodsOCfrm.trbMinChange(Sender: TObject);
begin
  chartGraphics.Extent.XMin:= trbMin.Position;
end;

function TMethodsOCfrm.F(x: Double): Double;
begin
   F := bolzano.F(x);
end;

function TMethodsOCfrm.dF(x: Double): Double;
begin
   dF := bolzano.dF(x);
end;

function TMethodsOCfrm.G(x: Double): Double;
begin
   G := bolzano.G(x);
end;

end.

