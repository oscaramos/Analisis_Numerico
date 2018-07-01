unit Table;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, Menus, ParseMath;

type
  { TFormResults }
  TFormResults = class(TForm)
    btnGraph: TButton;
    EdiFx: TEdit;
    EdiGx: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    stgResults: TStringGrid;
    sequenceXn, sequenceError: TStringList;
    procedure btnGraphClick(Sender: TObject);
    procedure doResults();
    procedure FormCreate(Sender: TObject);
  private

  end;
function Fx(x: Double): Double;
function Gx(x: Double): Double;
function dFx(x: Double): Double;

const
  colN  = 0;
  colXn = 1;
  colError = 2;

var
  ParseFx, ParsedFx, ParseGx: TParseMath;
  FormResults: TFormResults;


implementation
uses MethodsOCmain;


procedure TFormResults.doResults();
var
  i: Integer;
begin
  with stgResults do begin
    i := sequenceXn.count;
    RowCount := sequenceXn.count;
    cols[colXn].assign(sequenceXn);
    cols[colError].assign(sequenceError);
    for i:=1 to sequenceXn.Count - 1 do
        Cells[colN, i] := IntToStr(i);
  end;
end;

procedure TFormResults.FormCreate(Sender: TObject);
begin
  ParseFx  := TParseMath.create();
  ParsedFx := TParseMath.create();
  ParseGx  := TParseMath.create();
  ParseFx.addVariable('x',0);
  ParsedFx.addVariable('x',0);
  ParseGx.addVariable('x',0);
end;

procedure TFormResults.btnGraphClick(Sender: TObject);
var
  sFx, sdFx, sGx: String;
begin
   sFx := ediFx.Text;
   sGx := ediGx.Text;
   ParseFx.Expression:= sFx;
   ParsedFx.Expression:= sdFx;
   ParseGx.Expression:= sGx;
   if sFx.Length  <> 0 then
       MethodsOCfrm.chartGraphicsFuncSeries1.active := True
   else
       MethodsOCfrm.chartGraphicsFuncSeries1.active := False;
   if sdFx.Length <> 0 then
       MethodsOCfrm.chartGraphicsFuncSeries2.active := True
   else
       MethodsOCfrm.chartGraphicsFuncSeries2.active := False;
   if sGx.Length  <> 0 then
       MethodsOCfrm.chartGraphicsFuncSeries3.active := True
   else
       MethodsOCfrm.chartGraphicsFuncSeries3.active := False;
end;

function Fx(x: Double): Double;
begin
  ParseFx.NewValue( 'x', x );
  Result := ParseFx.Evaluate();
end;
function dFx(x: Double): Double;
var
  f,h: Double;
begin
  f := Fx(x);
  h := 0.0001;
  Result := (Fx(x+h)-f)/h;
end;

function Gx(x: Double): Double;
begin
  ParseGx.NewValue( 'x', x );
  Result := ParseGx.Evaluate();
end;



{$R *.lfm}

end.

