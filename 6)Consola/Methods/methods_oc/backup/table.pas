unit Table;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, Menus;

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
    procedure showResults();
    procedure FormCreate(Sender: TObject);
  end;

var
  FormResults: TFormResults;


const
  colN  = 0;
  colXn = 1;
  colError = 2;

implementation
uses MethodsOCmain;

procedure TFormResults.showResults();
var
  i: Integer;
begin
  with stgResults do begin
    Clear;
    RowCount := sequenceXn.count;
    cols[colXn].assign(sequenceXn);
    cols[colError].assign(sequenceError);
    for i:=1 to sequenceXn.Count - 1 do
        Cells[colN, i] := IntToStr(i);
  end;
end;

procedure TFormResults.FormCreate(Sender: TObject);
begin

end;

procedure TFormResults.btnGraphClick(Sender: TObject);
var
  sFx, sGx: String;
begin
   sFx := ediFx.Text;
   sGx := ediGx.Text;
   MethodsOCfrm.bolzano.setExpressionFx(sFx);
   MethodsOCfrm.bolzano.setExpressionGx(sGx);
   if sFx.Length  <> 0 then
       MethodsOCfrm.chartGraphicsFuncSeries1.active := True
   else
       MethodsOCfrm.chartGraphicsFuncSeries1.active := False;
   if sGx.Length  <> 0 then
       MethodsOCfrm.chartGraphicsFuncSeries2.active := True
   else
       MethodsOCfrm.chartGraphicsFuncSeries2.active := False;
end;



{$R *.lfm}

end.

