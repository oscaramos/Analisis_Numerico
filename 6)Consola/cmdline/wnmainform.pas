{ Copyright (C) 2007 Julian Schutsch

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
  
  This Software is GPL, not LGPL as the libary it uses !
  
  Changelog
    10.8.2007 : Added "Buttons" Unit to avoid "TButton" missing error on 0.9.22 (Linux)
}
{
    Por: Oscar Daniel Ramos Ramirez
}
unit wnmainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Graphics, Dialogs, ExtCtrls, LCLType,
  ucmdbox, StdCtrls, Controls, Buttons, Menus, Grids, ComCtrls, TAGraph,
  TASeries, functionParser, StatementParser, adaptors, SuperExpressionParser,
  variableSys, PreSuperParser;

type

  { TWMainForm }

  TWMainForm = class(TForm)
    chrMain: TChart;
    chrMainAreaSeries1: TAreaSeries;
    chrPointsLineSeries: TLineSeries;
    chrXaxis: TConstantLine;
    chrYaxis: TConstantLine;
    CmdBox: TCmdBox;
    FontDialog: TFontDialog;
    memDoc: TMemo;
    MenuItem1: TMenuItem;
    PageControl1: TPageControl;

    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    ReaderTimer: TTimer;
    ProcessTimer: TTimer;
    stgVars: TStringGrid;
    Variables: TTabSheet;
    Documentation: TTabSheet;
    procedure CmdBoxClick(Sender: TObject);
    procedure CmdBoxInput(ACmdBox: TCmdBox; Input: String);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ProcessTimerTimer(Sender: TObject);
    procedure ReaderTimerTimer(Sender: TObject);
  private
    TextPosition : Integer;
    DText        : TStringList;
    FProcess     : Integer;
  private
    funcParser  : TFunctionParser;
    statParser  : TStatementParser;
    sys_adaptor : TAdaptorSys;
    preSuperParser: TPreSuperParser;
  public
    sys_variable: TVariableSys;
    spexprparser :TSuperExpressionParser;

  private
    function parseInput(input: String):Boolean;
    procedure initGlobalParameters();
    procedure updateVariableStringGrid();
    procedure fillMemDoc();

  end; 

var WMainForm: TWMainForm;


implementation
var Dir:String;

{ TWMainForm }

procedure TWMainForm.ReaderTimerTimer(Sender: TObject);
var i:Integer;
    s:String;
begin
 for i:=0 to 0 do
 begin
  s:='';
  s:=DText[TextPosition];{+#13#10;}
  Inc(TextPosition);
  CmdBox.TextColors(clLime,clBlack);
  CmdBox.Writeln(s);
  if (TextPosition>=DText.Count) then
  begin
   CmdBox.ClearLine;
   CmdBox.TextColor(clLime);
   CmdBox.Writeln(#27#10#196);
   TextPosition        := 0;
   ReaderTimer.Enabled := False;
  end;
 end;
end;

procedure TWMainForm.FormCreate(Sender: TObject);
begin
 DoubleBuffered := True;
 DText          := TStringList.Create;
 if FileExists(Dir+'/helpfunc.txt') then DText.LoadFromFile(Dir+'/helpfunc.txt');
 CmdBox.StartRead(clLime,clBlack,'>>',clYellow,clBlack);
 funcParser   := TFunctionParser.Create;
 sys_adaptor  := TAdaptorSys.Create;
 statParser := TStatementParser.Create;
 sys_variable := TVariableSys.Create;
 spexprparser := TSuperExpressionParser.Create;
 preSuperParser := TPreSuperParser.Create;
 initGlobalParameters;
 chrPointsLineSeries.ShowPoints:=True;
 //chrMain.Proportional:=True;
 fillMemDoc;
end;

procedure TWMainForm.initGlobalParameters();
begin
 sys_variable.Insert('h'       , '0.001');
 sys_variable.Insert('n'       , '1000');
 sys_variable.Insert('decimals', '4');
 sys_variable.Insert('error'   , '0.0001');
 sys_variable.Insert('color'   , 'lime');
 updateVariableStringGrid;
end;

procedure TWMainForm.updateVariableStringGrid();
const
  COLVARNAMES  = 0;
  COLVARVALUES = 1;
  COLVARTYPES = 2;
var
   ListVarNames, ListVarValues, ListVarTypes: TStringList;
   i: Integer;
begin
  ListVarNames  := sys_variable.getColVarName();
  ListVarValues := sys_variable.getColVarValue();
  ListVarTypes := sys_variable.getColVarType();
  with stgVars do begin
     RowCount := FixedRows + sys_variable.getNumValues();
     for i := 1 to RowCount-FixedRows do begin
       Cells[COLVARNAMES, i] := ListVarNames [i-1];
       Cells[COLVARVALUES,i] := ListVarValues[i-1];
       Cells[COLVARTYPES,i] := ListVarTypes[i-1];
     end;
  end;
end;

procedure TWMainForm.CmdBoxInput(ACmdBox: TCmdBox; Input: String);

begin
  Input:=LowerCase(Input);
  CmdBox.TextColors(clLime,clBlack);
  if Input='help' then
  begin
   CmdBox.TextColors(clLime,clBlack);

  end else
  if Input='clearhistory' then
  begin
   CmdBox.TextColor(clLime);
   CmdBox.Writeln('Clear History...');
   CmdBox.TextColor(clLime);
   CmdBox.ClearHistory;
  end else
  if Input='start' then
  begin
   TextPosition:=0;
   ReaderTimer.Enabled:=true;
   CmdBox.TextColors(clLime,clBlack);
   CmdBox.Writeln('Start...');
  end else if Input='stop' then
  begin
   TextPosition:=0;
   ReaderTimer.Enabled:=false;
   CmdBox.TextColors(clLime,clBlack);
   CmdBox.Writeln('Stop...');
  end else if Input='pause' then
  begin
   ReaderTimer.Enabled:=false;
   CmdBox.TextColors(clLime,clBlack);
   CmdBox.Writeln('Pause...');
  end else if Input='resume' then
  begin
   ReaderTimer.Enabled:=true;
   CmdBox.TextColors(clLime,clBlack);
   CmdBox.Writeln('Continue..');
  end else if Input='clear' then
  begin
   CmdBox.Clear;
  end else if Input='exit' then close
  else if self.parseInput(input) //Nueva funcionalidad
  then else begin
     CmdBox.TextColors(clLime,ClRed);
     CmdBox.Writeln('Invalid Command!');
  end;
 CmdBox.StartRead(clLime,clBlack,'>>',clYellow,clBlack);
end;


//superstring.Countchar(s: Char);
//Retorna el nro de ocurrencias de s en superstring.
function TWMainForm.parseInput(input: String): Boolean;
var
    varname, varexpr, finalvalue, output: String;

    procedure ProcedureStatementParser();
    begin
        //Obtengo el nombre de la variable y su expresion.
        varname := statParser.GetVarname();
        varexpr := statParser.GetExpression();

        //Preparseo la expresion (Resuelvo las funciones y las matrices)
        varexpr := preSuperParser.preParse(varexpr);

        //Parseo lo que queda de la expresion con TParseMath
        spexprparser.Expression := varexpr;

        //Calculo el valor final de la expresion.
        finalvalue := spexprparser.Evaluate();

        //Lo inserto en un sistema administrador de variables
        sys_variable.Insert(varname, finalvalue);

        //Mi salida sera el valor final de dicha expresion
        output := finalvalue;

        //Actualizo mi StringGrid del formulario con respecto a sys_variable
        updateVariableStringGrid;
    end;

    function getFormat(): String;
    var s: String; i:Integer;
    begin
     s := '0.';
     for i:=1 to StrToInt(sys_variable.getGlobalVarValue('decimals')) do
         s := s + '0';
     Result := s;
    end;

begin
   Result := False;
   statParser.parser(input);
   if not statParser.fails() then begin
      try
         ProcedureStatementParser;
      except
         on E:Exception do
           CmdBox.Writeln('Error: ' + E.Message );
      end;
      if getType(output) = 'Double' then
        output := FormatFloat(getFormat() , StrToFloat(output));

      CmdBox.Writeln( output );
      Result := True;
      exit;
   end;
end;

procedure TWMainForm.fillMemDoc();
begin
 with memDoc do begin
  Clear;
  Append('Oscar Daniel Ramos Ramirez');
  Append('----------------------------------------------------------------------------');
  Append('>>raiz(f, a, b, type = 4)');
  Append('get global: Error, color');
  Append('Return Root as Double');
  Append('-case type of');
  Append('1: biseccion');
  Append('2: falsa pos');
  Append('3: newton raphson');
  Append('4: secante');
  Append('5: Biseccion and Secant');
  Append('example: raiz(sin(x), -10, 10)');
  Append('ex: raiz(x^2 - x^3, -10, 10)');
  Append('ex: raiz(x^2 - x + 2, -10, 10)');
  Append('----------------------------------------------------------------------------');
  Append('>>senl([f1, f2, ..., fn], [a0, b0, ..., z0])');
  Append('Max: 4 variables(x,y,z,w)');
  Append('No gets from global');
  Append('Return Matrix: String');
  Append('ex: senl([x^2 + y^2 + 5; x^2 - y^2 - 1], [2;1])');
  Append('----------------------------------------------------------------------------');
  Append('lagrange([x1 x2 ... xn; y1 y2 ... yn]) ');
  Append('No gets from global');
  Append('Return Polinomy: String');
  Append('Ex: lagrange([-5 2 6 10; 125 8 216 1000])');
  Append('Falla en: f1 = lagrange([1 -1 2 -2 3 -3; 0.54 0.54 -0.42 -0.42 0.99 0.99])');
  Append('----------------------------------------------------------------------------');
  Append('integral(f, a, b, type = 2(Simpson13)) ');
  Append('get from global: n, color');
  Append('Return integral: Double');
  Append('-case type of');
  Append('1: Trapecio');
  Append('2: Simpson13');
  Append('3: Simpson38');
  Append('ex: integral(sin(x), -3.14, 3.14, 2)');
  Append('----------------------------------------------------------------------------');
  Append('area(f, a, b, type = 2(Simpson13))');
  Append('get global: n, color');
  Append('Return integral: Double');
  Append('-case type of');
  Append('1: Trapecio');
  Append('2: Simpson13');
  Append('3: Simpson38');
  Append('ex: area(sin(x), -3.14, 3.14, 2)');
  Append('----------------------------------------------------------------------------');
  Append('edo(df, X0, Y0, Xn, type = 5(Dormand Price))');
  Append('get global: h, color');
  Append('Return diff: Double');
  Append('-case type of');
  Append('1: Euler');
  Append('2: Heun');
  Append('3: Runge Kutta 3');
  Append('4: Runge Kutta 4');
  Append('5: Dormand Price');
  Append('ex: edo(x*y, 1, 1, 2)');
  Append('----------------------------------------------------------------------------');
  Append('intersection(f1, f2, a, b)');
  Append('get global: h, error, color');
  Append('Return void: String');
  Append('ex: intersection(sen(x), cos(x), 0, 10)');
  Append('ex: intersection(x^2 - x^3, 0, -2, 2)');
  Append('Se recomienda usar un h>=0.1');
  Append('Si el h es muy pequeño, se tardara en calcular');
  Append('Solo dibuja raices donde se cumpla el teorema de bolzano');
  Append('----------------------------------------------------------------------------');
  Append('plot2d(f, a, b, color = globalColor)');
  Append('get global: color');
  Append('Return void: String');
  Append('----------------------------------------------------------------------------');
  Append('func2d(f, color = globalColor)');
  Append('get global: color');
  Append('Return void: String');
  Append('----------------------------------------------------------------------------');
  Append('clearplot()');
  Append('no gets from global');
  Append('Return void: String');
  Append('----------------------------------------------------------------------------');
  Append('Proportional()');
  Append('Switch between Tchar proportional and no proportional');
  Append('----------------------------------------------------------------------------');
  Append('Example of use');
  Append('decimals = 8');
  Append('pi = 3.1416');
  Append('a = pi / 2');
  Append('b = 3*pi / 2');
  Append('mat = [-5 2 6 10; 125 8 216 1000]');
  Append('f = lagrange(mat)');
  Append('clearplot()');
  Append('plot2d(f, a, b, skyblue) ');
  Append('clearplot()');
  Append('color = ''red'' ');
  Append('f = ''sin(x)'' ');
  Append('raiz(f, a, b, 1) + area(f, a, pi)');
  Append('color = ''lime'' ');
  Append('end = Ans + area(f, pi, b, 1)');
 end;
end;

procedure TWMainForm.CmdBoxClick(Sender: TObject);
begin

end;


procedure TWMainForm.ProcessTimerTimer(Sender: TObject);
begin
 if FProcess=100 then
 begin
  CmdBox.ClearLine;
  ProcessTimer.Enabled:=False;
 end
 else
 begin
  CmdBox.TextColors(clRed,clBlue);
  CmdBox.Write('Processing ['+IntToStr(FProcess)+'%]'#13);
 end;
 Inc(FProcess);
end;

procedure TWMainForm.FormDestroy(Sender: TObject);
begin
 DText.Free;
end;



initialization
  {$I wnmainform.lrs}
  Dir:=ExtractFileDir(ParamStr(0));
end.

