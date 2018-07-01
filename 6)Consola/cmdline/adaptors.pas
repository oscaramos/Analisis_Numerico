unit adaptors;

{$mode objfpc}

interface

uses
  Classes, SysUtils, Matrix, class_bolzano,
  regexpr, TASeries, TAFuncSeries, TAGraph, Graphics, ParseMath,
  logicInterpolation, integrales, MethodsDif, newtonGenMethod, variableSys, SEDO;
type
  TAdaptorSys = class
    public
      constructor create();
      function evaluate(funcName: String; list_par: TStringList): String;
    private
      function adp_raiz()     : String; //adp = adapter
      function adp_newtonGen(): String;
      function adp_lagrange() : String;
      function adp_integral() : String;
      function adp_area()     : String;
      function adp_methodDif(): String;
    private
      function _intern_adp_raiz(f, a, b, mt, drawp: String):String;
    private
      function          met_plot2d(): String;
      procedure _intern_met_plot2d(f, a, b, color: String);
      function          met_func2d(): String;
      procedure _intern_met_func2d(f, color: String);
      function met_clearplot(): String;
    private
      function met_sedo(): String;
      function met_intersection(): String;
      function met_proportional(): String;

    private
      function _intern_integral_and_Area(calculateArea: Boolean): String;
      procedure recognizeNumArgs();
      function getGlobal(name: String): String;
    private
      list_par: TStringList;
      ZeroPar, OnePar, TwoPar, TreePar, FourPar, FivePar, SixPar, SevenPar, EightPar, NinePar, TenPar: Boolean;
      function_list: TList;

  end;
function StrToMatrix(s: String): TMatrix;

function StrToStringList(s: String): TStringlist;

function CompleteInputColor(s: String): String;


const
  AngleSexagedecimal = 0;
  AngleRadian = 1;

  fn_raiz     = 'raiz'; //fn means function name
  fn_senl     = 'senl';
  fn_lagrange = 'lagrange';
  fn_integral = 'integral';
  fn_area     = 'area';
  fn_edo      = 'edo';
  fn_plot2d   = 'plot2d';
  fn_func2d   = 'func2d';
  fn_clearplot= 'clearplot';
  fn_intersection= 'intersection';
  fn_sedo        = 'sedo';
  fn_proportional = 'proportional';


implementation
uses wnmainform;
constructor TAdaptorSys.create();
begin
   function_list := TList.Create;
end;

function TAdaptorSys.evaluate(funcName: String; list_par: TStringList): String;
begin
   self.list_par := list_par;
   recognizeNumArgs;
   case funcName of //funcname siempre sera transformada a minusculas
     fn_raiz      :Result := adp_raiz();
     fn_senl      :Result := adp_newtonGen();
     fn_lagrange  :Result := adp_lagrange();
     fn_integral  :Result := adp_integral();
     fn_area      :Result := adp_area();
     fn_edo       :Result := adp_methodDif();
     fn_plot2d    :Result := met_plot2d();
     fn_func2d    :Result := met_func2d();
     fn_clearplot :Result := met_clearplot();
     fn_intersection:Result:=met_intersection();
     fn_sedo      :Result := met_sedo();
     fn_proportional:Result:=met_proportional();
     else raise Exception.create('La función ingresada es desconocida');
     //Result := ; //Adicional;
   end;
end;

//raiz(f, a, b, Type = 4, draw = 1(True))
//get global: Error, color
//Return Double
//type = 1 -> biseccion
//     = 2 -> falsa pos
//     = 3 -> newton raph
//     = 4 -> secant
function TAdaptorSys.adp_raiz(): String;
const
  default_type = 4; //secant
  default_draw = True;

var
  f: String;
  a, b, X0, error, RootFound: Double;
  mt : Integer;
  draw: Boolean;
  raiz : TOCMethods;

      procedure receiveParameters();
      begin
       f := list_par[0];
       a := StrToFloat(list_par[1]);
       b := StrToFloat(list_par[2]);
       error := StrToFloat(getGlobal('error'));
      end;

      procedure receiveDefaultParameters();
      begin
        if FourPar or FivePar then
          mt := StrToInt(list_par[3])
        else
          mt := default_type;
        if FivePar then
          draw := StrToInt(list_par[4]) = 1
        else
          draw := default_draw;
      end;


      procedure setX0(); inline;
      begin
         X0 := (a+b)/2;
      end;

begin
   if not (TreePar or FourPar or FivePar) then
      raise Exception.create('TAdaptorSys.adp_raiz: Numero incorrecto de parametros');
   receiveParameters;
   receiveDefaultParameters;
   case mt of
     1,2,5:; (*nothing, for this else condition*)
     3,4: setX0();
     else raise Exception.create('TAdaptorSys.adp_raiz: Metodo no reconocido, solo son validos valores de [1-4]');
   end;

   raiz := TOCMethods.Create();
   raiz.b := a;
   raiz.e := b;
   raiz.X0 :=  X0;
   raiz.setExpressionFx(f);
   raiz.ErrorAllowed := error;
   raiz.FunctionType := mt;
   raiz.calculateMode := isModeNormal;
   RootFound := raiz.Execute();

   Result := FloatToStr(RootFound);
   if draw then begin
     case mt of
       1,2,5: _intern_met_plot2d(f, FloatToStr(a), FloatToStr(b), getGlobal('color'));
       3,4: _intern_met_func2d(f, getGlobal('color'));
     end;
     WMainForm.chrPointsLineSeries.AddXY(RootFound,0);
   end;
end;

//senl([f1, f2, ..., fm], [a0, b0, ..., vn])
//senl([x^2 + y^2 + 5; x^2 - y^2 - 1], [2;1])
//Max: 4 variables
//Return Matrix
function TAdaptorSys.adp_newtonGen(): String;
var
  list_f, list_vars: TStringList;
  X0: TMatrix;
  senl: TNewtonGen;
  i: Integer;
    procedure receiveNormalParameters();
    begin
       list_f := StrToStringList(list_par[0]);
       X0 := StrToMatrix(list_par[1]);
       list_vars := TStringList.Create;
    end;
    procedure getvariables();
    begin
       if X0.getNumRows() >= 1 then
          list_vars.Add('x');
       if X0.getNumRows() >= 2 then
          list_vars.Add('y');
       if X0.getNumRows() >= 3 then
          list_vars.Add('z');
       if X0.getNumRows() >= 4 then
          list_vars.Add('w');
    end;

begin
   if not TwoPar then raise Exception.Create('TAdaptorSys.adp_newtonGen, num de parametros incorrectos');
   receiveNormalParameters;
   getvariables;
   senl := TNewtonGen.create(list_f.Count, list_vars.Count);
   list_vars.Delimiter := ' ';
   senl.initVariables( list_vars.DelimitedText );
   senl.initArrFunctions;
   for i:=0 to list_f.Count-1 do begin
       senl.setExpression(list_f[i], i);
   end;
   for i:=0 to list_vars.Count-1 do begin
       senl.setElementX0(X0.getElement(i,0), i);
   end;
   Result := MatrixToStr(senl.Calculate());
end;


//lagrange(Puntos...)
//lagrange([x1 x2 ... xn; y1 y2 ... yn])
//Return String
function TAdaptorSys.adp_lagrange(): String;
var
  points : TMatrix;
  lagrange: TLagrange;
  polinomy: String;
  a, b: Double;
  i: Integer;
begin
   if not OnePar then
      raise Exception.create('TAdaptorSys.adp_lagrange: Numero incorrecto de parametros');
    points := StrToMatrix(list_par[0]);

    lagrange := TLagrange.create(points);
    polinomy := lagrange.getPolinomy();
    a := lagrange.limInferior;
    b := lagrange.limSuperior;
    _intern_met_plot2d(polinomy, FloatToStr(a), FloatToStr(b), getGlobal('color'));
    Result := polinomy;
    WMainForm.chrPointsLineSeries.LineType:=ltNone;

    for i:=0 to points.getNumColumns()-1 do begin
        WMainForm.chrPointsLineSeries.AddXY(points.getElement(0,i),points.getElement(1,i));
    end;

end;

//integral(f, a, b, type = 2(Simpson13))
//Return Double
function TAdaptorSys.adp_integral(): String;
begin
   Result := _intern_integral_and_Area(False); //Calcula integral
end;

//area(f, a, b, type = 2(Simpson13))
//Return Double
function TAdaptorSys.adp_area(): String;
begin
   Result := _intern_integral_and_Area(True);  //Calcula area
end;

//integral_and_area(f, a, b, type = 2(Simpson13))
//gets global n, color
//Return Double
function TAdaptorSys._intern_integral_and_Area(calculateArea: Boolean): String;
const
  default_type = 2;
  //simpson 1/3
  //type = 1 -> Trapecio
  //     = 2 -> Simpson13
  //     = 3 -> Simpson38
var
  f: String;
  a, b: Double;
  n, mt: Integer;
  integral: TMethodIntegral;
  //BEGIN PROCEDURES

          procedure receiveNormalParameters();
          begin
            f := list_par[0];
            a := StrToFloat(list_par[1]);
            b := StrToFloat(list_par[2]);
            n := StrToInt(getGlobal('n'));
          end;
          procedure receiveDefaultParameters();
          begin
            if FourPar then
               mt := StrToInt(list_par[3])
            else
               mt := default_type;
          end;

          procedure plotArea();
          var
            parseMath: TParseMath;

                 procedure initf();
                 begin
                   parseMath := TParseMath.Create();
                   parseMath.Expression := f;
                   parseMath.AddVariable('x',0);
                 end;

                 function area_f(x: Double): Double;
                 begin
                   parseMath.NewValue('x', x);
                   area_f := ParseMath.Evaluate();
                 end;

          const
            h = 1e-2;
          var
            area: TAreaSeries;
            xn: Double;
            colorArea: TColor;

          begin
            initf;
            colorArea := StringToColor(CompleteInputColor(getGlobal('color')));
            area := TAreaSeries.Create(WMainForm.chrMain);
            with area do begin
                UseZeroLevel:=True;
                ZeroLevel:=0.0;
                SeriesColor := colorArea;
                AreaBrush.Color := colorArea;
                AreaLinesPen.Color:= colorArea;
                Transparency := 150;
            end;
            Xn := a;
            repeat
               area.AddXY(Xn,area_f(Xn));
               Xn := Xn + h;
            until xn > b;
            function_list.Add(area);
            WMainForm.chrMain.AddSeries(area);
          end;
  //END PROCEDURES

begin
   if not (TreePar or FourPar) then
      raise Exception.create('TAdaptorSys._intern_integral_and_Area: Numero incorrecto de parametros');
   receiveNormalParameters;
   receiveDefaultParameters;
   integral := TMethodIntegral.Create;
   integral.a := a;
   integral.b := b;
   integral.n := n;
   integral.MethodType := mt;
   integral.setExpr(f);
   integral.findArea := calculateArea;
   Result := FloatToStr(integral.Evaluate());
   _intern_met_plot2d(f, FloatToStr(a), FloatToStr(b), getGlobal('color'));
   if calculateArea then
      plotArea;
end;


//edo(df; X0; Y0; Xn; type = 5(Dormand Price))
//get global h, color
//Return Double
function TAdaptorSys.adp_methodDif(): String;
const
  default_type = 5; //Dormand
  //type = 1 -> Euler
  //     = 2 -> Heun (Euler mejorado)
  //     = 3 -> Runge Kutta 3
  //     = 4 -> Runge Kutta 4
  //     = 5 -> Dormand Price
var
  df: String;
  X0, Y0, Xn, h: Double;
  mt: Integer;
  line: TLineSeries;
  edo: TMethodsDif;
      procedure receiveNormalParameters();
      begin
          df := list_par[0];
          X0 := StrToFloat(list_par[1]);
          Y0 := StrToFloat(list_par[2]);
          Xn := StrToFloat(list_par[3]);
          h  := StrToFloat(getGlobal('h'));
      end;

      procedure receiveDefaultParameters();
      begin
          if FivePar then
             mt := StrToInt(list_par[4])
          else
             mt := default_type;
      end;

      procedure createTLineSeries();
      begin
          line := TLineSeries.Create(WMainForm.chrMain);
          function_list.Add(line);
          with line do begin
            LinePen.Color := StringToColor(CompleteInputColor(getGlobal('color')));
          end;
          WMainForm.chrMain.AddSeries(line);
      end;

      procedure showchr();
      var
         XNew, YNew: Double;
         i, s: Integer; //s es signo
      begin
         XNew := X0;
         if not edo.isReversed then s := 1 else s := -1;
         for i:=1 to edo.n+1 do begin
            YNew := StrToFloat(edo.table[0][i]);
            line.AddXY(XNew, YNew);
            XNew := XNew + h*s;
         end;
      end;

begin
  if not (FourPar or FivePar) then
      raise Exception.create('TAdaptorSys.adp_methodDif: Numero incorrecto de parametros');
  receiveNormalParameters;
  receiveDefaultParameters;
  case mt of
    1,2,3,4,5:;
    else raise Exception.Create('TAdaptorSys.adp_methodDif: Metodo no reconocido, este argumento puede tener valores de entre [1-5]');
  end;

  edo := TMethodsDif.Create;
  edo.X0 := X0;
  edo.Y0 := Y0;
  edo.n := Trunc(abs(Xn-X0)/h);
  edo.h := h;
  edo.methodtype := mt;
  edo.isReversed:= Xn < X0;
  edo.setExpression(df);

  Result := FloatToStr(edo.Execute);
  createTLineSeries;
  showchr;
end;


function TAdaptorSys._intern_adp_raiz(f, a, b, mt, drawp: String):String;
var
  newlist_par: TStringList;

begin
  newlist_par := TStringList.Create;
  newlist_par.Add(f);
  newlist_par.Add(a);
  newlist_par.Add(b);
  newlist_par.Add(mt);
  newlist_par.Add(drawp);
  Result := self.Evaluate(fn_raiz, newlist_par);
end;


//plot2d(f, a, b, color = globalColor)
//get global: globalColor
//Return Void
function TAdaptorSys.met_plot2d(): String;
var
  a, b, x, h: Double;
  color_grafica: TColor;
  strf: String;
  active_function: Integer;
  parseMath: TParseMath;

     procedure receiveParameters();
     begin
       strf := list_par[0];
       a    := StrToFloat(list_par[1]);
       b    := StrToFloat(list_par[2]);
       h    := StrToFloat(getGlobal('h'));
     end;

     procedure receiveDefaultParameters();
      begin
        if FourPar then
          color_grafica := StringToColor(CompleteInputColor(list_par[3]))
        else
          color_grafica := StringToColor(CompleteInputColor(getGlobal('color')));
      end;

     procedure initf();
     begin
       parseMath := TParseMath.Create();
       parseMath.Expression := strf;
       parseMath.AddVariable('x',0);
     end;

     function f(x: Double): Double;
     begin
       parseMath.NewValue('x', x);
       f := ParseMath.Evaluate();
     end;

     procedure plotear();
     begin
       x := a;
       with TLineSeries(function_list[active_function]) do
       repeat
             AddXY( x, f(x) );
             x:= x + h
       until ( x >= b );
     end;

begin
  if not(TreePar or FourPar) then raise Exception.Create('TAdaptorSys.met_plot2d: num incorrecto de parametros');
  receiveParameters;
  receiveDefaultParameters;
  initf;

  function_list.Add( TLineSeries.Create(WMainForm.chrMain));
  active_function := function_list.Count - 1;
  with TLineSeries(function_list[active_function] ) do begin
  //  Name:= LineSeriesName + IntToStr(active_function); Causa problemas
    LinePen.Color := color_grafica;
  end;
  WMainForm.chrMain.AddSeries(TLineSeries(function_list[active_function] ));
  plotear;
  Result := 'void';
end;

procedure TAdaptorSys._intern_met_plot2d(f, a, b, color: String);
var
  newlist_par: TStringList;
begin
  newlist_par := TStringList.Create;
  newlist_par.Add(f);
  newlist_par.Add(a);
  newlist_par.Add(b);
  newlist_par.Add(color);
  self.Evaluate(fn_plot2d, newlist_par);
end;

//Adicional: hacer que no (se haga antizoom hasta cubrir toda la funcion).
//func2d(f, color = globalColor)
//get global: color
//Return Void
function TAdaptorSys.met_func2d(): String;
const
  liminf = '-100';
  limsup = '100';
var
  f: String;
  color_grafica: String;
begin
   if not(OnePar or TwoPar) then
     raise Exception.Create('TAdaptorSys.met_func2d: Num parametros no valido');
   f := list_par[0];
   if TwoPar then
     color_grafica := list_par[1]
   else
     color_grafica := getGlobal('color');
   _intern_met_plot2d(f, liminf, limsup, color_grafica);
   Result := 'void';
end;

procedure TAdaptorSys._intern_met_func2d(f, color: String);
var
  newlist_par: TStringList;
begin
  newlist_par := TStringList.Create;
  newlist_par.Add(f);
  newlist_par.Add(color);
  self.Evaluate('func2d', newlist_par);
end;

//clearplot()
function TAdaptorSys.met_clearplot(): String;
var
  xaxis, yaxis: TConstantLine;
  tpoints: TLineSeries;
begin
  function_list.Clear;
  WMainForm.chrMain.ClearSeries;
  xaxis := TConstantLine.Create(WMainForm.chrMain);
  yaxis := TConstantLine.Create(WMainForm.chrMain);
  tpoints := TLineSeries.Create(WMainForm.chrMain);

  xaxis.LineStyle := TLineStyle.lsHorizontal;
  yaxis.LineStyle := TLineStyle.lsVertical;
  xaxis.SeriesColor := clWhite;
  yaxis.SeriesColor := clWhite;
  tpoints.Name := 'chrPointsLineSeries';
  tpoints.ShowPoints:=True;

  WMainForm.chrMain.AddSeries(xaxis);
  WMainForm.chrMain.AddSeries(yaxis);
  WMainForm.chrMain.AddSeries(tpoints);
  WMainForm.chrPointsLineSeries := tpoints;


  Result := 'void';
end;

//sedo
function TAdaptorSys.met_sedo(): String;
begin
  //Paro el avance por falta de tiempo
end;

//intersection(f1, f2, a, b)
//get global h, error, color.
function TAdaptorSys.met_intersection(): String;
var
  f1, f2, F: String;
  a, b, h, newX, root: Double;
  parse: TParseMath;

  procedure ReceiveNormalParameters();
  begin
    f1 := list_par[0];
    f2 := list_par[1];
    F := Concat(f1,' - ',f2);        //F(x) = f1(x) - f2(x) = 0
    a := StrToFloat(list_par[2]);
    b := StrToFloat(list_par[3]);
    h := StrToFloat(getGlobal('h'));
  end;
  procedure setf();
  begin
    parse := TParseMath.Create();
    parse.Expression:=f1; //o f2
    parse.AddVariable('x', 0);
  end;

  function fx(x: Double):Double;
  begin
    parse.NewValue('x',x);;
    fx := parse.Evaluate();

  end;

begin
  if not FourPar then raise Exception.Create('TAdaptorSys.met_intersection: Num incorrecto de parametros');
  ReceiveNormalParameters;
  setf;
  newX := a;
  _intern_met_plot2d(f1, FloatToStr(a), FloatToStr(b), getGlobal('color'));
  _intern_met_plot2d(f2, FloatToStr(a), FloatToStr(b), getGlobal('color'));
  repeat
     try
        root := StrToFloat(_intern_adp_raiz(F,
                                            FloatToStr(newX),
                                            FloatToStr(newX+h),
                                            '1', '0'));
        WMainForm.chrPointsLineSeries.AddXY(root, fx(root) );
     except
       on e:Exception do //nothing
     end;
     newX := newX + h - h/10;
  until newX > b;
  Result := 'void';

end;

//Proportional
function TAdaptorSys.met_proportional(): String;
begin
   if not ZeroPar then raise Exception.Create('TAdaptorSys.met_proportional: Num incorrecto de parametros');
   WMainForm.chrMain.Proportional := not WMainForm.chrMain.Proportional;
   Result := 'void';
end;

procedure TAdaptorSys.recognizeNumArgs();
type PtrBoolean = ^Boolean;
const MAXPARS = 10;
var
  XPar: array[0..MAXPARS] of PtrBoolean;
  i: Integer;

  procedure initXPar();
  begin
    XPar[0] := @ZeroPar;  XPar[1] := @OnePar;   XPar[2] := @TwoPar;
    XPar[3] := @TreePar;  XPar[4] := @FourPar;  XPar[5] := @FivePar;
    XPar[6] := @SixPar;   XPar[7] := @SevenPar; XPar[8] := @EightPar;
    XPar[9] := @NinePar;  XPar[10] := @TenPar;
  end;

begin
  if list_par.Count > MAXPARS then raise Exception.create('El nro de argumentos excede al maximo permitido');
  initXPar;
  for i:=0 to MAXPARS do
     XPar[i]^ := False;   //Todos los XPars son nulos
  XPar[list_par.Count]^ := True; //Menos el XPar correcto
end;



function TAdaptorSys.getGlobal(name: String): String;
begin
  Result := WMainForm.sys_variable.getGlobalVarValue(name);
end;


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
//////////////////////////////////TOOLS////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

//StrToMatrix requiere de la unidad regexpr
//uses regexpr;
//[a b c; d e f; ...; x y z]
function StrToMatrix(s: String): TMatrix;
type
  arr_of_Double = array of double;
  arr_of_arr_of_Double = array of array of double;
const
  trash = '*';
var//idx is index
  idx, endidx, sep_idx: Integer;
  contColumns, contRows, fixedColumns: Integer;
  rawrow: TStringList;
  rawmatrix: array of TStringList;
  parsedMatrix: arr_of_arr_of_Double;

    procedure parseRow(substr: String);//substr "1  2 3" or "  4  5 6"
    var re: TRegExpr;
    begin
         rawrow := TStringList.Create;
         contColumns := 0;
         contRows := contRows + 1;
         re := TRegExpr.Create('-?\d+(\.\d+)?'); //\w matches alphanumeric elements
         if re.Exec(substr) then begin
             repeat
                 rawrow.Add(re.Match[0]);
                 contColumns := contColumns + 1;
             until not re.ExecNext;
         end;
    end;
    procedure produceParsedMatrix();
    var
      parsedRow: arr_of_Double;
      i, j: Integer;
    begin
      SetLength(parsedMatrix, contRows);
      SetLength(parsedRow,fixedColumns);

      for i:=0 to contRows-1 do begin
          for j:=0 to fixedcolumns-1 do begin
            parsedRow[j] := strToFloat(rawMatrix[i][j]);
          end;
          parsedMatrix[i] := Copy(parsedRow, 0, fixedColumns);
      end;
    end;
begin
   contRows := 0;
   idx  := Pos('[', s);
   endidx := Pos(']', s);
   repeat
     sep_idx := Pos(';', s);
     if sep_idx = 0 then sep_idx := endidx;
     s[sep_idx] := trash;

     parseRow(Copy(s, idx+1, sep_idx-idx-1));
     idx := sep_idx;

     if(contRows = 1) then
         fixedColumns := contColumns
     else if(contColumns <> fixedColumns) then raise Exception.create('StrToMatrix: Número de columnas no fijas');
     SetLength(rawMatrix, contRows);
     rawMatrix[contRows-1] := TStringList.Create;
     rawMatrix[contRows-1].Assign(rawrow);
   until sep_idx = endidx;
   produceParsedMatrix;

   Result := TMatrix.create(contRows, fixedColumns, parsedMatrix);
end;



//MatrixToStr definition at NewtonGenMethod unit

//'[x^2 + 2*x + 1; x^5 + x*y + 7]'
function StrToStringList(s: String): TStringList;
const
  trash = '*';
var//idx is index
  idx, endidx, sep_idx: Integer;
  rawrow: String;

begin
   Result := TStringList.Create;
   idx  := Pos('[', s);
   endidx := Pos(']', s);
   repeat
     sep_idx := Pos(';', s);
     if sep_idx = 0 then sep_idx := endidx;
     s[sep_idx] := trash;

     rawrow := Copy(s, idx+1, sep_idx-idx-1);
     Result.Add(rawrow);
     idx := sep_idx;
   until sep_idx = endidx;
end;

function CompleteInputColor(s: String): String;
begin
  Result := Trim(s); //Remueve los espacios en blanco
  Result[1] := UpperCase(Result[1])[1];
  Result := 'cl' + Result;
end;

end.

