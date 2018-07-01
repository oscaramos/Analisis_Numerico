unit SuperExpressionParser;

{$mode objfpc}

interface

uses
  Classes, SysUtils, math, fpexprpars;

type
  TSuperExpressionParser = Class

  Private
      FParser: TFPExpressionParser;
      identifier: array of TFPExprIdentifierDef;
      procedure AddFunctions();
  private
      (* Obsoleto, de esto se encarga la unidad PreSuperParser
      procedure ExprRaiz( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
      procedure ExprSenl( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
      procedure ExprLagrange( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
      procedure ExprIntegral( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
      procedure ExprArea( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
      procedure ExprEdo( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
      procedure ExprPlot2d( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
      procedure ExprFunc2d( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
      procedure ExprClearPlot( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
      procedure ExprIntersection( var Result: TFPExpressionResult; Const Args: TExprParameterArray);

      function getGlobalValue(gv: String): String;
      *)
  Public

      Expression: string;
      function NewFloatValue( Variable:string; Value: Double ): Double;
      function NewStringValue( Variable:string; Value: string ): Double;
      procedure AddFloatVariable( Variable: string; Value: Double );
      procedure AddString( Variable: string; Value: string );
      function Evaluate(  ): String;

      constructor create();
      destructor destroy;

  end;

implementation
uses wnmainform;



constructor TSuperExpressionParser.create;
begin
   FParser:= TFPExpressionParser.Create( nil );
   FParser.Builtins := [ bcMath ];
   AddFunctions();

end;

destructor TSuperExpressionParser.destroy;
begin
    FParser.Destroy;
end;

function TSuperExpressionParser.NewFloatValue( Variable: string; Value: Double ): Double;
begin
    FParser.IdentifierByName(Variable).AsFloat:= Value;
end;

function TSuperExpressionParser.NewStringValue( Variable: string; Value: string ): Double;
begin
    FParser.IdentifierByName(Variable).AsString:= Value;
end;

function TSuperExpressionParser.Evaluate(): String;
var
   r: String;

begin
     FParser.Expression:= Expression;
     r := FParser.Evaluate.ResString;
     if r = '' then
         Result := FloatToStr( ArgToFloat( FParser.Evaluate ) )
     else
         Result := r;
end;

function IsNumber(AValue: TExprFloat): Boolean;
begin
  result := not (IsNaN(AValue) or IsInfinite(AValue) or IsInfinite(-AValue));
end;



procedure ExprFloor(var Result: TFPExpressionResult; Const Args: TExprParameterArray); // maximo entero
var
  x: Double;
begin
   x := ArgToFloat( Args[ 0 ] );
   if x > 0 then
     Result.ResFloat:= trunc( x )

   else
     Result.ResFloat:= trunc( x ) - 1;

end;

Procedure ExprTan( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
   x := ArgToFloat( Args[ 0 ] );
   if IsNumber(x) and ((frac(x - 0.5) / pi) <> 0.0) then
      Result.resFloat := tan(x)

   else
     Result.resFloat := NaN;
end;

Procedure ExprSin( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
   x := ArgToFloat( Args[ 0 ] );
   Result.resFloat := sin(x)

end;

Procedure ExprCos( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
   x := ArgToFloat( Args[ 0 ] );
   Result.resFloat := cos(x)

end;

Procedure ExprLn( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
    x := ArgToFloat( Args[ 0 ] );
   if IsNumber(x) and (x > 0) then
      Result.resFloat := ln(x)

   else
     Result.resFloat := NaN;

end;

Procedure ExprLog( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
    x := ArgToFloat( Args[ 0 ] );
   if IsNumber(x) and (x > 0) then
      Result.resFloat := ln(x) / ln(10)

   else
     Result.resFloat := NaN;

end;

Procedure ExprSQRT( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
    x := ArgToFloat( Args[ 0 ] );
   if IsNumber(x) and (x > 0) then
      Result.resFloat := sqrt(x)

   else
     Result.resFloat := NaN;

end;

Procedure ExprPower( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x,y: Double;
begin
   x := ArgToFloat( Args[ 0 ] );
   y := ArgToFloat( Args[ 1 ] );


   Result.resFloat := power(x,y);

end;
(* OBSOLETO
//example: raiz('cos(x)', 4, 4)
//example: raiz('cos(x)', 3, 5, 1, 0.001)
//raiz(f, a, b, Method Type = 4(secante), error = 0.0001)
//Return Double
procedure TSuperExpressionParser.ExprRaiz( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
   f, a, b, mt, error: String;
   list_args: TStringList;
begin
   f := Args[0].ResString;
   a := FloatToStr( ArgToFloat(Args[1]) );
   b := FloatToStr( ArgToFloat(Args[2]) );
   mt := getGlobalValue('raiz_mt');
   error := getGlobalValue('error');
   list_args := TStringList.Create;
   list_args.Add(f);
   list_args.Add(a);
   list_args.Add(b);
   list_args.Add(mt);
   list_args.Add(error);

   Result.ResFloat := StrToFloat(adaptorsys.evaluate(fn_raiz, list_args));
end;

//ex: senl([x^2 + y^2 + 5; x^2 - y^2 - 1], [2;1])
//senl([f1, f2, ..., fm], [a0, b0, ..., vn])
//Return Matrix
procedure TSuperExpressionParser.ExprSenl( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
   func, init: String;
   list_args: TStringList;
begin
   func := Args[0].ResString;
   init := Args[1].ResString;

   list_args := TStringList.Create;
   list_args.Add(func);
   list_args.Add(init);
   Result.ResString := adaptorsys.evaluate(fn_senl, list_args);
end;

//ex: lagrange('[0 1 3 5; 0 1 9 25]')
//lagrange(Puntos...)
//lagrange([x1 x2 ... xn; y1 y2 ... yn])
//Return String
procedure TSuperExpressionParser.ExprLagrange( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
   list_args: TStringList;
   points: String;
begin
   points := Args[0].ResString;

   list_args := TStringList.Create;
   list_args.Add(points);
   Result.ResString := adaptorsys.evaluate(fn_lagrange, list_args);

end;

//integral(f; a; b; method = Simpson13)
//Return Double
procedure TSuperExpressionParser.ExprIntegral( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
   list_args: TStringList;
   f, a, b, mt:String;
begin
   f := Args[0].ResString;
   a := FloatToStr(ArgToFloat(Args[1]));
   b := FloatToStr(ArgToFloat(Args[2]));
   mt := getGlobalValue('integral_mt');

   list_args := TStringList.Create;
   list_args.Add(f);
   list_args.Add(a);
   list_args.Add(b);
   list_args.Add(mt);
   Result.ResString := adaptorsys.evaluate(fn_integral, list_args);

end;

//area(f; a; b; method = Simpson13)
//Return Double
procedure TSuperExpressionParser.ExprArea( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
   list_args: TStringList;
   f, a, b, mt:String;
begin
   f := Args[0].ResString;
   a := FloatToStr(ArgToFloat(Args[1]));
   b := FloatToStr(ArgToFloat(Args[2]));
   mt := getGlobalValue('integral_mt');

   list_args := TStringList.Create;
   list_args.Add(f);
   list_args.Add(a);
   list_args.Add(b);
   list_args.Add(mt);
   Result.ResString := adaptorsys.evaluate(fn_area, list_args);

end;

//edo(df; X0; Y0; Xn; method = Dormand Price; h = 0.0001)
//Return Double
procedure TSuperExpressionParser.ExprEdo( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
   list_args: TStringList;
   f, X0, Y0, Xn, mt, h:String;
begin
   f := Args[0].ResString;
   X0 := FloatToStr(ArgToFloat(Args[1]));
   Y0 := FloatToStr(ArgToFloat(Args[2]));
   Xn := FloatToStr(ArgToFloat(Args[3]));
   mt := getGlobalValue('edo_mt');
   h := getGlobalValue('h');

   list_args := TStringList.Create;
   list_args.Add(f);
   list_args.Add(X0);
   list_args.Add(Y0);
   list_args.Add(Xn);
   list_args.Add(mt);
   list_args.Add(h);

   Result.ResString := adaptorsys.evaluate(fn_edo, list_args);

end;

//plot2d(f, a, b, color = black)
//Return 'void'
procedure TSuperExpressionParser.ExprPlot2d( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
   list_args: TStringList;
   f, a, b, color: String;
begin
   f := Args[0].ResString;
   a := FloatToStr(ArgToFloat(Args[1]));
   b := FloatToStr(ArgToFloat(Args[2]));
   color := getGlobalValue('color');

   list_args := TStringList.Create;
   list_args.Add(f);
   list_args.Add(a);
   list_args.Add(b);
   list_args.Add(color);
   Result.ResString := adaptorsys.evaluate(fn_plot2d, list_args);

end;

//func2d(f, color = black)
//Return 'void'
procedure TSuperExpressionParser.ExprFunc2d( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
   list_args: TStringList;
   f, color: String;
begin
   f := Args[0].ResString;
   color := getGlobalValue('color');

   list_args := TStringList.Create;
   list_args.Add(f);
   list_args.Add(color);
   Result.ResString := adaptorsys.evaluate(fn_func2d, list_args);

end;

//clearplot
//Return 'void'
procedure TSuperExpressionParser.ExprClearPlot( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
   list_args: TStringList;
begin
   list_args := TStringList.Create;
   Result.ResString := adaptorsys.evaluate(fn_clearplot, list_args);
end;

//intersection(f1, f2, a, b)
procedure TSuperExpressionParser.ExprIntersection( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
   list_args: TStringList;
   f1, f2, a, b: String;

begin
   f1 := Args[0].ResString;
   f2 := Args[1].ResString;
   a := FloatToStr(ArgToFloat(Args[2]));
   b := FloatToStr(ArgToFloat(Args[3]));

   list_args := TStringList.Create;
   list_args.Add(f1);
   list_args.Add(f2);
   list_args.Add(a);
   list_args.Add(b);
   Result.ResString := adaptorsys.evaluate(fn_intersection, list_args);
end;

function TSuperExpressionParser.getGlobalValue(gv: String): String;
begin
   Result := wmainform.sys_variable.getGlobalVarValue(gv);
end;
*)

Procedure TSuperExpressionParser.AddFunctions();
begin
   with FParser.Identifiers do begin
       AddFunction('tan', 'F', 'F', @ExprTan);
       AddFunction('sin', 'F', 'F', @ExprSin);
       AddFunction('sen', 'F', 'F', @ExprSin);
       AddFunction('cos', 'F', 'F', @ExprCos);
       AddFunction('ln', 'F', 'F', @ExprLn);
       AddFunction('log', 'F', 'F', @ExprLog);
       AddFunction('sqrt', 'F', 'F', @ExprSQRT);
       AddFunction('floor', 'F', 'F', @ExprFloor );
       AddFunction('power', 'F', 'FF', @ExprPower); //two float arguments 'FF' , returns float
   end;
   (* Esta por quedar obsoleto
   with FParser.Identifiers do begin
       AddFunction(fn_raiz, 'F', 'SFF', @ExprRaiz);
       AddFunction(fn_senl, 'S', 'SS', @ExprSenl);
       AddFunction(fn_lagrange, 'S', 'S', @ExprLagrange);
       AddFunction(fn_integral, 'F', 'SFF', @ExprIntegral);
       AddFunction(fn_area, 'F', 'SFF', @ExprArea);
       AddFunction(fn_edo, 'F', 'SFFF', @ExprEdo);

       AddFunction(fn_plot2d, 'S', 'SFF', @ExprPlot2d);
       AddFunction(fn_func2d, 'S', 'S', @ExprFunc2d);
       AddFunction(fn_clearplot, 'S', '', @ExprClearPlot);
       AddFunction(fn_intersection, 'S', 'SSFF', @ExprIntersection);

   end;
   *)
end;



procedure TSuperExpressionParser.AddFloatVariable( Variable: string; Value: Double );
var Len: Integer;
begin
   Len:= length( identifier ) + 1;
   SetLength( identifier, Len ) ;
   identifier[ Len - 1 ]:= FParser.Identifiers.AddFloatVariable( Variable, Value);

end;

procedure TSuperExpressionParser.AddString( Variable: string; Value: string );
var Len: Integer;
begin
   Len:= length( identifier ) + 1;
   SetLength( identifier, Len ) ;

   identifier[ Len - 1 ]:= FParser.Identifiers.AddStringVariable( Variable, Value);
end;

end.

