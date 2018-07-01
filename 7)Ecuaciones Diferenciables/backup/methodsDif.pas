unit MethodsDif;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParseMath;

type
  TMethodsDif = class
    //input
    X0,Y0: Double;
    n: Integer;
    h: Double;
    methodType: Integer;
    isReversed: Boolean; //When Xn < X0.
    //output
    table: array of TStringList; //Siempre contiene a Yn en la posicion 0. Puede contener otras columnas como IYn, k1 to kn, m, etc. Las cabeceras siempre estan en table[i][0] 0<=i<=size-1. Los valores numericos en cualquier celdas menos las de la cabecera
    num_cols_table: Integer;
  public
    constructor create();
    destructor destroy(); override; //Puede causar problemas
    procedure setExpression(expr: String);
    function Execute(): Double;
  private
    parse: TParseMath;
    function f(x,y: Double): Double; //f'(x,y), f se lee f prima
    function eulerMethod(): Double;
    function improvedEulerMethod(): Double;
    function rungeKuttaMethod(): Double;
    function dormandPriceMethod(): Double;
    procedure setTable(col: Integer);
    function op(x,y: Double): Double; inline; //op means operator
  end;

const
  Euler = 0;
  ImprovedEuler = 1;
  RungeKutta = 2;
  DormandPrice = 3;

implementation

constructor TMethodsDif.create();
begin
  parse := TParseMath.create;
  parse.AddVariable('x',0);
  parse.AddVariable('y',0);

end;

destructor TMethodsDif.destroy();
  procedure freeThisArray(arr: array of TStringList);
  var obj: TStringList;
  begin
    for obj in arr do begin
       obj.free;
    end;
    Finalize(arr);
  end;

begin
  parse.free;
  freeThisArray(table);
end;


function TMethodsDif.Execute(): Double;
begin
  case methodType of
    Euler:         Result := eulerMethod();
    ImprovedEuler: Result := improvedEulerMethod();
    RungeKutta:    Result := rungeKuttaMethod();
    DormandPrice:  Result := dormandPriceMethod();
  end;
end;

function TMethodsDif.eulerMethod(): Double;
const num_cols = 1; YnCol = 0; //'num_cols' significa 'numero_columnas'.
var
  Xn, Yn: Double;
  i: Integer;
  //Procedures
    procedure initTable();
    begin
      table[YnCol].Add('Yn');
      table[YnCol].Add(FloatToStr(Y0));
    end;

    procedure addRow(Yn: Double);
    begin
      table[YnCol].Add(FloatToStr(Yn));
    end;
  //End procedures
begin
  Xn := X0;
  Yn := Y0;
  setTable(num_cols);
  initTable;
  for i:=1 to n do begin
     Yn := op(Yn, h*f(Xn,Yn));
     Xn := op(Xn, h);
     addRow(Yn);
  end;
  Result := Yn;
end;

function TMethodsDif.improvedEulerMethod(): Double;
const num_cols = 2; IYnCol = 0; YnCol = 1;
  //Procedures
    procedure initTable();
    begin
      table[IYnCol].Add('IYn');
      table[YnCol].Add ('Yn');

      table[IYnCol].Add(FloatToStr(Y0));
      table[YnCol].Add(FloatToStr(Y0));
    end;

    procedure addRow(IYn, Yn: Double);
    begin
      table[IYnCol].Add(FloatToStr(IYn));
      table[YnCol] .Add(FloatToStr(Yn));
    end;

  //end procedures

var
  Xn, Yn, IYn: Double;
  i: Integer;
begin
  Xn := X0;
  Yn := Y0;
  IYn := Y0;
  setTable(num_cols);
  initTable;

  for i:=1 to n do begin
     Yn  := op(IYn, h*f(Xn,IYn));
     IYn := op(IYn, (h/2)*( f(Xn, IYn) + f(Xn+h, Yn) ));
     Xn := op(Xn, h);
     addRow(IYn, Yn);
  end;
  Result := IYn;
end;

function TMethodsDif.rungeKuttaMethod(): Double;
const
  num_cols = 6; YnCol = 0;
  K1Col = 1; K2Col = 2; K3Col = 3; K4Col = 4;
  mCol = 5;
  //Procedures
  procedure initTable();
  begin
    table[YnCol].Add('Yn');
    table[K1Col].Add('K1');
    table[K2Col].Add('K2');
    table[K3Col].Add('K3');
    table[K4Col].Add('K4');
    table[mCol].Add('m');

    table[YnCol].Add(FloatToStr(Y0));
    table[K1Col].Add('--');
    table[K2Col].Add('--');
    table[K3Col].Add('--');
    table[K4Col].Add('--');
    table[mCol].Add ('--');
  end;

  procedure addRow(Yn, K1, K2, K3, K4, m: Double);
  begin
    table[YnCol].Add(FloatToStr(Yn));
    table[K1Col].Add(FloatToStr(K1));
    table[K2Col].Add(FloatToStr(K2));
    table[K3Col].Add(FloatToStr(K3));
    table[K4Col].Add(FloatToStr(K4));
    table[mCol].Add(FloatToStr(m));
  end;

var
  Xn, Yn: Double;
  m, K1, K2, K3, K4: Double;
  i: Integer;
begin
  Xn := X0;
  Yn := Y0;
  setTable(num_cols);
  initTable;
  for i:=1 to n do begin
     K1 := f(Xn,Yn);
     K2 := f(Xn + h/2, Yn + (K1*h)/2);
     K3 := f(Xn + h/2, Yn + (K2*h)/2);
     K4 := f(Xn + h  , Yn +    K3*h );

     m := (1/6) * (K1 + 2*K2 + 2*K3 + K4);
     Yn := Yn + h*m;
     Xn := Xn + h;
     addRow(Yn,K1,K2,K3,K4,m);
  end;
  Result := Yn;
end;

function TMethodsDif.dormandPriceMethod(): Double;
const num_cols = 7; YnCol = 0;
  K1Col = 1; K2Col = 2; K3Col = 3; K4Col = 4; K5Col = 5; K6Col = 6;
//procedures
    procedure initTable();
    begin
      table[YnCol].Add('Yn');
      table[K1Col].Add('K1');
      table[K2Col].Add('K2');
      table[K3Col].Add('K3');
      table[K4Col].Add('K4');
      table[K5Col].Add('K5');
      table[K6Col].Add('K6');

      table[YnCol].Add(FloatToStr(Y0));
      table[K1Col].Add('--');
      table[K2Col].Add('--');
      table[K3Col].Add('--');
      table[K4Col].Add('--');
      table[K5Col].Add('--');
      table[K6Col].Add('--');
    end;

    procedure addRow(Yn, K1, K2, K3, K4, K5, K6: Double); inline;
    begin
       table[YnCol].Add(FloatToStr(Yn));
       table[K1Col].Add(FloatToStr(K1));
       table[K2Col].Add(FloatToStr(K2));
       table[K3Col].Add(FloatToStr(K3));
       table[K4Col].Add(FloatToStr(K4));
       table[K5Col].Add(FloatToStr(K5));
       table[K6Col].Add(FloatToStr(K6));
    end;
//end procedures

var
  Xn, Yn: Double;
  K1, K2, K3, K4, K5, K6: Double;
  i : Integer;
begin
  Xn := X0;
  Yn := Y0;
  setTable(num_cols);
  initTable;

  for i:=1 to n do begin
     K1 := h*f(Xn            , Yn);
     K2 := h*f(Xn + h/5      , Yn + K1/5);
     K3 := h*f(Xn + (3*h)/10 , Yn + (3*K1)/40 + (9*K2)/40);
     K4 := h*f(Xn + (4*h)/5  , Yn + (44*K1)/45- (56*K2)/15 + (32*K3)/9);
     K5 := h*f(Xn + (8*h)/9  , Yn + (19372*K1)/6561 - (25360*K2)/2187 + (64448*K3)/6561 - (212*K4)/729);
     K6 := h*f(Xn + h        , Yn + (9017*K1)/3168  - (355*K2)/33     + (46732*K3)/5247 + (49/176)*K4  - (5103*K5)/18656);
     Yn := Yn + ((35*K1)/384 +  (500*K3)/1113  + (125*K4)/192 - (2187*K5)/6784  + (11*K6)/84);
     Xn := Xn + h;
     addRow(Yn,K1,K2,K3,K4,K5,K6);
  end;
  Result := Yn;

end;

function TMethodsDif.f(x,y: Double): Double;
begin
  parse.NewValue('x', x);
  parse.NewValue('y', y);
  f := parse.Evaluate;
end;

procedure TMethodsDif.setExpression(expr: String);
begin
  parse.Expression := expr;
end;

procedure TMethodsDif.setTable(col: Integer);
var i: Integer;
begin
  SetLength(table, col);
  for i:=0 to col-1 do begin
     table[i] := TStringList.Create;
  end;

  num_cols_table := col;
end;

function TMethodsDif.op(x,y: Double): Double; inline;
begin
   if not isReversed then
      Result := x + y
   else
      Result := x - y;
end;

end.

