unit class_taylor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

type
  TTaylor = class
    ErrorAllowed: Real;
    Sequence,
    FunctionList: TstringList;
    FunctionType: Integer;
    AngleType: Integer;
    x: Real;
    function Execute(): Real;
    private
      Error,
      Angle: Real;
      function sen(): Real;
      function cos(): Real;
      function tan(): Real;
      function arcsen(): Real;
      function arccos(): Real;
      function arctan(): Real;
      function senh(): Real;
      function cosh(): Real;
      function tanh(): Real;
      function exp(): Double;


    public

      constructor create;
      destructor Destroy; override;

  end;

const
  IsSin = 0;
  IsCos = 1;
  IsTan = 2;
  IsArcSen = 3;
  IsArcCos = 4;
  IsArcTan = 5;
  IsSinh = 6;
  IsCosh = 7;
  IsTanh = 8;
  IsExp = 9;

  AngleSexagedecimal = 0;
  AngleRadian = 1;

implementation

const
  Top = 100;

constructor TTaylor.create;
begin
  Sequence:= TStringList.Create;
  FunctionList:= TStringList.Create;
  FunctionList.AddObject( 'sen', TObject( IsSin ) );
  FunctionList.AddObject( 'cos', TObject( IsCos ) );
  FunctionList.AddObject( 'tan', TObject( IsTan ) );
  FunctionList.AddObject( 'arcsen', TObject( IsArcSen ) );
  FunctionList.AddObject( 'arccos', TObject( IsArcCos ) );
  FunctionList.AddObject( 'arctan', TObject( IsArcTan ) );
  FunctionList.AddObject( 'sinh', TObject( IsSinh ) );
  FunctionList.AddObject( 'cosh', TObject( IsCosh ) );
  FunctionList.AddObject( 'tanh', TObject( IsTanh ) );
  FunctionList.AddObject( 'exp', TObject( IsExp ) );
  Sequence.Add('');
  Error:= Top;
  x:= 0;

end;

destructor TTaylor.Destroy;
begin
  Sequence.Destroy;
  FunctionList.Destroy;
end;

function Power( b: Real; n: Integer ): Extended;
var i: Integer;
begin
   Result:= 1;
   for i:= 1 to n do
      Result:= Result * b;

end;

function Factorial( n: Integer ): Extended;
begin

     if n > 1 then
        Result:= n * Factorial( n -1 )

     else if n >= 0 then
        Result:= 1

     else
        Result:= 0;
end;

function Combinatory(a: Integer; b: Integer): Extended;
begin
     Result := Trunc(Factorial(a)/(Factorial(a-b)*Factorial(b)));
end;

function Bernoulli(k: Integer): Extended;
var
    i: Integer;
begin
   Result := 0;
   if k = 0 then
        Result := 1
   else if k = 1 then
        Result := -1/2
   else if k mod 2 = 1 then
        Result := 0
   else
        begin
        for i:=0 to k-1 do
            Result := Result + Combinatory(k,i) * (Bernoulli(i)/(k + 1 - i));
        Result := - Result;
        end;
end;

function TTaylor.Execute( ): Real;
begin
   Result:= -1.0;
   case AngleType of
        AngleRadian: Angle:= x;
        AngleSexagedecimal: Angle:= x * pi/180;
   end;
   case FunctionType of
        IsSin: Result:= sen();
        IsCos: Result:= cos();
        IsTan: Result:= tan();
        IsArcSen: Result:= arcsen();
        IsArcCos: Result:= arccos();
        IsArcTan: Result:= arctan();
        IsSinh: Result:= senh();
        IsCosh: Result:= cosh();
        IsTanh: Result:= tanh();
        IsExp: Result:= exp();
   end;
   if AngleType = AngleSexagedecimal then
     case FunctionType of
          IsArcSen: Result:= Result * 180/pi;
          IsArcCos: Result:= Result * 180/pi;
          IsArcTan: Result:= Result * 180/pi;
     end


end;

function TTaylor.sen(): Real;
var xn: Real;
     n: Integer;
begin
   Result:= 0;
   n:= 0;

   repeat
     xn:= Result;

     Result:= Result + Power(-1, n)/Factorial( 2*n + 1 ) * Power(Angle, 2*n + 1);
     if n > 0 then
        Error:= abs( Result - xn );

     Sequence.Add( FloatToStr( Result ) );
     n:= n + 1;

   until ( Error <= ErrorAllowed ) or ( n >= Top ) ;

end;


function TTaylor.cos(): Real;
var xn: real;
    n: Integer;

begin
  Result:= 0;
  n:= 0;

  repeat
    xn:= Result;
    Result:= Result + Power( -1, n)/Factorial(2*n) * Power( Angle, 2*n );
    Sequence.Add( FloatToStr( Result ) );
    if n > 0 then
       Error:= abs( Result - xn );

    n:= n + 1;
  until ( Error < ErrorAllowed ) or ( n >= Top );

end;

function TTaylor.tan(): Real;
var xn: real;
    n: Integer;

begin
  if abs(Angle) >= pi/2 then
  begin
       Result := -1;
       ShowMessage('El valor ingresado no cumple con la condicion de entrada');
       exit();
  end;
  Result:= 0;
  n:= 1;

  repeat
    xn:= Result;
    Result:= Result + ((Bernoulli(2*n) * power(-4,n) * (1 - power(4,n))) / factorial(2*n)) * power(Angle,2*n-1);
    Sequence.Add( FloatToStr( Result ) );
    if n > 0 then
       Error:= abs( Result - xn );

    n:= n + 1;
  until ( Error < ErrorAllowed ) or ( n >= Top );

end;

function TTaylor.arcsen(): Real;
var xn:Real;
    n:Integer;
begin
    Result := 0;
    n := 0;
    if abs(x) > 1 then begin
       Result := -1;
       ShowMessage('El valor ingresado no cumple con la condicion de entrada');
       exit();
    end;
    repeat
      xn:= Result;
      Result:= Result + factorial(2*n)/(power(factorial(n),2)* power(4,n) * (2*n+1)) * power(x,2*n+1);
      Sequence.Add( FloatToStr( Result ) );
      if n > 0 then
         Error:= abs( Result - xn );
      n:= n + 1;
    until ( Error < ErrorAllowed ) or ( n >= Top );
end;

function TTaylor.arccos(): Real;
begin
    if abs(x) > 1 then begin
       Result := -1;
       ShowMessage('El valor ingresado no cumple con la condicion de entrada');
       exit();
    end;
    Result := pi/2 - arcsen();
end;

function TTaylor.arctan(): Real;
var xn:Real;
    n:Integer;
begin
    Result := 0;
    n := 0;
    if abs(x) > 1 then begin
       Result := -1;
       ShowMessage('El valor ingresado no cumple con la condicion de entrada');
       exit();
    end;
    repeat
      xn:= Result;
      Result:= Result + factorial(2*n)/(power(4,n) * power(factorial(n),2) * (2*n + 1)) * power(Angle,2*n+1);
      Sequence.Add( FloatToStr( Result ) );
      if n > 0 then
         Error:= abs( Result - xn );
      n:= n + 1;
    until ( Error < ErrorAllowed ) or ( n >= Top );
    Result := pi/2 - Result
end;

function TTaylor.senh(): Real;
var xn:Real;
    n:Integer;
begin
    Result := 0;
    n := 0;
    repeat
      xn:= Result;
      Result:= Result + power(Angle, 2*n+1)/factorial(2*n + 1);
      Sequence.Add( FloatToStr( Result ) );
      if n > 0 then
         Error:= abs( Result - xn );
      n:= n + 1;
    until ( Error < ErrorAllowed ) or ( n >= Top );
end;

function TTaylor.cosh(): Real;
var xn:Real;
    n:Integer;
begin
    Result := 0;
    n := 0;
    repeat
      xn:= Result;
      Result:= Result + power(Angle, 2*n)/factorial(2*n);
      Sequence.Add( FloatToStr( Result ) );
      if n > 0 then
         Error:= abs( Result - xn );
      n:= n + 1;
    until ( Error < ErrorAllowed ) or ( n >= Top );
end;

function TTaylor.tanh(): Real;
var xn:Real;
    n:Integer;
begin
    Result := 0;
    n := 0;
    if abs(Angle) > pi/2 then begin
       Result := -1;
       ShowMessage('El valor ingresado no cumple con la condicion de entrada');
       exit();
    end;
    repeat
      xn:= Result;
      Result:= Result + ((Bernoulli(2*n)
      *power(4,n)*(power(4,n) - 1))/Factorial(2*n)) * power(Angle,2*n - 1);
      Sequence.Add( FloatToStr( Result ) );
      if n > 0 then
         Error:= abs( Result - xn );
      n:= n + 1;
    until ( Error < ErrorAllowed ) or ( n >= Top );
end;


function TTaylor.exp(): Double;
var
    xn: Real;
    n: Integer;
begin
    Result:= 0;
    n := 0;
    repeat
        xn:= Result;
        Result := Result + power(x,n) / factorial(n);
        Sequence.Add( FloatToStr( Result ) );
        if n > 0 then
         Error:= abs( Result - xn );
        n:= n + 1;
    until ( Error < ErrorAllowed ) or ( n >= Top );
end;

end.

