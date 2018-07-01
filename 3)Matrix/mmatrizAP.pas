unit MMatriz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Dialogs;

type
    vect = array of real;

type
  RD = record
    T: boolean;
    D: real;
  end;

type
    TMatriz = class
      public
        x,y: integer;
        m: array of array of real;
        constructor Create(x_, y_: integer);
        destructor Destroy(); override;
        function Trans(): TMatriz;
        function Det(): RD;
        function Zero(s,i: integer): vect;
        function Cof(a: TMatriz; r,c: integer): real;
        function Inverse(a: real): TMatriz;
        function Invers(): TMatriz;
        function Adjunta(): TMatriz;
        function Esc(a: real): TMatriz;
        procedure Dlu(l,u: TMatriz);
        function Re(r: integer): boolean;
        procedure Print();
        function Se(l,u: TMatriz; s: array of real): vect;
        function Sol(l,u: TMatriz; s: array of real): vect;
        function division(a,b: TMatriz):TMatriz;
        function Traza(a: TMatriz):RD;
       // function triang(M1:TMatriz):Real;
    end;

Operator *(a,b: TMatriz): TMatriz;
Operator +(a,b: TMatriz): TMatriz;
Operator -(a,b: TMatriz): TMatriz;

implementation

constructor TMatriz.Create(x_,y_: integer);
var i,j: integer;
begin
     x:= x_;
     y:= y_;
     SetLength(m,x,y);
     for i:=0 to x-1 do
         for j:=0 to y-1 do
             m[i,j]:= 0;
end;

destructor TMatriz.Destroy();
begin
end;

function TMatriz.Trans(): TMatriz;
var i,j: integer;
begin
     Result:= TMatriz.Create(Self.y,Self.x);
     for i:=0 to Self.x-1 do
         for j:=0 to Self.y-1 do
             Result.m[j,i]:= Self.m[i,j];
end;

function TMatriz.Cof(a: TMatriz; r,c: integer): real;
var i,j,i1,j1: integer;
var MT: TMatriz;
begin
     MT:= TMatriz.Create(a.x-1,a.x-1);
     i1:=0; j1:=0;
     for i:=0 to a.x-1 do begin
         for j:=0 to a.x-1 do begin
             if (i<>r) and (j<>c) then begin
                 MT.m[i1,j1]:= a.m[i,j];
                 j1:= j1+1;
                 if (j1>=(a.x-1)) then begin
                     i1:=i1+1;
                     j1:=0;
                 end;
             end;
         end;
     end;
     Result:= (Power(-1,r+c))*(MT.Det().D);
     MT.Destroy();
end;

procedure TMatriz.Print();
var i,j: integer;
begin
     for i:=0 to Self.x-1 do begin
         for j:=0 to Self.y-1 do begin
             Write(FloatToStr(Self.m[i,j])+' ');
         end;
         Write(LineEnding);
     end;
end;

function TMatriz.Det(): RD;      //triangulariacion
var i: integer;
begin
     Result.T:= True;
     if Self.x <> Self.y then
        Result.T:= False
     else if Self.x = 1 then
        Result.D:= Self.m[0,0]
     else if Self.x = 2 then
        Result.D:= (Self.m[0,0]*Self.m[1,1])-(Self.m[0,1]*Self.m[1,0])
     else begin
        for i:=0 to Self.x-1 do
            Result.D:= Result.D+(Self.m[0,i]*Self.Cof(Self,0,i));
     end;
end;

function TMatriz.Re(r: integer): boolean;
var i,j: integer;
begin
     Result:= False;
     for i:=0 to Self.x-1 do begin
         if(Self.m[i,r]<>0) then begin
            for j:=0 to Self.y-1 do
                Self.m[r,j]:= Self.m[r,j] + Self.m[i,j];
            Result:= True;
            Break;
         end;
     end;
end;

procedure TMatriz.Dlu(l,u: TMatriz);
var
   i,j,k: integer;
   s: real;
begin
     WriteLn(LineEnding);
     for i:=0 to Self.x-1 do begin
         u.m[i,i] := 1;
         for j:=i to Self.x-1 do begin
             s:= 0;
             for k:=0 to i-1 do
                 s:= s+(l.m[j,k]*u.m[k,i]);
             l.m[j,i] := Self.m[j,i]-s;
         end;

         for j:=i+1 to Self.x-1 do begin
             s:= 0;
             for k:=0 to i-1 do
                 s:= s+(l.m[i,k]*u.m[k,j]);
             WriteLn('lbe');
             l.Print();
             {if(l.m[i,i] = 0) then begin
                 WriteLn('laf');
                 l.Re(i);
                 l.Print();
             end;}
             u.m[i,j] := ((Self.m[i,j]-s)/l.m[i,i]);
         end;
     end;

end;

function TMatriz.Sol(l,u: TMatriz; s: array of real): vect;
var
   z: array of real;
   su: real;
   i,j: integer;
begin
     SetLength(Result, l.x);
     SetLength(z, l.x);
     Result[0]:= 0; z[0]:= 0;
     for i:=0 to Self.x-1 do begin
         su:= 0;
         for j:=0 to Self.x-1 do begin
             su := su + (l.m[i,j]*z[j]);
         end;
         z[i] := (s[i]-su)/l.m[i,i];
     end;

     for i:=Self.x-1 downto 0 do begin
         su:= 0;
         for j:=Self.x-1 downto i+1 do
             su := su +(u.m[i,j]*Result[j]);
         Result[i] := ((z[i]-su)/u.m[i,i]);
     end;
end;

function TMatriz.Se(l,u: TMatriz; s: array of real): vect;
begin
     Result := Self.Sol(l,u,s);
end;

function TMatriz.Zero(s,i: integer): vect;
var j: integer;
begin
     SetLength(Result,s);
     for j:=0 to s-1 do
         Result[j] := 0;
     Result[i] := 1;
end;

{LU .. falta sumar las filas para que [i][i]!=0}
function TMatriz.Invers(): TMatriz;
var
   L,U: TMatriz;
   s,r: array of real;
   i,j,sv: integer;
begin
     sv:= Self.x;
     SetLength(s,Self.x); SetLength(r,Self.x);
     L := TMatriz.Create(Self.x,Self.y);
     U := TMatriz.Create(Self.x,Self.y);
     Result:= TMatriz.Create(Self.x,Self.y);

     Self.Dlu(L,U);
     for i:=0 to Self.x-1 do begin
         s := Self.Zero(sv,i);
         r := Self.Sol(L,U,s);
         for j:=0 to Self.x-1 do
             Result.m[j,i] := r[j];
     end;

     L.Destroy();
     U.Destroy();
end;

{Cofactores}
function TMatriz.Inverse(a: real): TMatriz;
var i,j: integer;
begin
     Result:= TMatriz.Create(Self.x,Self.y);
     for i:=0 to Result.x-1 do
         for j:=0 to Result.y-1 do
             Result.m[i,j]:= Self.Cof(Self,i,j)/a;

     Result:= Result.Trans();
end;

function TMatriz.Esc(a: real): TMatriz;
var i,j: integer;
begin
     Result:= TMatriz.Create(Self.x,Self.y);
     for i:=0 to Result.x-1 do
         for j:=0 to Result.y-1 do
             Result.m[i,j]:= Self.m[i,j]*a;
end;

function TMatriz.Adjunta(): TMatriz;
var i,j: integer;
begin
     Result:= TMatriz.Create(Self.x,Self.y);
     for i:=0 to Result.x-1 do
         for j:=0 to Result.y-1 do
             Result.m[i,j]:= Self.Cof(Self,i,j);
end;

Operator +(a,b: TMatriz): TMatriz;
var i,j: integer;
begin
  if (a.x = b.x) and (a.y = b.y) then
  begin
    Result:= TMatriz.Create(a.x,a.y);
    for i:=0 to a.x-1 do
        for j:=0 to a.y-1 do
            Result.m[i,j]:= a.m[i,j] + b.m[i,j]
  end
  else
  begin
      Result:= TMatriz.Create(0,0);
  end;
end;

Operator -(a,b: TMatriz): TMatriz;
var i,j: integer;
begin
  if (a.x = b.x) and (a.y = b.y) then
  begin
    Result:= TMatriz.Create(a.x,a.y);
    for i:=0 to a.x-1 do
        for j:=0 to a.y-1 do
            Result.m[i,j]:= a.m[i,j] - b.m[i,j]
  end
  else
  begin
      WriteLn('Order of Matrix are Different');
      Result:= TMatriz.Create(0,0);
  end;
end;

Operator *(a,b: TMatriz): TMatriz;
var i,j,k: integer;
begin
  if a.y = b.x then
  begin
      Result:= TMatriz.Create(a.x,b.y);
      for i:=0 to Result.x-1 do
          for j:=0 to Result.y-1 do
              for k:=0 to a.y-1 do
                  Result.m[i,j]:= Result.m[i,j]+(a.m[i,k]*b.m[k,j]);
  end
  else
  begin
      ShowMessage('Order of Matrix are Different');
      Result:= TMatriz.Create(0,0);
  end;
end;

function TMatriz.division(a,b: TMatriz):TMatriz;
 var inv_b:TMatriz; det_b:RD;
begin
     det_b := b.Det();
     inv_b := b.Inverse(det_b.D);
     Result:=TMatriz.Create(a.x,inv_b.y);
     Result:= a * inv_b;


end;
function TMatriz.Traza(a: TMatriz):RD;
  var
  i,j : integer;
  C:real;
  begin
  for i:=0 to a.y-1 do
     begin
       C:=a.m[i,i]+C;//col
     end;
  result.D:=C;
 end;
{
function TMatriz.triang(M1:TMatriz):Real;
var i,j:Integer;
  upper,lower:TMatriz;
begin
  //Evalua que sea una matriz cuadrada
  if(length(M1.m)<>length(M1.m[0])) then
  exit;
  //Caso de que sea 2x2
  if(length(M1.m)=2) then
  Result:=M1.m[0][0]*M1.m[1][1]-M1.m[0][1]*M1.m[1][0];
  //Triangularizacion
  //Set de tamanios Upper y Lower
  setLength(Upper,length(M1.m),length(M1.m[0]));
  setLength(Lower,length(M1.m),length(M1.m[0]));
  //Lower y Upper
  for i:=0 to length(M1.m)-1 do
    for j:=0 to length(M1.m)-1 do
    begin
        if(i<=j) then
        begin
          upper.m[i][j]:=M1.m[i][j];
          lower.m[i][j]:=0;
        end
        else
        begin
          upper.m[i][j]:=0;
          lower.m[i][j]:=M1.m[i][j];
        end;
        if(i=j) then
        lower.m[i][j]:=1;
    end;
  //Se halla el determinante

  Result:=1;
  for i:=0 to length(m1.m)-1 do
      //det(M1)=det(upper)*det(lower)
      Result:=Result*upper.m[i][i];
end;
  }
end.

