unit LogicInterpolation;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, Matrix;
type
    TPoint = class x,y: Real; end;

    TLagrange = class
      arrPoints : array of TPoint;
      cont_points: Integer; //Total points
    public
      constructor create(n: Integer);
      constructor create(mat: TMatrix);
      function getPolinomy(): String;
      function limInferior(): Double;
      function limSuperior(): Double;
    end;

implementation

constructor TLagrange.create(n: Integer);
var
  i: Integer;
begin
    self.cont_points := n;
    SetLength(arrPoints, n);
    for i:=0 to n-1 do
       arrPoints[i] := TPoint.create();

end;

constructor TLagrange.create(mat: TMatrix); //mat debe tener formato [x1 x2 ... xn; y1 y2 ... yn]
var i: Integer;
begin
    cont_points := mat.getNumColumns();
    SetLength(arrPoints, cont_points);
    for i:=0 to cont_points-1 do begin
      arrPoints[i]  := TPoint.create();
      arrPoints[i].x := mat.getElement(0, i);
      arrPoints[i].y := mat.getElement(1, i);
    end;
end;

function TLagrange.getPolinomy(): String;
var
  i,j : Integer;
  cf, num : Real; //coeficiente
  SubPol: TStringList;
  p: String;
begin
  Result := '';

  for i:=0 to cont_points-1 do begin
    SubPol := TStringList.create();
    SubPol.Delimiter:=' ';
    cf := 1;
    for j:=0 to cont_points-1 do begin
      if j = i then continue;
      num := arrPoints[j].x;
      if num > 0 then
        p := '(x-'+FloatToStr(num)+')'
      else if num = 0 then
        p := '(x)'
      else
        p := '(x+'+FloatToStr(abs(num))+')';
      if (j <> cont_points-1) then
        if not ((i = cont_points-1) and (j  = cont_points-2)) then
           p := p + '*';
      SubPol.add(p);
      cf := cf * (arrPoints[i].x - arrPoints[j].x);
    end;
    Result := Result + '(' +FloatToStr(arrPoints[i].y) + '/' + FloatToStr(cf) + ')*'
    + SubPol.DelimitedText;
    if i <> cont_points-1 then
      Result := Result + ' + ';
  end;
end;

function TLagrange.limInferior(): Double;
var
  i: Integer;
  min: Double;
begin
  min := arrPoints[0].x;
  for i:=1 to cont_points-1 do begin
    if(arrPoints[i].x < min) then
      min := arrPoints[i].x;
  end;
  Result := min;
end;

function TLagrange.limSuperior(): Double;
var
  i: Integer;
  max: Double;
begin
  max := arrPoints[0].x;
  for i:=1 to cont_points-1 do begin
    if(arrPoints[i].x > max) then
      max := arrPoints[i].x;
  end;
  Result := max;
end;

end.

