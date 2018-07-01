unit matrix;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TMatrix = class
    private
      _matrix: array of array of Double;
      _rows, _columns: Integer;
    public
      constructor create(rows: Integer; columns: Integer);
      constructor create(e: TMatrix);
      procedure setElement(i:Integer; j:Integer; e: Double);
      function getElement(i:Integer; j:Integer): Double;
      function Add(e: TMatrix):TMatrix;
      function Sub(e: TMatrix):TMatrix;
      function MultMat(e: TMatrix):TMatrix;
      function MultEsc(x: Double): TMatrix;
      function power(x: Integer):TMatrix;

      function inverse(): TMatrix;
      function division(e: TMatrix): TMatrix;
      function det(): Double;

      function trans(): TMatrix;
      function trz(): Double;

      function cofactor(i: Integer;j: Integer): Double;
      function norm(): Double;

      function getNumRows(): Integer;
      function getNumColumns(): Integer;
      procedure printContent();
  end;


  function formatDouble(r: Double):String;

implementation
  function formatDouble(r: Double):String;
  begin
    Result :=  Format('%.3f ',[r]);
  end;

  constructor TMatrix.create(rows: Integer; columns: Integer);
  begin
      setLength(_matrix, rows, columns);
      _rows := rows;
      _columns := columns;
  end;

  constructor TMatrix.create(e: TMatrix);
  var
    i: Integer;
  begin
      setLength(_matrix, e._rows, e._columns);
      _rows := e._rows;
      _columns := e._columns;
      for i:=0 to _rows-1 do begin
        _matrix[i] := Copy(e._matrix[i],0,_columns);
      end;
  end;

  procedure TMatrix.setElement(i:Integer; j:Integer; e: Double);
  begin
      if (0 > i) or (i >= _rows) or (0 > j) or (j >= _columns) then begin
         raise Exception.create('TMatrix.setElement: Fila o columna no accesible');
         exit();
      end;
      _matrix[i,j] := e;
  end;

  function TMatrix.getElement(i:Integer; j:Integer): Double;
  begin
      if (0 > i) or (i >= _rows) or (0 > j) or (j >= _columns) then begin
         raise Exception.create('TMatrix.setElement: Fila o columna no accesible');
         exit();
      end;
      Result := _matrix[i,j];
  end;

  procedure TMatrix.printContent();
  var
    i,j: Integer;
  begin
    for i:= 0 to _rows-1 do begin
        for j:=0 to _columns-1 do
            write(formatDouble(_matrix[i,j]));
        writeln();
        writeln();
    end;
    writeln('------------------');
  end;

  function TMatrix.Add(e: TMatrix):TMatrix;
  var
    i,j: Integer;
  begin
    if(_rows <> e._rows) or (_columns <> e._columns) then begin
      raise Exception.create('TMatrix.Add: No es una matriz cuadrada');
      exit();
    end;
    Result:= TMatrix.create(_rows,_columns);
    for i:= 0 to _rows-1 do begin
        for j:=0 to _columns-1 do begin
            Result._matrix[i,j] := _matrix[i,j] + e._matrix[i,j];
        end;

    end;
  end;

  function TMatrix.Sub(e: TMatrix):TMatrix;
  var
    i,j: Integer;
  begin
    if(_rows <> e._rows) or (_columns <> e._columns) then begin
      raise Exception.create('TMatrix.Sub: No es una matriz cuadrada');
      exit();
    end;
    Result:= TMatrix.create(_rows,_columns);
    for i:= 0 to _rows-1 do begin
        for j:=0 to _columns-1 do begin
            Result._matrix[i,j] := _matrix[i,j] - e._matrix[i,j];
        end;

    end;
  end;

  function TMatrix.MultMat(e: TMatrix):TMatrix;
  var
    i,j,k: Integer;
    cont: Double;
  begin
    if _columns <> e._rows then begin
      raise Exception.create('TMatrix.MultMat: No es una matriz cuadrada');
      exit();
    end;

    Result:= TMatrix.create(_rows,e._columns);
    cont := 0;
    for i:= 0 to _rows-1 do begin
        for j:=0 to e._columns-1 do begin
            cont := 0;
            for k:=0 to _columns-1 do begin
                cont := cont + _matrix[i,k] * e._matrix[k,j];
            end;
            Result._matrix[i,j] := cont;
        end;

    end;
  end;

  function TMatrix.MultEsc(x: Double): TMatrix;
  var
    i,j: Integer;
  begin
    Result := TMatrix.create(self);
    for i:=0 to _rows-1 do
        for j:=0 to _columns-1 do
            Result._matrix[i,j] := x * _matrix[i,j];

  end;

  function TMatrix.inverse(): TMatrix;
  var
    i,j : Integer;
    d, cof : Double;
    MatCof: TMatrix;
  begin
    if _columns <> _rows then begin
       raise Exception.create('TMatrix.inverse: No es una matriz cuadrada');
       exit();
    end;
    d := self.det();
    if(d = 0) then begin
       raise Exception.create('TMatrix.inverse: La matriz no tiene inversa porque su determinante es cero');
       exit();
    end;

    Inverse := TMatrix.create(_rows,_columns);
    MatCof  := TMatrix.create(_rows,_columns);
    for i:=0 to _columns-1 do begin
        for j:=0 to _columns-1 do begin
           cof := self.cofactor(i,j);
           if (i+j) mod 2 <> 0 then cof := -cof;
           MatCof._matrix[i,j] := cof;
        end;
    end;
    Inverse := MatCof.trans().multEsc(1/self.det()) ;
  end;

  function TMatrix.division(e: TMatrix): TMatrix;
  begin
    Division := self.MultMat(e.inverse());
  end;

  function TMatrix.det(): Double;
  var
    i,c: Integer;
  begin
    if _columns <> _rows then begin
       raise Exception.create('TMatrix.det: No es una matriz cuadrada');
       exit();
    end;
    if _columns = 1 then begin
       Result := _matrix[0,0];
       exit();
    end;
    if _columns = 2 then begin(*Caso base*)
       Result := _matrix[0,0]*_matrix[1,1] - (_matrix[1,0] * _matrix[0,1]);
       exit();
    end;

    Result := 0;
    c := 1;
    for i:=0 to _columns-1 do begin
      Result := Result + c*_matrix[0,i]*self.cofactor(0,i);
      c := -1*c;
    end;


  end;

  function TMatrix.power(x: Integer):TMatrix;
  var
    i: Integer;
  begin
    if _columns <> _rows then begin
       raise Exception.create('TMatrix.Power: No es una matriz cuadrada');
       exit();
    end;
    if x < 1 then begin
       raise Exception.create('TMatrix.Power: La potencia es menor a 1');
       exit();
    end;
    Result := TMatrix.create(self);
    for i:=2 to x do begin
      Result := Result.multMat(self);
    end;
  end;

  function TMatrix.trans(): TMatrix;
  var
    i,j: Integer;
  begin
    Result := TMatrix.create(_columns,_rows);
    for i:=0 to _rows-1 do begin
        for j :=0 to _columns-1 do begin
          Result._matrix[j,i] := _matrix[i,j];
        end;
    end;

  end;

  function TMatrix.cofactor(i: Integer;j: Integer): Double;
  var
    r,r2,c,c2,n: Integer; //r,c recorre la matriz original, r2,c2 la submatriz.
    SubMat: TMatrix;
  begin
  if _columns <> _rows then begin
       raise Exception.create('TMatrix.cofactor: No es una matriz cuadrada');
       exit();
  end;
  SubMat := TMatrix.create(_rows-1,_columns-1);
  n := _rows;
  r := 0;
  r2 := 0;
  while r <= n-1 do begin
    if(r = i) then begin
       r := r + 1;
       if r = n then
          break;
    end;
    c := 0;
    c2 := 0;
    while c <= n-1 do begin
      if(c = j) then begin
         c := c + 1;
         if c = n then
            break;
      end;
      SubMat._matrix[r2,c2] := self._matrix[r,c];
      c2 := c2 + 1; c := c + 1;
    end;
    r2 := r2 + 1; r := r + 1;
  end;

  Cofactor := SubMat.det();
  end;

  function TMatrix.trz(): Double;
  var
    i: Integer;
  begin
    if _columns <> _rows then begin
       raise Exception.create('TMatrix.trz: No es una matriz cuadrada');
       exit();
    end;

    Result := 0;
    for i:=0 to _rows-1 do
        Result := Result + _matrix[i,i];
  end;

  function TMatrix.norm(): Double;
  var
    i,j : Integer;
    cont, e: Double;
  begin
    cont := 0;
    for i:=0 to _rows-1 do
        for j:=0 to _columns-1 do begin
            e := _matrix[i,j];
            cont := cont + e*e;
        end;
    Norm := sqrt(cont)

  end;

  function TMatrix.getNumRows(): Integer;
  begin
       Result := _rows;
  end;

  function TMatrix.getNumColumns(): Integer;
  begin
       Result := _Columns;
  end;

end.

