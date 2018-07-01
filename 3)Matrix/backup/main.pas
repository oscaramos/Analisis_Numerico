program main;
uses matrix;

var
    mat1, matR: TMatrix;

begin
  mat1 := TMatrix.create(4,4);

  mat1.setElement(0,0,0);
  mat1.setElement(0,1,1);
  mat1.setElement(0,2,0);
  mat1.setElement(0,3,0);
  mat1.setElement(1,0,0);
  mat1.setElement(1,1,0);
  mat1.setElement(1,2,1);
  mat1.setElement(1,3,0);
  mat1.setElement(2,0,0);
  mat1.setElement(2,1,0);
  mat1.setElement(2,2,0);
  mat1.setElement(2,3,1);
  mat1.setElement(3,0,0);
  mat1.setElement(3,1,0);
  mat1.setElement(3,2,0);
  mat1.setElement(3,3,0);

  mat1.printContent();
  matR := mat1.power(3);
  matR.printContent();
  readln();
end.

