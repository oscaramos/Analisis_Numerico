program Graph;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, class_bolzano, MethodsOCMain, Table
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMethodsOCfrm, MethodsOCfrm);
  Application.CreateForm(TFormResults, FormResults);
  Application.Run;
end.

