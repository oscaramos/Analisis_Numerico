program CmdLineExample;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg
  { add your units here }, wnmainform, cmdbox, adaptors,
  variableSys, superExpressionParser, PreSuperParser, SEDO;

begin
  Application.Initialize;
  Application.CreateForm(TWMainForm, WMainForm);
  Application.Run;
end.

