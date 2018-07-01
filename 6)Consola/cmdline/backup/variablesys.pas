unit variableSys;

{$mode objfpc}

interface

uses
  Classes, SysUtils, SuperExpressionParser, RegExpr;

const
   NUMCOLS = 3;
   COLVARNAME = 1;
   COLVARVALUE = 2;
   COLVARTYPE = 3;

type

  TVariableSys = class
  public
    constructor Create();
    //operations
    function  Insert(varname, varvalue: String): Boolean;
    function  find  (varname: String; var idx: Integer): Boolean;
    procedure ModifyValue(varname, varvalue: String);
    procedure reset_and_update_superexprparser();
    //gets
    function getPossibleVarValue(varname: String; var varvalue: String): Boolean;
    function getGlobalVarValue(varname: String): String;
    function getColVarName(): TStringList;
    function getColVarValue(): TStringList;
    function getColVarType(): TStringList;
    function getNumValues(): Integer;

  private
    table: array[1..NUMCOLS] of TStringList;
    n: Integer;
  end;

  function getType(s: String): String;

implementation
uses wnmainform;

constructor TVariableSys.Create();
var
  i: Integer;
begin
    for i:=1 to NUMCOLS do
      table[i] := TStringList.Create;
end;

//operations


function TVariableSys.Insert(varname, varvalue: String): Boolean;
var
  idx: Integer;
  tp: String;
begin
    if find(varname, idx) then begin
       ModifyValue(varname, varvalue);
       Result := False;
       exit;
    end;
    table[COLVARNAME] .Add(varname);
    table[COLVARVALUE].Add(varvalue);
    tp := getType(varvalue);
    table[COLVARTYPE] .Add(tp);
    n := n + 1;
    reset_and_update_superexprparser;
end;

function TVariableSys.find(varname: String; var idx: Integer): Boolean;
var i: Integer;
begin
    for i:=0 to n-1 do
      if(table[COLVARNAME][i] = varname ) then begin
         idx := i;
         Result := True;
         exit;
      end;
    Result := False;
end;

procedure TVariableSys.ModifyValue(varname, varvalue: String);
var
  idx: Integer;
  tp: String;

begin
    if not find(varname, idx) then exit;
    table[COLVARVALUE][idx] := varvalue;
    tp := getType(varvalue);
    table[COLVARTYPE][idx]  := tp;
    reset_and_update_superexprparser;
end;

procedure TVariableSys.reset_and_update_superexprparser();
var i:Integer; varname, value, tp:String ;
begin
    WMainForm.spexprparser := TSuperExpressionParser.Create();
    for i:=0 to n-1 do begin
      varname := table[COLVARNAME] [i];
      value   := table[COLVARVALUE][i];
      tp      := table[COLVARTYPE] [i];
      if (tp = 'Integer') or (tp = 'Double') then
      WMainForm.spexprparser.AddFloatVariable(varname,StrToFloat(value))
      else if (tp = 'String') then
      WMainForm.spexprparser.AddString(varname, value);
    end;

end;

//gets
function TVariableSys.getPossibleVarValue(varname: String; var varvalue: String): Boolean;
var i: Integer;
begin
    if not find(varname, i) then begin
       Result := False;
       exit;
    end;
    varvalue := table[COLVARVALUE][i];
end;

function TVariableSys.getGlobalVarValue(varname: String): String;
var i: Integer;
begin
    if not find(varname, i) then begin
       raise Exception.Create('No existe la variable global que deberia existir!: ''' + varname + '''');
       Result := 'undefined';
       exit;
    end;
    Result := table[COLVARVALUE][i];
end;

function TVariableSys.getColVarName(): TStringList;
begin
    Result := table[COLVARNAME];
end;

function TVariableSys.getColVarValue(): TStringList;
begin
    Result := table[COLVARVALUE];
end;

function TVariableSys.getColVarType(): TStringList;
begin
    Result := table[COLVARTYPE];
end;


function TVariableSys.getNumValues(): Integer;
begin
    Result := n;
end;

function getType(s: String): String;
var
  ToInteger: Integer;
  ToDouble: Double;
  ErrCode: Integer;
  isGoodCast: Boolean;
  ToFindsquarebracket: TRegExpr; //De alguna manera no funciona con if (Pos(s, '[')<>0) and (Pos(s, ']')<>0) then
const
   SquareBracketPattern = '\[.*\]';
begin
    val(s, ToInteger, ErrCode);
    IsGoodCast := ErrCode = 0;
    if IsGoodCast then Result := 'Integer'
    else begin
       val(s, ToDouble, ErrCode);
       IsGoodCast := ErrCode = 0;
       if IsGoodCast then Result := 'Double'
       else begin
          ToFindsquarebracket := TRegExpr.Create(SquareBracketPattern);
          if ToFindsquarebracket.Exec(s) then
             Result := 'Matrix'
          else
             Result := 'String';
       end;
    end;
end;

end.

