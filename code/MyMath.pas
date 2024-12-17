unit MyMath;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TNoiseSuppressor = class
  protected
    FAccumulation: Array of Single;
    FIndex: Integer;
    procedure SetCountLimit(limit: Integer);
    function GetCountLimit: Integer;
    function GetValue: Single;
  public
    constructor Create;
    procedure Update(const Value: Single);
    property CountLimit: Integer read GetCountLimit write SetCountLimit;
    property Value: Single read GetValue;
  end;

implementation

constructor TNoiseSuppressor.Create;
begin
  FIndex:= 0;
end;

procedure TNoiseSuppressor.Update(const Value: Single);
begin
  FAccumulation[FIndex]:= Value;
  FIndex:= FIndex + 1;

  if (FIndex >= Length(FAccumulation)) then
    FIndex:= 0;
end;

procedure TNoiseSuppressor.SetCountLimit(limit: Integer);
begin
  if (Length(FAccumulation) <> limit) then
    SetLength(FAccumulation, limit);
end;

function TNoiseSuppressor.GetCountLimit: Integer;
begin
  Result:= Length(FAccumulation);
end;

function TNoiseSuppressor.GetValue: Single;
var
  val: Single;
begin
  Result:= 0.0;

  for val in FAccumulation do
    Result:= Result + val;

  Result:= Result / Length(FAccumulation);
end;

end.

