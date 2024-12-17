unit MyMath;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TNoiseSuppressor = class
  protected
    FValue: Single;
    FAccumulation: Array of Single;
    FCount: Integer;
  public
    constructor Create;
    procedure Update(const Value: Single; const CountLimit: Integer);
    property Value: Single read FValue;
  end;

implementation

constructor TNoiseSuppressor.Create;
begin
  FValue:= 0.0;
  FCount:= 0;
end;

procedure TNoiseSuppressor.Update(const Value: Single;
                                  const CountLimit: Integer);
var
  val, summ: Single;
begin
  if (Length(FAccumulation) <> CountLimit) then
    SetLength(FAccumulation, CountLimit);

  FAccumulation[FCount]:= Value;
  FCount:= FCount + 1;

  if (FCount >= CountLimit) then
  begin
    FCount:= 0;
    summ:= 0.0;

    for val in FAccumulation do
      summ:= summ + val;

    FValue:= summ;
  end;
end;

end.

