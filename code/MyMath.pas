unit MyMath;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TNoiseSuppressor = class
  protected
    FValue: Single;
    FAccumulation: Single;
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
  FAccumulation:= 0.0;
  FCount:= 0;
end;

procedure TNoiseSuppressor.Update(const Value: Single;
                                  const CountLimit: Integer);
begin
  FCount:= FCount + 1;
  FAccumulation:= FAccumulation + Value;

  if (FCount >= CountLimit) then
  begin
    FValue:= FAccumulation / FCount;
    FAccumulation:= 0.0;
    FCount:= 0;
  end;
end;

end.

