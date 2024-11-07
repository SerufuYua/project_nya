unit MyClassUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  generic TDynamic<T: TObject> = class
  public
    class function Cast(objectOfCalss: TObject): T; static;
  end;

implementation

class function TDynamic.Cast(objectOfCalss: TObject): T;
begin
  if (objectOfCalss is T) then
    Result:= objectOfCalss as T
  else
    Result:= nil;
end;

end.

