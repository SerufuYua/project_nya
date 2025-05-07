unit NyaCastleUiUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleControls;

procedure StepChangeSlider(slider: TCastleFloatSlider; step: Single);

implementation

procedure StepChangeSlider(slider: TCastleFloatSlider; step: Single);
var
  value: Single;
begin
  value:= slider.Value;
  value:= value + step;

  if (value < slider.Min) then value:= slider.Min
  else if (value > slider.Max) then value:= slider.Max;

  slider.Value:= value;
end;

end.

