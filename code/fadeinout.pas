unit FadeInOut;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTimeUtils, CastleVectors, CastleControls;

type
  TRectangleFader = class
  public
    constructor Create(rectangle: TCastleRectangleControl);
    procedure SetFade(valueFrom, valueTo: Single; time: TFloatTime);
    procedure AnimateLineFade(SecondsPassed: TFloatTime);
    procedure AnimateQuadFade(SecondsPassed: TFloatTime);
  protected
    FRectangle: TCastleRectangleControl;
    FValueFrom: Single;
    FValueTo: Single;
    FFadeTime: TFloatTime;
    FColor: TVector3;
    AnimationTime: TFloatTime;
  end;

implementation

uses
  CastleUtils;

constructor TRectangleFader.Create(rectangle: TCastleRectangleControl);
begin
  FRectangle:= rectangle;
  FColor:= FRectangle.Color.XYZ;
  FRectangle.Exists:= False;
end;

procedure TRectangleFader.SetFade(valueFrom, valueTo: Single; time: TFloatTime);
begin
  FValueFrom:= valueFrom;
  FValueTo:= valueTo;
  FFadeTime:= time;
  AnimationTime:= 0;
  FRectangle.Color:= Vector4(FColor, valueFrom);
  FRectangle.Exists:= True;
end;

procedure TRectangleFader.AnimateLineFade(SecondsPassed: TFloatTime);
var
  koeff, value: Single;
begin
  if NOT FRectangle.Exists then Exit;

  AnimationTime:= AnimationTime + SecondsPassed;

  if (AnimationTime <= FFadeTime) then
  begin
    koeff:= AnimationTime / FFadeTime;
    value:= Lerp(koeff, FValueFrom, FValueTo);
    FRectangle.Color:= Vector4(FColor, value)
  end else
    FRectangle.Exists:= False;
end;

procedure TRectangleFader.AnimateQuadFade(SecondsPassed: TFloatTime);
var
  koeff, value: Single;
begin
  if NOT FRectangle.Exists then Exit;

  AnimationTime:= AnimationTime + SecondsPassed;

  if (AnimationTime <= FFadeTime) then
  begin
    koeff:= AnimationTime / FFadeTime;
    value:= Lerp(koeff * koeff, FValueFrom, FValueTo);
    FRectangle.Color:= Vector4(FColor, value)
  end else
    FRectangle.Exists:= False;
end;

end.

