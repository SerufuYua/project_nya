unit FadeInOut;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTimeUtils, CastleVectors, CastleControls,
  CastleImages;

type
  TRectangleFader = class
  public
    constructor Create(rectangle: TCastleRectangleControl);
    procedure Fade(valueFrom, valueTo: Single; time: TFloatTime);
    procedure AnimateLineFade(SecondsPassed: TFloatTime);
    procedure AnimateQuadFade(SecondsPassed: TFloatTime);
  protected
    FRectangle: TCastleRectangleControl;
    FValueFrom: Single;
    FValueTo: Single;
    FFadeTime: TFloatTime;
    FColor: TVector3;
    FAnimationTime: TFloatTime;
  end;

  TImageFader = class
  public
    constructor Create(rectangle: TCastleImageControl);
    procedure Fade(image: TEncodedImage; time: TFloatTime);
    procedure AnimateLineFade(SecondsPassed: TFloatTime);
    procedure AnimateQuadFade(SecondsPassed: TFloatTime);
  protected
    FRectangle: TCastleImageControl;
    FFadeTime: TFloatTime;
    FAnimationTime: TFloatTime;
  end;

implementation

uses
  CastleUtils;

{ TRectangleFader }

constructor TRectangleFader.Create(rectangle: TCastleRectangleControl);
begin
  FRectangle:= rectangle;
  FColor:= FRectangle.Color.XYZ;
  FRectangle.Exists:= False;
end;

procedure TRectangleFader.Fade(valueFrom, valueTo: Single; time: TFloatTime);
begin
  FValueFrom:= valueFrom;
  FValueTo:= valueTo;
  FFadeTime:= time;
  FAnimationTime:= 0;
  FRectangle.Color:= Vector4(FColor, valueFrom);
  FRectangle.Exists:= True;
end;

procedure TRectangleFader.AnimateLineFade(SecondsPassed: TFloatTime);
var
  koeff, value: Single;
begin
  if NOT FRectangle.Exists then Exit;

  FAnimationTime:= FAnimationTime + SecondsPassed;

  if (FAnimationTime <= FFadeTime) then
  begin
    koeff:= FAnimationTime / FFadeTime;
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

  FAnimationTime:= FAnimationTime + SecondsPassed;

  if (FAnimationTime <= FFadeTime) then
  begin
    koeff:= FAnimationTime / FFadeTime;
    value:= Lerp(koeff * koeff, FValueFrom, FValueTo);
    FRectangle.Color:= Vector4(FColor, value)
  end else
    FRectangle.Exists:= False;
end;

{ TImageFader }

constructor TImageFader.Create(rectangle: TCastleImageControl);
begin
  FRectangle:= rectangle;
  FRectangle.Exists:= False;
end;

procedure TImageFader.Fade(image: TEncodedImage; time: TFloatTime);
begin
  FFadeTime:= time;
  FAnimationTime:= 0;
  FRectangle.Image:= image;
  FRectangle.Exists:= True;
end;

procedure TImageFader.AnimateLineFade(SecondsPassed: TFloatTime);
var
  koeff: Single;
begin
  if NOT FRectangle.Exists then Exit;

  FAnimationTime:= FAnimationTime + SecondsPassed;

  if (FAnimationTime <= FFadeTime) then
  begin
    koeff:= FAnimationTime / FFadeTime;
    FRectangle.Content.Color:= Vector4(1.0, 1.0, 1.0, 1.0 - koeff)
  end else
    FRectangle.Exists:= False;
end;

procedure TImageFader.AnimateQuadFade(SecondsPassed: TFloatTime);
var
  koeff: Single;
begin
  if NOT FRectangle.Exists then Exit;

  FAnimationTime:= FAnimationTime + SecondsPassed;

  if (FAnimationTime <= FFadeTime) then
  begin
    koeff:= FAnimationTime / FFadeTime;
    FRectangle.Content.Color:= Vector4(1.0, 1.0, 1.0, 1.0 - koeff * koeff)
  end else
    FRectangle.Exists:= False;
end;

end.

