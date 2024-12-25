unit NyaFadeEffect;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleUIControls, CastleColors, CastleGLImages, CastleTimeUtils;

type
  TNyaFadeEffect = class(TCastleUserInterface)
  protected
    FDuration: TFloatTime;
    FTime: TFloatTime;
    FIntensity: Single;
    FImage: TDrawableImage;
    FActive: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    procedure Render; override;
    procedure Fade(duration: TFloatTime = 0.5);
  published
  end;

implementation

uses
  CastleComponentSerialize, CastleImages, CastleVectors;

constructor TNyaFadeEffect.Create(AOwner: TComponent);
begin
  inherited;
  FActive:= False;
  FImage:= TDrawableImage.Create(nil, False, True { OwnsImage });
end;

destructor TNyaFadeEffect.Destroy;
begin
  FreeAndNil(FImage);
  inherited;
end;

procedure TNyaFadeEffect.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  if NOT FActive then Exit;

  FTime:= FTime + SecondsPassed;

  if (FTime <= FDuration) then
    FIntensity:= 1.0 - FTime / FDuration
  else
    FActive:= False;
end;

procedure TNyaFadeEffect.Render;
begin
  inherited;
  if NOT FActive then Exit;

  if Assigned(FImage) then
    FImage.Color:= Vector4(1.0, 1.0, 1.0, FIntensity);
    FImage.Draw(RenderRect);
end;


procedure TNyaFadeEffect.Fade(duration: TFloatTime = 0.5);
begin
  FDuration:= duration;
  FTime:= 0;
  FImage.Image:= Container.SaveScreen;
  FActive:= True;
end;

initialization
  RegisterSerializableComponent(TNyaFadeEffect, 'Fade Effect');
end.

