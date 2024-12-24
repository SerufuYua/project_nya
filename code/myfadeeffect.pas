unit MyFadeEffect;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleUIControls, CastleColors, CastleGLImages, CastleTimeUtils;

type
  TMyFadeEffect = class(TCastleUserInterface)
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

constructor TMyFadeEffect.Create(AOwner: TComponent);
begin
  inherited;
  FActive:= False;
  FImage:= TDrawableImage.Create(nil, False, True { OwnsImage });
end;

destructor TMyFadeEffect.Destroy;
begin
  FreeAndNil(FImage);
  inherited;
end;

procedure TMyFadeEffect.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  if NOT FActive then Exit;

  FTime:= FTime + SecondsPassed;

  if (FTime <= FDuration) then
    FIntensity:= 1.0 - FTime / FDuration
  else
    FActive:= False;
end;

procedure TMyFadeEffect.Render;
begin
  inherited;
  if NOT FActive then Exit;

  if Assigned(FImage) then
    FImage.Color:= Vector4(1.0, 1.0, 1.0, FIntensity);
    FImage.Draw(RenderRect);
end;


procedure TMyFadeEffect.Fade(duration: TFloatTime = 0.5);
begin
  FDuration:= duration;
  FTime:= 0;
  FImage.Image:= Container.SaveScreen;
  FActive:= True;
end;

initialization
  RegisterSerializableComponent(TMyFadeEffect, 'Fade Effect');
end.

