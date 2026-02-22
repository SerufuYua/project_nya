unit NyaSoundControl;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleBehaviors, CastleTransform, CastleClassUtils;

type
  TNyaSoundMap = class(TCastleBehavior)
  public
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

  TNyaSounder = class(TCastleComponent)
  protected
    FSound: TCastleSoundSource;
    FController: TCastleTransform;
    FTreshold: Single;
    FUseVolume: Boolean;
    procedure SetSound(value: TCastleSoundSource);
    procedure SetController(value: TCastleTransform);
  public
    const
      DefaultTreshold = 0.5;
      DefaultUseVolume = False;

    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property Sound: TCastleSoundSource read FSound write SetSound;
    property Controller: TCastleTransform read FController write SetController;
    property Treshold: Single read FTreshold write FTreshold
             {$ifdef FPC}default DefaultTreshold{$endif};
    property UseVolume: Boolean read FUseVolume write FUseVolume
             {$ifdef FPC}default DefaultUseVolume{$endif};
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils;

{ ========= ------------------------------------------------------------------ }
{ TNyaSoundMap --------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

procedure TNyaSoundMap.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  child: TComponent;
  sounder: TNyaSounder;
begin
  inherited;

  for child in self.NonVisualComponentsEnumerate do
  begin
    if (child is TNyaSounder) then
      sounder:= child as TNyaSounder;

    if (Assigned(sounder.Controller) AND Assigned(sounder.Sound)) then
    begin
      if (sounder.Controller.Translation.Y > sounder.Treshold) then
      begin
        sounder.Sound.SoundPlaying:= True;
        if sounder.UseVolume then
          sounder.Sound.Volume:= sounder.Controller.Translation.Y;
      end
      else
      begin
        sounder.Sound.SoundPlaying:= False;
      end;
    end;
  end;

end;

{ ========= ------------------------------------------------------------------ }
{ TNyaSounder ---------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TNyaSounder.Create(AOwner: TComponent);
begin
  inherited;

  FTreshold:= DefaultTreshold;
  FUseVolume:= DefaultUseVolume;
end;

procedure TNyaSounder.SetSound(value: TCastleSoundSource);
begin
  if (FSound <> value) then
    FSound:= value;
end;

procedure TNyaSounder.SetController(value: TCastleTransform);
begin
  if (FController <> value) then
    FController:= value;
end;

function TNyaSounder.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Sound', 'Controller', 'Treshold', 'UseVolume'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaSoundMap, ['Nya Sound', 'Nya Sound Activator Map']);
  RegisterSerializableComponent(TNyaSounder, ['Nya Sound', 'Nya Sound Activator']);
end.

