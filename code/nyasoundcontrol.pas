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
    procedure SetSound(value: TCastleSoundSource);
    procedure SetController(value: TCastleTransform);
  public
    const
      DefaultTreshold = 0.5;

    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property Sound: TCastleSoundSource read FSound write SetSound;
    property Controller: TCastleTransform read FController write SetController;
    property Treshold: Single read FTreshold write FTreshold
             {$ifdef FPC}default DefaultTreshold{$endif};
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
  for child in self.NonVisualComponentsEnumerate do
  begin
    if (child is TNyaSounder) then
      sounder:= child as TNyaSounder;

    if (Assigned(sounder.Controller) AND Assigned(sounder.Sound)) then
      sounder.Sound.SoundPlaying:= (sounder.Controller.Translation.Y > sounder.Treshold);
  end;

end;

{ ========= ------------------------------------------------------------------ }
{ TNyaSounder ---------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TNyaSounder.Create(AOwner: TComponent);
begin
  inherited;

  FTreshold:= DefaultTreshold;
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
       'Sound', 'Controller', 'Treshold'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaSoundMap, ['Nya Sound', 'Nya Sound Activator Map']);
  RegisterSerializableComponent(TNyaSounder, ['Nya Sound', 'Nya Sound Activator']);
end.

