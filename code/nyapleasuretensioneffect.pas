unit NyaPleasureTensionEffect;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleControls, CastleClassUtils;

type
  TNyaPleasureTensionEffect = class(TCastleDesign)
  protected
    FDelta: Single;
    FTension: Single;
    FPleasure: Single;
    procedure SetTension(value: Single);
    procedure SetPleasure(value: Single);
  public
    const
      DefaultDelta = 0.01;
      DefaultTension = 1.0;
      DefaultPleasure = 1.0;

      constructor Create(AOwner: TComponent); override;
      function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property Delta: Single read FDelta write FDelta
           {$ifdef FPC}default DefaultDelta{$endif};
    property Tension: Single read FTension write SetTension
           {$ifdef FPC}default DefaultTension{$endif};
    property Pleasure: Single read FPleasure write SetPleasure
           {$ifdef FPC}default DefaultPleasure{$endif};
end;

implementation

uses
  CastleComponentSerialize, CastleColors, CastleVectors, CastleUtils,
  CastleParticleEmitter;

constructor TNyaPleasureTensionEffect.Create(AOwner: TComponent);
begin
  inherited;

  FDelta:= DefaultDelta;
  FTension:= DefaultTension;
  FPleasure:= DefaultPleasure;
end;

procedure TNyaPleasureTensionEffect.SetTension(value: Single);
var
  TensionFlash: TCastleImageControl;
  Color: TCastleColor;
  DropsEmitter: TCastleParticleEmitter;
  DropsEffect: TCastleParticleEffect;
begin
  ClampVar(value, 0.0, 1.0);
  if abs(FTension - value) < Delta then Exit;
  FTension:= value;

  TensionFlash:= DesignedComponent('TensionFlash', False) as TCastleImageControl;
  if NOT Assigned(TensionFlash) then Exit;

  Color:= TensionFlash.Color;
  TensionFlash.Color:= Vector4(Color.RGB, FTension);

  DropsEmitter:= DesignedComponent('DropsEmitter', False) as TCastleParticleEmitter;
  if NOT Assigned(DropsEmitter) then Exit;

  DropsEmitter.Exists:= FTension > 0.0;

  DropsEffect:= DesignedComponent('DropsEffect', False) as TCastleParticleEffect;
  if NOT Assigned(DropsEmitter) then Exit;

  DropsEffect.MaxParticles:= round(50.0 * FTension);
end;

procedure TNyaPleasureTensionEffect.SetPleasure(value: Single);
var
  PleasureFlash: TCastleImageControl;
  Color: TCastleColor;
  HeartsEmitter: TCastleParticleEmitter;
  HeartsEffect: TCastleParticleEffect;
begin
  ClampVar(value, 0.0, 1.0);
  if abs(FPleasure - value) < Delta then Exit;
  FPleasure:= value;

  PleasureFlash:= DesignedComponent('PleasureFlash', False) as TCastleImageControl;
  if NOT Assigned(PleasureFlash) then Exit;

  Color:= PleasureFlash.Color;
  PleasureFlash.Color:= Vector4(Color.RGB, FPleasure);

  HeartsEmitter:= DesignedComponent('HeartsEmitter', False) as TCastleParticleEmitter;
  if NOT Assigned(HeartsEmitter) then Exit;

  HeartsEmitter.Exists:= FPleasure > 0.0;

  HeartsEffect:= DesignedComponent('HeartsEffect', False) as TCastleParticleEffect;
  if NOT Assigned(HeartsEffect) then Exit;

  HeartsEffect.MaxParticles:= round(50.0 * FPleasure);
end;

function TNyaPleasureTensionEffect.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Tension', 'Pleasure'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaPleasureTensionEffect, 'Pleasure/Tension Effect');
end.

