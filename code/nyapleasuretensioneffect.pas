unit NyaPleasureTensionEffect;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleUIControls, CastleControls, CastleClassUtils,
  CastleParticleEmitter;

type
  TNyaPleasureTensionEffect = class(TCastleUserInterface)
  protected
    FDesign: TCastleDesign;
    FTensionFlash: TCastleImageControl;
    FDropsEmitter: TCastleParticleEmitter;
    FDropsEffect: TCastleParticleEffect;
    FPleasureFlash: TCastleImageControl;
    FHeartsEmitter: TCastleParticleEmitter;
    FHeartsEffect: TCastleParticleEffect;
  protected
    FUrl: String;
    FTension: Single;
    FPleasure: Single;
    procedure SetUrl(const Value: String); virtual;
    procedure SetTension(value: Single);
    procedure SetPleasure(value: Single);
    procedure ApplyTension;
    procedure ApplyPleasure;
  public
    const
      DefaultTension = 1.0;
      DefaultPleasure = 1.0;

      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property Url: String read FUrl write SetUrl;
    property Tension: Single read FTension write SetTension
           {$ifdef FPC}default DefaultTension{$endif};
    property Pleasure: Single read FPleasure write SetPleasure
           {$ifdef FPC}default DefaultPleasure{$endif};
end;

implementation

uses
  CastleComponentSerialize, CastleColors, CastleVectors, CastleUtils
  {$ifdef CASTLE_DESIGN_MODE}
  , PropEdits, CastlePropEdits
  {$endif};

constructor TNyaPleasureTensionEffect.Create(AOwner: TComponent);
begin
  inherited;

  FDesign:= nil;
  FUrl:= '';
  FTension:= DefaultTension;
  FPleasure:= DefaultPleasure;
end;

destructor TNyaPleasureTensionEffect.Destroy;
begin
  if Assigned(FDesign) then
    FreeAndNil(FDesign);

  inherited;
end;

procedure TNyaPleasureTensionEffect.SetUrl(const value: String);
begin
  if (FUrl = value) then Exit;
  FUrl:= value;

  if NOT Assigned(FDesign) then
  begin
    FDesign:= TCastleDesign.Create(Self);
    FDesign.SetTransient;
    InsertFront(FDesign);
  end;

  FDesign.Url:= value;
  FDesign.FullSize:= True;

  FTensionFlash:= FDesign.DesignedComponent('TensionFlash', False) as TCastleImageControl;
  FDropsEmitter:= FDesign.DesignedComponent('DropsEmitter', False) as TCastleParticleEmitter;
  FDropsEffect:= FDesign.DesignedComponent('DropsEffect', False) as TCastleParticleEffect;
  FPleasureFlash:= FDesign.DesignedComponent('PleasureFlash', False) as TCastleImageControl;
  FHeartsEmitter:= FDesign.DesignedComponent('HeartsEmitter', False) as TCastleParticleEmitter;
  FHeartsEffect:= FDesign.DesignedComponent('HeartsEffect', False) as TCastleParticleEffect;

  ApplyTension;
  ApplyPleasure;
end;

procedure TNyaPleasureTensionEffect.SetTension(value: Single);
begin
  ClampVar(value, 0.0, 1.0);
  if (FTension = value) then Exit;
  FTension:= value;
  ApplyTension;
end;

procedure TNyaPleasureTensionEffect.SetPleasure(value: Single);
begin
  ClampVar(value, 0.0, 1.0);
  if (FPleasure = value) then Exit;
  FPleasure:= value;
  ApplyPleasure;
end;

procedure TNyaPleasureTensionEffect.ApplyTension;
var
  tensionColor: TCastleColor;
begin
  if Assigned(FTensionFlash) then
  begin
    tensionColor:= FTensionFlash.Color;
    FTensionFlash.Color:= Vector4(tensionColor.RGB, FTension);
  end;

  if Assigned(FDropsEmitter) then
    FDropsEmitter.Exists:= FTension > 0.0;

  if Assigned(FDropsEffect) then
    FDropsEffect.MaxParticles:= round(50.0 * FTension);
end;

procedure TNyaPleasureTensionEffect.ApplyPleasure;
var
  pleasureColor: TCastleColor;
begin
  if Assigned(FPleasureFlash) then
  begin
    pleasureColor:= FPleasureFlash.Color;
    FPleasureFlash.Color:= Vector4(pleasureColor.RGB, FPleasure);
  end;

  if Assigned(FHeartsEmitter) then
    FHeartsEmitter.Exists:= FPleasure > 0.0;

  if Assigned(FHeartsEffect) then
    FHeartsEffect.MaxParticles:= round(50.0 * FPleasure);
end;

function TNyaPleasureTensionEffect.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Url', 'Tension', 'Pleasure'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaPleasureTensionEffect, 'Nya Pleasure/Tension Effect');

  {$ifdef CASTLE_DESIGN_MODE}
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaPleasureTensionEffect, 'URL',
                         TDesignURLPropertyEditor);
  {$endif}
end.

