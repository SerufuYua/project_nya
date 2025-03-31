unit NyaWorldCondition;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleUIControls, CastleClassUtils, CastleControls,
  CastleTimeUtils;

type
  TNyaWorldCondition = class(TCastleUserInterface)
  protected { Boy Exists }
    FBoyExists: boolean;
    FBoyExistsRemaining: TFloatTime;
    function BoyExistsInterval: TFloatTime;
    procedure OnBoyExists;
  protected
    procedure RestoreCondition;
    procedure SaveCondition;
  public
    const
      DefaultBoyExists = False;
      DefaultBoyExistsInterval: TFloatTime = 3.0 * 60.0;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: Boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  public
    property BoyExists: boolean read FBoyExists;
  end;


implementation

uses
  CastleComponentSerialize, CastleUtils, IniFiles, Math;

{$I nyaworldconsts.inc}

const
  Section = 'World';
  BoyExistsStr = 'BoyExists';
  BoyTimerStr = 'BoyTimer';

constructor TNyaWorldCondition.Create(AOwner: TComponent);
begin
  inherited;

  RestoreCondition;
end;

destructor TNyaWorldCondition.Destroy;
begin
  SaveCondition;

  inherited;
end;

procedure TNyaWorldCondition.Update(const SecondsPassed: Single;
                                    var HandleInput: Boolean);
begin
  inherited;

  { processing Boy Exists Time }
  FBoyExistsRemaining:= FBoyExistsRemaining - SecondsPassed;
  if (FBoyExistsRemaining <= 0.0) then
    OnBoyExists;
end;

function TNyaWorldCondition.BoyExistsInterval: TFloatTime;
begin
  Result:= RandomFloatRange(DefaultBoyExistsInterval * 0.5,
                            DefaultBoyExistsInterval * 1.5);
end;

procedure TNyaWorldCondition.OnBoyExists;
begin
  FBoyExistsRemaining:= BoyExistsInterval;
  FBoyExists:= NOT FBoyExists;
end;

function TNyaWorldCondition.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'any1', 'any2'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

procedure TNyaWorldCondition.RestoreCondition;
var
  ini: TCustomIniFile;
begin
  ini:= TMemIniFile.Create(IniFileName);
  ini.FormatSettings.DecimalSeparator:= '|';
  ini.Options:= [ifoFormatSettingsActive];

  FBoyExists:= ini.ReadBool(Section, BoyExistsStr, False);
  FBoyExistsRemaining:= ini.ReadFloat(Section, BoyTimerStr, BoyExistsInterval);

  ini.Free;
end;

procedure TNyaWorldCondition.SaveCondition;
var
  ini: TCustomIniFile;
begin
  ini:= TMemIniFile.Create(IniFileName);
  ini.FormatSettings.DecimalSeparator:= '|';
  ini.Options:= [ifoFormatSettingsActive];

  ini.WriteBool(Section, BoyExistsStr, FBoyExists);
  ini.WriteFloat(Section, BoyTimerStr, FBoyExistsRemaining);

  ini.Free;
end;

initialization
  RegisterSerializableComponent(TNyaWorldCondition, ['World', 'Nya World Condition']);
end.

