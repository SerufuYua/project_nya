unit NyaWorldCondition;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleUIControls, CastleClassUtils, CastleControls,
  CastleTimeUtils;

type
  TNyaWorldCondition = class(TCastleUserInterface)
  protected
    type
      { Boy Condition }
      TBoyCondition = class
      public
        const
          DefaultBoyExists = False;
          DefaultBoyExistsInterval: TFloatTime = 3.0 * 60.0;
          DefaultBoyExistsIntervalVariance: TFloatTime = 30.0;
          DefaultBoySearched = False;
      public
        FBoyExists: Boolean;
        FBoyExistsRemaining: TFloatTime;
        FBoySearched: Boolean;
        constructor Create;
        procedure Update(const SecondsPassed: Single);
        function BoyExistsInterval: TFloatTime;
        procedure OnBoyExists;
      end;
    var
      FBoyCondition: TBoyCondition;
  protected
    function GetBoyExists: Boolean;
    function GetBoySearched: Boolean;
    procedure SetBoySearched(value: Boolean);
    function GetSpacePlaneExists: Boolean;
    procedure RestoreCondition;
    procedure SaveCondition;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: Boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  public
    property BoyExists: boolean read GetBoyExists;
    property BoySearched: boolean read GetBoySearched write SetBoySearched;
    property SpacePlaneExists: boolean read GetSpacePlaneExists;
  end;


implementation

uses
  CastleComponentSerialize, CastleUtils, IniFiles, Math;

{$I nyaworldconsts.inc}

const
  Section = 'World';

{ ========= ------------------------------------------------------------------ }
{ TBoyCondition -------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

const
  BoyExistsStr = 'BoyExists';
  BoyTimerStr = 'BoyTimer';
  BoySearchedStr = 'BoySearched';

  constructor TNyaWorldCondition.TBoyCondition.Create;
begin
  FBoyExists:= DefaultBoyExists;
  FBoyExistsRemaining:= BoyExistsInterval;
  FBoySearched:= DefaultBoySearched;
end;

procedure TNyaWorldCondition.TBoyCondition.Update(const SecondsPassed: Single);
begin
  FBoyExistsRemaining:= FBoyExistsRemaining - SecondsPassed;
  if (FBoyExistsRemaining <= 0.0) then
    OnBoyExists;
end;

function TNyaWorldCondition.TBoyCondition.BoyExistsInterval: TFloatTime;
begin
  Result:= RandomFloatRange(
             DefaultBoyExistsInterval - DefaultBoyExistsIntervalVariance,
             DefaultBoyExistsInterval + DefaultBoyExistsIntervalVariance);
end;

procedure TNyaWorldCondition.TBoyCondition.OnBoyExists;
begin
  FBoyExistsRemaining:= BoyExistsInterval;
  FBoyExists:= NOT FBoyExists;
end;

{ ========= ------------------------------------------------------------------ }
{ TNyaWorldCondition --------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TNyaWorldCondition.Create(AOwner: TComponent);
begin
  inherited;

  FBoyCondition:= TBoyCondition.Create;
  RestoreCondition;
end;

destructor TNyaWorldCondition.Destroy;
begin
  SaveCondition;
  freeAndNil(FBoyCondition);

  inherited;
end;

procedure TNyaWorldCondition.Update(const SecondsPassed: Single;
                                    var HandleInput: Boolean);
begin
  inherited;

  FBoyCondition.Update(SecondsPassed);
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

function TNyaWorldCondition.GetBoyExists: Boolean;
begin
  Result:= FBoyCondition.FBoyExists AND FBoyCondition.FBoySearched;
end;

function TNyaWorldCondition.GetBoySearched: Boolean;
begin
  Result:= FBoyCondition.FBoySearched;
end;

procedure TNyaWorldCondition.SetBoySearched(value: Boolean);
begin
  FBoyCondition.FBoySearched:= value;
end;

function TNyaWorldCondition.GetSpacePlaneExists: Boolean;
begin
  Result:= FBoyCondition.FBoyExists;
end;

procedure TNyaWorldCondition.RestoreCondition;
var
  ini: TCustomIniFile;
begin
  ini:= TMemIniFile.Create(IniFileName);
  ini.FormatSettings.DecimalSeparator:= '|';
  ini.Options:= [ifoFormatSettingsActive];

  FBoyCondition.FBoyExists:= ini.ReadBool(Section, BoyExistsStr,
                              FBoyCondition.DefaultBoyExists);
  FBoyCondition.FBoyExistsRemaining:= ini.ReadFloat(Section, BoyTimerStr,
                                       FBoyCondition.BoyExistsInterval);
  FBoyCondition.FBoySearched:= ini.ReadBool(Section, BoySearchedStr, FBoyCondition.DefaultBoySearched);

  ini.Free;
end;

procedure TNyaWorldCondition.SaveCondition;
var
  ini: TCustomIniFile;
begin
  ini:= TMemIniFile.Create(IniFileName);
  ini.FormatSettings.DecimalSeparator:= '|';
  ini.Options:= [ifoFormatSettingsActive];

  ini.WriteBool(Section, BoyExistsStr, FBoyCondition.FBoyExists);
  ini.WriteFloat(Section, BoyTimerStr, FBoyCondition.FBoyExistsRemaining);
  ini.WriteBool(Section, BoySearchedStr, FBoyCondition.FBoySearched);

  ini.Free;
end;

initialization
  RegisterSerializableComponent(TNyaWorldCondition, ['World', 'Nya World Condition']);
end.

