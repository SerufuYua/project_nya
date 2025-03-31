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
      BoyCondition: TBoyCondition;
  protected
    function GetBoyExists: Boolean;
    function GetBoySearched: Boolean;
    procedure SetBoySearched(value: Boolean);
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

  BoyCondition:= TBoyCondition.Create;
  RestoreCondition;
end;

destructor TNyaWorldCondition.Destroy;
begin
  SaveCondition;
  freeAndNil(BoyCondition);

  inherited;
end;

procedure TNyaWorldCondition.Update(const SecondsPassed: Single;
                                    var HandleInput: Boolean);
begin
  inherited;

  BoyCondition.Update(SecondsPassed);
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
  Result:= BoyCondition.FBoyExists;
end;

function TNyaWorldCondition.GetBoySearched: Boolean;
begin
  Result:= BoyCondition.FBoySearched;
end;

procedure TNyaWorldCondition.SetBoySearched(value: Boolean);
begin
  BoyCondition.FBoySearched:= value;
end;

procedure TNyaWorldCondition.RestoreCondition;
var
  ini: TCustomIniFile;
begin
  ini:= TMemIniFile.Create(IniFileName);
  ini.FormatSettings.DecimalSeparator:= '|';
  ini.Options:= [ifoFormatSettingsActive];

  BoyCondition.FBoyExists:= ini.ReadBool(Section, BoyExistsStr,
                              BoyCondition.DefaultBoyExists);
  BoyCondition.FBoyExistsRemaining:= ini.ReadFloat(Section, BoyTimerStr,
                                       BoyCondition.BoyExistsInterval);
  BoyCondition.FBoySearched:= ini.ReadBool(Section, BoySearchedStr, BoyCondition.DefaultBoySearched);

  ini.Free;
end;

procedure TNyaWorldCondition.SaveCondition;
var
  ini: TCustomIniFile;
begin
  ini:= TMemIniFile.Create(IniFileName);
  ini.FormatSettings.DecimalSeparator:= '|';
  ini.Options:= [ifoFormatSettingsActive];

  ini.WriteBool(Section, BoyExistsStr, BoyCondition.FBoyExists);
  ini.WriteFloat(Section, BoyTimerStr, BoyCondition.FBoyExistsRemaining);
  ini.WriteBool(Section, BoySearchedStr, BoyCondition.FBoySearched);

  ini.Free;
end;

initialization
  RegisterSerializableComponent(TNyaWorldCondition, ['World', 'Nya World Condition']);
end.

