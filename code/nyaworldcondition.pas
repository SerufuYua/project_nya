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
      protected
        const
          DefaultBoyExists = False;
          DefaultFirstTalkDone = False;
          DefaultBoyExistsInterval: TFloatTime = 3.0 * 60.0;
          DefaultBoyExistsIntervalVariance: TFloatTime = 30.0;
          DefaultBoySearched = False;
      protected
        FBoyExists: Boolean;
        FBoyExistsRemaining: TFloatTime;
        FBoySearched: Boolean;
        FFirstTalkDone: Boolean;
        function GetBoyExists: Boolean;
        function BoyExistsInterval: TFloatTime;
        procedure OnBoyExists;
      public
        constructor Create;
        procedure Update(const SecondsPassed: Single);
        property Exists: boolean read GetBoyExists;
        property Searched: boolean read FBoySearched write FBoySearched;
        property FirstTalkDone: boolean read FFirstTalkDone write FFirstTalkDone;
      end;
    var
      FBoyCondition: TBoyCondition;
  protected
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
    property Boy: TBoyCondition read FBoyCondition;
    property SpacePlaneExists: boolean read GetSpacePlaneExists;
  end;


implementation

uses
  CastleComponentSerialize, CastleUtils, IniFiles, Math;

{$I nyaworldconst.inc}

const
  Section = 'World';

{ ========= ------------------------------------------------------------------ }
{ TBoyCondition -------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

const
  BoyExistsStr = 'BoyExists';
  BoyFirstTalkDone = 'BoyFirstTalkDone';
  BoyTimerStr = 'BoyTimer';
  BoySearchedStr = 'BoySearched';

  constructor TNyaWorldCondition.TBoyCondition.Create;
begin
  FBoyExists:= DefaultBoyExists;
  FFirstTalkDone:= DefaultFirstTalkDone;
  FBoyExistsRemaining:= BoyExistsInterval;
  FBoySearched:= DefaultBoySearched;
end;

procedure TNyaWorldCondition.TBoyCondition.Update(const SecondsPassed: Single);
begin
  FBoyExistsRemaining:= FBoyExistsRemaining - SecondsPassed;
  if (FBoyExistsRemaining <= 0.0) then
    OnBoyExists;
end;

function TNyaWorldCondition.TBoyCondition.GetBoyExists: Boolean;
begin
  Result:= FBoyExists AND FBoySearched;
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

  FBoyCondition.FFirstTalkDone:=  ini.ReadBool(Section, BoyFirstTalkDone,
                                  FBoyCondition.DefaultFirstTalkDone);

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
  ini.WriteBool(Section, BoyFirstTalkDone, FBoyCondition.FFirstTalkDone);
  ini.WriteFloat(Section, BoyTimerStr, FBoyCondition.FBoyExistsRemaining);
  ini.WriteBool(Section, BoySearchedStr, FBoyCondition.FBoySearched);

  ini.Free;
end;

initialization
  RegisterSerializableComponent(TNyaWorldCondition, ['World', 'Nya World Condition']);
end.

