unit NyaWorldCondition;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleUIControls, CastleClassUtils, CastleControls,
  CastleTimeUtils;

type
  TBoyLocation = (Away, InRoom, InHovel);

  TNyaWorldCondition = class(TCastleUserInterface)
  protected
    type
      { Boy Condition }
      TBoyCondition = class
      protected
        const
          DefaultBoyLocation = Away;
          DefaultFirstTalkDone = False;
          DefaultBoyLocationInterval: TFloatTime = 60.0;
          DefaultBoyLocationIntervalVariance: TFloatTime = 15.0;
          DefaultBoySearched = False;
      protected
        FBoyLocation: TBoyLocation;
        FBoyLocationRemaining: TFloatTime;
        FBoySearched: Boolean;
        FFirstTalkDone: Boolean;
        function GetBoyLocation: TBoyLocation;
        function BoyLocationInterval: TFloatTime;
        procedure OnBoyLocation;
      public
        constructor Create;
        procedure Update(const SecondsPassed: Single);
        property Location: TBoyLocation read GetBoyLocation;
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
  BoyLocationStr = 'BoyLocation';
  BoyFirstTalkDone = 'BoyFirstTalkDone';
  BoyTimerStr = 'BoyTimer';
  BoySearchedStr = 'BoySearched';

  constructor TNyaWorldCondition.TBoyCondition.Create;
begin
  FBoyLocation:= DefaultBoyLocation;
  FFirstTalkDone:= DefaultFirstTalkDone;
  FBoyLocationRemaining:= BoyLocationInterval;
  FBoySearched:= DefaultBoySearched;
end;

procedure TNyaWorldCondition.TBoyCondition.Update(const SecondsPassed: Single);
begin
  FBoyLocationRemaining:= FBoyLocationRemaining - SecondsPassed;
  if (FBoyLocationRemaining <= 0.0) then
    OnBoyLocation;
end;

function TNyaWorldCondition.TBoyCondition.GetBoyLocation: TBoyLocation;
begin
  if FBoySearched then
    Result:= FBoyLocation
  else
    Result:= DefaultBoyLocation;
end;

function TNyaWorldCondition.TBoyCondition.BoyLocationInterval: TFloatTime;
begin
  Result:= RandomFloatRange(
             DefaultBoyLocationInterval - DefaultBoyLocationIntervalVariance,
             DefaultBoyLocationInterval + DefaultBoyLocationIntervalVariance);
end;

procedure TNyaWorldCondition.TBoyCondition.OnBoyLocation;
var
  locNum, nHigh, nLow: Integer;
begin
  FBoyLocationRemaining:= BoyLocationInterval;

  nLow:= Ord(Low(TBoyLocation));
  nHigh:= Ord(High(TBoyLocation)) + 1;

  locNum:= RandomRange(nLow, nHigh);

  { if FirstTalk with Boy isn't done then boy isn't go in Girl's Home itself }
  if ((NOT FirstTalkDone) AND (TBoyLocation(locNum) = TBoyLocation.InRoom)) then
    FBoyLocation:= TBoyLocation.InHovel
  else
    FBoyLocation:= TBoyLocation(locNum);
end;

{ ========= ------------------------------------------------------------------ }
{ TNyaWorldCondition --------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TNyaWorldCondition.Create(AOwner: TComponent);
begin
  inherited;

  Randomize;
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
  Result:= FBoyCondition.FBoyLocation <> TBoyLocation.Away;
end;

procedure TNyaWorldCondition.RestoreCondition;
var
  ini: TCustomIniFile;
begin
  ini:= TMemIniFile.Create(IniFileName);
  ini.FormatSettings.DecimalSeparator:= '|';
  ini.Options:= [ifoFormatSettingsActive];

  FBoyCondition.FBoyLocation:= TBoyLocation(
                               ini.ReadInteger(Section, BoyLocationStr,
                               Ord(FBoyCondition.DefaultBoyLocation)));

  FBoyCondition.FFirstTalkDone:=  ini.ReadBool(Section, BoyFirstTalkDone,
                                  FBoyCondition.DefaultFirstTalkDone);

  FBoyCondition.FBoyLocationRemaining:= ini.ReadFloat(Section, BoyTimerStr,
                                        FBoyCondition.BoyLocationInterval);

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

  ini.WriteInteger(Section, BoyLocationStr, Ord(FBoyCondition.FBoyLocation));
  ini.WriteBool(Section, BoyFirstTalkDone, FBoyCondition.FFirstTalkDone);
  ini.WriteFloat(Section, BoyTimerStr, FBoyCondition.FBoyLocationRemaining);
  ini.WriteBool(Section, BoySearchedStr, FBoyCondition.FBoySearched);

  ini.Free;
end;

initialization
  RegisterSerializableComponent(TNyaWorldCondition, ['World', 'Nya World Condition']);
end.

