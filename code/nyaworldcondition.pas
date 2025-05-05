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
          DefaultLocation = Away;
          DefaultFirstTalkDone = False;
          DefaultLocationInterval: TFloatTime = 60.0;
          DefaultLocationIntervalVariance: TFloatTime = 15.0;
          DefaultSearched = False;
      protected
        FLocation: TBoyLocation;
        FLocationRemaining: TFloatTime;
        FSearched: Boolean;
        FFirstTalkDone: Boolean;
        function GetLocation: TBoyLocation;
        function LocationInterval: TFloatTime;
        procedure OnLocation;
      public
        constructor Create;
        procedure Update(const SecondsPassed: Single);
        property Location: TBoyLocation read GetLocation write FLocation;
        property Searched: boolean read FSearched write FSearched;
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
  FLocation:= DefaultLocation;
  FFirstTalkDone:= DefaultFirstTalkDone;
  FLocationRemaining:= LocationInterval;
  FSearched:= DefaultSearched;
end;

procedure TNyaWorldCondition.TBoyCondition.Update(const SecondsPassed: Single);
begin
  FLocationRemaining:= FLocationRemaining - SecondsPassed;
  if (FLocationRemaining <= 0.0) then
    OnLocation;
end;

function TNyaWorldCondition.TBoyCondition.GetLocation: TBoyLocation;
begin
  if FSearched then
    Result:= FLocation
  else
    Result:= DefaultLocation;
end;

function TNyaWorldCondition.TBoyCondition.LocationInterval: TFloatTime;
begin
  Result:= RandomFloatRange(
             DefaultLocationInterval - DefaultLocationIntervalVariance,
             DefaultLocationInterval + DefaultLocationIntervalVariance);
end;

procedure TNyaWorldCondition.TBoyCondition.OnLocation;
var
  locNum, nHigh, nLow: Integer;
begin
  FLocationRemaining:= LocationInterval;

  nLow:= Ord(Low(TBoyLocation));
  nHigh:= Ord(High(TBoyLocation)) + 1;

  locNum:= RandomRange(nLow, nHigh);

  { if FirstTalk with Boy isn't done then boy isn't go in Girl's Home itself }
  if ((NOT FirstTalkDone) AND (TBoyLocation(locNum) = TBoyLocation.InRoom)) then
    FLocation:= TBoyLocation.InHovel
  else
    FLocation:= TBoyLocation(locNum);
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
  Result:= FBoyCondition.FLocation <> TBoyLocation.Away;
end;

procedure TNyaWorldCondition.RestoreCondition;
var
  ini: TCustomIniFile;
begin
  ini:= TMemIniFile.Create(IniFileName);
  ini.FormatSettings.DecimalSeparator:= '|';
  ini.Options:= [ifoFormatSettingsActive];

  FBoyCondition.FLocation:= TBoyLocation(
                               ini.ReadInteger(Section, BoyLocationStr,
                               Ord(FBoyCondition.DefaultLocation)));

  FBoyCondition.FFirstTalkDone:=  ini.ReadBool(Section, BoyFirstTalkDone,
                                  FBoyCondition.DefaultFirstTalkDone);

  FBoyCondition.FLocationRemaining:= ini.ReadFloat(Section, BoyTimerStr,
                                        FBoyCondition.LocationInterval);

  FBoyCondition.FSearched:= ini.ReadBool(Section, BoySearchedStr, FBoyCondition.DefaultSearched);

  ini.Free;
end;

procedure TNyaWorldCondition.SaveCondition;
var
  ini: TCustomIniFile;
begin
  ini:= TMemIniFile.Create(IniFileName);
  ini.FormatSettings.DecimalSeparator:= '|';
  ini.Options:= [ifoFormatSettingsActive];

  ini.WriteInteger(Section, BoyLocationStr, Ord(FBoyCondition.FLocation));
  ini.WriteBool(Section, BoyFirstTalkDone, FBoyCondition.FFirstTalkDone);
  ini.WriteFloat(Section, BoyTimerStr, FBoyCondition.FLocationRemaining);
  ini.WriteBool(Section, BoySearchedStr, FBoyCondition.FSearched);

  ini.Free;
end;

initialization
  RegisterSerializableComponent(TNyaWorldCondition, ['World', 'Nya World Condition']);
end.

