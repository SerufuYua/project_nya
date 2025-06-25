unit NyaWorldCondition;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleUIControls, CastleClassUtils, CastleControls,
  CastleTimeUtils, NyaCharaDress;

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
          DefaultVisible = False;
      protected
        FVisible: Boolean;
        FLocation: TBoyLocation;
        FLocationRemaining: TFloatTime;
        FSearched: Boolean;
        FFirstTalkDone: Boolean;
        FDresser: TCharaDresser;
        function GetLocation: TBoyLocation;
        function LocationInterval: TFloatTime;
        procedure OnLocation;
      public
        constructor Create;
        procedure Update(const SecondsPassed: Single);
        property Location: TBoyLocation read GetLocation write FLocation;
        property Searched: boolean read FSearched write FSearched;
        property FirstTalkDone: boolean read FFirstTalkDone write FFirstTalkDone;
        property Visible: boolean read FVisible write FVisible;
        property Dresser: TCharaDresser write FDresser;
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
  CastleComponentSerialize, CastleUtils, CastleConfig, Math;

const
  Section = 'World';
  BoyStr = 'Boy';

{ ========= ------------------------------------------------------------------ }
{ TBoyCondition -------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

const
  LocationStr = 'Location';
  FirstTalkDone = 'FirstTalkDone';
  TimerStr = 'Timer';
  SearchedStr = 'Searched';

  constructor TNyaWorldCondition.TBoyCondition.Create;
begin
  FLocation:= DefaultLocation;
  FFirstTalkDone:= DefaultFirstTalkDone;
  FLocationRemaining:= LocationInterval;
  FSearched:= DefaultSearched;
  FVisible:= DefaultVisible;
  FDresser:= nil;
end;

procedure TNyaWorldCondition.TBoyCondition.Update(const SecondsPassed: Single);
begin
  FLocationRemaining:= FLocationRemaining - SecondsPassed;
  if ((FLocationRemaining <= 0.0) AND (NOT Visible)) then
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
  suitList: Array of String;
  newSuit: String;
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

  { change dressing }
  if Assigned(FDresser) then
  begin
    suitList:= FDresser.SuitList;
    newSuit:= suitList[RandomRange(0, High(suitList))];
    FDresser.DressSuit(newSuit);
  end;
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
  path: String;
begin
  path:= Section + '/' + BoyStr + '/';

  FBoyCondition.FLocation:= TBoyLocation(UserConfig.GetValue(path +
                                         LocationStr,
                                         Ord(FBoyCondition.DefaultLocation)));

  FBoyCondition.FFirstTalkDone:=  UserConfig.GetValue(path +
                                    FirstTalkDone,
                                    FBoyCondition.DefaultFirstTalkDone);

  FBoyCondition.FLocationRemaining:= UserConfig.GetFloat(path + TimerStr,
                                       FBoyCondition.LocationInterval);

  FBoyCondition.FSearched:= UserConfig.GetValue(path + SearchedStr,
                                                FBoyCondition.DefaultSearched);
end;

procedure TNyaWorldCondition.SaveCondition;
var
  path: String;
begin
  path:= Section + '/' + BoyStr + '/';

  UserConfig.SetValue(path + LocationStr, Ord(FBoyCondition.FLocation));
  UserConfig.SetValue(path + FirstTalkDone, FBoyCondition.FFirstTalkDone);
  UserConfig.SetFloat(path + TimerStr, FBoyCondition.FLocationRemaining);
  UserConfig.SetValue(path + SearchedStr, FBoyCondition.FSearched);

  UserConfig.Save;
end;

initialization
  RegisterSerializableComponent(TNyaWorldCondition, ['World', 'Nya World Condition']);
end.

