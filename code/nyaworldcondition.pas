unit NyaWorldCondition;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleUIControls, CastleClassUtils, CastleControls,
  CastleTimeUtils, NyaCharaDress;

type
  TBoyLocation = (Away, VisitInRoom, VisitInHovel, HomeWorking, HomeSleep);
  TBoyStatus   = (InSearch, FirstTalkDone);
  TBoyStatuses = Set of TBoyStatus;

  TNyaWorldCondition = class(TCastleUserInterface)
  protected
    type
      { Boy Condition }
      TBoyCondition = class
      protected
        const
          DefaultLocation = Away;
          DefaultSearched = False;
          DefaultVisible = False;
      protected
        FLocationInterval: TFloatTime;
        FLocationIntervalVariance: TFloatTime;
        FVisible: Boolean;
        FLocation: TBoyLocation;
        FLocationRemaining: TFloatTime;
        FStatus: TBoyStatuses;
        FDresser: TCharaDresser;
        function GetLocationInterval: TFloatTime;
        procedure OnLocation;
      public
        constructor Create;
        procedure Update(const SecondsPassed: Single);
        property LocationInterval: TFloatTime read FLocationInterval write FLocationInterval;
        property LocationIntervalVariance: TFloatTime read FLocationIntervalVariance write FLocationInterval;
        property Visible: boolean read FVisible write FVisible;
        property Location: TBoyLocation read FLocation write FLocation;
        property Status: TBoyStatuses read FStatus write FStatus;
        property Dresser: TCharaDresser write FDresser;
      end;
    var
      FBoyCondition: TBoyCondition;
  protected
    const
      DefaultLocationInterval = 60.0;
      DefaultLocationIntervalVariance = 15.0;

    procedure SetBoyLocationInterval(value: Single);
    function GetBoyLocationInterval: Single;
    procedure SetBoyLocationIntervalVariance(value: Single);
    function GetBoyLocationIntervalVariance: Single;
    procedure RestoreCondition;
    procedure SaveCondition;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: Boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    property Boy: TBoyCondition read FBoyCondition;
  published
    property BoyLocationInterval: Single read GetBoyLocationInterval write SetBoyLocationInterval
             {$ifdef FPC}default DefaultLocationInterval{$endif};
    property BoyLocationIntervalVariance: Single read GetBoyLocationIntervalVariance write SetBoyLocationIntervalVariance
             {$ifdef FPC}default DefaultLocationIntervalVariance{$endif};
  end;


implementation

uses
  CastleComponentSerialize, CastleUtils, CastleConfig, Math, TypInfo;

const
  Section = 'World';
  BoySatus = 'Status';
  BoyStr = 'Boy';

{ ========= ------------------------------------------------------------------ }
{ TBoyCondition -------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

const
  LocationStr = 'Location';
  TimerStr = 'Timer';

  constructor TNyaWorldCondition.TBoyCondition.Create;
begin
  FLocationInterval:= DefaultLocationInterval;
  FLocationIntervalVariance:= DefaultLocationIntervalVariance;
  FLocation:= DefaultLocation;
  FStatus:= [];
  FLocationRemaining:= GetLocationInterval;
  FVisible:= DefaultVisible;
  FDresser:= nil;
end;

procedure TNyaWorldCondition.TBoyCondition.Update(const SecondsPassed: Single);
begin
  FLocationRemaining:= FLocationRemaining - SecondsPassed;
  if ((FLocationRemaining <= 0.0) AND (NOT Self.Visible)) then
  begin
    FLocationRemaining:= GetLocationInterval;
    OnLocation;
  end;
end;

function TNyaWorldCondition.TBoyCondition.GetLocationInterval: TFloatTime;
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
  nLow:= Ord(Low(TBoyLocation));
  nHigh:= Ord(High(TBoyLocation)) + 1;

  locNum:= RandomRange(nLow, nHigh);

  { if FirstTalk with Boy isn't done then boy isn't go in Girl's Home itself }
  if ((NOT (TBoyStatus.FirstTalkDone in Status)) AND
      (TBoyLocation(locNum) = TBoyLocation.VisitInRoom)) then
    FLocation:= TBoyLocation.VisitInHovel
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
       'BoyLocationInterval', 'BoyLocationIntervalVariance'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

procedure TNyaWorldCondition.SetBoyLocationInterval(value: Single);
begin
  FBoyCondition.LocationInterval:= value;
end;

function TNyaWorldCondition.GetBoyLocationInterval: Single;
begin
  Result:= FBoyCondition.LocationInterval;
end;

procedure TNyaWorldCondition.SetBoyLocationIntervalVariance(value: Single);
begin
  FBoyCondition.LocationIntervalVariance:= value;
end;

function TNyaWorldCondition.GetBoyLocationIntervalVariance: Single;
begin
  Result:= FBoyCondition.LocationIntervalVariance;
end;

procedure TNyaWorldCondition.RestoreCondition;
var
  boyStatus: TBoyStatus;
  path: String;
begin
  path:= Section + '/' + BoyStr + '/';

  FBoyCondition.FLocationRemaining:= UserConfig.GetFloat(path + TimerStr,
                                       FBoyCondition.GetLocationInterval);

  FBoyCondition.FLocation:= TBoyLocation(GetEnumValue(TypeInfo(TBoyLocation),
                              (UserConfig.GetValue(path + LocationStr,
                                 GetEnumName(TypeInfo(TBoyLocation), 0)))));

  For boyStatus in TBoyStatuses do
    if UserConfig.GetValue(path + GetEnumName(TypeInfo(TBoyStatus), Ord(boyStatus)), False) then
      FBoyCondition.Status:= FBoyCondition.Status + [boyStatus];
end;

procedure TNyaWorldCondition.SaveCondition;
var
  boyStatus: TBoyStatus;
  path: String;
begin
  path:= Section + '/' + BoyStr + '/';

  UserConfig.SetFloat(path + TimerStr, FBoyCondition.FLocationRemaining);

  UserConfig.SetValue(path + LocationStr,
                      GetEnumName(TypeInfo(TBoyLocation),
                                  Ord(FBoyCondition.FLocation)));

  For boyStatus in TBoyStatuses do
    UserConfig.SetValue(path + GetEnumName(TypeInfo(TBoyStatus), Ord(boyStatus)),
                        (boyStatus in FBoyCondition.Status));

  UserConfig.Save;
end;

initialization
  RegisterSerializableComponent(TNyaWorldCondition, ['World', 'Nya World Condition']);
end.

