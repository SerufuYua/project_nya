unit NyaWorldCondition;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleUIControls, CastleClassUtils, CastleControls,
  CastleTimeUtils, NyaCharaDress;

type
  TBoyLocation = (Away, VisitInRoom, VisitInHovel, HomeWorking, HomeSleep);
  TBoyStatus   = (InSearch, FirstTalkDone, FirstVisitDone);
  TBoyStatuses = Set of TBoyStatus;

  TNyaWorldCondition = class(TCastleUserInterface)
  protected
    type
      { Boy Condition }
      TBoyCondition = class
      protected
        const
          DefaultLocation = Away;
          DefaultLocationInterval: TFloatTime = 60.0;
          DefaultLocationIntervalVariance: TFloatTime = 15.0;
          DefaultSearched = False;
          DefaultVisible = False;
      protected
        FVisible: Boolean;
        FLocation: TBoyLocation;
        FLocationRemaining: TFloatTime;
        FStatus: TBoyStatuses;
        FDresser: TCharaDresser;
        function LocationInterval: TFloatTime;
        procedure OnLocation;
      public
        constructor Create;
        procedure Update(const SecondsPassed: Single);
        property Visible: boolean read FVisible write FVisible;
        property Location: TBoyLocation read FLocation write FLocation;
        property Status: TBoyStatuses read FStatus write FStatus;
        property Dresser: TCharaDresser write FDresser;
      end;
    var
      FBoyCondition: TBoyCondition;
  protected
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
  FLocation:= DefaultLocation;
  FStatus:= [];
  FLocationRemaining:= LocationInterval;
  FVisible:= DefaultVisible;
  FDresser:= nil;
end;

procedure TNyaWorldCondition.TBoyCondition.Update(const SecondsPassed: Single);
begin
  FLocationRemaining:= FLocationRemaining - SecondsPassed;
  if ((FLocationRemaining <= 0.0) AND (NOT Self.Visible)) then
  begin
    FLocationRemaining:= LocationInterval;
    OnLocation;
  end;
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
       'any1', 'any2'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

procedure TNyaWorldCondition.RestoreCondition;
var
  boyStatus: TBoyStatus;
  path: String;
begin
  path:= Section + '/' + BoyStr + '/';

  FBoyCondition.FLocationRemaining:= UserConfig.GetFloat(path + TimerStr,
                                       FBoyCondition.LocationInterval);

  FBoyCondition.FLocation:= TBoyLocation(GetEnumValue(TypeInfo(TBoyLocation),
                              (UserConfig.GetValue(path + LocationStr,
                                 GetEnumName(TypeInfo(TBoyLocation), 0)))));

  For boyStatus in TBoyStatuses do
    UserConfig.GetValue(path + GetEnumName(TypeInfo(TBoyStatus), Ord(boyStatus)),
                        False);
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

