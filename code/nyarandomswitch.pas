unit NyaRandomSwitch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleUIControls, CastleClassUtils;

type
  TSwitchEvent = procedure (const AEnable: Boolean) of object;

  TNyaRandomSwitch = class(TCastleUserInterface)
  protected
    FTime, FTimeMeanON, FTimeDevON, FTimeMeanOFF, FTimeDevOFF,
      FTimeON, FTimeOFF: Single;
    FValue: Boolean;
    FOnSwitch: TSwitchEvent;
  public
    const
      DefaultTimeMeanON = 0.2;
      DefaultTimeDevON = 1.0;
      DefaultTimeMeanOFF = 1.0;
      DefaultTimeDevOFF = 3.0;
      StartValue = True;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: Boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    property Value: Boolean read FValue write FValue
             {$ifdef FPC}default StartValue{$endif};
  published
    property TimeON_Mean: Single read FTimeMeanON write FTimeMeanON
             {$ifdef FPC}default DefaultTimeMeanON{$endif};
    property TimeON_Deviation: Single read FTimeDevON write FTimeDevON
             {$ifdef FPC}default DefaultTimeDevON{$endif};
    property TimeOFF_Mean: Single read FTimeMeanOFF write FTimeMeanOFF
             {$ifdef FPC}default DefaultTimeMeanOFF{$endif};
    property TimeOFF_Deviation: Single read FTimeDevOFF write FTimeDevOFF
             {$ifdef FPC}default DefaultTimeDevOFF{$endif};
    property OnSwitch: TSwitchEvent read FOnSwitch write FOnSwitch;
  end;


implementation

uses
  CastleUtils, Math, CastleComponentSerialize;

constructor TNyaRandomSwitch.Create(AOwner: TComponent);
begin
  inherited;

  FTime:= 0.0;
  FOnSwitch:= nil;
  FTimeMeanON:= DefaultTimeMeanON;
  FTimeDevON:= DefaultTimeDevON;
  FTimeMeanOFF:= DefaultTimeMeanOFF;
  FTimeDevOFF:= DefaultTimeDevOFF;
  FTimeON:= DefaultTimeDevON;
  FTimeOFF:= DefaultTimeDevOFF;
  FValue:= StartValue;
end;

destructor TNyaRandomSwitch.Destroy;
begin
  inherited;
end;

procedure TNyaRandomSwitch.Update(const SecondsPassed: Single;
                                  var HandleInput: Boolean);
begin
  inherited;
  FTime:= FTime + SecondsPassed;

  if FValue Then
  begin
    if (FTime >=  FTimeON) then
    begin
      FTime:= 0.0;
      FTimeON:= RandG(FTimeMeanON, FTimeDevON);
      FValue:= False;
      if Assigned(FOnSwitch) then
        FOnSwitch(FValue);
    end;
  end
  else
  begin
    if (FTime >=  FTimeOFF) then
    begin
      FTime:= 0.0;
      FTimeOFF:= RandG(FTimeMeanOFF, FTimeDevOFF);
      FValue:= True;
      if Assigned(FOnSwitch) then
        FOnSwitch(FValue);
    end;
  end;
end;

function TNyaRandomSwitch.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'TimeON_Mean', 'TimeON_Deviation', 'TimeOFF_Mean', 'TimeOFF_Deviation'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaRandomSwitch, 'Nya Random Switch');
end.

