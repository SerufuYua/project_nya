unit BaseViewRace;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleControls, CastleConfig, CastleUIControls,
  BaseViewRide;

const
  BestTimePath = 'ride/time/BestTime';
  DefaultBestTime = -1.0;

type
  TBaseViewRace = class(TBaseViewRide)
  published
    GroupTime: TCastleUserInterface;
    LabelTimeValue, LabelBestTimeValue: TCastleLabel;
  protected
    FEnableTimer, FRaceStarted: Boolean;
    FTrackTimer, FBestTime: Single;
    FCheckPointNum: Integer;
    procedure ShowTime(const value: Single);
    procedure ShowBestTime(const value: Single);
    procedure DoTouchSwitch(const Sender: TObject; Touch: Boolean); override;
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    procedure Pause; override;
    procedure Resume; override;
  end;

implementation

uses
  CastleSoundEngine, GameSound, NyaSwitch;

procedure TBaseViewRace.Start;
begin
  inherited;
  FEnableTimer:= False;
  FRaceStarted:= False;
  FTrackTimer:= 0.0;
  FCheckPointNum:= 0;

  { load Best Time }
  FBestTime:= UserConfig.GetFloat(BestTimePath, DefaultBestTime);
  ShowBestTime(FBestTime);
end;

procedure TBaseViewRace.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  { Executed every frame. }

  { update Track Timer }
  if FEnableTimer AND FRaceStarted then
  begin
    FTrackTimer:= FTrackTimer + SecondsPassed;
    ShowTime(FTrackTimer);
  end;

  inherited;
end;

procedure TBaseViewRace.ShowTime(const value: Single);
begin
  LabelTimeValue.Caption:= value.ToString(ffFixed, 3, 2);
end;

procedure TBaseViewRace.ShowBestTime(const value: Single);
begin
  if (value <= 0.0) then
    LabelBestTimeValue.Caption:= '---'
  else
    LabelBestTimeValue.Caption:= value.ToString(ffFixed, 3, 2);
end;

procedure TBaseViewRace.DoTouchSwitch(const Sender: TObject; Touch: Boolean);
var
  switch: TNyaSwitch;
begin
  inherited;

  switch:= Sender as TNyaSwitch;
  if NOT Assigned(switch) then Exit;

  if (switch.Tag < 1) then Exit;

  if Touch then
  begin
    Status.Caption:= switch.ActionString;

    if(FTouchedSwitch <> switch) then
    begin
      Status.Caption:= switch.ActionString;
      FTouchedSwitch:= switch;
      SoundEngine.Play(NamedSound('SfxInspect'));
    end;

    { process Check Points }
    if ((switch.Name = 'CheckSwitchStart') AND (NOT FEnableTimer)) then
    begin
      FTrackTimer:= 0.0;
      GroupTime.Exists:= True;
      FEnableTimer:= True;
      FRaceStarted:= True;
      ShowBestTime(FBestTime);
    end;

    if (FCheckPointNum = (switch.Tag - 1)) then
    begin
      FCheckPointNum:= switch.Tag;

      if switch.Name = 'CheckSwitchFinish' then
      begin
        FEnableTimer:= False;
        FRaceStarted:= False;
        FCheckPointNum:= 0;

        { save Best Time }
        if ((FTrackTimer > 0.0) AND
            ((FBestTime <= 0.0) OR (FBestTime > FTrackTimer))) then
        begin
          FBestTime:= FTrackTimer;
          UserConfig.SetFloat(BestTimePath, FBestTime);
          UserConfig.Save;
        end;
      end;
    end else
      Notifications.Show('Warning: wrong way');
  end else
  begin
    if(FTouchedSwitch = switch) then
    begin
      FTouchedSwitch:= nil;
      Status.Caption:= '';
    end;
  end;
end;

procedure TBaseViewRace.Pause;
begin
  inherited;
  FEnableTimer:= False;
end;

procedure TBaseViewRace.Resume;
begin
  inherited;
  if FRaceStarted then
    FEnableTimer:= True;
end;

end.

