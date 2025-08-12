unit BaseViewRace;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleControls, CastleConfig, BaseViewRideNew;

const
  BestTimePath = 'ride/time/BestTime';
  DefaultBestTime = -1.0;

type
  TBaseViewRace = class(TBaseViewRideNew)
  published
    LabelSpeedValue, LabelTimeValue, LabelBestTimeValue: TCastleLabel;
  protected
    FEnableTimer: Boolean;
    FTrackTimer, FBestTime: Single;
    procedure ShowTime(const value: Single);
    procedure ShowBestTime(const value: Single);
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  end;

implementation

procedure TBaseViewRace.Start;
begin
  inherited;
  FEnableTimer:= False;
  FTrackTimer:= 0.0;

  { load Best Time }
  FBestTime:= UserConfig.GetFloat(BestTimePath, DefaultBestTime);
  ShowBestTime(FBestTime);
end;

procedure TBaseViewRace.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  { Executed every frame. }

  { update Track Timer }
  if FEnableTimer then
  begin
    FTrackTimer:= FTrackTimer + SecondsPassed;
    ShowTime(FTrackTimer);
  end;

  { Show Speed. Convert m/s to km/h }
  if Assigned(FVehicle) then
    LabelSpeedValue.Caption:= (FVehicle.ForwardVelocity * 60 * 60 / 1000).ToString(ffFixed, 3, 0);

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

end.

