unit GameViewTravelTest;

interface

uses
  Classes, BaseViewRideNew,
  NyaActorChara, NyaActorVehicle,
  CastleLivingBehaviors, NyaRandomWalk;

type
  TViewTravelTest = class(TBaseViewRideNew)
  protected
    procedure DoActivateSwitch(Sender: TObject); override;
  public
    procedure Start; override;
  end;

var
  ViewTravelTest: TViewTravelTest;

implementation

uses
  NyaVehicleNavigation, CastleSoundEngine, GameSound,
  GameViewDressingMenu, NyaSwitch;

procedure TViewTravelTest.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapTest.castle-user-interface';

  { set Girl Character }
  MainActor:= Map.DesignedComponent('CharaGirl') as TNyaActorChara;

  { set vehicle Navigation }
  FVehicleNavigation:= Map.DesignedComponent('VehicleNavigation') as TNyaVehicleNavigation;

  { Play music }
  SoundEngine.LoopingChannel[0].Sound:= nil;

  inherited;
end;

procedure TViewTravelTest.DoActivateSwitch(Sender: TObject);
var
  switch: TNyaSwitch;
begin
  switch:= Sender as TNyaSwitch;
  if NOT Assigned(switch) then Exit;

  inherited;

  Case switch.Name of
  'SwitchMoto': SitToVehicle(Map.DesignedComponent('VehicleMoto') as TNyaActorVehicle);
  else
    Notifications.Show('There is nothing to do');
  end;
end;

end.
