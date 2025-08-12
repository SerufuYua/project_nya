unit GameViewTravelTest;

interface

uses
  Classes, BaseViewRideNew, NyaActorChara, NyaActorVehicle,
  CastleLivingBehaviors, NyaRandomWalk;

type
  TViewTravelTest = class(TBaseViewRide)
  public
    procedure Start; override;
  end;

var
  ViewTravelTest: TViewTravelTest;

implementation

uses
  CastleSoundEngine, GameSound,
  GameViewDressingMenu;

procedure TViewTravelTest.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapTest.castle-user-interface';

  { set Girl Character }
  MainActor:= Map.DesignedComponent('CharaGirl') as TNyaActorChara;

  { set Vehicle }
  Vehicle:= Map.DesignedComponent('VehicleMoto') as TNyaActorVehicle;

  { Play music }
  SoundEngine.LoopingChannel[0].Sound:= nil;

  inherited;
end;

end.
