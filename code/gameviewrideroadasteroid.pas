unit GameViewRideRoadAsteroid;

interface

uses
  Classes,
  BaseViewRide, NyaActorChara, NyaActorVehicle;

type
  TViewRideRoadAsteroid = class(TBaseViewRide)
  public
    procedure Start; override;
  end;

var
  ViewRideRoadAsteroid: TViewRideRoadAsteroid;

implementation

uses
  SysUtils, CastleViewport, CastleScene, CastleUtils, CastleVectors,
  CastleComponentSerialize,
  CastleSoundEngine, GameSound,
  NyaSwitch, NyaCastleUtils, NyaWorldCondition,
  GameViewDressingMenu, GameViewTravelContainerRoom, GameViewMain,
  GameViewPlayTogether, GameViewConversationMenu;

procedure TViewRideRoadAsteroid.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapRide_RoadAsteroid.castle-user-interface';

  { set Girl Character }
  MainActor:= Map.DesignedComponent('VehicleMoto') as TNyaActorVehicle;

  { Play music }
  SoundEngine.LoopingChannel[0].Sound:= NamedSound('MusicOutdoors');

  inherited;
end;

end.
