unit GameViewTravelSpaceJunk;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform,
  BaseViewRide, NyaActor, NyaActorChara, NyaActorVehicle, NyaVehicleNavigation;

type
  TViewTravelSpaceJunk = class(TBaseViewRide)
  public
    procedure Start; override;
  protected
    procedure DoActivateSwitch(Sender: TObject); override;
  protected
    procedure GetToGoShip;
  end;

var
  ViewTravelSpaceJunk: TViewTravelSpaceJunk;

implementation

uses
  SysUtils, CastleViewport, CastleScene, CastleUtils, CastleVectors,
  CastleComponentSerialize,
  CastleSoundEngine, GameSound,
  NyaSwitch, NyaCastleUtils, NyaWorldCondition,
  GameViewDressing, GameViewPlayGirl, GameViewPlaySolo,
  GameViewPlayTogether, GameViewTravelRoadAsteroid, GameViewMain,
  GameViewConversation;

procedure TViewTravelSpaceJunk.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapTravel_SpaceJunk.castle-user-interface';

  { set Girl Character }
  MainActor:= Map.DesignedComponent('CharaGirl') as TNyaActorChara;

  { set vehicle Navigation }
  FVehicleNavigation:= Map.DesignedComponent('VehicleNavigation') as TNyaVehicleNavigation;

  { Play music }
  SoundEngine.LoopingChannel[0].Sound:= NamedSound('MusicOutdoors');

  inherited;
end;

procedure TViewTravelSpaceJunk.DoActivateSwitch(Sender: TObject);
var
  switch: TNyaSwitch;
begin
  switch:= Sender as TNyaSwitch;
  if NOT Assigned(switch) then Exit;

  inherited;

  Case switch.Name of
  'GoShipSwitch':
    GetToGoShip;
  'SwitchMoto':
    SitToVehicle(Map.DesignedComponent('VehicleMoto') as TNyaActorVehicle);
  else
    Notifications.Show('There is nothing to do');
  end;
end;

{ ========= ------------------------------------------------------------------ }
{ Get To Go ------------------------------------------------------------------ }
{ ========= ------------------------------------------------------------------ }

procedure TViewTravelSpaceJunk.GetToGoShip;
begin

end;

end.

