unit GameViewTravelSpaceshipLabRoom;

interface

uses
  Classes, CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform,
  BaseViewTravel, NyaActor, NyaActorChara;

type
  TViewTravelSpaceshipLabRoom = class(TBaseViewTravel)
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  protected
    procedure DoActivateSwitch(Sender: TObject); override;
  protected
    procedure GetToGoOut;
  end;

var
  ViewTravelSpaceshipLabRoom: TViewTravelSpaceshipLabRoom;

implementation

uses
  SysUtils, CastleViewport, CastleScene, CastleUtils, CastleVectors,
  CastleComponentSerialize,
  CastleSoundEngine, GameSound,
  NyaSwitch, NyaCastleUtils, NyaWorldCondition,
  GameViewDressing,
  GameViewTravelRoadAsteroid, GameViewMain, GameViewTravelSpaceshipIndoors,
  GameViewConversation;

procedure TViewTravelSpaceshipLabRoom.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapTravel_SpaceshipLabRoom.castle-user-interface';

  { set Girl Character }
  MainActor:= Map.DesignedComponent('CharaGirl') as TNyaActorChara;

  { Play music }
  SoundEngine.LoopingChannel[0].Sound:= NamedSound('MusicInRoom');

  inherited;
end;

procedure TViewTravelSpaceshipLabRoom.Update(const SecondsPassed: Single;
                                             var HandleInput: boolean);
begin

  inherited;
end;

procedure TViewTravelSpaceshipLabRoom.DoActivateSwitch(Sender: TObject);
var
  switch: TNyaSwitch;
begin
  switch:= Sender as TNyaSwitch;
  if NOT Assigned(switch) then Exit;

  inherited;

  Case switch.Name of
  'SwitchGoOut': GetToGoOut;
  else
    Notifications.Show('There is nothing to do');
  end;
end;

{ ========= ------------------------------------------------------------------ }
{ Get To Go ------------------------------------------------------------------ }
{ ========= ------------------------------------------------------------------ }

procedure TViewTravelSpaceshipLabRoom.GetToGoOut;
begin
  GetToGo(ViewTravelSpaceshipIndoors, Vector3(16.0, 0.025, -28.6),
                                      Vector4(0.0, 1.0, 0.0, Deg(-90.0)));
end;

end.
