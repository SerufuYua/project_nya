unit GameViewTravelSpaceshipIndoors;

interface

uses
  Classes, CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform,
  BaseViewTravel, NyaActor, NyaActorChara;

type
  TViewTravelSpaceshipIndoors = class(TBaseViewTravel)
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  protected
    procedure DoActivateSwitch(Sender: TObject); override;
  protected
    procedure GetToGoOut;
    procedure GetToGoToLab;
  end;

var
  ViewTravelSpaceshipIndoors: TViewTravelSpaceshipIndoors;

implementation

uses
  SysUtils, CastleViewport, CastleScene, CastleUtils, CastleVectors,
  CastleComponentSerialize,
  CastleSoundEngine, GameSound,
  NyaSwitch, NyaCastleUtils, NyaWorldCondition,
  GameViewDressing,
  GameViewTravelRoadAsteroid, GameViewMain, GameViewTravelSpaceJunk,
  GameViewTravelSpaceshipLabRoom, GameViewConversation;

procedure TViewTravelSpaceshipIndoors.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapTravel_SpaceshipIndoors.castle-user-interface';

  { set Girl Character }
  MainActor:= Map.DesignedComponent('CharaGirl') as TNyaActorChara;

  { Play music }
  SoundEngine.LoopingChannel[0].Sound:= NamedSound('MusicSpaceJunk');

  inherited;
end;

procedure TViewTravelSpaceshipIndoors.Update(const SecondsPassed: Single;
                                          var HandleInput: boolean);
begin

  inherited;
end;

procedure TViewTravelSpaceshipIndoors.DoActivateSwitch(Sender: TObject);
var
  switch: TNyaSwitch;
begin
  switch:= Sender as TNyaSwitch;
  if NOT Assigned(switch) then Exit;

  inherited;

  Case switch.Name of
  'SwitchGoOut': GetToGoOut;
  'SwitchGoToLab': GetToGoToLab;
  else
    Notifications.Show('There is nothing to do');
  end;
end;

{ ========= ------------------------------------------------------------------ }
{ Get To Go ------------------------------------------------------------------ }
{ ========= ------------------------------------------------------------------ }

procedure TViewTravelSpaceshipIndoors.GetToGoOut;
begin
  GetToGo(ViewTravelSpaceJunk, Vector3(-9.75, 12.4, 130.5),
                               Vector4(0.0, 1.0, 0.0, Deg(0.0)));
end;

procedure TViewTravelSpaceshipIndoors.GetToGoToLab;
begin
  GetToGo(ViewTravelSpaceshipLabRoom);
end;

end.
