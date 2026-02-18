unit GameViewTravelSpaceshipIndoors;

interface

uses
  Classes, CastleUIControls, CastleControls, CastleScene, CastleKeysMouse,
  CastleTransform, BaseViewTravel, NyaActor, NyaActorChara, NyaRandomSwitch,
  X3DNodes;

type
  TViewTravelSpaceshipIndoors = class(TBaseViewTravel)
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  protected
    FLightNodeS: TSpotLightNode;
    FLightNodeP: TPointLightNode;
    FIntensityS, FIntensityP: Single;
    FLightOFF, FLightON: TCastleScene;
    FRandomSwitch: TNyaRandomSwitch;
    procedure DoActivateSwitch(Sender: TObject); override;
    procedure DoRandomSwitch(const AEnable: Boolean);
  protected
    procedure GetToGoOut;
    procedure GetToGoToLab;
  end;

var
  ViewTravelSpaceshipIndoors: TViewTravelSpaceshipIndoors;

implementation

uses
  SysUtils, CastleViewport, CastleUtils, CastleVectors,
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

  { Bad Light }
  with (Map.DesignedComponent('SpaceshipIndoors') as TCastleScene) do
  begin
    FLightNodeS:= Node(TSpotLightNode,  'Light4S') as TSpotLightNode;
    FLightNodeP:= Node(TPointLightNode, 'Light4P') as TPointLightNode;
  end;
  FIntensityS:= FLightNodeS.Intensity;
  FIntensityP:= FLightNodeP.Intensity;
  FLightOFF:= Map.DesignedComponent('LightOFF') as TCastleScene;
  FLightON:= Map.DesignedComponent('LightON') as TCastleScene;
  FRandomSwitch:= Map.DesignedComponent('RandomSwitch') as TNyaRandomSwitch;
  FRandomSwitch.OnSwitch:= {$ifdef FPC}@{$endif}DoRandomSwitch;

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

procedure TViewTravelSpaceshipIndoors.DoRandomSwitch(const AEnable: Boolean);
var
  intS, intP: Single;
begin
  if AEnable then
  begin
    intS:= FIntensityS;
    intP:= FIntensityP;
  end
  else
  begin
    intS:= 0.0;
    intP:= 0.0;
  end;

  FLightOFF.Visible:= NOT AEnable;
  FLightON.Visible:= AEnable;

  FLightNodeS.Intensity:= intS;
  FLightNodeP.Intensity:= intP;
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
