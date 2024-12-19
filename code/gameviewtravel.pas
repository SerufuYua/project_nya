unit GameViewTravel;

interface

uses Classes, BaseViewTravel,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleTransform, CastleNotifications, ActorChara, CastleDebugTransform,
  MyThirdPersonCameraNavigation, MySpectatorCameraNavigation,
  MyThirdPersonCharaNavigation, MySwitch;

type
  TViewTravel = class(TBaseViewPlay)
  public
    procedure Start; override;
  end;

var
  ViewTravel: TViewTravel;

implementation

uses
  GameViewLoading, GameViewMain, CastleViewport, CastleScene, MyCastleUtils,
  CastleComponentSerialize, CastleFonts, SysUtils, GameViewDressingMenu;

procedure TViewTravel.Start;
var
  actorScene: TCastleTransformDesign;
begin
  { set map }
  Map.Url:= 'castle-data:/MapTravel_ContainerRoom.castle-transform';

  { Create Girl Character instance }
  actorScene:= Map.DesignedComponent('CharaGirl') as TCastleTransformDesign;
  FActorMain:= TActorChara.Create(actorScene, 'Girl');

  inherited;
end;

end.
