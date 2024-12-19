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
  protected
    procedure TouchSwitch(const Sender: TObject; Touch: Boolean); override;
    procedure ActivateSwitch(Sender: TObject); override;
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

procedure TViewTravel.TouchSwitch(const Sender: TObject; Touch: Boolean);
begin
  inherited;
  { any additional actions for some specail switch }
end;

procedure TViewTravel.ActivateSwitch(Sender: TObject);
var
  switch: TMySwitch;
begin
  switch:= Sender as TMySwitch;
  if NOT Assigned(switch) then Exit;

  Case switch.Name of
  'MySwitchTest':
    begin
      Notifications.Show('Lets play with my Toy!');
    end;
  end;
end;

end.
