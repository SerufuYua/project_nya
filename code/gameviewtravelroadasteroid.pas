unit GameViewTravelRoadAsteroid;

interface

uses
  Classes, CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform,
  BaseViewTravel, NyaActor;

type
  TViewTravelRoadAsteroid = class(TBaseViewTravel)
  public
    procedure Start; override;
  protected
    FActorSpacePlane: TNyaActor;
    procedure DoTouchSwitch(const Sender: TObject; Touch: Boolean); override;
    procedure DoActivateSwitch(Sender: TObject); override;
  protected
    procedure ConversationSpacePlane;
  protected
    procedure GetToGoHome;
  end;

var
  ViewTravelRoadAsteroid: TViewTravelRoadAsteroid;

implementation

uses
  SysUtils, CastleViewport, CastleScene, CastleUtils, CastleVectors,
  CastleComponentSerialize,
  NyaActorChara, NyaSwitch, NyaCastleUtils,
  GameViewDressingMenu, GameViewTravelContainerRoom, GameViewMain,
  GameViewConversationMenu;

procedure TViewTravelRoadAsteroid.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapTravel_RoadAsteroid.castle-user-interface';

  { set Girl Character }
  MainActor:= Map.DesignedComponent('CharaGirl') as TNyaActorChara;

  { set Space Plane Character }
  FActorSpacePlane:= Map.DesignedComponent('SpacePlane') as TNyaActor;

  inherited;
end;

procedure TViewTravelRoadAsteroid.DoTouchSwitch(const Sender: TObject; Touch: Boolean);
var
  switch: TNyaSwitch;
begin
  inherited;

  switch:= Sender as TNyaSwitch;
  if NOT Assigned(switch) then Exit;
end;

procedure TViewTravelRoadAsteroid.DoActivateSwitch(Sender: TObject);
var
  switch: TNyaSwitch;
begin
  switch:= Sender as TNyaSwitch;
  if NOT Assigned(switch) then Exit;

  Case switch.Name of
  'GoRoomSwitch':
    GetToGoHome;
  'SpacePlaneSwitch':
    ConversationSpacePlane;
  else
    Notifications.Show('There is nothing to do');
  end;
end;

{ ========= ------------------------------------------------------------------ }
{ Conversations -------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

procedure TViewTravelRoadAsteroid.ConversationSpacePlane;
var
  messages: TMessages;
begin
  SetLength(messages, 5);
  messages[0].FActor:= MainActor;
  messages[0].FMessage:= '<p>Hi!</p><p>How are you?!</p>';
  messages[1].FActor:= FActorSpacePlane;
  messages[1].FMessage:= '<p>Yo! I&apos;m fine.</p>';
  messages[2].FActor:= MainActor;
  messages[2].FMessage:= '<p>Have you seen my friend?</p>';
  messages[3].FActor:= FActorSpacePlane;
  messages[3].FMessage:= '<p>Why are you asking me? I&apos;m not a spy for him. ' +
                         'I&apos;m just his vehicle. Look for him yourself...</p>' +
                         '<p>Maybe he&apos;s at your home, I don&apos;t know.</p>';
  messages[4].FActor:= MainActor;
  messages[4].FMessage:= '<p>Thanks! See ya!</p>';
  Container.PushView(TViewConversationMenu.CreateUntilStopped(
                     messages,
                     nil,
                     nil));
end;

{ ========= ------------------------------------------------------------------ }
{ Get To Go ------------------------------------------------------------------ }
{ ========= ------------------------------------------------------------------ }

procedure TViewTravelRoadAsteroid.GetToGoHome;
begin
  SaveCharasCondition();
  GetToGo(ViewTravelContainerRoom, Vector3(-0.66, 0.15, 1.66),
                                   Vector4(0.0, 1.0, 0.0, Deg(150.0)));
end;

end.
