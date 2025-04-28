unit GameViewTravelRoadAsteroid;

interface

uses
  Classes, CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform,
  BaseViewTravel, NyaActor, NyaActorChara;

type
  TViewTravelRoadAsteroid = class(TBaseViewTravel)
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  protected
    FActorBoy: TNyaActorChara;
    FActorSpacePlane: TNyaActor;
    procedure DoTouchSwitch(const Sender: TObject; Touch: Boolean); override;
    procedure DoActivateSwitch(Sender: TObject); override;
  protected
    procedure ConversationSpacePlane;
  protected
    procedure TalkToPlaneOk;
  protected
    procedure GetToGoHome;
  end;

var
  ViewTravelRoadAsteroid: TViewTravelRoadAsteroid;

implementation

uses
  SysUtils, CastleViewport, CastleScene, CastleUtils, CastleVectors,
  CastleComponentSerialize,
  NyaSwitch, NyaCastleUtils, NyaWorldCondition,
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
  FActorSpacePlane.Exists:= WorldCondition.SpacePlaneExists;

  { set Boy Character }
  FActorBoy:= Map.DesignedComponent('CharaBoy') as TNyaActorChara;
  FActorBoy.Exists:= WorldCondition.Boy.Location = TBoyLocation.InHovel;

  inherited;
end;

procedure TViewTravelRoadAsteroid.Update(const SecondsPassed: Single;
                                          var HandleInput: boolean);
begin

  { update Boy Exists only when area where Boy is not in view }
  if NOT PointVisible(FActorSpacePlane.Translation) then
    FActorSpacePlane.Exists:= WorldCondition.SpacePlaneExists;

  { update Boy Exists only when area where Boy is not in view }
  if NOT PointVisible(FActorBoy.Translation) then
    FActorBoy.Exists:= WorldCondition.Boy.Location = TBoyLocation.InHovel;

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
  if WorldCondition.Boy.FirstTalkDone then
  begin
    SetLength(messages, 3);
    messages[0].FActor:= MainActor;
    messages[0].FMessage:= '<p>Hi! How are you!</p>';
    messages[1].FActor:= FActorSpacePlane;
    messages[1].FMessage:= '<p>Fine! How are you?</p>';
    messages[2].FActor:= MainActor;
    messages[2].FMessage:= '<p>Nya!</p>';
    Container.PushView(TViewConversationMenu.CreateUntilStopped(
                       messages,
                       nil,
                       nil));
  end else
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
                       {$ifdef FPC}@{$endif}TalkToPlaneOk,
                       nil));
  end;
end;

{ ========= ------------------------------------------------------------------ }
{ TalkTo Result -------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

procedure TViewTravelRoadAsteroid.TalkToPlaneOk;
begin
  WorldCondition.Boy.Searched:= True;
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
