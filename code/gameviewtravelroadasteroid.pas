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
    procedure ConversationBoy;
  protected
    procedure TalkToPlaneOk;
    procedure TalkToBoyOk;
  protected
    procedure GetToGoBoy;
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
  GameViewPlayTogether, GameViewConversationMenu;

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
  { update Boy visibility }
  WorldCondition.Boy.Visible:= PointVisible(FActorBoy.Translation) OR
                               PointVisible(FActorSpacePlane.Translation);

  { update Space Plane Exists }
  FActorSpacePlane.Exists:= WorldCondition.SpacePlaneExists;

  { update Boy Exists }
  FActorBoy.Exists:= (WorldCondition.Boy.Location = TBoyLocation.InHovel);

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
  'BoySwitch':
    ConversationBoy;
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

procedure TViewTravelRoadAsteroid.ConversationBoy;
var
  messages: TMessages;
begin
  if WorldCondition.Boy.FirstTalkDone then
  begin
    SetLength(messages, 2);
    messages[0].FActor:= MainActor;
    messages[0].FMessage:= '<p>Hey! Let&apos;s play together! Nya!</p>';
    messages[1].FActor:= FActorBoy;
    messages[1].FMessage:= '<p>Ah... what... sure. Let&apos;s play! Nya!</p>';
    Container.PushView(TViewConversationMenu.CreateUntilStopped(
                       messages,
                       {$ifdef FPC}@{$endif}GetToGoBoy,
                       nil));
  end else
  begin
    SetLength(messages, 9);
    messages[0].FActor:= MainActor;
    messages[0].FMessage:= '<p>Hi Nya! Super cool that you stopped by!</p>';
    messages[1].FActor:= FActorBoy;
    messages[1].FMessage:= '<p>Yo! You look nya!</p>';
    messages[2].FActor:= MainActor;
    messages[2].FMessage:= '<p>Ha! Thanks! And you&apos;re really super cute!</p>';
    messages[3].FActor:= FActorBoy;
    messages[3].FMessage:= '<p>Have you been waiting for me for a long time?</p>';
    messages[4].FActor:= MainActor;
    messages[4].FMessage:= '<p>I don&apos;t know, maybe forever or maybe a few minutes. I know we should be together, but where are we?</p>';
    messages[5].FActor:= FActorBoy;
    messages[5].FMessage:= '<p>What do you mean where? We&apos;re in post-mortalis. We&apos;re dead, have you forgotten?</p>';
    messages[6].FActor:= MainActor;
    messages[6].FMessage:= '<p>Um... I remember how you are leaving. It was... it was terrible. I don&apos;t remember what happened to me after that. I just remember waking up, knowing you&apos;d be here somewhere.</p>';
    messages[7].FActor:= FActorBoy;
    messages[7].FMessage:= '<p>If you remember, I warned you many times that could happen that I would leave much earlier than you. You should have been ready for this.</p>';
    messages[8].FActor:= MainActor;
    messages[8].FMessage:= '<p>Yes, I remember that... I remember... But in the end, here we are. We are together again! I am ultra happy!</p>';
    Container.PushView(TViewConversationMenu.CreateUntilStopped(
                       messages,
                       {$ifdef FPC}@{$endif}TalkToBoyOk,
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

procedure TViewTravelRoadAsteroid.TalkToBoyOk;
begin
  WorldCondition.Boy.FirstTalkDone:= True;
end;

{ ========= ------------------------------------------------------------------ }
{ Get To Go ------------------------------------------------------------------ }
{ ========= ------------------------------------------------------------------ }

procedure TViewTravelRoadAsteroid.GetToGoBoy;
begin
  WorldCondition.Boy.Location:= TBoyLocation.InRoom;
  SaveCharasCondition();
  GetToGo(ViewPlayTogether);
end;

procedure TViewTravelRoadAsteroid.GetToGoHome;
begin
  SaveCharasCondition();
  GetToGo(ViewTravelContainerRoom, Vector3(-0.66, 0.15, 1.66),
                                   Vector4(0.0, 1.0, 0.0, Deg(150.0)));
end;

end.
