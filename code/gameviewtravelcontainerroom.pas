unit GameViewTravelContainerRoom;

interface

uses
  Classes, CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform,
  BaseViewTravel, NyaActor, NyaActorChara;

type
  TViewTravelContainerRoom = class(TBaseViewTravel)
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  protected
    FActorBoy: TNyaActorChara;
    procedure DoTouchSwitch(const Sender: TObject; Touch: Boolean); override;
    procedure DoActivateSwitch(Sender: TObject); override;
  protected
    procedure ConversationToyA;
    procedure ConversationBed;
    procedure ConversationBoy;
  protected
    procedure TalkToBoyOk;
  protected
    procedure GetToGoToyA;
    procedure GetToGoBed;
    procedure GetToGoBoy;
    procedure GetToGoOut;
  end;

var
  ViewTravelContainerRoom: TViewTravelContainerRoom;

implementation

uses
  SysUtils, CastleViewport, CastleScene, CastleUtils, CastleVectors,
  CastleComponentSerialize,
  NyaSwitch, NyaCastleUtils, NyaWorldCondition,
  GameViewDressingMenu, GameViewPlayGirl, GameViewPlaySolo,
  GameViewPlayTogether, GameViewTravelRoadAsteroid, GameViewMain,
  GameViewConversationMenu;

procedure TViewTravelContainerRoom.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapTravel_ContainerRoom.castle-user-interface';

  { set Girl Character }
  MainActor:= Map.DesignedComponent('CharaGirl') as TNyaActorChara;

  { set Boy Character }
  FActorBoy:= Map.DesignedComponent('CharaBoy') as TNyaActorChara;
  FActorBoy.Exists:= WorldCondition.Boy.Location = TBoyLocation.InRoom;

  inherited;
end;

procedure TViewTravelContainerRoom.Update(const SecondsPassed: Single;
                                          var HandleInput: boolean);
begin

  { update Boy Exists only when area where Boy is not in view }
  if NOT PointVisible(FActorBoy.Translation) then
    FActorBoy.Exists:= WorldCondition.Boy.Location = TBoyLocation.InRoom;

  inherited;
end;

procedure TViewTravelContainerRoom.DoTouchSwitch(const Sender: TObject; Touch: Boolean);
var
  switch: TNyaSwitch;
begin
  inherited;

  switch:= Sender as TNyaSwitch;
  if NOT Assigned(switch) then Exit;

  Case switch.Name of
  'BoySwitch':
    begin
      if Touch then
        FActorBoy.AutoAnimation:= 'GAME.BOY_VISITING.SEAT_WITH_PDA.LOOKING'
      else
        FActorBoy.AutoAnimation:= 'GAME.BOY_VISITING.SEAT_WITH_PDA.WORKING';
    end;
  end;
end;

procedure TViewTravelContainerRoom.DoActivateSwitch(Sender: TObject);
var
  switch: TNyaSwitch;
begin
  switch:= Sender as TNyaSwitch;
  if NOT Assigned(switch) then Exit;

  Case switch.Name of
  'ToyASwitch':
    ConversationToyA;
  'BoySwitch':
    ConversationBoy;
  'GoOutSwitch':
    GetToGoOut;
  'BedSwitch':
    ConversationBed;
  else
    Notifications.Show('There is nothing to do');
  end;
end;

{ ========= ------------------------------------------------------------------ }
{ Conversations -------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

procedure TViewTravelContainerRoom.ConversationToyA;
var
  messages: TMessages;
begin
  if FActorBoy.Exists then
  begin
    SetLength(messages, 1);
    messages[0].FActor:= MainActor;
    messages[0].FMessage:= '<p>Well... Maybe I&apos;ll play sometime ' +
                           'later... when I&apos;ll be alone with myself.</p>';
    Container.PushView(TViewConversationMenu.CreateUntilStopped(
                       messages,
                       nil,
                       nil));
  end
  else
  begin
    SetLength(messages, 1);
    messages[0].FActor:= MainActor;
    messages[0].FMessage:= '<p>Let&apos;s play with my Toy!</p>';
    Container.PushView(TViewConversationMenu.CreateUntilStopped(
                       messages,
                       {$ifdef FPC}@{$endif}GetToGoToyA,
                       nil));
  end;
end;

procedure TViewTravelContainerRoom.ConversationBed;
var
  messages: TMessages;
begin
  if FActorBoy.Exists then
  begin
    SetLength(messages, 1);
    messages[0].FActor:= MainActor;
    messages[0].FMessage:= '<p>I can&apos;t just go to bed when I have guests.</p>';
    Container.PushView(TViewConversationMenu.CreateUntilStopped(
                       messages,
                       nil,
                       nil));
  end
  else
  begin
    SetLength(messages, 1);
    messages[0].FActor:= MainActor;
    messages[0].FMessage:= '<p>Maybe relax a little in bed...</p>';
    Container.PushView(TViewConversationMenu.CreateUntilStopped(
                       messages,
                       {$ifdef FPC}@{$endif}GetToGoBed,
                       nil));
  end;
end;

procedure TViewTravelContainerRoom.ConversationBoy;
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

procedure TViewTravelContainerRoom.TalkToBoyOk;
begin
  WorldCondition.Boy.FirstTalkDone:= True;
end;

{ ========= ------------------------------------------------------------------ }
{ Get To Go ------------------------------------------------------------------ }
{ ========= ------------------------------------------------------------------ }

procedure TViewTravelContainerRoom.GetToGoToyA;
begin
  SaveCharasCondition();
  GetToGo(ViewPlayGirl);
end;

procedure TViewTravelContainerRoom.GetToGoBed;
begin
  SaveCharasCondition();
  GetToGo(ViewPlaySolo);
end;

procedure TViewTravelContainerRoom.GetToGoBoy;
begin
  SaveCharasCondition();
  GetToGo(ViewPlayTogether);
end;

procedure TViewTravelContainerRoom.GetToGoOut;
begin
  SaveCharasCondition();
  GetToGo(ViewTravelRoadAsteroid, Vector3(-1.1, 0.1, -21.0),
                                  Vector4(0.0, 1.0, 0.0, Deg(25.0)));
end;

end.
