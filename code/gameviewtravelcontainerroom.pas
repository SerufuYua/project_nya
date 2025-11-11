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
    procedure SaveCharasCondition; override;
  protected
    procedure ConversationToyA;
    procedure ConversationBed;
    procedure ConversationBoy;
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
  CastleSoundEngine, GameSound,
  NyaSwitch, NyaCastleUtils, NyaWorldCondition,
  GameViewDressing, GameViewPlayGirl, GameViewPlaySolo,
  GameViewPlayTogether, GameViewTravelRoadAsteroid, GameViewMain,
  GameViewConversation;

procedure TViewTravelContainerRoom.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapTravel_ContainerRoom.castle-user-interface';

  { set Girl Character }
  MainActor:= Map.DesignedComponent('CharaGirl') as TNyaActorChara;

  { set Boy Character }
  FActorBoy:= Map.DesignedComponent('CharaBoy') as TNyaActorChara;
  WorldCondition.Boy.Dresser:= FActorBoy.Dresser;
  FActorBoy.Exists:= (WorldCondition.Boy.Location = TBoyLocation.VisitInRoom);

  { Play music }
  SoundEngine.LoopingChannel[0].Sound:= NamedSound('MusicInRoom');

  inherited;
end;

procedure TViewTravelContainerRoom.Update(const SecondsPassed: Single;
                                          var HandleInput: boolean);
begin
  { update Boy visibility }
  WorldCondition.Boy.Visible:= PointVisible(FActorBoy.Translation);

  { update Boy Exists }
  FActorBoy.Exists:= (WorldCondition.Boy.Location = TBoyLocation.VisitInRoom) AND
                     (WorldCondition.Boy.Status = TBoyStatus.FirstTalkDone);

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

  inherited;

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

procedure TViewTravelContainerRoom.SaveCharasCondition;
begin
  inherited;
  FActorBoy.SaveCondition;
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
    Container.PushView(TViewConversation.CreateUntilStopped(
                       messages,
                       nil,
                       nil));
  end
  else
  begin
    SetLength(messages, 1);
    messages[0].FActor:= MainActor;
    messages[0].FMessage:= '<p>Let&apos;s play with my Toy!</p>';
    Container.PushView(TViewConversation.CreateUntilStopped(
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
    Container.PushView(TViewConversation.CreateUntilStopped(
                       messages,
                       nil,
                       nil));
  end
  else
  begin
    SetLength(messages, 1);
    messages[0].FActor:= MainActor;
    messages[0].FMessage:= '<p>Maybe relax a little in bed...</p>';
    Container.PushView(TViewConversation.CreateUntilStopped(
                       messages,
                       {$ifdef FPC}@{$endif}GetToGoBed,
                       nil));
  end;
end;

procedure TViewTravelContainerRoom.ConversationBoy;
var
  messages: TMessages;
begin
  SetLength(messages, 2);
  messages[0].FActor:= MainActor;
  messages[0].FMessage:= '<p>Hey! Let&apos;s play together! Nya!</p>';
  messages[1].FActor:= FActorBoy;
  messages[1].FMessage:= '<p>Ah... what... sure. Let&apos;s play! Nya!</p>';
  Container.PushView(TViewConversation.CreateUntilStopped(
                     messages,
                     {$ifdef FPC}@{$endif}GetToGoBoy,
                     nil));
end;

{ ========= ------------------------------------------------------------------ }
{ Get To Go ------------------------------------------------------------------ }
{ ========= ------------------------------------------------------------------ }

procedure TViewTravelContainerRoom.GetToGoToyA;
begin
  GetToGo(ViewPlayGirl);
end;

procedure TViewTravelContainerRoom.GetToGoBed;
begin
  GetToGo(ViewPlaySolo);
end;

procedure TViewTravelContainerRoom.GetToGoBoy;
begin
  GetToGo(ViewPlayTogether);
end;

procedure TViewTravelContainerRoom.GetToGoOut;
begin
  GetToGo(ViewTravelRoadAsteroid, Vector3(-1.1, 0.1, -21.0),
                                  Vector4(0.0, 1.0, 0.0, Deg(25.0)));
end;

end.
