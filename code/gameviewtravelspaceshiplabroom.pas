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
    FWakeUpCount: Integer;
    FActorBoy: TNyaActorChara;
    FPositionBoyTable, FPositionBoyBed: TCastleTransform;
    procedure DoTouchSwitch(const Sender: TObject; Touch: Boolean); override;
    procedure DoActivateSwitch(Sender: TObject); override;
  protected
    procedure ConversationBoyTable;
    procedure ConversationBoyBed;
  protected
    procedure IncWakeUpCount;
    procedure TalkToBoyTable;
  protected
    procedure GetToGoBoy;
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
  GameViewPlayTogetherLab, GameViewConversation;

procedure TViewTravelSpaceshipLabRoom.Start;
begin
  FWakeUpCount:= 0;

  { set map }
  Map.Url:= 'castle-data:/MapTravel_SpaceshipLabRoom.castle-user-interface';

  { set Girl Character }
  MainActor:= Map.DesignedComponent('CharaGirl') as TNyaActorChara;

  { set Boy Character }
  FActorBoy:= Map.DesignedComponent('CharaBoy') as TNyaActorChara;
  WorldCondition.Boy.Dresser:= FActorBoy.Dresser;
  FActorBoy.Exists:= (WorldCondition.Boy.Location in
                      [TBoyLocation.HomeSleep, TBoyLocation.HomeWorking]) AND
                      (TBoyStatus.InSearch in WorldCondition.Boy.Status);

  FPositionBoyTable:= Map.DesignedComponent('PositionBoyTable') as TCastleTransform;
  FPositionBoyBed:= Map.DesignedComponent('PositionBoyBed') as TCastleTransform;

  { Play music }
  SoundEngine.LoopingChannel[0].Sound:= NamedSound('MusicInRoom');

  inherited;
end;

procedure TViewTravelSpaceshipLabRoom.Update(const SecondsPassed: Single;
                                             var HandleInput: boolean);
begin
  { update Plane visibility }
  WorldCondition.Boy.Visible:= PointVisible(FPositionBoyTable.Translation) OR
                               PointVisible(FPositionBoyBed.Translation);

  { update Space Plane Exists }
  FActorBoy.Exists:= (WorldCondition.Boy.Location in
                      [TBoyLocation.HomeSleep, TBoyLocation.HomeWorking]) AND
                      (TBoyStatus.InSearch in WorldCondition.Boy.Status);

  { set Boy to place }
  if ((FActorBoy.Parent <> FPositionBoyBed) AND (WorldCondition.Boy.Location = TBoyLocation.HomeSleep)) then
  begin
    FActorBoy.Translation:= TVector3.Zero;
    FActorBoy.Rotation:= TVector4.Zero;
    FActorBoy.Parent:= FPositionBoyBed;
    FActorBoy.AutoAnimation:= 'GAME.BOY_HOME.SLEEP';
  end
  else if ((FActorBoy.Parent <> FPositionBoyTable) AND (WorldCondition.Boy.Location = TBoyLocation.HomeWorking)) then
  begin
    FActorBoy.Translation:= TVector3.Zero;
    FActorBoy.Rotation:= TVector4.Zero;
    FActorBoy.Parent:= FPositionBoyTable;
    FActorBoy.AutoAnimation:= 'GAME.BOY_HOME.SEAT_WITH_PC.WORKING';
  end;

  inherited;
end;

procedure TViewTravelSpaceshipLabRoom.DoTouchSwitch(const Sender: TObject; Touch: Boolean);
var
  switch: TNyaSwitch;
begin
  inherited;

  switch:= Sender as TNyaSwitch;
  if NOT Assigned(switch) then Exit;

  Case switch.Name of
  'SwitchTable':
    begin
      if (FActorBoy.Parent = FPositionBoyTable) then
      begin
        if Touch then
          FActorBoy.AutoAnimation:= 'GAME.BOY_HOME.SEAT_WITH_PC.LOOKING'
        else
          FActorBoy.AutoAnimation:= 'GAME.BOY_HOME.SEAT_WITH_PC.WORKING';
      end;
    end;
  end;
end;

procedure TViewTravelSpaceshipLabRoom.DoActivateSwitch(Sender: TObject);
var
  switch: TNyaSwitch;
begin
  switch:= Sender as TNyaSwitch;
  if NOT Assigned(switch) then Exit;

  inherited;

  Case switch.Name of
  'SwitchTable': ConversationBoyTable;
  'SwitchBed': ConversationBoyBed;
  'SwitchGoOut': GetToGoOut;
  else
    Notifications.Show('There is nothing to do');
  end;
end;

{ ========= ------------------------------------------------------------------ }
{ Conversations -------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

procedure TViewTravelSpaceshipLabRoom.ConversationBoyTable;
var
  messages: TMessages;
begin
  if (FActorBoy.Exists AND (FActorBoy.Parent = FPositionBoyTable)) then
  begin
    if (TBoyStatus.FirstTalkDone in WorldCondition.Boy.Status) then
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
    end else
    begin
      SetLength(messages, 9);
      messages[0].FActor:= MainActor;
      messages[0].FMessage:= '<p>I found You! It&apos;s super cool!</p>';
      messages[1].FActor:= FActorBoy;
      messages[1].FMessage:= '<p>Yo! Now You found me first. You look nya!</p>';
      messages[2].FActor:= MainActor;
      messages[2].FMessage:= '<p>Ha! Thanks! And you&apos;re really super cute!</p>';
      messages[3].FActor:= FActorBoy;
      messages[3].FMessage:= '<p>Have you been search me for a long time?</p>';
      messages[4].FActor:= MainActor;
      messages[4].FMessage:= '<p>Well it wasn&apos;t so long. I know we should be together, but where are we?</p>';
      messages[5].FActor:= FActorBoy;
      messages[5].FMessage:= '<p>What do you mean where? We&apos;re in post-mortalis. We&apos;re dead, have you forgotten?</p>';
      messages[6].FActor:= MainActor;
      messages[6].FMessage:= '<p>Um... I remember how you are leaving. It was... it was terrible. I don&apos;t remember what happened to me after that. I just remember waking up, knowing you&apos;d be here somewhere.</p>';
      messages[7].FActor:= FActorBoy;
      messages[7].FMessage:= '<p>If you remember, I warned you many times that could happen that I would leave much earlier than you. You should have been ready for this.</p>';
      messages[8].FActor:= MainActor;
      messages[8].FMessage:= '<p>Yes, I remember that... I remember... But in the end, here we are. We are together again! I am ultra happy!</p>';
      Container.PushView(TViewConversation.CreateUntilStopped(
                         messages,
                         {$ifdef FPC}@{$endif}TalkToBoyTable,
                         nil));
    end;
  end
  else
  begin
    SetLength(messages, 1);
    messages[0].FActor:= MainActor;
    messages[0].FMessage:= '<p>Looks like there someone working with... something...</p>';
    Container.PushView(TViewConversation.CreateUntilStopped(
                       messages,
                       nil,
                       nil));
  end;
end;

procedure TViewTravelSpaceshipLabRoom.ConversationBoyBed;
var
  messages: TMessages;

begin
  if (FActorBoy.Exists AND (FActorBoy.Parent = FPositionBoyBed)) then
  begin
    if (FWakeUpCount < 2) then
    begin
      SetLength(messages, 1);
      messages[0].FActor:= MainActor;
      messages[0].FMessage:= '<p>Maybe htere&apos;s no need to wake up he...</p>';
      Container.PushView(TViewConversation.CreateUntilStopped(
                         messages,
                         {$ifdef FPC}@{$endif}IncWakeUpCount,
                         nil));
    end
    else if (FWakeUpCount = 2) then
    begin
      SetLength(messages, 1);
      messages[0].FActor:= MainActor;
      messages[0].FMessage:= '<p>Maybe I can... ah am...</p>';
      Container.PushView(TViewConversation.CreateUntilStopped(
                         messages,
                         {$ifdef FPC}@{$endif}IncWakeUpCount,
                         nil));
    end
    else
    begin
      SetLength(messages, 1);
      messages[0].FActor:= MainActor;
      messages[0].FMessage:= '<p>And however I want... I want it...</p>' +
                             '<p>Heeey! Wake up! I want to play!</p>';
      Container.PushView(TViewConversation.CreateUntilStopped(
                         messages,
                         {$ifdef FPC}@{$endif}GetToGoBoy,
                         nil));
    end;
  end
  else
  begin
    SetLength(messages, 1);
    messages[0].FActor:= MainActor;
    messages[0].FMessage:= '<p>Don&apos;t go to another&apos;s bed. It&apos;s not civilly.</p>';
    Container.PushView(TViewConversation.CreateUntilStopped(
                       messages,
                       nil,
                       nil));
  end;
end;

{ ========= ------------------------------------------------------------------ }
{ TalkTo Result -------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

procedure TViewTravelSpaceshipLabRoom.IncWakeUpCount;
begin
  if (TBoyStatus.FirstTalkDone in WorldCondition.Boy.Status) then
    FWakeUpCount:= FWakeUpCount + 1;
end;

procedure TViewTravelSpaceshipLabRoom.TalkToBoyTable;
begin
  WorldCondition.Boy.Status:= WorldCondition.Boy.Status + [TBoyStatus.FirstTalkDone];
end;

{ ========= ------------------------------------------------------------------ }
{ Get To Go ------------------------------------------------------------------ }
{ ========= ------------------------------------------------------------------ }

procedure TViewTravelSpaceshipLabRoom.GetToGoBoy;
begin
  WorldCondition.Boy.Location:= TBoyLocation.HomeSleep;
  GetToGo(ViewPlayTogetherLab);
end;

procedure TViewTravelSpaceshipLabRoom.GetToGoOut;
begin
  GetToGo(ViewTravelSpaceshipIndoors, Vector3(15.0, 0.1, -28.6),
                                      Vector4(0.0, 1.0, 0.0, Deg(-90.0)));
end;

end.
