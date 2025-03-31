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
  NyaSwitch, NyaCastleUtils,
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
  FActorBoy.Exists:= WorldCondition.BoyExists AND WorldCondition.BoySearched;

  inherited;
end;

procedure TViewTravelContainerRoom.Update(const SecondsPassed: Single;
                                          var HandleInput: boolean);
begin

  { update Boy Exists only when area where Boy is not in view }
  if NOT PointVisible(FActorBoy.Translation) then
    FActorBoy.Exists:= WorldCondition.BoyExists AND WorldCondition.BoySearched;

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
        FActorBoy.AutoAnimation:= 'GAME.INROOM.SEAT_WITH_PDA.LOOKING'
      else
        FActorBoy.AutoAnimation:= 'GAME.INROOM.SEAT_WITH_PDA.WORKING';
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
    messages[0].FMessage:= '<p>Maybe I&apos;ll play alone</p>';
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
    messages[0].FMessage:= '<p>I don&apos;t think that sleep ' +
                           'when I have guest is good idea</p>';
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
  SetLength(messages, 2);
  messages[0].FActor:= MainActor;
  messages[0].FMessage:= '<p>Hey! Let&apos;s play together! Nya!</p>';
  messages[1].FActor:= FActorBoy;
  messages[1].FMessage:= '<p>Ah... what... sure. Let&apos;s play! Nya!</p>';
  Container.PushView(TViewConversationMenu.CreateUntilStopped(
                     messages,
                     {$ifdef FPC}@{$endif}GetToGoBoy,
                     nil));
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
