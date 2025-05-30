unit GameViewPlaySolo;

interface

uses
  Classes, BaseViewPlay;

type
  TViewPlaySolo = class(TBaseViewPlay)
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  protected
    procedure GetToGoBack; override;
    procedure GiveSpeak(const SecondsPassed: Single);
  end;

var
  ViewPlaySolo: TViewPlaySolo;

implementation

uses
  SysUtils, CastleTransform, CastleUtils, CastleVectors,
  CastleSoundEngine, GameSound,
  GameViewTravelContainerRoom, GameViewSpeakWindow, NyaPlayLogic;

procedure TViewPlaySolo.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapPlay_Solo.castle-user-interface';

  { Play music }
  SoundEngine.LoopingChannel[0].Sound:= NamedSound('MusicInRoom');

  inherited;
end;

procedure TViewPlaySolo.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  GiveSpeak(SecondsPassed);

  inherited;
end;

procedure TViewPlaySolo.GiveSpeak(const SecondsPassed: Single);
begin
  Case FActorsLogic.Status of
  TActorStatus.Start:
    if FActorsLogic.StatusChanged then
      Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'Damn...'));
  TActorStatus.Go:
    if FActorsLogic.StatusChanged then
      Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'A-a-ah.. Damn...'));
  TActorStatus.FastGo:
    if FActorsLogic.StatusChanged then
      Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'Oh... M-m-m... C&apos;mon'));
  TActorStatus.Finish:
    if FActorsLogic.StatusChanged then
      Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'Nyaaaaa... ah...'));
  TActorStatus.Relax:
    ;
  end;
end;

procedure TViewPlaySolo.GetToGoBack;
begin
  GetToGo(ViewTravelContainerRoom, Vector3(0.34, 0.15, -0.23),
                                   Vector4(0.0, 1.0, 0.0, Deg(-32.0)));
end;

end.
