unit GameViewPlaySolo;

interface

uses
  Classes, BaseViewPlay;

type
  TViewPlaySolo = class(TBaseViewPlay)
  protected
    FTime: Single;
    FSpeakOnceDone: Boolean;

    procedure GetToGoBack; override;
    procedure GiveSpeak;
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  end;

var
  ViewPlaySolo: TViewPlaySolo;

implementation

uses
  SysUtils, CastleTransform, CastleUtils, CastleVectors,
  CastleSoundEngine, GameSound,
  GameViewTravelContainerRoom, GameViewSpeakWindow, NyaSpeechBubble, NyaPlayLogic;

procedure TViewPlaySolo.Start;
begin
  FTime:= 0.0;
  FSpeakOnceDone:= True;

  { set map }
  Map.Url:= 'castle-data:/MapPlay_Solo.castle-user-interface';

  { Play music }
  SoundEngine.LoopingChannel[0].Sound:= NamedSound('MusicInRoom');

  inherited;
end;

procedure TViewPlaySolo.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  FTime:= FTime + SecondsPassed;
  GiveSpeak;

  inherited;
end;

procedure TViewPlaySolo.GiveSpeak;
var
  timeOut: Single;
  i: integer;
begin
  Case FActorsLogic.Status of
  TActorStatus.Start:
    begin
      if FActorsLogic.StatusChanged then
      begin
        FTime:= 0.0;
        FSpeakOnceDone:= False;
      end;

      if NOT FSpeakOnceDone then
      begin
        timeOut:= RandomFloatRange(1.0, 3.0);
        if (FTime > timeOut) then
        begin
          Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'Damn...'));
          FSpeakOnceDone:= True;
          FTime:= 0.0;
        end;
      end;
    end;
  TActorStatus.Go:
    begin
      if FActorsLogic.StatusChanged then
      begin
        Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'A-ah... Damn...'));
        FTime:= 0.0;
      end;

      timeOut:= RandomFloatRange(4.0, 16.0);
      if (FTime > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 3);
        case i of
          1: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'A-a-ah...'));
          2: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'M-m-mph...'));
          3: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'Yeah...'));
        end;
        FTime:= 0.0;
      end;
    end;
  TActorStatus.FastGo:
    begin
      if FActorsLogic.StatusChanged then
      begin
        Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'O-o-oh!.. Come on!'));
        FTime:= 0.0;
      end;

      timeOut:= RandomFloatRange(3, 8.0);
      if (FTime > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 3);
        case i of
          1: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'Argh!.. F-f-f!.. ', 0.125));
          2: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'A-ah! Ayeah!', 0.125));
          3: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'Come on!', 0.125));
        end;
        FTime:= 0.0;
      end;
    end;
  TActorStatus.Finish:
    if FActorsLogic.StatusChanged then
      Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'Nyaaaaa!!. ah!..'));
  TActorStatus.Relax:
    begin
      if FActorsLogic.StatusChanged then
        FTime:= 8.0;

      timeOut:= RandomFloatRange(12, 24.0);
      if (FTime > timeOut) then
      begin
        Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'Zzz...', 1.0));
        FTime:= 0.0;
      end;

    end;
  end;
end;

procedure TViewPlaySolo.GetToGoBack;
begin
  GetToGo(ViewTravelContainerRoom, Vector3(0.34, 0.15, -0.23),
                                   Vector4(0.0, 1.0, 0.0, Deg(-32.0)));
end;

end.
