unit GameViewPlayGirl;

interface

uses
  Classes, BaseViewPlay;

type
  TViewPlayGirl = class(TBaseViewPlay)
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
  ViewPlayGirl: TViewPlayGirl;

implementation

uses
  SysUtils, CastleTransform, CastleUtils, CastleVectors,
  CastleSoundEngine, GameSound,
  GameViewTravelContainerRoom, GameViewSpeakWindow, NyaPlayLogic;

procedure TViewPlayGirl.Start;
begin
  FTime:= 0.0;
  FSpeakOnceDone:= True;

  { set map }
  Map.Url:= 'castle-data:/MapPlay_GirlToyA.castle-user-interface';

  { Play music }
  SoundEngine.LoopingChannel[0].Sound:= NamedSound('MusicInRoom');

  inherited;
end;

procedure TViewPlayGirl.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  FTime:= FTime + SecondsPassed;
  GiveSpeak;

  inherited;
end;

procedure TViewPlayGirl.GiveSpeak;
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
          Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'A-a-aw!... Yeah >.<'));
          FSpeakOnceDone:= True;
          FTime:= 0.0;
        end;
      end;
    end;
  TActorStatus.Go:
    begin
      if FActorsLogic.StatusChanged then
      begin
        Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'O-o-oh!'));
        FTime:= 0.0;
      end;

      timeOut:= RandomFloatRange(4.0, 16.0);
      if (FTime > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 3);
        case i of
          1: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'M-m-mha! A-a-ah!'));
          2: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'Nya-a-a-a!'));
          3: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'Yeah... Yeah!'));
        end;
        FTime:= 0.0;
      end;
    end;
  TActorStatus.FastGo:
    begin
      if FActorsLogic.StatusChanged then
      begin
        Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'U-u-urf! Aha! Nya!'));
        FTime:= 0.0;
      end;

      timeOut:= RandomFloatRange(3, 8.0);
      if (FTime > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 3);
        case i of
          1: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'Ah! Ah!', 0.125));
          2: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'Ayeah! Nyah!', 0.125));
          3: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'Ayeh-Ayeh-Ayeh!', 0.125));
        end;
        FTime:= 0.0;
      end;
    end;
  TActorStatus.Finish:
    if FActorsLogic.StatusChanged then
      Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'A-U-u-ufh! NYA-A-A-A!'));
  TActorStatus.Relax:
    begin
      if FActorsLogic.StatusChanged then
        FTime:= 0.0;

      timeOut:= RandomFloatRange(12, 32.0);
      if (FTime > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 3);
        case i of
          1: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'm-m-m...', 1.0));
          2: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'ha-a-ah... ha-a-ah...', 1.0));
          3: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], '...want to more...', 1.0));
        end;
        FTime:= 0.0;
      end;
    end;
  end;
end;

procedure TViewPlayGirl.GetToGoBack;
begin
  GetToGo(ViewTravelContainerRoom, Vector3(-0.9, 0.15, -0.84),
                                   Vector4(0.0, 1.0, 0.0, Deg(25.0)));
end;

end.
