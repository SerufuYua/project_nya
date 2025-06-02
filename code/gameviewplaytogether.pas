unit GameViewPlayTogether;

interface

uses
  Classes, BaseViewPlay;

type
  TViewPlayTogether = class(TBaseViewPlay)
  protected
    FTime1, FTime2: Single;
    FSpeakOnceDone: Boolean;

    procedure GetToGoBack; override;
    procedure GiveSpeak;
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  end;

var
  ViewPlayTogether: TViewPlayTogether;

implementation

uses
  SysUtils, CastleTransform, CastleUtils, CastleVectors,
  CastleSoundEngine, GameSound,
  GameViewTravelContainerRoom, GameViewSpeakWindow, NyaPlayLogic;

procedure TViewPlayTogether.Start;
begin
  FTime1:= 0.0;
  FTime2:= 0.0;
  FSpeakOnceDone:= True;

  { set map }
  Map.Url:= 'castle-data:/MapPlay_Together.castle-user-interface';

  { Play music }
  SoundEngine.LoopingChannel[0].Sound:= NamedSound('MusicTogether');

  inherited;
end;

procedure TViewPlayTogether.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  FTime1:= FTime1 + SecondsPassed;
  FTime2:= FTime2 + SecondsPassed;
  GiveSpeak;

  inherited;
end;

procedure TViewPlayTogether.GiveSpeak;
var
  timeOut: Single;
  i: integer;
begin
  Case FActorsLogic.Status of
  TActorStatus.Wait:
    begin
      if FActorsLogic.StatusChanged then
      begin
        FTime1:= 20.0;
        FTime2:= 10.0;
      end;

      timeOut:= RandomFloatRange(24, 36.0);
      if (FTime1 > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 4);
        case i of
          1: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'nya...'));
          2: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'c&apos;mon...'));
          3: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'ah... so big...'));
          4: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'cutie...'));
        end;
        FTime1:= 0.0;
      end;

      timeOut:= RandomFloatRange(24, 36.0);
      if (FTime2 > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 3);
        case i of
          1: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[1], 'nya...'));
          2: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[1], 'nya-nya...'));
          3: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[1], 'aaah...'));
        end;
        FTime2:= 0.0;
      end;
    end;
  TActorStatus.Start:
    begin
      if FActorsLogic.StatusChanged then
      begin
        FTime1:= 0.0;
        FSpeakOnceDone:= False;
      end;

      if NOT FSpeakOnceDone then
      begin
        timeOut:= RandomFloatRange(1.0, 3.0);
        if (FTime1 > timeOut) then
        begin
          Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'a-A-A-yeah!'));
          Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[1], 'm-m-ha'));
          FSpeakOnceDone:= True;
          FTime1:= 0.0;
        end;
      end;
    end;
  TActorStatus.Go:
    begin
      if FActorsLogic.StatusChanged then
      begin
        Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'O-o-oh! A-a-A-ah!'));
        FTime1:= 0.0;
        FTime2:= 0.0;
      end;

      timeOut:= RandomFloatRange(4.0, 16.0);
      if (FTime1 > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 3);
        case i of
          1: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'M-m-mha! A-a-ah!'));
          2: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'Nya-a-a-a!'));
          3: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'Yeah... Yeah!'));
        end;
        FTime1:= 0.0;
      end;

      timeOut:= RandomFloatRange(6.0, 18.0);
      if (FTime2 > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 3);
        case i of
          1: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[1], 'Ha-ah... Nya-ah'));
          2: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[1], 'M-m-mh...'));
          3: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[1], 'Ah Nya...'));
        end;
        FTime2:= 0.0;
      end;
    end;
  TActorStatus.FastGo:
    begin
      if FActorsLogic.StatusChanged then
      begin
        Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'Yeah! Faster! Nya!'));
        FTime1:= 0.0;
        FTime2:= 0.0;
      end;

      timeOut:= RandomFloatRange(3, 8.0);
      if (FTime1 > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 4);
        case i of
          1: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'Ah-Ah-Ah!!', 0.125));
          2: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'Ye-Ye-Yeah!!', 0.125));
          3: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'M-m-m-mhAh!!', 0.125));
          4: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'Ny-a-a-ah!!', 0.125));
        end;
        FTime1:= 0.0;
      end;

      timeOut:= RandomFloatRange(3, 8.0);
      if (FTime2 > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 3);
        case i of
          1: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[1], 'Mhah-mhah-mhah!', 0.125));
          2: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[1], 'O-o-o-oh!', 0.125));
          3: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[1], 'Hah! Hah!', 0.125));
        end;
        FTime2:= 0.0;
      end;
end;
  TActorStatus.Finish:
    if FActorsLogic.StatusChanged then
    begin
      Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'NYA-A-A-A! Coo-oo-ool!', 0.125));
      Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[1], 'A-AH! M-m-m...!', 0.125));
    end;
  TActorStatus.Relax:
    begin
      if FActorsLogic.StatusChanged then
      begin
        FTime1:= 8.0;
        FTime2:= 8.0;
      end;

      timeOut:= RandomFloatRange(12, 32.0);
      if (FTime1 > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 4);
        case i of
          1: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'nya... nya-A...', 1.0));
          2: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], 'a-a-ah... a-a-ah...', 1.0));
          3: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], '...It was cool...', 1.0));
          4: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[0], '...ah great...', 1.0));
        end;
        FTime1:= 0.0;
      end;

      timeOut:= RandomFloatRange(16, 36.0);
      if (FTime2 > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 3);
        case i of
          1: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[1], 'hah... hah...', 1.0));
          2: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[1], 'nya-a-a...', 1.0));
          3: Container.PushView(TViewSpeakMenu.CreateUntilStopped(FActorsLogic.Charas[1], '...You was little rude...', 1.0));
        end;
        FTime2:= 0.0;
      end;
    end;
  end;
end;

procedure TViewPlayTogether.GetToGoBack;
begin
  GetToGo(ViewTravelContainerRoom, Vector3(0.34, 0.15, -0.23),
                                   Vector4(0.0, 1.0, 0.0, Deg(-32.0)));
end;

end.
