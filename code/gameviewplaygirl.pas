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
  GameViewTravelContainerRoom, NyaSpeechBubble, NyaPlayLogic;

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
          SpeechArea.InsertFront(TNyaSpeechBubble.Create(SpeechArea,
            FActorsLogic.Charas[0], 'A-a-aw!... Yeah >.<', FFont));
          FSpeakOnceDone:= True;
          FTime:= 0.0;
        end;
      end;
    end;
  TActorStatus.Go:
    begin
      if FActorsLogic.StatusChanged then
      begin
        SpeechArea.InsertFront(TNyaSpeechBubble.Create(SpeechArea,
          FActorsLogic.Charas[0], 'O-o-oh!', FFont));
        FTime:= 0.0;
      end;

      timeOut:= RandomFloatRange(4.0, 16.0);
      if (FTime > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 3);
        case i of
          1: SpeechArea.InsertFront(TNyaSpeechBubble.Create(SpeechArea,
               FActorsLogic.Charas[0], 'M-m-mha! A-a-ah!', FFont));
          2: SpeechArea.InsertFront(TNyaSpeechBubble.Create(SpeechArea,
               FActorsLogic.Charas[0], 'Nya-a-a-a!', FFont));
          3: SpeechArea.InsertFront(TNyaSpeechBubble.Create(SpeechArea,
               FActorsLogic.Charas[0], 'Yeah... Yeah!', FFont));
        end;
        FTime:= 0.0;
      end;
    end;
  TActorStatus.FastGo:
    begin
      if FActorsLogic.StatusChanged then
      begin
        SpeechArea.InsertFront(TNyaSpeechBubble.Create(SpeechArea,
               FActorsLogic.Charas[0], 'U-u-urf! Aha! Nya!', FFont));
        FTime:= 0.0;
      end;

      timeOut:= RandomFloatRange(3, 8.0);
      if (FTime > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 3);
        case i of
          1: SpeechArea.InsertFront(TNyaSpeechBubble.Create(SpeechArea,
               FActorsLogic.Charas[0], 'Ah! Ah!', FFont, 0.125));
          2: SpeechArea.InsertFront(TNyaSpeechBubble.Create(SpeechArea,
               FActorsLogic.Charas[0], 'Ayeah! Nyah!', FFont, 0.125));
          3: SpeechArea.InsertFront(TNyaSpeechBubble.Create(SpeechArea,
               FActorsLogic.Charas[0], 'Ayeh-Ayeh-Ayeh!', FFont, 0.125));
        end;
        FTime:= 0.0;
      end;
    end;
  TActorStatus.Finish:
    if FActorsLogic.StatusChanged then
      SpeechArea.InsertFront(TNyaSpeechBubble.Create(SpeechArea,
        FActorsLogic.Charas[0], 'A-U-u-ufh! NYA-A-A-A!', FFont, 0.125));
  TActorStatus.Relax:
    begin
      if FActorsLogic.StatusChanged then
        FTime:= 0.0;

      timeOut:= RandomFloatRange(12, 32.0);
      if (FTime > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 3);
        case i of
          1: SpeechArea.InsertFront(TNyaSpeechBubble.Create(SpeechArea,
               FActorsLogic.Charas[0], 'm-m-m...', FFont, 1.0));
          2: SpeechArea.InsertFront(TNyaSpeechBubble.Create(SpeechArea,
               FActorsLogic.Charas[0], 'ha-a-ah... ha-a-ah...', FFont, 1.0));
          3: SpeechArea.InsertFront(TNyaSpeechBubble.Create(SpeechArea,
               FActorsLogic.Charas[0], '...want to more...', FFont, 1.0));
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
