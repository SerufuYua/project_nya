unit GameViewPlayTogether;

interface

uses
  Classes, BaseViewPlay, CastleScene;

type
  TViewPlayTogether = class(TBaseViewPlay)
  protected
    FSpeechArea: TCastleBox;
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
  GameViewTravelContainerRoom, NyaSpeechBubble, NyaPlayLogic;

procedure TViewPlayTogether.Start;
begin
  FTime1:= 0.0;
  FTime2:= 0.0;
  FSpeakOnceDone:= True;

  { set map }
  Map.Url:= 'castle-data:/MapPlay_Together.castle-user-interface';

  { Speech Area }
  FSpeechArea:= Map.DesignedComponent('SpeechArea') as TCastleBox;

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
          1: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[0], 'nya...', FFont);
          2: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[0], 'c&apos;mon...', FFont);
          3: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[0], 'ah... so big...', FFont);
          4: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[0], 'cutie...', FFont);
        end;
        FTime1:= 0.0;
      end;

      timeOut:= RandomFloatRange(24, 36.0);
      if (FTime2 > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 3);
        case i of
          1: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[1], 'nya...', FFont);
          2: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[1], 'nya-nya...', FFont);
          3: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[1], 'aaah...', FFont);
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
          TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                  FActorsLogic.Charas[0], 'a-A-A-yeah!', FFont);

          TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                  FActorsLogic.Charas[1], 'm-m-ha', FFont);
          FSpeakOnceDone:= True;
          FTime1:= 0.0;
        end;
      end;
    end;
  TActorStatus.Go:
    begin
      if FActorsLogic.StatusChanged then
      begin
        TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                FActorsLogic.Charas[0], 'O-o-oh! A-a-A-ah!', FFont);
        FTime1:= 0.0;
        FTime2:= 0.0;
      end;

      timeOut:= RandomFloatRange(4.0, 16.0);
      if (FTime1 > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 3);
        case i of
          1: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[0], 'M-m-mha! A-a-ah!', FFont);
          2: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[0], 'Nya-a-a-a!', FFont);
          3: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[0], 'Yeah... Yeah!', FFont);
        end;
        FTime1:= 0.0;
      end;

      timeOut:= RandomFloatRange(6.0, 18.0);
      if (FTime2 > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 3);
        case i of
          1: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[1], 'Ha-ah... Nya-ah', FFont);
          2: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[1], 'M-m-mh...', FFont);
          3: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[1], 'Ah Nya...', FFont);
        end;
        FTime2:= 0.0;
      end;
    end;
  TActorStatus.FastGo:
    begin
      if FActorsLogic.StatusChanged then
      begin
        TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                FActorsLogic.Charas[0], 'Yeah! Faster! Nya!', FFont);
        FTime1:= 0.0;
        FTime2:= 0.0;
      end;

      timeOut:= RandomFloatRange(3, 8.0);
      if (FTime1 > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 4);
        case i of
          1: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[0], 'Ah-Ah-Ah!!', FFont, 0.125);
          2: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[0], 'Ye-Ye-Yeah!!', FFont, 0.125);
          3: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[0], 'M-m-m-mhAh!!', FFont, 0.125);
          4: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[0], 'Ny-a-a-ah!!', FFont, 0.125);
        end;
        FTime1:= 0.0;
      end;

      timeOut:= RandomFloatRange(3, 8.0);
      if (FTime2 > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 3);
        case i of
          1: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[1], 'Mhah-mhah-mhah!', FFont, 0.125);
          2: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[1], 'O-o-o-oh!', FFont, 0.125);
          3: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[1], 'Hah! Hah!', FFont, 0.125);
        end;
        FTime2:= 0.0;
      end;
end;
  TActorStatus.Finish:
    if FActorsLogic.StatusChanged then
    begin
      TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                              FActorsLogic.Charas[0], 'NYA-A-A-A! Coo-oo-ool!', FFont);
      TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                              FActorsLogic.Charas[1], 'A-AH! M-m-m...!', FFont);
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
          1: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                              FActorsLogic.Charas[0], 'nya... nya-A...', FFont, 1.0);
          2: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                              FActorsLogic.Charas[0], 'a-a-ah... a-a-ah...', FFont, 1.0);
          3: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                              FActorsLogic.Charas[0], '...It was cool...', FFont, 1.0);
          4: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                              FActorsLogic.Charas[0], '...ah great...', FFont, 1.0);
        end;
        FTime1:= 0.0;
      end;

      timeOut:= RandomFloatRange(16, 36.0);
      if (FTime2 > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 3);
        case i of
          1: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                              FActorsLogic.Charas[1], 'hah... hah...', FFont, 1.0);
          2: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                              FActorsLogic.Charas[1], 'nya-a-a...', FFont, 1.0);
          3: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                              FActorsLogic.Charas[1], '...You was little rude...', FFont, 1.0);
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
