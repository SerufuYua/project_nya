unit GameViewPlaySolo;

interface

uses
  Classes, BaseViewPlay, CastleScene;

type
  TViewPlaySolo = class(TBaseViewPlay)
  protected
    FSpeechArea: TCastleBox;
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
  GameViewTravelContainerRoom, NyaSpeechBubble, NyaPlayLogic;

procedure TViewPlaySolo.Start;
begin
  FTime:= 0.0;
  FSpeakOnceDone:= True;

  { set map }
  Map.Url:= 'castle-data:/MapPlay_Solo.castle-user-interface';

  { Speech Area }
  FSpeechArea:= Map.DesignedComponent('SpeechArea') as TCastleBox;

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
          TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                  FActorsLogic.Charas[0], 'Damn...', FFont);

          FSpeakOnceDone:= True;
          FTime:= 0.0;
        end;
      end;
    end;
  TActorStatus.Go:
    begin
      if FActorsLogic.StatusChanged then
      begin
        TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                FActorsLogic.Charas[0], 'A-ah... Damn...', FFont);
        FTime:= 0.0;
      end;

      timeOut:= RandomFloatRange(4.0, 16.0);
      if (FTime > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 3);
        case i of
          1: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[0], 'A-a-ah...', FFont);
          2: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[0], 'M-m-mph...', FFont);
          3: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[0], 'Yeah...', FFont);
        end;
        FTime:= 0.0;
      end;
    end;
  TActorStatus.FastGo:
    begin
      if FActorsLogic.StatusChanged then
      begin
        TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                FActorsLogic.Charas[0], 'O-o-oh!.. Come on!', FFont);
        FTime:= 0.0;
      end;

      timeOut:= RandomFloatRange(3, 8.0);
      if (FTime > timeOut) then
      begin
        i:= RandomIntRangeInclusive(1, 3);
        case i of
          1: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[0], 'Argh!.. F-f-f!.. ', FFont, 0.125);
          2: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[0], 'A-ah! Ayeah!', FFont, 0.125);
          3: TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                     FActorsLogic.Charas[0], 'Come on!', FFont, 0.125);
        end;
        FTime:= 0.0;
      end;
    end;
  TActorStatus.Finish:
    if FActorsLogic.StatusChanged then
      TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                              FActorsLogic.Charas[0], 'Nyaaaaa!!. ah!..', FFont);
  TActorStatus.Relax:
    begin
      if FActorsLogic.StatusChanged then
        FTime:= 8.0;

      timeOut:= RandomFloatRange(12, 24.0);
      if (FTime > timeOut) then
      begin
        TNyaSpeechBubble.Create(FMainViewport, FSpeechArea.WorldBoundingBox,
                                FActorsLogic.Charas[0], 'Zzz...', FFont, 1.0);
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
