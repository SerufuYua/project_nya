unit GameViewPlayTogetherLab;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, BaseViewPlayTogether, CastleScene;

type
  TViewPlayTogetherLab = class(TBaseViewPlayTogether)
  protected
    procedure GetToGoBack; override;
  public
    procedure Start; override;
  end;

var
  ViewPlayTogetherLab: TViewPlayTogetherLab;

implementation

uses
  SysUtils, CastleTransform, CastleUtils, CastleVectors,
  CastleSoundEngine, GameSound,
  GameViewTravelSpaceshipLabRoom, NyaSpeechBubble, NyaPlayLogic;

procedure TViewPlayTogetherLab.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapPlay_TogetherLab.castle-user-interface';

  { Play music }
  SoundEngine.LoopingChannel[0].Sound:= NamedSound('MusicTogether');

  inherited;
end;

procedure TViewPlayTogetherLab.GetToGoBack;
begin
  GetToGo(ViewTravelSpaceshipLabRoom, Vector3(1.82, 0.1, 0.85),
                                      Vector4(0.0, 1.0, 0.0, Deg(180.0)));
end;

end.

