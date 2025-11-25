unit GameViewPlayTogether;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, BaseViewPlayTogether, CastleScene;

type
  TViewPlayTogether = class(TBaseViewPlayTogether)
  protected
    procedure GetToGoBack; override;
  public
    procedure Start; override;
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
  { set map }
  Map.Url:= 'castle-data:/MapPlay_Together.castle-user-interface';

  { Play music }
  SoundEngine.LoopingChannel[0].Sound:= NamedSound('MusicTogether');

  inherited;
end;

procedure TViewPlayTogether.GetToGoBack;
begin
  GetToGo(ViewTravelContainerRoom, Vector3(0.34, 0.15, -0.23),
                                   Vector4(0.0, 1.0, 0.0, Deg(-32.0)));
end;

end.

