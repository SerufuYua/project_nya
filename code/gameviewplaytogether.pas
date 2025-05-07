unit GameViewPlayTogether;

interface

uses
  Classes, BaseViewPlay;

type
  TViewPlayTogether = class(TBaseViewPlay)
  public
    procedure Start; override;
  protected
    procedure GetToGoBack; override;
  end;

var
  ViewPlayTogether: TViewPlayTogether;

implementation

uses
  SysUtils, CastleTransform, CastleUtils, CastleVectors,
  CastleSoundEngine, GameSound,
  GameViewTravelContainerRoom;

procedure TViewPlayTogether.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapPlay_Together.castle-user-interface';

  { Play music }
  SoundEngine.LoopingChannel[0].Sound:= NamedSound('MusicInRoom');

  inherited;
end;

procedure TViewPlayTogether.GetToGoBack;
begin
  GetToGo(ViewTravelContainerRoom, Vector3(0.34, 0.15, -0.23),
                                   Vector4(0.0, 1.0, 0.0, Deg(-32.0)));
end;

end.
