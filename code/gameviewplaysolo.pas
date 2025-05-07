unit GameViewPlaySolo;

interface

uses
  Classes, BaseViewPlay;

type
  TViewPlaySolo = class(TBaseViewPlay)
  public
    procedure Start; override;
  protected
    procedure GetToGoBack; override;
  end;

var
  ViewPlaySolo: TViewPlaySolo;

implementation

uses
  SysUtils, CastleTransform, CastleUtils, CastleVectors,
  CastleSoundEngine, GameSound,
  GameViewTravelContainerRoom;

procedure TViewPlaySolo.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapPlay_Solo.castle-user-interface';

  { Play InRoom music }
  SoundEngine.LoopingChannel[0].Sound:= NamedSound('MusicInRoom');

  inherited;
end;

procedure TViewPlaySolo.GetToGoBack;
begin
  GetToGo(ViewTravelContainerRoom, Vector3(0.34, 0.15, -0.23),
                                   Vector4(0.0, 1.0, 0.0, Deg(-32.0)));
end;

end.
