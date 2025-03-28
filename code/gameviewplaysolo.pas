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
  GameViewTravelContainerRoom;

procedure TViewPlaySolo.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapPlay_Solo.castle-user-interface';

  inherited;
end;

procedure TViewPlaySolo.GetToGoBack;
begin
  inherited;
  GetToGo(ViewTravelContainerRoom, Vector3(0.34, 0.15, -0.23),
                                   Vector4(0.0, 1.0, 0.0, Deg(-32.0)));
end;

end.
