unit GameViewPlayGirl;

interface

uses
  Classes, BaseViewPlay;

type
  TViewPlayGirl = class(TBaseViewPlay)
  public
    procedure Start; override;
  protected
    procedure GetToGoBack; override;
  end;

var
  ViewPlayGirl: TViewPlayGirl;

implementation

uses
  SysUtils, CastleTransform, CastleUtils, CastleVectors,
  GameViewTravelContainerRoom;

procedure TViewPlayGirl.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapPlay_GirlToyA.castle-user-interface';

  inherited;
end;

procedure TViewPlayGirl.GetToGoBack;
begin
  inherited;
  GetToGo(ViewTravelContainerRoom, Vector3(-0.9, 0.15, -0.84),
                                   Vector4(0.0, 1.0, 0.0, Deg(25.0)));
end;

end.
