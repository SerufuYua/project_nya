unit GameViewPlayGirl;

interface

uses
  Classes, BaseViewPlay;

type
  TViewPlayGirl = class(TBaseViewPlay)
  public
    procedure Start; override;
  end;

var
  ViewPlayGirl: TViewPlayGirl;

implementation

uses
  SysUtils, CastleTransform;

procedure TViewPlayGirl.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapPlay_GirlToyA.castle-user-interface';

  inherited;
end;

end.
