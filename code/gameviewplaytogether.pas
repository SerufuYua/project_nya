unit GameViewPlayTogether;

interface

uses
  Classes, BaseViewPlay;

type
  TViewPlayTogether = class(TBaseViewPlay)
  public
    procedure Start; override;
  end;

var
  ViewPlayTogether: TViewPlayTogether;

implementation

uses
  SysUtils, CastleTransform;

procedure TViewPlayTogether.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapPlay_Together.castle-user-interface';

  inherited;
end;

end.
