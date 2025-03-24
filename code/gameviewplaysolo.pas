unit GameViewPlaySolo;

interface

uses
  Classes, BaseViewPlay;

type
  TViewPlaySolo = class(TBaseViewPlay)
  public
    procedure Start; override;
  end;

var
  ViewPlaySolo: TViewPlaySolo;

implementation

uses
  SysUtils, CastleTransform;

procedure TViewPlaySolo.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapPlay_Solo.castle-user-interface';

  inherited;
end;

end.
