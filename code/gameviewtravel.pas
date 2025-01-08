unit GameViewTravel;

interface

uses Classes, BaseViewTravel,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleTransform, NyaActorChara,
  NyaSwitch;

type
  TViewTravel = class(TBaseViewPlay)
  public
    procedure Start; override;
  protected
    procedure DoTouchSwitch(const Sender: TObject; Touch: Boolean); override;
    procedure DoActivateSwitch(Sender: TObject); override;
  end;

var
  ViewTravel: TViewTravel;

implementation

uses
  GameViewPlayGirl, GameViewPlayTogether, GameViewMain,
  CastleViewport, CastleScene, NyaCastleUtils, CastleComponentSerialize,
  CastleFonts, SysUtils, GameViewDressingMenu;

procedure TViewTravel.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapTravel_ContainerRoom.castle-user-interface';

  { se Girl Character }
  FActorMain:= Map.DesignedComponent('CharaGirl') as TNyaActorChara;

  inherited;
end;

procedure TViewTravel.DoTouchSwitch(const Sender: TObject; Touch: Boolean);
begin
  inherited;
  { any additional actions for some specail switch }
end;

procedure TViewTravel.DoActivateSwitch(Sender: TObject);
var
  switch: TNyaSwitch;
begin
  switch:= Sender as TNyaSwitch;
  if NOT Assigned(switch) then Exit;

  Case switch.Name of
  'ToyASwitch':
    begin
      Notifications.Show('Lets play with my Toy!');
      FGetToGo:= ViewPlayGirl;
    end;
  'BedSwitch':
    begin
      Notifications.Show('Lets play in my Bed!');
      FGetToGo:= ViewPlayTogether;
    end;
  else
    Notifications.Show('There is nothing to use');
  end;
end;

end.
