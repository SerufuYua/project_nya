unit GameViewPlayGirl;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleTransform,
  CharaGirlBehavior, ToysForGirlBehavior;

type
  TViewPlayGirl = class(TCastleView)
  published
    LabelFps: TCastleLabel;
    BtnDress: TCastleButton;
    BtnBack: TCastleButton;
    BtnStop: TCastleButton;
    BtnPause: TCastleButton;
    BtnPlayA1: TCastleButton;
    BtnPlayA2: TCastleButton;
    BtnPlayA3: TCastleButton;
    BtnPlayA4: TCastleButton;
    BtnPlayA5: TCastleButton;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: boolean); override;
  private
    GirlBehavior: TCharaGirlBehavior;
    ToysBehavior: TToysForGirlBehavior;
    DressMenu: boolean;
    procedure ClickBack(Sender: TObject);
    procedure ClickStop(Sender: TObject);
    procedure ClickPause(Sender: TObject);
    procedure ClickDress(Sender: TObject);
    procedure ClickPlay(Sender: TObject);
  end;

var
  ViewPlayGirl: TViewPlayGirl;

implementation

uses GameViewMain, CastleScene, CharaBehavior, GameViewDressingMenu;

constructor TViewPlayGirl.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl:= 'castle-data:/gameviewplaygirl.castle-user-interface';
  DressMenu:= False;
end;

procedure TViewPlayGirl.Start;
var
  girlScene, toysScene: TCastleTransformDesign;
begin
  inherited;
  { Executed once when view starts }

  BtnBack.OnClick:= {$ifdef FPC}@{$endif}ClickBack;
  BtnStop.OnClick:= {$ifdef FPC}@{$endif}ClickStop;
  BtnPause.OnClick:= {$ifdef FPC}@{$endif}ClickPause;
  BtnDress.OnClick:= {$ifdef FPC}@{$endif}ClickDress;
  BtnPlayA1.OnClick:= {$ifdef FPC}@{$endif}ClickPlay;
  BtnPlayA2.OnClick:= {$ifdef FPC}@{$endif}ClickPlay;
  BtnPlayA3.OnClick:= {$ifdef FPC}@{$endif}ClickPlay;
  BtnPlayA4.OnClick:= {$ifdef FPC}@{$endif}ClickPlay;
  BtnPlayA5.OnClick:= {$ifdef FPC}@{$endif}ClickPlay;

  { Create Girl Character instance }
  girlScene:= DesignedComponent('CharaGirl') as TCastleTransformDesign;
  GirlBehavior:= TCharaGirlBehavior.Create(FreeAtStop);
  girlScene.AddBehavior(GirlBehavior);

  { Create Toys instance }
  toysScene:= DesignedComponent('Toys') as TCastleTransformDesign;
  ToysBehavior:= TToysForGirlBehavior.Create(FreeAtStop);
  toysScene.AddBehavior(ToysBehavior);
end;

procedure TViewPlayGirl.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption:= 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewPlayGirl.ClickBack(Sender: TObject);
begin
  Container.View:= ViewMain;
end;

procedure TViewPlayGirl.ClickStop(Sender: TObject);
begin
  GirlBehavior.ActionIdle;
end;

procedure TViewPlayGirl.ClickPause(Sender: TObject);
begin
  GirlBehavior.ActionPause;
end;

procedure TViewPlayGirl.ClickDress(Sender: TObject);
begin
  { Show Dressing Menu }
  if DressMenu then
  begin
    Container.PopView;
    DressMenu:= False;
  end else
  begin
    Container.PushView(ViewDressingMenu);
    ViewDressingMenu.SetChara(GirlBehavior);
    DressMenu:= True;
  end;
end;

procedure TViewPlayGirl.ClickPlay(Sender: TObject);
var
  button: TCastleButton;
begin
  button:= Sender as TCastleButton;
  if NOT Assigned(button) then exit;

  Case button.Name of
  'BtnPlayA1':
    begin
      GirlBehavior.ActionPlayA1;
      ToysBehavior.ActionPlayA1;
    end;
  'BtnPlayA2':
    begin
      GirlBehavior.ActionPlayA2;
      ToysBehavior.ActionPlayA2;
    end;
  'BtnPlayA3':
    begin
      GirlBehavior.ActionPlayA3;
      ToysBehavior.ActionPlayA3;
    end;
  'BtnPlayA4':
    begin
      GirlBehavior.ActionPlayA4;
      ToysBehavior.ActionPlayA4;
    end;
  'BtnPlayA5':
    begin
      GirlBehavior.ActionPlayA5;
      ToysBehavior.ActionPlayA5;
    end;
  end;

end;

end.
