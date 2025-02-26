unit BaseViewPlay;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleUIControls, CastleControls, CastleNotifications, CastleClassUtils,
  CastleColors, CastleKeysMouse, CastleTransform, CastleCameras,
  NyaFadeEffect, NyaPlayLogic, NyaPleasureTensionEffect, NyaLoadingBar;

type
  TBaseViewPlay = class(TCastleView)
  published
    Map: TCastleDesign;
    LabelFps: TCastleLabel;
    BtnBack: TCastleButton;
    BtnStop: TCastleButton;
    BtnNext: TCastleButton;
    FloatSliderSpeed: TCastleFloatSlider;
    GaugePleasure: TNyaLoadingBar;
    GaugeTension: TNyaLoadingBar;
    ImageControlDressing: TCastleImageControl;
    GroupDressingButtons: TCastlePackedGroup;
    GroupActionSelect: TCastlePackedGroup;
    FadeEffect: TNyaFadeEffect;
    PleasureTensionEffect: TNyaPleasureTensionEffect;
    Notifications: TCastleNotifications;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
  protected
    FActorsLogic: TNyaPlayLogic;
    FObserverNavigation: TCastleWalkNavigation;
    procedure ClickAction(Sender: TObject);
    procedure ClickDress(Sender: TObject);
    procedure ClickControl(Sender: TObject);
    procedure ChangedSpeed(Sender: TObject);
    procedure SetDressButtons;
    procedure SetActionsList(actList: TCastleComponent);
    procedure SetUIColor;
    procedure SaveCharasCondition;
    procedure DoStart(Sender: TObject);
  end;

implementation

uses
  GameViewTravelContainerRoom, GameViewDressingMenu, GameViewLoading, CastleComponentSerialize,
  CastleScene, CastleFonts, CastleViewport, CastleVectors,
  StrUtils, NyaCastleUtils, NyaActor, NyaActorChara, NyaActorToyA;

constructor TBaseViewPlay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DesignUrl:= 'castle-data:/gameviewplay.castle-user-interface';
end;

procedure TBaseViewPlay.Start;
var
  actors: TActorsList;
  actorsRoot, child: TCastleTransform;
begin
  inherited;

  { set Buttons }
  BtnBack.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnStop.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnNext.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  FloatSliderSpeed.OnChange:= {$ifdef FPC}@{$endif}ChangedSpeed;

  { get Navigation }
  FObserverNavigation:= Map.DesignedComponent('ObserverNavigation') as TCastleWalkNavigation;

  { set Actors Logic }
  actors:= [];
  actorsRoot:= Map.DesignedComponent('SceneActors') as TCastleTransform;

  for child in actorsRoot do
  begin
    if (child is TNyaActor) then
      Insert(child, actors, 0);
  end;

  FActorsLogic:= TNyaPlayLogic.Create(actors, FadeEffect);
  { set dress buttons }
  SetDressButtons;

  { set actions list }
  SetActionsList(Map.DesignedComponent('ActionsList') as TCastleComponent);

  { set color }
  SetUIColor;

  { set initial action }
  WaitForRenderAndCall({$ifdef FPC}@{$endif}DoStart);
end;

procedure TBaseViewPlay.Stop;
begin
  if Assigned(FActorsLogic) then
    FreeAndNil(FActorsLogic);
  inherited;
end;

procedure TBaseViewPlay.Update(const SecondsPassed: Single;
                               var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }

  { update FPS }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption:= 'FPS: ' + Container.Fps.ToString;

  { upade pleasure/tension effects }
  FActorsLogic.Update(SecondsPassed);
  GaugePleasure.GaugeValue:= FActorsLogic.Pleasure;
  GaugeTension.GaugeValue:= FActorsLogic.Tension;
  PleasureTensionEffect.Pleasure:= FActorsLogic.Pleasure / FActorsLogic.DefaultThresholdFastGo;
  PleasureTensionEffect.Tension:= FActorsLogic.Tension;

  { Release Dressing Menu Buttons }
  if NOT (Container.FrontView = ViewDressingMenu) then
    ImageControlDressing.Exists:= True;
end;

function TBaseViewPlay.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { enable camera control }
  if Event.IsMouseButton(buttonRight) OR Event.IsMouseButton(buttonMiddle) then
  begin
    FObserverNavigation.MouseLook:= True;
    Exit(true);
  end;
end;

function TBaseViewPlay.Release(const Event: TInputPressRelease): boolean;
begin
  { disable camera control }
  if Event.IsMouseButton(buttonRight) OR Event.IsMouseButton(buttonMiddle) then
    FObserverNavigation.MouseLook:= False;

  Result := inherited;
end;

procedure TBaseViewPlay.ClickAction(Sender: TObject);
var
  btnDress: TCastleButton;
begin
  btnDress:= Sender as TCastleButton;
  if NOT Assigned(btnDress) then Exit;

  FActorsLogic.Stop;
  FActorsLogic.ActNum:= btnDress.Tag;
end;

procedure TBaseViewPlay.ClickDress(Sender: TObject);
var
  chara: TNyaActorChara;
  btnDress: TCastleButton;
begin
  btnDress:= Sender as TCastleButton;
  if NOT Assigned(btnDress) then Exit;

  { Show Dressing Menu }
  if NOT (Container.FrontView = ViewDressingMenu) then
  begin
    Container.PushView(ViewDressingMenu);

    { find selected chara }
    for chara in FActorsLogic.Charas do
    begin
      if (chara.ActorName = btnDress.Caption) then
      begin
        ViewDressingMenu.SetChara(chara);
        Break;
      end;
    end;

    ImageControlDressing.Exists:= False;
  end;
end;

procedure TBaseViewPlay.ClickControl(Sender: TObject);
var
  button: TCastleButton;
begin
  button:= Sender as TCastleButton;
  if NOT Assigned(button) then exit;

  Case button.Name of
  'BtnBack':
    begin
      Notifications.Show('saving characters condition...');
      SaveCharasCondition();
      ViewLoading.SetToLoad(ViewTravelContainerRoom);
      Container.View:= ViewLoading;
    end;
  'BtnStop':
    begin
      FActorsLogic.Stop;
    end;
  'BtnNext':
    begin
      FActorsLogic.NextPart;
    end;
  end;
end;

procedure TBaseViewPlay.SetDressButtons;
var
  chara: TNyaActorChara;
  newBtn, sampleBtn: TCastleButton;
  myBtnFactory: TCastleComponentFactory;
  myFont: TCastleAbstractFont;
begin
  if ((GroupDressingButtons.ControlsCount > 0) AND
      (GroupDressingButtons.Controls[0] is TCastleButton)) then
  begin
    sampleBtn:= GroupDressingButtons.Controls[0] as TCastleButton;
    myFont:= sampleBtn.CustomFont;
    myBtnFactory:= TCastleComponentFactory.Create(self);
    myBtnFactory.LoadFromComponent(sampleBtn);
  end else
  begin
    sampleBtn:= nil;
    myBtnFactory:= nil;
  end;

  GroupDressingButtons.ClearControls;

  for chara in FActorsLogic.Charas do
  begin
    if Assigned(myBtnFactory) then
    begin
      newBtn:= myBtnFactory.ComponentLoad(GroupDressingButtons) as TCastleButton;
      newBtn.CustomFont:= myFont;
    end else
      newBtn:= TCastleButton.Create(GroupDressingButtons);

    newBtn.Caption:= chara.ActorName;
    newBtn.OnClick:= {$ifdef FPC}@{$endif}ClickDress;
    GroupDressingButtons.InsertFront(newBtn);
  end;

  if Assigned(myBtnFactory) then
    FreeAndNil(myBtnFactory);
end;

procedure TBaseViewPlay.SetActionsList(actList: TCastleComponent);
var
  num, i: Integer;
  actionDescr: TCastleComponent;
  newBtn, sampleBtn: TCastleButton;
  myBtnFactory: TCastleComponentFactory;
  myFont: TCastleAbstractFont;
begin
  num:= actList.NonVisualComponentsCount;
  if (num < 1) then Exit;

  if ((GroupActionSelect.ControlsCount > 0) AND
      (GroupActionSelect.Controls[0] is TCastleButton)) then
  begin
    sampleBtn:= GroupActionSelect.Controls[0] as TCastleButton;
    myFont:= sampleBtn.CustomFont;
    myBtnFactory:= TCastleComponentFactory.Create(self);
    myBtnFactory.LoadFromComponent(sampleBtn);
  end else
  begin
    sampleBtn:= nil;
    myBtnFactory:= nil;
  end;

  GroupActionSelect.ClearControls;

  for i:= 0 to (num - 1) do
  begin
    if Assigned(myBtnFactory) then
    begin
      newBtn:= myBtnFactory.ComponentLoad(GroupActionSelect) as TCastleButton;
      newBtn.CustomFont:= myFont;
    end else
      newBtn:= TCastleButton.Create(GroupActionSelect);

    actionDescr:= actList.NonVisualComponents[i] as TCastleComponent;
    newBtn.Caption:= ReplaceStr(actionDescr.Name, '_', ' ');
    newBtn.Tag:= actionDescr.Tag;
    newBtn.OnClick:= {$ifdef FPC}@{$endif}ClickAction;
    GroupActionSelect.InsertFront(newBtn);
  end;

  if Assigned(myBtnFactory) then
    FreeAndNil(myBtnFactory);
end;

procedure TBaseViewPlay.ChangedSpeed(Sender: TObject);
var
  slider: TCastleFloatSlider;
begin
  slider:= Sender as TCastleFloatSlider;
  if NOT Assigned(slider) then Exit;

  FActorsLogic.Speed:= slider.Value;
end;

procedure TBaseViewPlay.SetUIColor;
var
  rootItem: TCastleUserInterface;
  item: TCastleImageControl;
  alpha: single;
begin
  rootItem:= DesignedComponent('SceneMain') as TCastleUserInterface;

  for item in GetAllUIImages(rootItem) do
  begin
      alpha:= item.Color.W;
      if (item.Tag = 1) then
        item.Color:= Vector4(FActorsLogic.CombinedColor, alpha);
  end;
end;

procedure TBaseViewPlay.SaveCharasCondition;
var
  chara: TNyaActorChara;
begin
  for chara in FActorsLogic.Charas do
    chara.SaveCondition;
end;

procedure TBaseViewPlay.DoStart(Sender: TObject);
begin
  FActorsLogic.Stop;
  Notifications.Show('Lets Play!');
end;

end.

