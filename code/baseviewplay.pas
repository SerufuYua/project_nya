unit BaseViewPlay;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleUIControls, CastleControls, CastleNotifications, CastleClassUtils,
  CastleColors, CastleKeysMouse, CastleTransform,
  BaseView, NyaFadeEffect, NyaPlayLogic, NyaPleasureTensionEffect,
  NyaLoadingBar, CastleFonts, CastleViewport;

type
  TBaseViewPlay = class(TBaseView)
  published
    LabelFps: TCastleLabel;
    BtnSettings: TCastleButton;
    BtnBack: TCastleButton;
    BtnStop: TCastleButton;
    BtnNext: TCastleButton;
    BtnMinus: TCastleButton;
    BtnPlus: TCastleButton;
    BtnCamera: TCastleButton;
    FloatSliderSpeed: TCastleFloatSlider;
    GaugePleasure: TNyaLoadingBar;
    GaugeTension: TNyaLoadingBar;
    GroupDressingButtons: TCastlePackedGroup;
    GroupActionSelect: TCastlePackedGroup;
    GroupCameraList: TCastlePackedGroup;
    ImageControlCamList: TCastleImageControl;
    FadeEffect: TNyaFadeEffect;
    PleasureTensionEffect: TNyaPleasureTensionEffect;
    Notifications: TCastleNotifications;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: boolean); override;
    procedure Pause; override;
    procedure Resume; override;
  protected
    FCameraList: array of TCastleCamera;
    FFont: TCastleAbstractFont;
    FActorsLogic: TNyaPlayLogic;
    procedure FocusButton(const Sender: TCastleUserInterface);
    procedure FocusList(const Sender: TCastleUserInterface);
    procedure ClickAction(Sender: TObject);
    procedure ClickDress(Sender: TObject);
    procedure ClickControl(Sender: TObject);
    procedure ClickCam(Sender: TObject);
    procedure ChangedSpeed(Sender: TObject);
    procedure ShowCameraList(Sender: TObject);
    procedure SetDressButtons;
    procedure SetActionsList(actList: TCastleComponent);
    procedure SetCamButtons;
    procedure SaveCharasCondition;
    procedure DoStart(Sender: TObject);
  protected
    procedure GetToGoBack; virtual; abstract;
  end;

implementation

uses
  GameViewDressingMenu, GameViewLoading, GameViewSettingsMenu,
  CastleComponentSerialize,
  CastleSoundEngine, GameSound,
  CastleScene, CastleVectors, CastleCameras,
  StrUtils, NyaCastleUtils, NyaActor, NyaActorChara, NyaCastleUiUtils;

constructor TBaseViewPlay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DesignUrl:= 'castle-data:/gameviewplay.castle-user-interface';
end;

procedure TBaseViewPlay.Start;
var
  actor: TNyaActor;
  actors: TActorsList;
  actorsRoot, child: TCastleTransform;
  cam: TCastleCamera;
begin
  inherited;

  { set Buttons }
  BtnSettings.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnBack.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnStop.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnNext.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  FloatSliderSpeed.OnChange:= {$ifdef FPC}@{$endif}ChangedSpeed;
  BtnMinus.OnClick:= {$ifdef FPC}@{$endif}ChangedSpeed;
  BtnPlus.OnClick:= {$ifdef FPC}@{$endif}ChangedSpeed;
  BtnCamera.OnClick:= {$ifdef FPC}@{$endif}ShowCameraList;

  BtnSettings.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  BtnBack.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  BtnStop.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  BtnNext.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  FloatSliderSpeed.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  BtnMinus.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  BtnPlus.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  BtnCamera.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;

  { get Navigation }
  FCameraNavigation:= Map.DesignedComponent('ObserverNavigation') as TCastleMouseLookNavigation;

  { set Actors Logic }
  actors:= [];
  actorsRoot:= Map.DesignedComponent('SceneActors') as TCastleTransform;

  for child in actorsRoot do
  begin
    if (child is TNyaActor) then
      Insert(child, actors, 0);
  end;

  FActorsLogic:= TNyaPlayLogic.Create(actors, FadeEffect);

  { set Font }
  FFont:= DesignedComponent('future_n0t_found_32') as TCastleAbstractFont;

  { set Camera List }
  FCameraList:= [];
  if Assigned(FMainViewport.Camera) then
    Insert(FMainViewport.Camera, FCameraList, Length(FCameraList));

  for actor in actors do
    for cam in actor.Cameras do
      Insert(cam, FCameraList, Length(FCameraList));

  { set dress buttons }
  SetDressButtons;

  { set actions list }
  SetActionsList(Map.DesignedComponent('ActionsList') as TCastleComponent);

  { set cameras buttons }
  SetCamButtons;

  { set color }
  SetUIColor(FActorsLogic.CombinedColor);

  { set initial action }
  WaitForRenderAndCall({$ifdef FPC}@{$endif}DoStart);
end;

procedure TBaseViewPlay.Stop;
begin
  SaveCharasCondition;
  if Assigned(FActorsLogic) then
    FreeAndNil(FActorsLogic);
  inherited;
end;

procedure TBaseViewPlay.Update(const SecondsPassed: Single;
                               var HandleInput: boolean);
begin
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

  inherited;
end;

procedure TBaseViewPlay.FocusButton(const Sender: TCastleUserInterface);
begin
  SoundEngine.Play(NamedSound('SfxButtonFocus'));
end;

procedure TBaseViewPlay.FocusList(const Sender: TCastleUserInterface);
begin
  SoundEngine.Play(NamedSound('SfxListFocus'));
end;

procedure TBaseViewPlay.ClickAction(Sender: TObject);
var
  btnDress: TCastleButton;
begin
  btnDress:= Sender as TCastleButton;
  if NOT Assigned(btnDress) then Exit;

  SoundEngine.Play(NamedSound('SfxButtonPress'));

  FActorsLogic.Stop;
  FActorsLogic.ActNum:= btnDress.Tag;
end;

procedure TBaseViewPlay.ClickDress(Sender: TObject);
var
  chara: TNyaActorChara;
  btnDress: TCastleButton;
  i: integer;
  dressFound: Boolean;
begin
  btnDress:= Sender as TCastleButton;
  if NOT Assigned(btnDress) then Exit;

  SoundEngine.Play(NamedSound('SfxListPress'));

  { Check if Dreesing menu already opened }
  dressFound:= False;
  for i:= 0 to (Container.ViewStackCount - 1) do
  begin
    if (Container.ViewStack[i] is TViewDressingMenu) then
    begin
      dressFound:= False;
      break;
    end;
  end;

  { Show Dressing Menu }
  if NOT dressFound then
  begin
    { find selected chara }
    for chara in FActorsLogic.Charas do
    begin
      if (chara.ActorName = btnDress.Caption) then
      begin
        Container.PushView(TViewDressingMenu.CreateUntilStopped(chara));
        Break;
      end;
    end;
  end;
end;

procedure TBaseViewPlay.ClickControl(Sender: TObject);
var
  button: TCastleButton;
begin
  button:= Sender as TCastleButton;
  if NOT Assigned(button) then exit;

  SoundEngine.Play(NamedSound('SfxButtonPress'));

  Case button.Name of
    'BtnSettings':
        if NOT (Container.FrontView is TViewSettingsMenu) then
          Container.PushView(TViewSettingsMenu.CreateUntilStopped);
    'BtnBack':
      GetToGoBack;
    'BtnStop':
      FActorsLogic.Stop;
    'BtnNext':
      FActorsLogic.NextPart;
  end;
end;

procedure TBaseViewPlay.ClickCam(Sender: TObject);
var
  cam: TCastleCamera;
  button: TCastleButton;
begin
  button:= Sender as TCastleButton;
  if NOT Assigned(button) then exit;

  for cam in FCameraList do
    if (button.Caption = cam.Name) then
    begin
      FMainViewport.Camera:= cam;
      Break;
    end;

  FCameraNavigation.Exists:= (FMainViewport.Camera = FCameraList[Low(FCameraList)]);
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
    newBtn.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusList;
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
    newBtn.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusList;
    GroupActionSelect.InsertFront(newBtn);
  end;

  if Assigned(myBtnFactory) then
    FreeAndNil(myBtnFactory);
end;

procedure TBaseViewPlay.SetCamButtons;
var
  cam: TCastleCamera;
  newBtn, sampleBtn: TCastleButton;
  myBtnFactory: TCastleComponentFactory;
  myFont: TCastleAbstractFont;
begin
  if ((GroupCameraList.ControlsCount > 0) AND
      (GroupCameraList.Controls[0] is TCastleButton)) then
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

  GroupCameraList.ClearControls;

  for cam in FCameraList do
  begin
    if Assigned(myBtnFactory) then
    begin
      newBtn:= myBtnFactory.ComponentLoad(GroupDressingButtons) as TCastleButton;
      newBtn.CustomFont:= myFont;
    end else
      newBtn:= TCastleButton.Create(GroupDressingButtons);

    newBtn.Caption:= cam.Name;
    newBtn.OnClick:= {$ifdef FPC}@{$endif}ClickCam;
    newBtn.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusList;
    GroupCameraList.InsertFront(newBtn);
  end;

  if Assigned(myBtnFactory) then
    FreeAndNil(myBtnFactory);
end;

procedure TBaseViewPlay.ChangedSpeed(Sender: TObject);
const
  step = 0.1;
var
  slider: TCastleFloatSlider;
  button: TCastleButton;
begin
  SoundEngine.Play(NamedSound('SfxButtonPress'));

  if (Sender is TCastleFloatSlider) then
  begin
    slider:= Sender as TCastleFloatSlider;
    FActorsLogic.Speed:= slider.Value;
  end
  else if (Sender is TCastleButton) then
  begin
    button:= Sender as TCastleButton;

    Case button.Name of
      'BtnMinus': StepChangeSlider(FloatSliderSpeed, -step);
      'BtnPlus': StepChangeSlider(FloatSliderSpeed, step);
    end;

    FActorsLogic.Speed:= FloatSliderSpeed.Value;
  end;
end;

procedure TBaseViewPlay.ShowCameraList(Sender: TObject);
begin
  ImageControlCamList.Exists:= NOT ImageControlCamList.Exists;
end;

procedure TBaseViewPlay.SaveCharasCondition;
var
  chara: TNyaActorChara;
begin
  for chara in FActorsLogic.Charas do
    chara.SaveCondition;
end;

procedure TBaseViewPlay.Pause;
begin
  { do nothing }
end;

procedure TBaseViewPlay.Resume;
begin
  { do nothing }
end;

procedure TBaseViewPlay.DoStart(Sender: TObject);
begin
  FActorsLogic.Stop;
  Notifications.Show('Info: use WASD to move');
  Notifications.Show('Info: use C to move down');
  Notifications.Show('Info: use Space to move up');
  Notifications.Show('Info: use SHIFT to move faster');
end;

end.

