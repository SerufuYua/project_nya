unit GameViewDressingMenu;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  NyaActorChara, CastleColors, NyaCharaDress;

type
  TViewDressingMenu = class(TCastleView)
  strict private
    type
      TViewDressingDialog = class(TCastleUserInterface)
      private
        MenuRoot: TCastleUserInterface;
        ListSuit: TCastleVerticalGroup;
        ListTop: TCastleVerticalGroup;
        ListBottom: TCastleVerticalGroup;
        ListArms: TCastleVerticalGroup;
        ListFoots: TCastleVerticalGroup;
        ListAccessories: TCastleVerticalGroup;
        BtnClose: TcastleButton;
      private
        FDresser: TCharaDresser;
        procedure UpdateSuitParts();
        procedure UpdateSuitParts(suitType: TSuitPart; groupList: TCastleVerticalGroup);
        procedure UpdateSuits();
        procedure UpdateAccessories();
        procedure FocusButton(const Sender: TCastleUserInterface);
        procedure FocusList(const Sender: TCastleUserInterface);
        procedure ClickSuitPart(Sender: TObject);
        procedure ClickSuit(Sender: TObject);
        procedure ClickAccesories(Sender: TObject);
        procedure ClickClose(Sender: TObject);
        procedure SetChara(chara: TNyaActorChara);
        procedure SetColor(color: TCastleColorRGB);
      public
        Closed: Boolean;
        constructor Create(AOwner: TComponent; chara: TNyaActorChara); reintroduce;
      end;
    var
      FDialog: TViewDressingDialog;
      FChara: TNyaActorChara;
  public
    constructor CreateUntilStopped(chara: TNyaActorChara);
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  end;

implementation

uses
  CastleScene, CastleComponentSerialize, CastleFonts, NyaCastleUtils,
  CastleSoundEngine, GameSound,
  SysUtils, StrUtils;

const
  ListTopStr = 'ListTop';
  ListBottomStr = 'ListBottom';
  ListArmsStr = 'ListArms';
  ListFootsStr = 'ListFoots';

{ ========= ------------------------------------------------------------------ }
{ TViewDressingDialog -------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TViewDressingMenu.TViewDressingDialog.Create(AOwner: TComponent;
                                                         chara: TNyaActorChara);
var
  UiOwner: TComponent;
  Ui: TCastleUserInterface;
begin
  inherited Create(AOwner);
  Closed:= False;

  // UiOwner is useful to keep reference to all components loaded from the design
  UiOwner := TComponent.Create(Self);

  { Load designed user interface }
  Ui := UserInterfaceLoad('castle-data:/gameviewdressingmenu.castle-user-interface', UiOwner);
  InsertFront(Ui);

  { Find components, by name, that we need to access from code }
  MenuRoot:= UiOwner.FindRequiredComponent('MenuRoot') as TCastleUserInterface;
  ListSuit:= UiOwner.FindRequiredComponent('ListSuit') as TCastleVerticalGroup;
  ListTop:= UiOwner.FindRequiredComponent(ListTopStr) as TCastleVerticalGroup;
  ListBottom:= UiOwner.FindRequiredComponent(ListBottomStr) as TCastleVerticalGroup;
  ListArms:= UiOwner.FindRequiredComponent(ListArmsStr) as TCastleVerticalGroup;
  ListFoots:= UiOwner.FindRequiredComponent(ListFootsStr) as TCastleVerticalGroup;
  ListAccessories:= UiOwner.FindRequiredComponent('ListAccessories') as TCastleVerticalGroup;
  BtnClose:= UiOwner.FindRequiredComponent('BtnClose') as TcastleButton;

  BtnClose.OnClick:= {$ifdef FPC}@{$endif}ClickClose;
  BtnClose.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;

  SetChara(chara);
end;

procedure TViewDressingMenu.TViewDressingDialog.SetChara(chara: TNyaActorChara);
begin
  if NOT Assigned(chara) then Exit;
  FDresser:= chara.Dresser();
  UpdateSuitParts();
  UpdateAccessories();

  { Set personal color as background }
  SetColor(chara.PersonalColor);
end;

procedure TViewDressingMenu.TViewDressingDialog.SetColor(color: TCastleColorRGB);
var
  item: TCastleImageControl;
  alpha: single;
begin
  for item in GetAllUIImages(MenuRoot) do
  begin
    if (item.Tag = 1) then
    begin
      alpha:= item.Color.W;
      item.Color:= Vector4(color.X, color.Y, color.Z, alpha);
    end;
  end;
end;

procedure TViewDressingMenu.TViewDressingDialog.UpdateSuitParts();
begin
  UpdateSuitParts(TSuitPart.Top, ListTop);
  UpdateSuitParts(TSuitPart.Bottom, ListBottom);
  UpdateSuitParts(TSuitPart.Arms, ListArms);
  UpdateSuitParts(TSuitPart.Foots, ListFoots);
  UpdateSuits();
end;

procedure TViewDressingMenu.TViewDressingDialog.UpdateSuitParts(suitType: TSuitPart;
                                            groupList: TCastleVerticalGroup);
var
  suitPart: TItemCondition;
  newBtn, sampleBtn: TCastleButton;
  myBtnFactory: TCastleComponentFactory;
  myFont: TCastleAbstractFont;
begin
  { take appearance of button }
  if ((groupList.ControlsCount > 0) AND
      (groupList.Controls[0] is TCastleButton)) then
  begin
    sampleBtn:= groupList.Controls[0] as TCastleButton;
    myFont:= sampleBtn.CustomFont;
    myBtnFactory:= TCastleComponentFactory.Create(self);
    myBtnFactory.LoadFromComponent(sampleBtn);
  end else
  begin
    sampleBtn:= nil;
    myBtnFactory:= nil;
  end;

  groupList.ClearControls;


  { create remove Suit Part button }
  if Assigned(myBtnFactory) then
  begin
    newBtn:= myBtnFactory.ComponentLoad(groupList) as TCastleButton;
    newBtn.CustomFont:= myFont;
  end else
    newBtn:= TCastleButton.Create(groupList);

  newBtn.Caption:= NoSuitPartStr;
  newBtn.OnClick:= {$ifdef FPC}@{$endif}ClickSuitPart;
  newBtn.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusList;
  groupList.InsertFront(newBtn);

  { create button suit part list }
  for suitPart in FDresser.SuitPartsList(suitType) do
  begin
    if Assigned(myBtnFactory) then
    begin
      newBtn:= myBtnFactory.ComponentLoad(groupList) as TCastleButton;
      newBtn.CustomFont:= myFont;
    end else
      newBtn:= TCastleButton.Create(groupList);

    newBtn.Caption:= suitPart.Name;
    newBtn.OnClick:= {$ifdef FPC}@{$endif}ClickSuitPart;
    newBtn.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusList;
    groupList.InsertFront(newBtn);
  end;

  if Assigned(myBtnFactory) then
    FreeAndNil(myBtnFactory);
end;

procedure TViewDressingMenu.TViewDressingDialog.UpdateSuits();
var
  newBtn, sampleBtn: TCastleButton;
  myBtnFactory: TCastleComponentFactory;
  myFont: TCastleAbstractFont;
  suit: String;
begin
  { take appearance of button }
  if ((ListSuit.ControlsCount > 0) AND
      (ListSuit.Controls[0] is TCastleButton)) then
  begin
    sampleBtn:= ListSuit.Controls[0] as TCastleButton;
    myFont:= sampleBtn.CustomFont;
    myBtnFactory:= TCastleComponentFactory.Create(self);
    myBtnFactory.LoadFromComponent(sampleBtn);
  end else
  begin
    sampleBtn:= nil;
    myBtnFactory:= nil;
  end;

  ListSuit.ClearControls;

  { create button suit list }
  for suit in FDresser.SuitList do
  begin
    if Assigned(myBtnFactory) then
    begin
      newBtn:= myBtnFactory.ComponentLoad(ListSuit) as TCastleButton;
      newBtn.CustomFont:= myFont;
    end else
      newBtn:= TCastleButton.Create(ListSuit);

    newBtn.Caption:= suit;
    newBtn.OnClick:= {$ifdef FPC}@{$endif}ClickSuit;
    newBtn.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusList;
    ListSuit.InsertFront(newBtn);
  end;

  if Assigned(myBtnFactory) then
    FreeAndNil(myBtnFactory);
end;

procedure TViewDressingMenu.TViewDressingDialog.UpdateAccessories();
var
  acessory: TItemCondition;
  newChk, sampleChk: TCastleCheckbox;
  myChkFactory: TCastleComponentFactory;
  myFont: TCastleAbstractFont;
begin
  { take appearance of button }
  if ((ListAccessories.ControlsCount > 0) AND
      (ListAccessories.Controls[0] is TCastleCheckbox)) then
  begin
    sampleChk:= ListAccessories.Controls[0] as TCastleCheckbox;
    myFont:= sampleChk.CustomFont;
    myChkFactory:= TCastleComponentFactory.Create(self);
    myChkFactory.LoadFromComponent(sampleChk);
  end else
  begin
    sampleChk:= nil;
    myChkFactory:= nil;
  end;

  ListAccessories.ClearControls;

  { create switch accessory list }
  for acessory in FDresser.AcessoriesList do
  begin
    if Assigned(myChkFactory) then
    begin
      newChk:= myChkFactory.ComponentLoad(ListAccessories) as TCastleCheckbox;
      newChk.CustomFont:= myFont;
    end else
      newChk:= TCastleCheckbox.Create(ListAccessories);

    newChk.Caption:= acessory.Name;
    newChk.Checked:= acessory.Visible;
    newChk.OnChange:= {$ifdef FPC}@{$endif}ClickAccesories;
    newChk.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusList;
    ListAccessories.InsertFront(newChk);
  end;
end;

procedure TViewDressingMenu.TViewDressingDialog.FocusButton(const Sender: TCastleUserInterface);
begin
  SoundEngine.Play(NamedSound('SfxButtonFocus'));
end;

procedure TViewDressingMenu.TViewDressingDialog.FocusList(const Sender: TCastleUserInterface);
begin
  SoundEngine.Play(NamedSound('SfxListFocus'));
end;

procedure TViewDressingMenu.TViewDressingDialog.ClickSuitPart(Sender: TObject);
var
  button: TCastleButton;
  suitType: TSuitPart;
begin
  button:= Sender as TCastleButton;
  if NOT Assigned(button) then exit;

  SoundEngine.Play(NamedSound('SfxListPress'));

  Case button.Parent.Name of
    ListTopStr: suitType:= TSuitPart.Top;
    ListBottomStr: suitType:= TSuitPart.Bottom;
    ListArmsStr: suitType:= TSuitPart.Arms;
    ListFootsStr: suitType:= TSuitPart.Foots;
  end;

  FDresser.DressSuitPart(suitType, button.Caption);
end;

procedure TViewDressingMenu.TViewDressingDialog.ClickSuit(Sender: TObject);
var
  button: TCastleButton;
begin
  button:= Sender as TCastleButton;
  if NOT Assigned(button) then exit;

  SoundEngine.Play(NamedSound('SfxListPress'));

  FDresser.DressSuit(button.Caption);
end;

procedure TViewDressingMenu.TViewDressingDialog.ClickAccesories(Sender: TObject);
var
  check: TCastleCheckbox;
begin
  check:= Sender as TCastleCheckbox;
  if NOT Assigned(check) then exit;

  SoundEngine.Play(NamedSound('SfxListPress'));

  FDresser.DressAcessory(check.Caption, check.Checked);
end;

procedure TViewDressingMenu.TViewDressingDialog.ClickClose(Sender: TObject);
begin
  SoundEngine.Play(NamedSound('SfxButtonPress'));
  Closed:= True;
end;

{ ========= ------------------------------------------------------------------ }
{ TViewDressingMenu ---------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TViewDressingMenu.CreateUntilStopped(chara: TNyaActorChara);
begin
  inherited CreateUntilStopped;
  DesignUrl:= 'castle-data:/bgdressing.castle-user-interface';
  FChara:= chara;
end;

procedure TViewDressingMenu.Start;
begin
  inherited;
  InterceptInput:= True;

  FDialog:= TViewDressingDialog.Create(FreeAtStop, FChara);
  FDialog.Anchor(hpRight);
  FDialog.Anchor(vpBottom);
  FDialog.FullSize:= True;
  InsertFront(FDialog);
end;

procedure TViewDressingMenu.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;

  if FDialog.Closed then
    Container.PopView(Self);
end;

end.
