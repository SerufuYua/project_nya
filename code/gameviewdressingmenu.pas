unit GameViewDressingMenu;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  NyaActorChara, CastleColors, CharaDress;

type
  TViewDressingMenu = class(TCastleView)
  published
    ImageControl: TCastleImageControl;
    ListSuit: TCastleVerticalGroup;
    ListTop: TCastleVerticalGroup;
    ListBottom: TCastleVerticalGroup;
    ListArms: TCastleVerticalGroup;
    ListFoots: TCastleVerticalGroup;
    ListAccessories: TCastleVerticalGroup;
    BtnClose: TcastleButton;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: boolean); override;
    procedure SetChara(chara: TNyaActorChara);
    procedure SetColor(color: TCastleColorRGB);
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
  end;

var
  ViewDressingMenu: TViewDressingMenu;

implementation

uses
  CastleScene, CastleComponentSerialize, CastleFonts, NyaCastleUtils,
  CastleSoundEngine, GameSound,
  SysUtils, StrUtils;

const
  RootItemStr = 'MenuRoot';
  ListTopStr = 'ListTop';
  ListBottomStr = 'ListBottom';
  ListArmsStr = 'ListArms';
  ListFootsStr = 'ListFoots';

constructor TViewDressingMenu.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewdressingmenu.castle-user-interface';
end;

procedure TViewDressingMenu.Start;
begin
  inherited;
  { Executed once when view starts. }

  BtnClose.OnClick:= {$ifdef FPC}@{$endif}ClickClose;
  BtnClose.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
end;

procedure TViewDressingMenu.Update(const SecondsPassed: Single;
                                   var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }
end;

procedure TViewDressingMenu.SetChara(chara: TNyaActorChara);
begin
  if NOT Assigned(chara) then Exit;
  FDresser:= chara.Dresser();
  UpdateSuitParts();
  UpdateAccessories();

  { Set personal color as background }
  SetColor(chara.PersonalColor);
end;

procedure TViewDressingMenu.SetColor(color: TCastleColorRGB);
var
  rootItem: TCastleUserInterface;
  item: TCastleImageControl;
  alpha: single;
begin
  rootItem:= DesignedComponent(RootItemStr) as TCastleUserInterface;

  for item in GetAllUIImages(rootItem) do
  begin
    if (item.Tag = 1) then
    begin
      alpha:= item.Color.W;
      item.Color:= Vector4(color.X, color.Y, color.Z, alpha);
    end;
  end;
end;

procedure TViewDressingMenu.UpdateSuitParts();
begin
  UpdateSuitParts(TSuitPart.Top, ListTop);
  UpdateSuitParts(TSuitPart.Bottom, ListBottom);
  UpdateSuitParts(TSuitPart.Arms, ListArms);
  UpdateSuitParts(TSuitPart.Foots, ListFoots);
  UpdateSuits();
end;

procedure TViewDressingMenu.UpdateSuitParts(suitType: TSuitPart;
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

procedure TViewDressingMenu.UpdateSuits();
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

procedure TViewDressingMenu.UpdateAccessories();
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

procedure TViewDressingMenu.FocusButton(const Sender: TCastleUserInterface);
begin
  SoundEngine.Play(NamedSound('SfxButtonFocus'));
end;

procedure TViewDressingMenu.FocusList(const Sender: TCastleUserInterface);
begin
  SoundEngine.Play(NamedSound('SfxListFocus'));
end;

procedure TViewDressingMenu.ClickSuitPart(Sender: TObject);
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

procedure TViewDressingMenu.ClickSuit(Sender: TObject);
var
  button: TCastleButton;
begin
  button:= Sender as TCastleButton;
  if NOT Assigned(button) then exit;

  SoundEngine.Play(NamedSound('SfxListPress'));

  FDresser.DressSuit(button.Caption);
end;

procedure TViewDressingMenu.ClickAccesories(Sender: TObject);
var
  check: TCastleCheckbox;
begin
  check:= Sender as TCastleCheckbox;
  if NOT Assigned(check) then exit;

  SoundEngine.Play(NamedSound('SfxListPress'));

  FDresser.DressAcessory(check.Caption, check.Checked);
end;

procedure TViewDressingMenu.ClickClose(Sender: TObject);
begin
  SoundEngine.Play(NamedSound('SfxButtonPress'));
  Container.PopView(self);
end;

end.
