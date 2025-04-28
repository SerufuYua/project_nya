unit GameViewDressingMenu;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  NyaActorChara, CastleColors, CharaDress;

type
  TViewDressingMenu = class(TCastleView)
  published
    ImageControl: TCastleImageControl;
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
    procedure UpdateAccessories();
    procedure ClickSuitPart(Sender: TObject);
    procedure ClickAccesories(Sender: TObject);
    procedure ClickClose(Sender: TObject);
  end;

var
  ViewDressingMenu: TViewDressingMenu;

implementation

uses CastleScene, CastleComponentSerialize, CastleFonts, NyaCastleUtils,
  SysUtils;

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
end;

procedure TViewDressingMenu.UpdateSuitParts(suitType: TSuitPart;
                                        groupList: TCastleVerticalGroup);
var
  suits: TItemConditions;
  suit: TItemCondition;
  newBtn, sampleBtn: TCastleButton;
  myBtnFactory: TCastleComponentFactory;
  myFont: TCastleAbstractFont;
begin
  suits:= FDresser.SuitPartsList(suitType);

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

  if Assigned(myBtnFactory) then
  begin
    newBtn:= myBtnFactory.ComponentLoad(groupList) as TCastleButton;
    newBtn.CustomFont:= myFont;
  end else
    newBtn:= TCastleButton.Create(groupList);

  newBtn.Caption:= NoSuitPartStr;
  newBtn.OnClick:= {$ifdef FPC}@{$endif}ClickSuitPart;
  groupList.InsertFront(newBtn);

  for suit in suits do
  begin
    if Assigned(myBtnFactory) then
    begin
      newBtn:= myBtnFactory.ComponentLoad(groupList) as TCastleButton;
      newBtn.CustomFont:= myFont;
    end else
      newBtn:= TCastleButton.Create(groupList);

    newBtn.Caption:= suit.Name;
    newBtn.OnClick:= {$ifdef FPC}@{$endif}ClickSuitPart;
    groupList.InsertFront(newBtn);
  end;

  if Assigned(myBtnFactory) then
    FreeAndNil(myBtnFactory);
end;

procedure TViewDressingMenu.UpdateAccessories();
var
  acessories: TItemConditions;
  acessory: TItemCondition;
  newChk, sampleChk: TCastleCheckbox;
  myChkFactory: TCastleComponentFactory;
  myFont: TCastleAbstractFont;
begin
  acessories:= FDresser.AcessoriesList();

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

  for acessory in acessories do
  begin
    if Assigned(myChkFactory) then
    begin
      newChk:= myChkFactory.ComponentLoad(ListAccessories) as TCastleCheckbox;
      newChk.CustomFont:= myFont;
    end else
      newChk:= TCastleCheckbox.Create(ListAccessories);

    newChk.Caption:= acessory.Name;
    newChk.Checked:= acessory.Visible;
    newChk.OnChange:= {$ifdef FPC}@{$endif} ClickAccesories;
    ListAccessories.InsertFront(newChk);
  end;
end;

procedure TViewDressingMenu.ClickSuitPart(Sender: TObject);
var
  button: TCastleButton;
  suitType: TSuitPart;
begin
  button:= Sender as TCastleButton;
  if NOT Assigned(button) then exit;

  Case button.Parent.Name of
    ListTopStr: suitType:= TSuitPart.Top;
    ListBottomStr: suitType:= TSuitPart.Bottom;
    ListArmsStr: suitType:= TSuitPart.Arms;
    ListFootsStr: suitType:= TSuitPart.Foots;
  else
    suitType:= TSuitPart.All;
  end;

  FDresser.WearSuitPart(suitType, button.Caption);
end;

procedure TViewDressingMenu.ClickAccesories(Sender: TObject);
var
  check: TCastleCheckbox;
begin
  check:= Sender as TCastleCheckbox;
  if NOT Assigned(check) then exit;

  FDresser.WearAcessory(check.Caption, check.Checked);
end;

procedure TViewDressingMenu.ClickClose(Sender: TObject);
begin
  Container.PopView(self);
end;

end.
