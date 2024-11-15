unit GameViewDressingMenu;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  ActorChara, CastleColors, CharaDress;

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
    procedure SetChara(chara: TActorChara);
    procedure SetColor(color: TCastleColorRGB);
  private
    FDresser: TCharaDresser;
    procedure UpdateSuits();
    procedure UpdateSuits(suitType: TSuits; groupList: TCastleVerticalGroup);
    procedure UpdateAccessories();
    procedure ClickSuit(Sender: TObject);
    procedure ClickAccesories(Sender: TObject);
    procedure ClickClose(Sender: TObject);
  end;

var
  ViewDressingMenu: TViewDressingMenu;

implementation

uses CastleScene, CastleComponentSerialize, CastleFonts, MyCastleUtils,
  SysUtils;

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

procedure TViewDressingMenu.SetChara(chara: TActorChara);
begin
  if NOT Assigned(chara) then Exit;
  FDresser:= chara.GetDresser();
  UpdateSuits();
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
  rootItem:= DesignedComponent('MenuRoot') as TCastleUserInterface;

  for item in GetAllUIImages(rootItem) do
  begin
      alpha:= item.Color.W;
      item.Color:= Vector4(color.X, color.Y, color.Z, alpha);
  end;
end;

procedure TViewDressingMenu.UpdateSuits();
begin
  UpdateSuits(TSuits.Top, ListTop);
  UpdateSuits(TSuits.Bottom, ListBottom);
  UpdateSuits(TSuits.Arms, ListArms);
  UpdateSuits(TSuits.Foots, ListFoots);
end;

procedure TViewDressingMenu.UpdateSuits(suitType: TSuits;
                                        groupList: TCastleVerticalGroup);
var
  suits: TItemConditions;
  suit: TItemCondition;
  newBtn, sampleBtn: TCastleButton;
  myBtnFactory: TCastleComponentFactory;
  myFont: TCastleAbstractFont;
begin
  suits:= FDresser.GetSuitsList(suitType);

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

  newBtn.Caption:= 'none';
  newBtn.OnClick:= {$ifdef FPC}@{$endif}ClickSuit;
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
    newBtn.OnClick:= {$ifdef FPC}@{$endif}ClickSuit;
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
  acessories:= FDresser.GetAcessoriesList();

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

procedure TViewDressingMenu.ClickSuit(Sender: TObject);
var
  button: TCastleButton;
  suitType: TSuits;
begin
  button:= Sender as TCastleButton;
  if NOT Assigned(button) then exit;

  Case button.Parent.Name of
  'ListTop': suitType:= TSuits.Top;
  'ListBottom': suitType:= TSuits.Bottom;
  'ListArms': suitType:= TSuits.Arms;
  'ListFoots': suitType:= TSuits.Foots;
  else
    suitType:= TSuits.All;
  end;

  FDresser.WearSuit(suitType, button.Caption);
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
