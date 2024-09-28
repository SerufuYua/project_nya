unit GameViewDressingMenu;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CharaBehavior, CastleColors, CharaDress;

type
  TViewDressingMenu = class(TCastleView)
  published
    RectangleControl: TCastleRectangleControl;
    ListTop: TCastleVerticalGroup;
    ListBottom: TCastleVerticalGroup;
    ListArms: TCastleVerticalGroup;
    ListFoots: TCastleVerticalGroup;
    ListAccessories: TCastleVerticalGroup;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: boolean); override;
    procedure SetChara(chara: TCharaBehavior);
    procedure SetColor(color: TCastleColorRGB);
  private
    Dresser: TCharaDresser;
    procedure UpdateSuits();
    procedure UpdateSuits(suitType: TSuits; groupList: TCastleVerticalGroup);
    procedure UpdateAccessories();
    procedure ClickSuit(Sender: TObject);
    procedure ClickAccesories(Sender: TObject);
  end;

var
  ViewDressingMenu: TViewDressingMenu;

implementation

uses CastleScene, MyCastleUtils;

constructor TViewDressingMenu.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewdressingmenu.castle-user-interface';
end;

procedure TViewDressingMenu.Start;
begin
  inherited;
  { Executed once when view starts. }
end;

procedure TViewDressingMenu.Update(const SecondsPassed: Single;
                                   var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }
end;

procedure TViewDressingMenu.SetChara(chara: TCharaBehavior);
begin
  if NOT Assigned(chara) then Exit;
  Dresser:= chara.GetDresser();
  UpdateSuits();
  UpdateAccessories();

  { Set personal color as background }
  SetColor(chara.PersonalColor);
end;

procedure TViewDressingMenu.SetColor(color: TCastleColorRGB);
var
  alpha: Single;
begin
  { Set personal color}
  alpha:= RectangleControl.Color.W;
  RectangleControl.Color:= Vector4(color.X, color.Y, color.Z, alpha);
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
  suits: TShapeNames;
  suit: String;
  newBtn: TCastleButton;
begin
  suits:= Dresser.GetSuitsList(suitType);
  groupList.ClearControls;

  newBtn:= TCastleButton.Create(groupList);
  newBtn.Caption:= 'none';
  newBtn.OnClick := {$ifdef FPC}@{$endif} ClickSuit;
  groupList.InsertFront(newBtn);

  for suit in suits do
  begin
    newBtn:= TCastleButton.Create(groupList);
    newBtn.Caption:= suit;
    newBtn.OnClick := {$ifdef FPC}@{$endif} ClickSuit;
    groupList.InsertFront(newBtn);
  end;
end;

procedure TViewDressingMenu.UpdateAccessories();
var
  acessories: TShapeNames;
  acessory: String;
  newChk: TCastleCheckbox;
begin
  acessories:= Dresser.GetAcessoriesList();
  ListAccessories.ClearControls;

  for acessory in acessories do
  begin
    newChk:= TCastleCheckbox.Create(ListAccessories);
    newChk.Caption:= acessory;
    newChk.OnChange := {$ifdef FPC}@{$endif} ClickAccesories;
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

  Dresser.WearSuit(suitType, button.Caption);
end;

procedure TViewDressingMenu.ClickAccesories(Sender: TObject);
var
  check: TCastleCheckbox;
begin
  check:= Sender as TCastleCheckbox;
  if NOT Assigned(check) then exit;

  Dresser.WearAcessory(check.Caption, check.Checked);
end;

end.
