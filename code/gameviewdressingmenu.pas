unit GameViewDressingMenu;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CharaBehavior, CastleColors, CharaDress;

type
  TViewDressingMenu = class(TCastleView)
  published
    RectangleControl: TCastleRectangleControl;
    VerticalGroupTop: TCastleVerticalGroup;
    VerticalGroupBottom: TCastleVerticalGroup;
    VerticalGroupArms: TCastleVerticalGroup;
    VerticalGroupFoots: TCastleVerticalGroup;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: boolean); override;
    procedure SetChara(chara: TCharaBehavior);
    procedure SetColor(color: TCastleColorRGB);
  private
    currentDresser: TCharaDresser;
    procedure UpdateSuits();
    procedure UpdateSuits(suitType: TSuits; groupList: TCastleVerticalGroup);
    procedure ClickSuit(Sender: TObject);
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
  currentDresser:= chara.GetDresser();
  UpdateSuits();

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
  UpdateSuits(TSuits.Top, VerticalGroupTop);
  UpdateSuits(TSuits.Bottom, VerticalGroupBottom);
  UpdateSuits(TSuits.Arms, VerticalGroupArms);
  UpdateSuits(TSuits.Foots, VerticalGroupFoots);
end;

procedure TViewDressingMenu.UpdateSuits(suitType: TSuits;
                                        groupList: TCastleVerticalGroup);
var
  suits: TShapeNames;
  suit: String;
  newLbl: TCastleLabel;
  newBtn: TCastleButton;
begin
  suits:= currentDresser.GetSuitsList(suitType);
  groupList.ClearControls;

  newLbl:= TCastleLabel.Create(FreeAtStop);
  newLbl.Caption:= '';

  newBtn:= TCastleButton.Create(FreeAtStop);
  newBtn.Caption:= 'none';
  newBtn.OnClick := {$ifdef FPC}@{$endif} ClickSuit;
  groupList.InsertFront(newBtn);

  for suit in suits do
  begin
    newBtn:= TCastleButton.Create(FreeAtStop);
    newBtn.Caption:= suit;
    newBtn.OnClick := {$ifdef FPC}@{$endif} ClickSuit;
    groupList.InsertFront(newBtn);
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
  'VerticalGroupTop': suitType:= TSuits.Top;
  'VerticalGroupBottom': suitType:= TSuits.Bottom;
  'VerticalGroupArms': suitType:= TSuits.Arms;
  'VerticalGroupFoots': suitType:= TSuits.Foots;
  else
    suitType:= TSuits.All;
  end;

  currentDresser.WearSuit(suitType, button.Caption);
end;

end.
