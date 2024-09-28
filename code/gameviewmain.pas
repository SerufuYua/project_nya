{ Main view, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleWindow, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse,
  CastleTransform, CharaBoyBehavior, CharaGirlBehavior;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    BtnExit: TCastleButton;
    BtnAnimStand: TCastleButton;
    BtnAnimWalk: TCastleButton;
    BtnAnimRun: TCastleButton;
    BtnSceneGirl: TCastleButton;
    BtnSceneTogether: TCastleButton;
    LabelFps: TCastleLabel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  private
    GirlBehavior: TCharaGirlBehavior;
    BoyBehavior: TCharaBoyBehavior;
    procedure ClickExit(Sender: TObject);
    procedure ClickCaharsStand(Sender: TObject);
    procedure ClickCaharsWalk(Sender: TObject);
    procedure ClickCaharsRun(Sender: TObject);
    procedure ClickSceneGirl(Sender: TObject);
    procedure ClicSceneTogether(Sender: TObject);
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, GameViewPlayGirl, GameViewPlayTogether;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
  GirlBehavior :=  nil;
end;

procedure TViewMain.Start;
var
  GirlScene: TCastleTransformDesign;
  BoyScene: TCastleTransformDesign;
begin
  inherited;

  BtnExit.OnClick:= {$ifdef FPC}@{$endif}ClickExit;
  BtnAnimStand.OnClick:= {$ifdef FPC}@{$endif}ClickCaharsStand;
  BtnAnimWalk.OnClick:= {$ifdef FPC}@{$endif}ClickCaharsWalk;
  BtnAnimRun.OnClick:= {$ifdef FPC}@{$endif}ClickCaharsRun;
  BtnSceneGirl.OnClick:= {$ifdef FPC}@{$endif}ClickSceneGirl;
  BtnSceneTogether.OnClick:= {$ifdef FPC}@{$endif}ClicSceneTogether;

  { Create Girl Character instance }
  GirlScene := DesignedComponent('CharaGirl') as TCastleTransformDesign;
  GirlBehavior := TCharaGirlBehavior.Create(FreeAtStop);
  GirlScene.AddBehavior(GirlBehavior);

  { Create Boy Character instance }
  BoyScene := DesignedComponent('CharaBoy') as TCastleTransformDesign;
  BoyBehavior := TCharaBoyBehavior.Create(FreeAtStop);
  BoyScene.AddBehavior(BoyBehavior);
end;

procedure TViewMain.Stop;
begin
  FreeAndNil(GirlBehavior);
  FreeAndNil(BoyBehavior);
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption:= 'FPS: ' + Container.Fps.ToString;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result:= inherited;
  if Result then Exit; // allow the ancestor to handle keys

{  if Event.IsKey(keyEscape) then
  begin
    Close;
    Exit(true);
  end;}

{  if Event.IsKey(keyX) then
  begin
    GirlBehavior.ActionWear('Swimsuit');
    BoyBehavior.ActionWear('Swimsuit');
    Exit(true);
  end;}
end;

procedure TViewMain.ClickExit(Sender: TObject);
begin
  Application.MainWindow.Close();
end;

procedure TViewMain.ClickCaharsStand(Sender: TObject);
begin
  GirlBehavior.ActionStand;
  BoyBehavior.ActionStand;
end;

procedure TViewMain.ClickCaharsWalk(Sender: TObject);
begin
  GirlBehavior.ActionWalk;
  BoyBehavior.ActionWalk;
end;

procedure TViewMain.ClickCaharsRun(Sender: TObject);
begin
  GirlBehavior.ActionRun;
  BoyBehavior.ActionRun;
end;

procedure TViewMain.ClickSceneGirl(Sender: TObject);
begin
  Container.View:= ViewPlayGirl;
end;

procedure TViewMain.ClicSceneTogether(Sender: TObject);
begin
  Container.View:= ViewPlayTogether;
end;

end.
