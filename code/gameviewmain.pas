{ Main view, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleWindow, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTimeUtils,
  CastleTransform, CastleQuaternions, CastleScene,
  CharaGirlBehavior, CharaBoyBehavior, FadeInOut;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    BtnExit: TCastleButton;
    BtnPlaySolo: TCastleButton;
    BtnPlayTogether: TCastleButton;
    LabelFps: TCastleLabel;
    LabelInfo1: TCastleLabel;
    LabelInfo2: TCastleLabel;
    CameraMain: TCastleCamera;
    ScreenRectangle: TCastleRectangleControl;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  private
    GirlBehavior: TCharaGirlBehavior;
    BoyBehavior: TCharaBoyBehavior;
    Fader: TRectangleFader;
    CameraRatation: TQuaternion;
    procedure ClickExit(Sender: TObject);
    procedure ClickSceneGirl(Sender: TObject);
    procedure ClicSceneTogether(Sender: TObject);
    procedure CharaActionWaiting;
    procedure UpdateCamera; { follow cameta rotation to cursor }
  end;

var
  ViewMain: TViewMain;

implementation

uses
  SysUtils, GameViewPlayGirl, GameViewPlayTogether;

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
  BtnPlaySolo.OnClick:= {$ifdef FPC}@{$endif}ClickSceneGirl;
  BtnPlayTogether.OnClick:= {$ifdef FPC}@{$endif}ClicSceneTogether;

  { set fog animator }
  Fader:= TRectangleFader.Create(ScreenRectangle);
  Fader.SetFade(1.0, 0.0, 3.0);

  { Create Girl Character instance }
  GirlScene := DesignedComponent('CharaGirl') as TCastleTransformDesign;
  GirlBehavior := TCharaGirlBehavior.Create(FreeAtStop);
  GirlScene.AddBehavior(GirlBehavior);

  { Create Boy Character instance }
  BoyScene := DesignedComponent('CharaBoy') as TCastleTransformDesign;
  BoyBehavior := TCharaBoyBehavior.Create(FreeAtStop);
  BoyScene.AddBehavior(BoyBehavior);

  { set character self emission }
  GirlBehavior.SelfEmission:= 0.15;
  BoyBehavior.SelfEmission:= 0.15;

  { remember initial camera rotation }
  CameraRatation:= QuatFromAxisAngle(CameraMain.Rotation);

  { default chara action }
  CharaActionWaiting;
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

  Fader.AnimateQuadFade(SecondsPassed);
  UpdateCamera;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result:= inherited;
  if Result then Exit; // allow the ancestor to handle keys
end;

procedure TViewMain.ClickExit(Sender: TObject);
begin
  Application.MainWindow.Close();
end;

procedure TViewMain.ClickSceneGirl(Sender: TObject);
begin
  Container.View:= ViewPlayGirl;
end;

procedure TViewMain.ClicSceneTogether(Sender: TObject);
begin
  Container.View:= ViewPlayTogether;
end;

procedure TViewMain.CharaActionWaiting;
begin
  GirlBehavior.PlayAnimation('GAME.TOGETHER.INTRO.WAITING');
  GirlBehavior.Pos:= Vector3(59, 0, 13);
  GirlBehavior.Rot:= Vector4(0, 0, 0, 0);
  BoyBehavior.PlayAnimation('GAME.TOGETHER.INTRO.WAITING');
  BoyBehavior.Pos:= Vector3(81, 0, 56);
  BoyBehavior.Rot:= Vector4(0, -1, 0, Pi/2);
end;

procedure TViewMain.UpdateCamera;
var
  cursorX, cursorY: Single;
  rotatorX, rotatorY: TQuaternion;
begin
  cursorX:= (Container.MousePosition.X / Container.PixelsWidth - 0.5) * 2.0;
  cursorY:= (Container.MousePosition.Y / Container.PixelsHeight - 0.5) * 2.0;

  rotatorX:= QuatFromAxisAngle(Vector4(0, 1, 0, -Pi/24.0 * cursorX));
  rotatorY:= QuatFromAxisAngle(Vector4(1, 0, 0, Pi/24.0 * cursorY));

  CameraMain.Rotation:= (CameraRatation * rotatorX * rotatorY).ToAxisAngle;
end;

end.
