{ Main view, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleWindow, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTimeUtils,
  CastleTransform, CastleQuaternions, CastleScene, CastleFlashEffect,
  ActorChara;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    BtnExit: TCastleButton;
    BtnStart: TCastleButton;
    BtnSettings: TCastleButton;
    LabelFps: TCastleLabel;
    LabelInfo1: TCastleLabel;
    LabelInfo2: TCastleLabel;
    CameraMain: TCastleCamera;
    FlashEffect: TCastleFlashEffect;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  private
    FActorGirl: TActorChara;
    FActorBoy: TActorChara;
    FCurPos: TVector2;
    FCameraRatation: TQuaternion;
    procedure ClickExit(Sender: TObject);
    procedure ClickStart(Sender: TObject);
    procedure ClicSettings(Sender: TObject);
    procedure CharaActionWaiting;
    procedure UpdateCamera(const SecondsPassed: Single); { follow cameta rotation to cursor }
  end;

var
  ViewMain: TViewMain;

implementation

uses
  SysUtils, CastleUtils, GameViewLoading, GameViewTravel;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl:= 'castle-data:/gameviewmain.castle-user-interface';
  FActorGirl:=  nil;
  FActorBoy:=  nil;
  FCurPos:= Vector2(0.0, 0.0);
end;

procedure TViewMain.Start;
var
  GirlScene: TCastleTransformDesign;
  BoyScene: TCastleTransformDesign;
begin
  inherited;

  BtnExit.OnClick:= {$ifdef FPC}@{$endif}ClickExit;
  BtnStart.OnClick:= {$ifdef FPC}@{$endif}ClickStart;
  BtnSettings.OnClick:= {$ifdef FPC}@{$endif}ClicSettings;

  { appear fade animator }
  FlashEffect.Flash(Vector4(0.0, 0.0, 0.0, 0.9), True);

  { Create Girl Character instance }
  GirlScene:= DesignedComponent('CharaGirl') as TCastleTransformDesign;
  FActorGirl:= TActorChara.Create(GirlScene, 'Girl');

  { Create Boy Character instance }
  BoyScene:= DesignedComponent('CharaBoy') as TCastleTransformDesign;
  FActorBoy:= TActorChara.Create(BoyScene, 'Boy');

  { set character self emission }
  FActorGirl.SelfEmission:= 0.15;
  FActorBoy.SelfEmission:= 0.15;

  { remember initial camera rotation }
  FCameraRatation:= QuatFromAxisAngle(CameraMain.Rotation);

  { default chara action }
  CharaActionWaiting;
end;

procedure TViewMain.Stop;
begin
  FreeAndNil(FActorGirl);
  FreeAndNil(FActorBoy);
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption:= 'FPS: ' + Container.Fps.ToString;

  UpdateCamera(SecondsPassed);
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

procedure TViewMain.ClickStart(Sender: TObject);
begin
  ViewLoading.SetToLoad(ViewTravel);
  Container.View:= ViewLoading;
end;

procedure TViewMain.ClicSettings(Sender: TObject);
begin
  { #todo : need game settings }
end;

procedure TViewMain.CharaActionWaiting;
begin
  FActorGirl.PlayAnimation('GAME.TOGETHER.INTRO.WAITING');
  FActorGirl.Translation:= Vector3(59, 0, 13);
  FActorGirl.Rotation:= Vector4(0, 0, 0, 0);
  FActorBoy.PlayAnimation('GAME.TOGETHER.INTRO.WAITING');
  FActorBoy.Translation:= Vector3(81, 0, 56);
  FActorBoy.Rotation:= Vector4(0, -1, 0, Pi/2);
end;

procedure TViewMain.UpdateCamera(const SecondsPassed: Single);
var
  curPos: TVector2;
  rotatorX, rotatorY: TQuaternion;
begin
  curPos.X:= (Container.MousePosition.X / Container.PixelsWidth - 0.5) * 2.0;
  curPos.Y:= (Container.MousePosition.Y / Container.PixelsHeight - 0.5) * 2.0;

  curPos:= Lerp(1.0 * SecondsPassed, FCurPos, curPos);

  rotatorX:= QuatFromAxisAngle(Vector4(0, 1, 0, -Pi/20.0 * curPos.X));
  rotatorY:= QuatFromAxisAngle(Vector4(1, 0, 0, Pi/24.0 * curPos.Y));

  CameraMain.Rotation:= (FCameraRatation * rotatorX * rotatorY).ToAxisAngle;

  FCurPos:= curPos;
end;

end.
