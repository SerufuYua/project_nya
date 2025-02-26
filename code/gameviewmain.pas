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
  NyaActorChara;

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
    CharaGirl: TNyaActorChara;
    CharaBoy: TNyaActorChara;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  private
    FCurPos: TVector2;
    FCameraRatation: TQuaternion;
    procedure ClickExit(Sender: TObject);
    procedure ClickStart(Sender: TObject);
    procedure ClicSettings(Sender: TObject);
    procedure UpdateCamera(const SecondsPassed: Single); { follow cameta rotation to cursor }
  end;

var
  ViewMain: TViewMain;

implementation

uses
  SysUtils, CastleUtils, GameViewLoading, GameViewTravelContainerRoom;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl:= 'castle-data:/gameviewmain.castle-user-interface';
  FCurPos:= Vector2(0.0, 0.0);
end;

procedure TViewMain.Start;
begin
  inherited;

  BtnExit.OnClick:= {$ifdef FPC}@{$endif}ClickExit;
  BtnStart.OnClick:= {$ifdef FPC}@{$endif}ClickStart;
  BtnSettings.OnClick:= {$ifdef FPC}@{$endif}ClicSettings;

  { appear fade animator }
  FlashEffect.Flash(Vector4(0.0, 0.0, 0.0, 0.9), True);

  { remember initial camera rotation }
  FCameraRatation:= QuatFromAxisAngle(CameraMain.Rotation);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption:= 'FPS: ' + Container.Fps.ToString;

  UpdateCamera(SecondsPassed);
end;

procedure TViewMain.ClickExit(Sender: TObject);
begin
  Application.MainWindow.Close();
end;

procedure TViewMain.ClickStart(Sender: TObject);
begin
  ViewLoading.SetToLoad(ViewTravelContainerRoom);
  Container.View:= ViewLoading;
end;

procedure TViewMain.ClicSettings(Sender: TObject);
begin
  { #todo : need game settings }
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
