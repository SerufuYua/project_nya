unit BaseView;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleUIControls, CastleVectors, CastleColors,
  CastleControls, CastleViewport, CastleKeysMouse, CastleDebugTransform,
  CastleCameras, NyaActor;

type
  TViewWarper = class(TCastleView)
  protected
    FGetToGo: TViewWarper;
    FActorMain: TNyaActor;
    FMainActorTrans: TVector3;
    FMainActorRot: TVector4;
    FMainActorNewTrans: Boolean;
    FMainActorNewRot: Boolean;
    procedure SetMainActor(AActor: TNyaActor);
    procedure SetMainActorTrans(value: TVector3);
    procedure SetMainActorRot(value: TVector4);
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    procedure GetToGo(ALocation: TViewWarper);
    procedure GetToGo(ALocation: TViewWarper;
                      ATranslation: TVector3;
                      ARotation: TVector4);
    property MainActor: TNyaActor read FActorMain write SetMainActor;
    property MainActorTrans: TVector3 read FMainActorTrans write SetMainActorTrans;
    property MainActorRot: TVector4 read FMainActorRot write SetMainActorRot;
  end;

  TBaseView = class(TViewWarper)
  published
    Map: TCastleDesign;
  public
    procedure Start; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    procedure Pause; override;
    procedure Resume; override;
  protected
    FMainViewport: TCastleViewport;
    FDebugAvatar: TDebugTransform;
    FCameraNavigation: TCastleMouseLookNavigation;
    FKeyDebug: TKey;
    procedure SetUIColor(AColor: TCastleColorRGB);
  end;

implementation

uses
  GameViewLoading, NyaCastleUtils;

{ ========= ------------------------------------------------------------------ }
{ TViewWarper ---------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

procedure TViewWarper.Start;
begin
  inherited;

  { reset go to map }
  FGetToGo:= nil;

  if Assigned(MainActor) then
  begin
    if FMainActorNewTrans then
      MainActor.Translation:= MainActorTrans;
    if FMainActorNewRot then
      MainActor.Rotation:= MainActorRot;
  end;

  FMainActorNewTrans:= False;
  FMainActorNewRot:= False;
end;

procedure TViewWarper.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;

  { change map }
  if Assigned(FGetToGo) then
  begin
    ViewLoading.SetToLoad(FGetToGo);
    Container.View:= ViewLoading;
    Exit;
  end;
end;

procedure TViewWarper.SetMainActor(AActor: TNyaActor);
begin
  if (FActorMain = AActor) then Exit;

  FActorMain:= AActor;
end;

procedure TViewWarper.GetToGo(ALocation: TViewWarper);
begin
  FGetToGo:= ALocation;
end;

procedure TViewWarper.GetToGo(ALocation: TViewWarper;
                              ATranslation: TVector3; ARotation: TVector4);
begin
  ALocation.MainActorTrans:= ATranslation;
  ALocation.MainActorRot:= ARotation;
  FGetToGo:= ALocation;
end;

procedure TViewWarper.SetMainActorTrans(value: TVector3);
begin
  FMainActorTrans:= value;
  FMainActorNewTrans:= True;
end;

procedure TViewWarper.SetMainActorRot(value: TVector4);
begin
  FMainActorRot:= value;
  FMainActorNewRot:= True;
end;

{ ========= ------------------------------------------------------------------ }
{ TBaseView ------------------------------------------------------------------ }
{ ========= ------------------------------------------------------------------ }

procedure TBaseView.Start;
begin
  inherited;

  { Visualize SceneAvatar bounding box, sphere, middle point, direction etc. }
  FDebugAvatar:= TDebugTransform.Create(FreeAtStop);
  FDebugAvatar.Parent:= MainActor;
  FDebugAvatar.Exists:= False;

  { set Main Viewport }
  if Assigned(Map) then
    FMainViewport:= Map.DesignedComponent('ViewportMain') as TCastleViewport
  else
    FMainViewport:= DesignedComponent('ViewportMain') as TCastleViewport;

  { set keys }
  FKeyDebug:= TKey.keyF4;
end;

procedure TBaseView.SetUIColor(AColor: TCastleColorRGB);
var
  rootItem: TCastleUserInterface;
  item: TCastleImageControl;
  alpha: single;
begin
  rootItem:= DesignedComponent('SceneMain') as TCastleUserInterface;

  for item in GetAllUIImages(rootItem) do
  begin
    alpha:= item.Color.W;
    if (item.Tag = 1) then
      item.Color:= Vector4(AColor, alpha);
  end;
end;

function TBaseView.Press(const Event: TInputPressRelease): Boolean;
begin
  Result:= inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { enable camera control }
  if Event.IsMouseButton(buttonRight) OR Event.IsMouseButton(buttonMiddle) then
  begin
    FCameraNavigation.MouseLook:= True;
    Exit(true);
  end;

  { show debug }
  if Event.IsKey(FKeyDebug) then
  begin
    FDebugAvatar.Exists:= NOT FDebugAvatar.Exists;
    Exit(true);
  end;
end;

function TBaseView.Release(const Event: TInputPressRelease): boolean;
begin
  { disable camera control }
  if Event.IsMouseButton(buttonRight) OR Event.IsMouseButton(buttonMiddle) then
    FCameraNavigation.MouseLook:= False;

  Result := inherited;
end;

procedure TBaseView.Pause;
begin
  inherited;
  if Assigned(FCameraNavigation) then
    FCameraNavigation.MouseLook:= False;
  if Assigned(FMainViewport) then
    FMainViewport.Items.TimeScale:= 0;
end;

procedure TBaseView.Resume;
begin
  inherited;
  if Assigned(FMainViewport) then
    FMainViewport.Items.TimeScale:= 1;
end;

end.

