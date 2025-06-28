unit NyaActorVehicle;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, NyaActor, CastleClassUtils, CastleSceneCore,
  NyaCharaDress, CastleTransform, NyaCastleUtils, CastleScene, CastleTimeUtils,
  CastleParticleEmitter,
  NyaSoundControl;

type
  TNyaActorVehicle = class(TNyaActor)
  protected
    FWheel1SceneName: String;
    FWheel2SceneName: String;
    FWheel3SceneName: String;
    FWheel4SceneName: String;
    FWheel1Scene: TCastleScene;
    FWheel2Scene: TCastleScene;
    FWheel3Scene: TCastleScene;
    FWheel4Scene: TCastleScene;
    FWheel1Speed: Single;
    FWheel2Speed: Single;
    FWheel3Speed: Single;
    FWheel4Speed: Single;
    procedure SetWheel1Scene(const value: String);
    procedure SetWheel2Scene(const value: String);
    procedure SetWheel3Scene(const value: String);
    procedure SetWheel4Scene(const value: String);
    procedure UpdateWheelScenes;
    procedure WheelRotor(const SecondsPassed: Single);
  public
    const
      DefaultWheelSpeed = 1.0;

    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property Wheel1SceneName: String read FWheel1SceneName write SetWheel1Scene;
    property Wheel2SceneName: String read FWheel2SceneName write SetWheel2Scene;
    property Wheel3SceneName: String read FWheel3SceneName write SetWheel3Scene;
    property Wheel4SceneName: String read FWheel4SceneName write SetWheel4Scene;
    property Wheel1Speed: Single read FWheel1Speed write FWheel1Speed
             {$ifdef FPC}default DefaultWheelSpeed{$endif};
    property Wheel2Speed: Single read FWheel2Speed write FWheel2Speed
             {$ifdef FPC}default DefaultWheelSpeed{$endif};
    property Wheel3Speed: Single read FWheel3Speed write FWheel3Speed
             {$ifdef FPC}default DefaultWheelSpeed{$endif};
    property Wheel4Speed: Single read FWheel4Speed write FWheel4Speed
             {$ifdef FPC}default DefaultWheelSpeed{$endif};
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, CastleVectors
  {$ifdef CASTLE_DESIGN_MODE}
  , PropEdits, CastlePropEdits
  {$endif};

constructor TNyaActorVehicle.Create(AOwner: TComponent);
begin
  inherited;

  FWheel1Scene:= nil;
  FWheel2Scene:= nil;
  FWheel3Scene:= nil;
  FWheel4Scene:= nil;
  FWheel1Speed:= DefaultWheelSpeed;
  FWheel2Speed:= DefaultWheelSpeed;
  FWheel3Speed:= DefaultWheelSpeed;
  FWheel4Speed:= DefaultWheelSpeed;
end;

procedure TNyaActorVehicle.Update(const SecondsPassed: Single;
                                  var RemoveMe: TRemoveType);
begin
  inherited;

  WheelRotor(SecondsPassed);
end;

procedure TNyaActorVehicle.SetWheel1Scene(const value: String);
begin
  if (FWheel1SceneName <> value) then
  begin
    FWheel1SceneName:= value;
    UpdateWheelScenes;
  end;
end;

procedure TNyaActorVehicle.SetWheel2Scene(const value: String);
begin
  if (FWheel2SceneName <> value) then
  begin
    FWheel2SceneName:= value;
    UpdateWheelScenes;
  end;
end;

procedure TNyaActorVehicle.SetWheel3Scene(const value: String);
begin
  if (FWheel3SceneName <> value) then
  begin
    FWheel3SceneName:= value;
    UpdateWheelScenes;
  end;
end;

procedure TNyaActorVehicle.SetWheel4Scene(const value: String);
begin
  if (FWheel4SceneName <> value) then
  begin
    FWheel4SceneName:= value;
    UpdateWheelScenes;
  end;
end;

procedure TNyaActorVehicle.UpdateWheelScenes;
var
  scene: TCastleScene;
begin
  for scene in FAllScenes do
  begin
    if (scene.Name = FWheel1SceneName) then
      FWheel1Scene:= scene;

    if (scene.Name = FWheel2SceneName) then
      FWheel2Scene:= scene;

    if (scene.Name = FWheel3SceneName) then
      FWheel3Scene:= scene;

    if (scene.Name = FWheel4SceneName) then
      FWheel4Scene:= scene;
  end;
end;

procedure TNyaActorVehicle.WheelRotor(const SecondsPassed: Single);
begin
  if Assigned(FWheel1Scene) then
    FWheel1Scene.Rotation:= Vector4(FWheel1Scene.Rotation.XYZ,
                                    FWheel1Scene.Rotation.W + ForwardVelocity * FWheel1Speed);
  if Assigned(FWheel2Scene) then
    FWheel2Scene.Rotation:= Vector4(FWheel2Scene.Rotation.XYZ,
                                    FWheel2Scene.Rotation.W + ForwardVelocity * FWheel2Speed);
  if Assigned(FWheel3Scene) then
    FWheel3Scene.Rotation:= Vector4(FWheel3Scene.Rotation.XYZ,
                                    FWheel3Scene.Rotation.W + ForwardVelocity * FWheel3Speed);
  if Assigned(FWheel4Scene) then
    FWheel4Scene.Rotation:= Vector4(FWheel4Scene.Rotation.XYZ,
                                    FWheel4Scene.Rotation.W + ForwardVelocity * FWheel4Speed);
end;

function TNyaActorVehicle.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Wheel1SceneName', 'Wheel2SceneName', 'Wheel3SceneName',
       'Wheel4SceneName', 'Wheel1Speed', 'Wheel2Speed', 'Wheel3Speed',
       'Wheel4Speed', 'MaxSpeed'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

{$ifdef CASTLE_DESIGN_MODE}
type
  { Property editor to select an Wheel }
  TNyaActorWheelSceneEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  function TNyaActorWheelSceneEditor.GetAttributes: TPropertyAttributes;
  begin
    Result:= [paMultiSelect, paValueList, paSortList, paRevertable];
  end;

  procedure TNyaActorWheelSceneEditor.GetValues(Proc: TGetStrProc);
  var
    Nav: TNyaActor;
    S: TCastleScene;
  begin
    Proc('');
    Nav:= GetComponent(0) as TNyaActor;
    for S in Nav.AllScenes do
      Proc(S.Name);
  end;
{$endif}

initialization
  RegisterSerializableComponent(TNyaActorVehicle, 'Nya Actor Vehicle');

  {$ifdef CASTLE_DESIGN_MODE}
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'Wheel1SceneName',
                         TNyaActorWheelSceneEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'Wheel2SceneName',
                         TNyaActorWheelSceneEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'Wheel3SceneName',
                         TNyaActorWheelSceneEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'Wheel4SceneName',
                         TNyaActorWheelSceneEditor);
  {$endif}
end.

