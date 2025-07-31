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
    FHeadlight: Boolean;
    FHeadlightOFFName, FHeadlightONName: String;
    FHeadlightOFF, FHeadlightON: TCastleScene;
    FStoplight: Boolean;
    FStoplightOFFName, FStoplightONName: String;
    FStoplightOFF, FStoplightON: TCastleScene;
    FWheel1SceneName, FWheel2SceneName,
      FWheel3SceneName, FWheel4SceneName: String;
    FWheel1Scene, FWheel2Scene, FWheel3Scene, FWheel4Scene: TCastleScene;
    FWheel1Dia, FWheel2Dia, FWheel3Dia, FWheel4Dia: Single;
    procedure SetWheel1Scene(const value: String);
    procedure SetWheel2Scene(const value: String);
    procedure SetWheel3Scene(const value: String);
    procedure SetWheel4Scene(const value: String);
    procedure UpdateWheelScenes;
    procedure WheelRotor(const SecondsPassed: Single);
    procedure SetHeadlight(const value: Boolean);
    procedure ApplyHeadlight;
    procedure SetHeadlightOFF(const value: String);
    procedure SetHeadlightON(const value: String);
    procedure SetStoplight(const value: Boolean);
    procedure ApplyStoplight;
    procedure SetStoplightOFF(const value: String);
    procedure SetStoplightON(const value: String);
    procedure UpdateLight;
    procedure SetUrl(const value: String); override;
  public
    const
      DefaultWheelDia = 0.0;
      DefaultHeadlight = False;
      DefaultStoplight = False;

    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property Wheel1SceneName: String read FWheel1SceneName write SetWheel1Scene;
    property Wheel2SceneName: String read FWheel2SceneName write SetWheel2Scene;
    property Wheel3SceneName: String read FWheel3SceneName write SetWheel3Scene;
    property Wheel4SceneName: String read FWheel4SceneName write SetWheel4Scene;
    property Headlight: Boolean read FHeadlight write SetHeadlight
             {$ifdef FPC}default DefaultHeadlight{$endif};
    property Stoplight: Boolean read FStoplight write SetStoplight
             {$ifdef FPC}default DefaultStoplight{$endif};

    property HeadlightOFFName: String read FHeadlightOFFName write SetHeadlightOFF;
    property HeadlightONName: String read FHeadlightONName write SetHeadlightON;
    property StoplightOFFName: String read FStoplightOFFName write SetStoplightOFF;
    property StoplightONName: String read FStoplightONName write SetStoplightON;
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

  FHeadlight:= DefaultHeadlight;
  FStoplight:= DefaultStoplight;

  FWheel1Scene:= nil;
  FWheel2Scene:= nil;
  FWheel3Scene:= nil;
  FWheel4Scene:= nil;
  FWheel1Dia:= DefaultWheelDia;
  FWheel2Dia:= DefaultWheelDia;
  FWheel3Dia:= DefaultWheelDia;
  FWheel4Dia:= DefaultWheelDia;
end;

procedure TNyaActorVehicle.Update(const SecondsPassed: Single;
                                  var RemoveMe: TRemoveType);
begin
  inherited;

  WheelRotor(SecondsPassed);
end;

procedure TNyaActorVehicle.SetUrl(const value: String);
begin
  inherited;

  UpdateLight;
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
    begin
      FWheel1Scene:= scene;
      FWheel1Dia:= FWheel1Scene.BoundingBox.SizeY;
    end;

    if (scene.Name = FWheel2SceneName) then
    begin
      FWheel2Scene:= scene;
      FWheel2Dia:= FWheel2Scene.BoundingBox.SizeY;
    end;

    if (scene.Name = FWheel3SceneName) then
    begin
      FWheel3Scene:= scene;
      FWheel3Dia:= FWheel3Scene.BoundingBox.SizeY;
    end;

    if (scene.Name = FWheel4SceneName) then
    begin
      FWheel4Scene:= scene;
      FWheel4Dia:= FWheel4Scene.BoundingBox.SizeY;
    end;
  end;
end;

procedure TNyaActorVehicle.WheelRotor(const SecondsPassed: Single);
begin
  if Assigned(FWheel1Scene) then
    FWheel1Scene.Rotation:= Vector4(FWheel1Scene.Rotation.XYZ,
                                    FWheel1Scene.Rotation.W +
                                    FWheel1Dia * Pi * FForwardShift);
  if Assigned(FWheel2Scene) then
    FWheel2Scene.Rotation:= Vector4(FWheel2Scene.Rotation.XYZ,
                                    FWheel2Scene.Rotation.W +
                                    FWheel2Dia * Pi * FForwardShift);
  if Assigned(FWheel3Scene) then
    FWheel3Scene.Rotation:= Vector4(FWheel3Scene.Rotation.XYZ,
                                    FWheel3Scene.Rotation.W +
                                    FWheel3Dia * Pi * FForwardShift);
  if Assigned(FWheel4Scene) then
    FWheel4Scene.Rotation:= Vector4(FWheel4Scene.Rotation.XYZ,
                                    FWheel4Scene.Rotation.W +
                                    FWheel4Dia * Pi * FForwardShift);
end;

procedure TNyaActorVehicle.SetHeadlight(const value: Boolean);
begin
  if (FHeadlight = value) then Exit;
  FHeadlight:= value;

  ApplyHeadlight;
end;

procedure TNyaActorVehicle.ApplyHeadlight;
begin
  if (Assigned(FHeadlightOFF) AND Assigned(FHeadlightON)) then
  begin
    FHeadlightOFF.Exists:= NOT FHeadlight;
    FHeadlightON.Exists:= FHeadlight;
  end;
end;

procedure TNyaActorVehicle.SetHeadlightOFF(const value: String);
begin
  if (FHeadlightOFFName = value) then Exit;
  FHeadlightOFFName:= value;
  UpdateLight;
end;

procedure TNyaActorVehicle.SetHeadlightON(const value: String);
begin
  if (FHeadlightONName = value) then Exit;
  FHeadlightONName:= value;
  UpdateLight;
end;

procedure TNyaActorVehicle.SetStoplight(const value: Boolean);
begin
  if (FStoplight = value) then Exit;
  FStoplight:= value;

  ApplyStoplight;
end;

procedure TNyaActorVehicle.ApplyStoplight;
begin
  if (Assigned(FStoplightOFF) AND Assigned(FStoplightON)) then
  begin
    FStoplightOFF.Exists:= NOT FStoplight;
    FStoplightON.Exists:= FStoplight;
  end;
end;

procedure TNyaActorVehicle.SetStoplightOFF(const value: String);
begin
  if (FStoplightOFFName = value) then Exit;
  FStoplightOFFName:= value;
  UpdateLight;
end;

procedure TNyaActorVehicle.SetStoplightON(const value: String);
begin
  if (FStoplightONName = value) then Exit;
  FStoplightONName:= value;
  UpdateLight;
end;

procedure TNyaActorVehicle.UpdateLight;
var
  scene: TCastleScene;
begin
  for scene in FAllScenes do
  begin
    if (scene.Name = FHeadlightOFFName) then
      FHeadlightOFF:= scene;
    if (scene.Name = FHeadlightONName) then
      FHeadlightON:= scene;
    if (scene.Name = FStoplightOFFName) then
      FStoplightOFF:= scene;
    if (scene.Name = FStoplightONName) then
      FStoplightON:= scene;
  end;

  ApplyHeadlight;
  ApplyStoplight;
end;

function TNyaActorVehicle.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Wheel1SceneName', 'Wheel2SceneName', 'Wheel3SceneName',
       'Wheel4SceneName', 'Wheel1Speed', 'Wheel2Speed', 'Wheel3Speed',
       'Wheel4Speed', 'MaxSpeed', 'Headlight', 'HeadlightOFFName',
       'HeadlightONName', 'Stoplight', 'StoplightOFFName',
       'StoplightONName'
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
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'HeadlightOFFName',
                         TNyaActorWheelSceneEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'HeadlightONName',
                         TNyaActorWheelSceneEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'StoplightOFFName',
                         TNyaActorWheelSceneEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'StoplightONName',
                         TNyaActorWheelSceneEditor);
  {$endif}
end.

