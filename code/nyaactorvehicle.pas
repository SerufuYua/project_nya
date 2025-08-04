unit NyaActorVehicle;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, NyaActor, CastleClassUtils, CastleSceneCore,
  CastleBehaviors, CastleTransform, NyaCastleUtils, CastleScene,
  CastleTimeUtils, CastleParticleEmitter,
  NyaSoundControl, NyaCharaDress;

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
    FSoundNeutralName, FSoundStartName, FSoundMoveName: String;
    FSoundSrcNeutral, FSoundSrcStart, FSoundSrcMove: TCastleSoundSource;
    procedure SetWheel1Scene(const value: String);
    procedure SetWheel2Scene(const value: String);
    procedure SetWheel3Scene(const value: String);
    procedure SetWheel4Scene(const value: String);
    procedure UpdateWheelScenes;
    procedure WheelRotor(const SecondsPassed: Single);
    procedure SetSoundNeutralName(const value: String);
    procedure SetSoundStartName(const value: String);
    procedure SetSoundMoveName(const value: String);
    procedure UpdateSounds;
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
    property SoundSrcNeutral: TCastleSoundSource read FSoundSrcNeutral;
    property SoundSrcStart: TCastleSoundSource read FSoundSrcStart;
    property SoundSrcMove: TCastleSoundSource read FSoundSrcMove;
  published
    property Wheel1SceneName: String read FWheel1SceneName write SetWheel1Scene;
    property Wheel2SceneName: String read FWheel2SceneName write SetWheel2Scene;
    property Wheel3SceneName: String read FWheel3SceneName write SetWheel3Scene;
    property Wheel4SceneName: String read FWheel4SceneName write SetWheel4Scene;
    property SoundNeutralName: String read FSoundNeutralName write SetSoundNeutralName;
    property SoundStartName: String read FSoundStartName write SetSoundStartName;
    property SoundMoveName: String read FSoundMoveName write SetSoundMoveName;
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

  UpdateWheelScenes;
  UpdateLight;
  UpdateSounds;
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
                                    FWheel1Dia * Pi * ForwardShift);
  if Assigned(FWheel2Scene) then
    FWheel2Scene.Rotation:= Vector4(FWheel2Scene.Rotation.XYZ,
                                    FWheel2Scene.Rotation.W +
                                    FWheel2Dia * Pi * ForwardShift);
  if Assigned(FWheel3Scene) then
    FWheel3Scene.Rotation:= Vector4(FWheel3Scene.Rotation.XYZ,
                                    FWheel3Scene.Rotation.W +
                                    FWheel3Dia * Pi * ForwardShift);
  if Assigned(FWheel4Scene) then
    FWheel4Scene.Rotation:= Vector4(FWheel4Scene.Rotation.XYZ,
                                    FWheel4Scene.Rotation.W +
                                    FWheel4Dia * Pi * ForwardShift);
end;

procedure TNyaActorVehicle.SetSoundNeutralName(const value: String);
begin
  if (FSoundNeutralName <> value) then
  begin
    FSoundNeutralName:= value;
    UpdateSounds;
  end;
end;

procedure TNyaActorVehicle.SetSoundStartName(const value: String);
begin
  if (FSoundStartName <> value) then
  begin
    FSoundStartName:= value;
    UpdateSounds;
  end;
end;

procedure TNyaActorVehicle.SetSoundMoveName(const value: String);
begin
  if (FSoundMoveName <> value) then
  begin
    FSoundMoveName:= value;
    UpdateSounds;
  end;
end;

procedure TNyaActorVehicle.UpdateSounds;
var
  behavior: TCastleBehavior;
begin
  if NOT Assigned(FDesign) then Exit;

  for behavior in GetAllBehavior(FDesign, TCastleSoundSource) do
  begin
    if (behavior.Name = FSoundNeutralName) then
      FSoundSrcNeutral:= behavior as TCastleSoundSource;

    if (behavior.Name = FSoundStartName) then
      FSoundSrcStart:= behavior as TCastleSoundSource;

    if (behavior.Name = FSoundMoveName) then
      FSoundSrcMove:= behavior as TCastleSoundSource;
  end;
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
       'Wheel4SceneName', 'SoundNeutralName', 'SoundStartName',
       'SoundMoveName', 'Headlight', 'HeadlightOFFName',
       'HeadlightONName', 'Stoplight', 'StoplightOFFName',
       'StoplightONName'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

{$ifdef CASTLE_DESIGN_MODE}
type
  { Property editor to select a scene for Whell, Light and other }
  TNyaVehicleSceneEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { Property editor to select a sound }
  TNyaSoundEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  function TNyaVehicleSceneEditor.GetAttributes: TPropertyAttributes;
  begin
    Result:= [paMultiSelect, paValueList, paSortList, paRevertable];
  end;

  procedure TNyaVehicleSceneEditor.GetValues(Proc: TGetStrProc);
  var
    Nav: TNyaActor;
    S: TCastleScene;
  begin
    Proc('');
    Nav:= GetComponent(0) as TNyaActor;
    for S in Nav.AllScenes do
      Proc(S.Name);
  end;

  function TNyaSoundEditor.GetAttributes: TPropertyAttributes;
  begin
    Result:= [paMultiSelect, paValueList, paSortList, paRevertable];
  end;

  procedure TNyaSoundEditor.GetValues(Proc: TGetStrProc);
  var
    Nav: TNyaActor;
    C: TCastleBehavior;
  begin
    Proc('');
    Nav:= GetComponent(0) as TNyaActor;
    for C in GetAllBehavior(Nav, TCastleSoundSource) do
      Proc(C.Name);
  end;
{$endif}

initialization
  RegisterSerializableComponent(TNyaActorVehicle, 'Nya Actor Vehicle');

  {$ifdef CASTLE_DESIGN_MODE}
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'Wheel1SceneName',
                         TNyaVehicleSceneEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'Wheel2SceneName',
                         TNyaVehicleSceneEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'Wheel3SceneName',
                         TNyaVehicleSceneEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'Wheel4SceneName',
                         TNyaVehicleSceneEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'HeadlightOFFName',
                         TNyaVehicleSceneEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'HeadlightONName',
                         TNyaVehicleSceneEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'StoplightOFFName',
                         TNyaVehicleSceneEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'StoplightONName',
                         TNyaVehicleSceneEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'SoundNeutralName',
                         TNyaSoundEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'SoundStartName',
                         TNyaSoundEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'SoundMoveName',
                         TNyaSoundEditor);
  {$endif}
end.

