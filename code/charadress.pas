unit CharaDress;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleScene, NyaCastleUtils;

type
  TSuitPart = (All, Top, Bottom, Foots, Arms);
  TDressSaver = class;

  TCharaDresser = class
  protected
    FScene: TCastleTransformDesign;
    function MainBody: TCastleScene; { main chara Body }
  public
    constructor Create(scene: TCastleTransformDesign);
    function SuitPartsList(suitPartType: TSuitPart): TItemConditions;
    function DressedSuitPart(suitPartType: TSuitPart): String;
    procedure WearSuitPart(suitPartType: TSuitPart; const suitPartName: String);
    function AcessoriesList: TItemConditions;
    procedure WearAcessory(const accessoryName: String; visible: boolean);
    procedure SaveCondition(const name:string);
    procedure RestoreCondition(const name:string);
  end;

  TDressSaver = class
  protected
    FDresser: TCharaDresser;
    FCharaName: String;
  public
    constructor Create(dresser: TCharaDresser; const charaName: String);
    procedure RestoreProperties;
    procedure SaveProperties;
  end;

implementation

uses
  X3DNodes, StrUtils, IniFiles;

const
  PrefixTop = 'top.';
  PrefixBottom = 'bottom.';
  PrefixFoots = 'foots.';
  PrefixArms = 'arms.';
  PrefixAccesory = 'accessory_';
  DefaultSuitStr = 'summer';
  BodynameStr = 'Body';

{$I nyaworldconst.inc}

{ ---------------------------------------------------------------------------- }
{ TCharaDresser -------------------------------------------------------------- }
{ ---------------------------------------------------------------------------- }

constructor TCharaDresser.Create(scene: TCastleTransformDesign);
begin
  FScene:= scene;
end;

function TCharaDresser.SuitPartsList(suitPartType: TSuitPart): TItemConditions;
var
  i: Integer;
  found: Boolean;
  fullNames: TItemConditions;
  shortName, newName: TItemCondition;
begin
  { get list of full suit parts shape names }
  Case suitPartType of
  Top: fullNames:= GetShapeNamesByNameStart(MainBody, PrefixTop);
  Bottom: fullNames:= GetShapeNamesByNameStart(MainBody, PrefixBottom);
  Foots: fullNames:= GetShapeNamesByNameStart(MainBody, PrefixFoots);
  Arms: fullNames:= GetShapeNamesByNameStart(MainBody, PrefixArms);
  All:
    begin
      fullNames:= [];
      Insert(GetShapeNamesByNameStart(MainBody, PrefixTop),
             fullNames, Length(fullNames));
      Insert(GetShapeNamesByNameStart(MainBody, PrefixBottom),
             fullNames, Length(fullNames));
      Insert(GetShapeNamesByNameStart(MainBody, PrefixFoots),
             fullNames, Length(fullNames));
      Insert(GetShapeNamesByNameStart(MainBody, PrefixArms),
             fullNames, Length(fullNames));
    end
  else
    fullNames:= [];
  end;

  { extract suit parts names from full names and remove dupe}
  Result:=[];

  for i:= 0 to (Length(fullNames) - 1) do
  begin
    found:= False;
    shortName.Name:= ExtractDelimited(2, fullNames[i].Name, ['.']);
    shortName.Visible:= fullNames[i].Visible;
    for newName in Result do
    begin
      if (String.Compare(shortName.Name, newName.Name) = 0) then
      begin
        found:= True;
        Break;
      end;
    end;
    if NOT found then
      Insert(shortName, Result, 0);
  end;
end;

function TCharaDresser.DressedSuitPart(suitPartType: TSuitPart): String;
var
  suitPart: TItemCondition;
begin
  for suitPart in SuitPartsList(suitPartType) do
  begin
    if suitPart.Visible then
    begin
      Result:= suitPart.Name;
      Exit
    end;
  end;
  Result:= 'none';
end;

procedure TCharaDresser.WearSuitPart(suitPartType: TSuitPart; const suitPartName: String);
var
  shapes: TShapeNodes;
  shape: TShapeNode;
  shapeName: String;
begin
  Case suitPartType of
  Top: shapes:= GetShapesByNameStart(MainBody, PrefixTop);
  Bottom: shapes:= GetShapesByNameStart(MainBody, PrefixBottom);
  Foots: shapes:= GetShapesByNameStart(MainBody, PrefixFoots);
  Arms: shapes:= GetShapesByNameStart(MainBody, PrefixArms);
  else
    shapes:= [];
  end;

  for shape in shapes do
  begin
    shapeName:= shape.X3DName;
    if shapeName.Contains(suitPartName) then
      shape.Visible:= True
    else
      shape.Visible:= False;
  end;
end;

function TCharaDresser.AcessoriesList: TItemConditions;
var
  i: Integer;
  acessoryNames: TItemConditions;
begin
  acessoryNames:= GetSceneNamesByNameStart(FScene, PrefixAccesory);

  for i:= 0 to (Length(acessoryNames) - 1) do
  begin
    delete(acessoryNames[i].Name, 1, Length(PrefixAccesory));
  end;

  Result:= acessoryNames;
end;

procedure TCharaDresser.WearAcessory(const accessoryName: String; visible: boolean);
var
  item: TCastleScene;
begin
  for item in GetAllScenes(FScene) do
  begin
    if item.Name.Contains(accessoryName) then
      item.Visible:= visible;
  end;
end;

procedure TCharaDresser.SaveCondition(const name:string);
var
  DressSaver: TDressSaver;
begin
  DressSaver:= TDressSaver.Create(self, name);
  DressSaver.SaveProperties;
  FreeAndNil(DressSaver);
end;

procedure TCharaDresser.RestoreCondition(const name:string);
var
  DressSaver: TDressSaver;
begin
  DressSaver:= TDressSaver.Create(self, name);
  DressSaver.RestoreProperties;
  FreeAndNil(DressSaver);
end;

function TCharaDresser.MainBody: TCastleScene;
begin
  Result:= FScene.DesignedComponent(BodynameStr, False) as TCastleScene;
end;

{ ---------------------------------------------------------------------------- }
{ TDressSaver ---------------------------------------------------------------- }
{ ---------------------------------------------------------------------------- }

constructor TDressSaver.Create(dresser: TCharaDresser; const charaName: String);
begin
  FDresser:= dresser;
  FCharaName:= charaName;
end;

procedure TDressSaver.RestoreProperties;
var
  ini: TCustomIniFile;
  accessory: TItemCondition;
  visible: boolean;
  suitPartName: String;
begin
  ini:= TMemIniFile.Create(IniFileName);
  ini.FormatSettings.DecimalSeparator := '|';
  ini.Options:= [ifoFormatSettingsActive];

  { suit parts }
  suitPartName:= ini.ReadString(FCharaName, PrefixTop, DefaultSuitStr);
  FDresser.WearSuitPart(Top, suitPartName);

  suitPartName:= ini.ReadString(FCharaName, PrefixBottom, DefaultSuitStr);
  FDresser.WearSuitPart(Bottom, suitPartName);

  suitPartName:= ini.ReadString(FCharaName, PrefixFoots, DefaultSuitStr);
  FDresser.WearSuitPart(Foots, suitPartName);

  suitPartName:= ini.ReadString(FCharaName, PrefixArms, DefaultSuitStr);
  FDresser.WearSuitPart(Arms, suitPartName);

  { accessories }
  for accessory in FDresser.AcessoriesList do
  begin
    visible:= ini.ReadBool(FCharaName, accessory.Name, False);
    FDresser.WearAcessory(accessory.Name, visible);
  end;

  ini.Free;
end;

procedure TDressSaver.SaveProperties;
var
  ini: TCustomIniFile;
  accessory: TItemCondition;
  suitPartName: String;
begin
  ini:= TMemIniFile.Create(IniFileName);
  ini.FormatSettings.DecimalSeparator := '|';
  ini.Options:= [ifoFormatSettingsActive];

  { suit parts }
  suitPartName:= FDresser.DressedSuitPart(Top);
  ini.WriteString(FCharaName, PrefixTop, suitPartName);

  suitPartName:= FDresser.DressedSuitPart(Bottom);
  ini.WriteString(FCharaName, PrefixBottom, suitPartName);

  suitPartName:= FDresser.DressedSuitPart(Foots);
  ini.WriteString(FCharaName, PrefixFoots, suitPartName);

  suitPartName:= FDresser.DressedSuitPart(Arms);
  ini.WriteString(FCharaName, PrefixArms, suitPartName);

  { accessories }
  for accessory in FDresser.AcessoriesList do
  begin
    ini.WriteBool(FCharaName, accessory.Name, accessory.Visible);
  end;

  ini.Free;
end;

end.

