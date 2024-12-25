unit CharaDress;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleScene, NyaCastleUtils;

type
  TSuits = (All, Top, Bottom, Foots, Arms);

  TCharaDresser = class
  public
    constructor Create(scene: TCastleTransformDesign);
    function GetSuitsList(suitType: TSuits): TItemConditions;
    function GetDressedSuit(suitType: TSuits): String;
    procedure WearSuit(suitType: TSuits; const suitName: String);
    function GetAcessoriesList(): TItemConditions;
    procedure WearAcessory(const accessoryName: String; visible: boolean);
  protected
    FScene: TCastleTransformDesign;
    function GetMainBody(): TCastleScene; { main chara Body }
  end;

  TDressSaver = class
  public
    constructor Create(dresser: TCharaDresser; charaName: String);
    procedure RestoreProperties;
    procedure SaveProperties;
  protected
    FDresser: TCharaDresser;
    FCharaName: String;
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
  IniFileName = 'condition.ini';

{ TCharaDresser}

constructor TCharaDresser.Create(scene: TCastleTransformDesign);
begin
  FScene:= scene;
end;

function TCharaDresser.GetSuitsList(suitType: TSuits): TItemConditions;
var
  i: Integer;
  found: Boolean;
  fullNames: TItemConditions;
  shortName, newName: TItemCondition;
begin
  { get list of full suit shape names }
  Case suitType of
  Top: fullNames:= GetShapeNamesByNameStart(GetMainBody(), PrefixTop);
  Bottom: fullNames:= GetShapeNamesByNameStart(GetMainBody(), PrefixBottom);
  Foots: fullNames:= GetShapeNamesByNameStart(GetMainBody(), PrefixFoots);
  Arms: fullNames:= GetShapeNamesByNameStart(GetMainBody(), PrefixArms);
  All:
    begin
      fullNames:= [];
      Insert(GetShapeNamesByNameStart(GetMainBody(), PrefixTop),
             fullNames, Length(fullNames));
      Insert(GetShapeNamesByNameStart(GetMainBody(), PrefixBottom),
             fullNames, Length(fullNames));
      Insert(GetShapeNamesByNameStart(GetMainBody(), PrefixFoots),
             fullNames, Length(fullNames));
      Insert(GetShapeNamesByNameStart(GetMainBody(), PrefixArms),
             fullNames, Length(fullNames));
    end
  else
    fullNames:= [];
  end;

  { extract suit names from full names and remove dupe}
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

function TCharaDresser.GetDressedSuit(suitType: TSuits): String;
var
  suit: TItemCondition;
begin
  for suit in GetSuitsList(suitType) do
  begin
    if suit.Visible then
    begin
      Result:= suit.Name;
      Exit
    end;
  end;
  Result:= 'none';
end;

procedure TCharaDresser.WearSuit(suitType: TSuits; const suitName: String);
var
  shapes: TShapeNodes;
  shape: TShapeNode;
  shapeName: String;
begin
  Case suitType of
  Top: shapes:= GetShapesByNameStart(GetMainBody(), PrefixTop);
  Bottom: shapes:= GetShapesByNameStart(GetMainBody(), PrefixBottom);
  Foots: shapes:= GetShapesByNameStart(GetMainBody(), PrefixFoots);
  Arms: shapes:= GetShapesByNameStart(GetMainBody(), PrefixArms);
  else
    shapes:= [];
  end;

  for shape in shapes do
  begin
    shapeName:= shape.X3DName;
    if shapeName.Contains(suitName) then
      shape.Visible:= True
    else
      shape.Visible:= False;
  end;
end;

function TCharaDresser.GetAcessoriesList(): TItemConditions;
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

function TCharaDresser.GetMainBody(): TCastleScene;
begin
  Result:= FScene.DesignedComponent('Body') as TCastleScene;
end;

{ TDressSaver }

constructor TDressSaver.Create(dresser: TCharaDresser; charaName: String);
begin
  FDresser:= dresser;
  FCharaName:= charaName;
  RestoreProperties;
end;

procedure TDressSaver.RestoreProperties;
var
  ini: TCustomIniFile;
  accessory: TItemCondition;
  visible: boolean;
  suitName: String;
begin
  ini:= TMemIniFile.Create(IniFileName);
  ini.FormatSettings.DecimalSeparator := '|';
  ini.Options:= [ifoFormatSettingsActive];

  { suits }
  suitName:= ini.ReadString(FCharaName, PrefixTop, 'summer_shirt');
  FDresser.WearSuit(Top, suitName);

  suitName:= ini.ReadString(FCharaName, PrefixBottom, 'briefs');
  FDresser.WearSuit(Bottom, suitName);

  suitName:= ini.ReadString(FCharaName, PrefixFoots, 'sneakers');
  FDresser.WearSuit(Foots, suitName);

  suitName:= ini.ReadString(FCharaName, PrefixArms, 'none');
  FDresser.WearSuit(Arms, suitName);

  { accessories }
  for accessory in FDresser.GetAcessoriesList() do
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
  suitName: String;
begin
  ini:= TMemIniFile.Create(IniFileName);
  ini.FormatSettings.DecimalSeparator := '|';
  ini.Options:= [ifoFormatSettingsActive];

  { suits }
  suitName:= FDresser.GetDressedSuit(Top);
  ini.WriteString(FCharaName, PrefixTop, suitName);

  suitName:= FDresser.GetDressedSuit(Bottom);
  ini.WriteString(FCharaName, PrefixBottom, suitName);

  suitName:= FDresser.GetDressedSuit(Foots);
  ini.WriteString(FCharaName, PrefixFoots, suitName);

  suitName:= FDresser.GetDressedSuit(Arms);
  ini.WriteString(FCharaName, PrefixArms, suitName);

  { accessories }
  for accessory in FDresser.GetAcessoriesList() do
  begin
    ini.WriteBool(FCharaName, accessory.Name, accessory.Visible);
  end;

  ini.Free;
end;

end.

