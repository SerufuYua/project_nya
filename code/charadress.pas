unit CharaDress;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleScene, NyaCastleUtils;

type
  TSuitPart = (Top, Bottom, Foots, Arms);

  TCharaDresser = class
  protected
    FScene: TCastleTransformDesign;
    function MainBody: TCastleScene; { main chara Body }
  public
    constructor Create(scene: TCastleTransformDesign);
    function SuitPartsList(suitPartType: TSuitPart): TItemConditions;
    function DressedSuitPart(suitPartType: TSuitPart): String;
    procedure DressSuitPart(suitPartType: TSuitPart; const suitPartName: String);
    function SuitList: TStringArray;
    procedure DressSuit(const suitName: String);
    function AcessoriesList: TItemConditions;
    procedure DressAcessory(const accessoryName: String; visible: boolean);
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

const
  NoSuitPartStr = 'none';

implementation

uses
  X3DNodes, StrUtils, CastleConfig;

type
  AllSuitPartTypes = set of TSuitPart;

const
  DressingStr = 'dressing';
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
  Result:= NoSuitPartStr;
end;

function TCharaDresser.SuitList: TStringArray;
var
  suitPartType: TSuitPart;
  suitPart: TItemCondition;
  suitPartList: Array of String;
  suitName: String;
  found: Boolean;
begin
  Result:= [];
  SetLength(Result, 0);

  suitPartList:= [];
  SetLength(suitPartList, 0);

  { look all suit parts in all suit types }
  for suitPartType in AllSuitPartTypes do
  begin
    { check is suit name already in part list }
    for suitPart in SuitPartsList(suitPartType) do
    begin
      found:= False;
      for suitName in suitPartList do
      begin
        if (suitName = suitPart.Name) then
        begin
          found:= True;
          Break;
        end;
      end;

      if NOT found then
        Insert(suitPart.Name, suitPartList, Length(suitPartList))
      else
        begin
          { check is suit name already in suit list }
          found:= False;
          for suitName in Result do
          begin
            if (suitName = suitPart.Name) then
            begin
              found:= True;
              Break;
            end;
          end;

          if NOT found then
            Insert(suitPart.Name, Result, Length(Result));
        end;
    end;
  end;
end;

procedure TCharaDresser.DressSuit(const suitName: String);
var
  suitPartType: TSuitPart;
begin
  for suitPartType in AllSuitPartTypes do
    DressSuitPart(suitPartType, suitName);
end;

procedure TCharaDresser.DressSuitPart(suitPartType: TSuitPart; const suitPartName: String);
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

procedure TCharaDresser.DressAcessory(const accessoryName: String; visible: boolean);
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
  path: String;
  accessory: TItemCondition;
  suitPartName: String;
  visible: Boolean;
begin
  path:= DressingStr + '/' + FCharaName + '/';

  { suit parts }
  suitPartName:= UserConfig.GetValue(path + PrefixTop, DefaultSuitStr);
  FDresser.DressSuitPart(Top, suitPartName);

  suitPartName:= UserConfig.GetValue(path + PrefixBottom, DefaultSuitStr);
  FDresser.DressSuitPart(Bottom, suitPartName);

  suitPartName:= UserConfig.GetValue(path + PrefixFoots, DefaultSuitStr);
  FDresser.DressSuitPart(Foots, suitPartName);

  suitPartName:= UserConfig.GetValue(path + PrefixArms, DefaultSuitStr);
  FDresser.DressSuitPart(Arms, suitPartName);

  { accessories }
  for accessory in FDresser.AcessoriesList do
  begin
    visible:= UserConfig.GetValue(path + accessory.Name, False);
    FDresser.DressAcessory(accessory.Name, visible);
  end;
end;

procedure TDressSaver.SaveProperties;
var
  path: String;
  accessory: TItemCondition;
begin
  path:= DressingStr + '/' + FCharaName + '/';

  { suit parts }

  UserConfig.SetValue(path + PrefixTop, FDresser.DressedSuitPart(Top));
  UserConfig.SetValue(path + PrefixBottom, FDresser.DressedSuitPart(Bottom));
  UserConfig.SetValue(path + PrefixFoots, FDresser.DressedSuitPart(Foots));
  UserConfig.SetValue(path + PrefixArms, FDresser.DressedSuitPart(Arms));


  { accessories }
  for accessory in FDresser.AcessoriesList do
  begin
    UserConfig.SetValue(path + accessory.Name, accessory.Visible);
  end;

  UserConfig.Save;
end;

end.

