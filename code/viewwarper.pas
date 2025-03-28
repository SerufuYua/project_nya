unit ViewWarper;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleUIControls, CastleVectors, NyaActorChara;

type
  TViewWarper = class(TCastleView)
  protected
    FGetToGo: TViewWarper;
    FActorMain: TNyaActorChara;
    FMainActorTrans: TVector3;
    FMainActorRot: TVector4;
    FMainActorNewTrans: Boolean;
    FMainActorNewRot: Boolean;
    procedure SetMainActor(AActor: TNyaActorChara);
    procedure SetMainActorTrans(value: TVector3);
    procedure SetMainActorRot(value: TVector4);
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    procedure GetToGo(ALocation: TViewWarper);
    procedure GetToGo(ALocation: TViewWarper;
                      ATranslation: TVector3;
                      ARotation: TVector4);
    property MainActor: TNyaActorChara read FActorMain write SetMainActor;
    property MainActorTrans: TVector3 read FMainActorTrans write SetMainActorTrans;
    property MainActorRot: TVector4 read FMainActorRot write SetMainActorRot;
  end;

implementation

uses
  GameViewLoading;

procedure TViewWarper.Start;
begin
  inherited;

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

procedure TViewWarper.SetMainActor(AActor: TNyaActorChara);
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

end.

