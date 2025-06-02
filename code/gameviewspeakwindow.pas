unit GameViewSpeakWindow;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  NyaActorChara;

type
  TViewSpeakMenu = class(TCastleView)
  strict private
    type
      TViewSpeakWindow = class(TCastleUserInterface)
      private
        ImageBG: TCastleImageControl;
        ActorName, TextMessage: TCastleLabel;
      public
        constructor Create(AOwner: TComponent;
                           chara: TNyaActorChara;
                           message: String); reintroduce;
        procedure SetTransparency(value: Single);
      end;
    var
      FWin: TViewSpeakWindow;
      FChara: TNyaActorChara;
      FMessage: String;
      FTime, FAppearTime, FLiveTime, FVanishTime: Single;
      FAllowedArea: Single;
  public
    const
      DefaultAppearTime = 0.25;
      DefaultLiveTime = 0.25;
      DefaultFVanishTime = 4.0;
      DefaultAllowedArea = 0.6;

    constructor CreateUntilStopped(chara: TNyaActorChara; message: String;
                                   timePerSymbol: Single = DefaultLiveTime;
                                   allowedArea: Single = DefaultAllowedArea);
    procedure Start; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: boolean); override;
  end;

implementation

uses
  CastleComponentSerialize, CastleColors, CastleUtils;

{ ========= ------------------------------------------------------------------ }
{ TViewSpeakWindow ----------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TViewSpeakMenu.TViewSpeakWindow.Create(AOwner: TComponent;
                                                   chara: TNyaActorChara;
                                                   message: String);
var
  UiOwner: TComponent;
  Ui: TCastleUserInterface;
  alpha: single;
begin
  inherited Create(AOwner);

  // UiOwner is useful to keep reference to all components loaded from the design
  UiOwner:= TComponent.Create(Self);

  { Load designed user interface }
  Ui:= UserInterfaceLoad('castle-data:/gameviewspeakwindow.castle-user-interface', UiOwner);
  InsertFront(Ui);

  { Find components, by name, that we need to access from code }
  ImageBG:= UiOwner.FindRequiredComponent('ImageBG') as TCastleImageControl;
  ActorName:= UiOwner.FindRequiredComponent('ActorName') as TCastleLabel;
  TextMessage:= UiOwner.FindRequiredComponent('TextMessage') as TCastleLabel;

  { set speaker Name }
  ActorName.Caption:= chara.ActorName;

  { set message }
  TextMessage.Caption:= message;

  { set Color }
  alpha:= ImageBG.Color.W;
  ImageBG.Color:= Vector4(chara.PersonalColor, alpha);
end;

procedure TViewSpeakMenu.TViewSpeakWindow.SetTransparency(value: Single);
var
  tmpColor: TCastleColor;
begin
  tmpColor:= ImageBG.Color;
  tmpColor.W:= value;
  ImageBG.Color:= tmpColor;

  tmpColor:= ActorName.Color;
  tmpColor.W:= value;
  ActorName.Color:= tmpColor;

  tmpColor:= TextMessage.Color;
  tmpColor.W:= value;
  TextMessage.Color:= tmpColor;
end;

{ ========= ------------------------------------------------------------------ }
{ TViewSpeakMenu ------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TViewSpeakMenu.CreateUntilStopped(chara: TNyaActorChara;
                                              message: String;
                                              timePerSymbol: Single;
                                              allowedArea: Single);
begin
  inherited CreateUntilStopped;
  FChara:= chara;
  FMessage:= message;
  FTime:= 0.0;
  FAppearTime:= DefaultAppearTime;
  FLiveTime:= timePerSymbol * Length(message);
  FVanishTime:= DefaultFVanishTime;
  FAllowedArea:= allowedArea;
end;

procedure TViewSpeakMenu.Start;
var
  scrHorPos, scrVerPos: Single;
begin
  inherited;

  FWin:= TViewSpeakWindow.Create(FreeAtStop, FChara, FMessage);
  FWin.SetTransparency(0.0);

  scrHorPos:= RandomFloatRange(-Container.UnscaledHeight * FAllowedArea / 2,
                               Container.UnscaledHeight * FAllowedArea / 2);
  scrVerPos:= RandomFloatRange(-Container.UnscaledHeight * FAllowedArea / 2,
                               Container.UnscaledHeight * FAllowedArea / 2);

  FWin.Anchor(hpMiddle, scrHorPos);
  FWin.Anchor(vpMiddle, scrVerPos);
  InsertFront(FWin);
end;

procedure TViewSpeakMenu.Update(const SecondsPassed: Single;
                                var HandleInput: boolean);
begin
  inherited;

  FTime:= FTime + SecondsPassed;

  if (FTime <= FAppearTime) then
    FWin.SetTransparency(FTime / FAppearTime)
  else if ((FTime >= (FAppearTime + FLiveTime)) AND (FTime <= (FAppearTime + FLiveTime + FVanishTime))) then
    FWin.SetTransparency((FVanishTime - (FTime - (FAppearTime + FLiveTime))) / FVanishTime)
  else if (FTime > (FAppearTime + FLiveTime + FVanishTime)) then
    Container.PopView(Self); { close }
end;

end.
