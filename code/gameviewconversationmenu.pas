unit GameViewConversationMenu;

interface

uses
  Classes, CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleColors, NyaActor;

type
  TOnAnswer = procedure of object;

  TMessage = record
    FActor: TNyaActor;
    FMessage: String;
  end;

  TMessages = Array of TMessage;

  TViewConversationMenu = class(TCastleView)
  strict private
    type
      TViewConversationDialog = class(TCastleUserInterface)
      strict private
        FParentView: TViewConversationMenu;
        FMessages: TMessages;
        FRootItem: TCastleUserInterface;
        FActorName: TCastleLabel;
        FTextMessage: TCastleLabel;
        FCounter: Integer;
        procedure ShowMessage(AMessage: TMessage);
        procedure SetColor(color: TCastleColorRGB);
        procedure ClickNext(Sender: TObject);
        procedure ClickCancel(Sender: TObject);
      public
        OnOk: TOnAnswer;
        OnCancel: TOnAnswer;
      public
        constructor Create(AOwner: TComponent); override;
        constructor Create(AOwner: TComponent;
                           AParentView: TViewConversationMenu;
                           AMessages: TMessages);
      end;
    var
      FDialog: TViewConversationDialog;
      FMessages: TMessages;
      FOnOk: TOnAnswer;
      FOnCancel: TOnAnswer;
  public
    constructor CreateUntilStopped(AMessages: TMessages;
                                   AOnOk: TOnAnswer;
                                   AOnCancel: TOnAnswer);
    procedure Start; override;
  end;

var
  ViewConversationMenu: TViewConversationMenu;

implementation

uses
  CastleComponentSerialize, NyaCastleUtils;

const
  SystemName = 'System';

{ ========= ------------------------------------------------------------------ }
{ TViewConversationDialog ---------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TViewConversationMenu.TViewConversationDialog.Create(AOwner: TComponent);
begin
  Create(AOwner, nil, []);
end;

constructor TViewConversationMenu.TViewConversationDialog.Create(AOwner: TComponent;
                                                                 AParentView: TViewConversationMenu;
                                                                 AMessages: TMessages);
var
  UiOwner: TComponent;
  Ui: TCastleUserInterface;
  ButtonNext: TCastleButton;
  ButtonCancel: TCastleButton;
begin
  inherited Create(AOwner);
  FParentView:= AParentView;
  FMessages:= AMessages;

  // UiOwner is useful to keep reference to all components loaded from the design
  UiOwner:= TComponent.Create(Self);

  { Load designed user interface }
  Ui:= UserInterfaceLoad('castle-data:/gameviewconversationmenu.castle-user-interface', UiOwner);
  InsertFront(Ui);

  { Find components, by name, that we need to access from code }
  FRootItem:= UiOwner.FindRequiredComponent('MenuRoot') as TCastleUserInterface;
  FActorName:= UiOwner.FindRequiredComponent('ActorName') as TCastleLabel;
  FTextMessage:= UiOwner.FindRequiredComponent('TextMessage') as TCastleLabel;
  ButtonNext:= UiOwner.FindRequiredComponent('ButtonNext') as TCastleButton;
  ButtonCancel:= UiOwner.FindRequiredComponent('ButtonCancel') as TCastleButton;

  ButtonNext.OnClick:= {$ifdef FPC}@{$endif}ClickNext;
  ButtonCancel.OnClick:= {$ifdef FPC}@{$endif}ClickCancel;

  AutoSizeToChildren:= True;

  FCounter:= 0;
  if Length(FMessages) > FCounter then
    ShowMessage(FMessages[FCounter]);
end;

procedure TViewConversationMenu.TViewConversationDialog.ShowMessage(AMessage: TMessage);
begin
  FTextMessage.Caption:= AMessage.FMessage;
  if Assigned(AMessage.FActor) then
  begin
    FActorName.Caption:= AMessage.FActor.ActorName;
    SetColor(AMessage.FActor.PersonalColor);
  end else
  begin
    FActorName.Caption:= SystemName;
    SetColor(WhiteRGB);
  end;
end;

procedure TViewConversationMenu.TViewConversationDialog.SetColor(color: TCastleColorRGB);
var
  item: TCastleImageControl;
  alpha: single;
begin
  for item in GetAllUIImages(FRootItem) do
  begin
    if (item.Tag = 1) then
    begin
      alpha:= item.Color.W;
      item.Color:= Vector4(color.X, color.Y, color.Z, alpha);
    end;
  end;
end;

procedure TViewConversationMenu.TViewConversationDialog.ClickNext(Sender: TObject);
begin
  FCounter:= FCounter + 1;
  if Length(FMessages) > FCounter then
    ShowMessage(FMessages[FCounter])
  else
  begin
    if Assigned(OnOk) then OnOk;
    Container.PopView(FParentView);
  end;
end;

procedure TViewConversationMenu.TViewConversationDialog.ClickCancel(Sender: TObject);
begin
  if Assigned(OnCancel) then OnCancel;
  Container.PopView(FParentView);
end;

{ ========= ------------------------------------------------------------------ }
{ TViewConversationMenu ------------------------------------------------------ }
{ ========= ------------------------------------------------------------------ }

constructor TViewConversationMenu.CreateUntilStopped(AMessages: TMessages;
                                                     AOnOk: TOnAnswer;
                                                     AOnCancel: TOnAnswer);
begin
  inherited CreateUntilStopped;
  DesignUrl:= 'castle-data:/bgdialog.castle-user-interface';
  FMessages:= AMessages;
  FOnOk:= AOnOk;
  FOnCancel:= AOnCancel;
end;

procedure TViewConversationMenu.Start;
begin
  inherited;
  InterceptInput:= True;

  FDialog:= TViewConversationDialog.Create(FreeAtStop, Self, FMessages);
  FDialog.OnOk:= FOnOk;
  FDialog.OnCancel:= FOnCancel;
  FDialog.Anchor(hpMiddle);
  FDialog.Anchor(vpBottom);
  InsertFront(FDialog);
end;

end.
