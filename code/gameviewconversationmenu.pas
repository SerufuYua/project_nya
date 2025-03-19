unit GameViewConversationMenu;

interface

uses
  Classes, CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TAnswer = (ansNext, ansOk, ansCancel);

  TViewConversationMenu = class(TCastleView)
  strict private
    type
      TViewConversationDialog = class(TCastleUserInterface)
      strict private
        procedure ClickNext(Sender: TObject);
        procedure ClickCancel(Sender: TObject);
      public
        View: TViewConversationMenu; { << set after creation }
      public
        constructor Create(AOwner: TComponent); override;
      end;
    var
      Dialog: TViewConversationDialog;
  public
    constructor CreateUntilStopped;
    procedure Start; override;
  end;

var
  ViewConversationMenu: TViewConversationMenu;

implementation

uses
  CastleComponentSerialize;

{ ========= ------------------------------------------------------------------ }
{ TViewConversationDialog ---------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TViewConversationMenu.TViewConversationDialog.Create(AOwner: TComponent);
var
  UiOwner: TComponent;
  Ui: TCastleUserInterface;
  ButtonNext: TCastleButton;
  ButtonCancel: TCastleButton;
begin
  inherited Create(AOwner);

  // UiOwner is useful to keep reference to all components loaded from the design
  UiOwner:= TComponent.Create(Self);

  { Load designed user interface }
  Ui:= UserInterfaceLoad('castle-data:/gameviewconversationmenu.castle-user-interface', UiOwner);
  InsertFront(Ui);

  { Find components, by name, that we need to access from code }
  ButtonNext:= UiOwner.FindRequiredComponent('ButtonNext') as TCastleButton;
  ButtonCancel:= UiOwner.FindRequiredComponent('ButtonCancel') as TCastleButton;

  ButtonNext.OnClick:= {$ifdef FPC}@{$endif}ClickNext;
  ButtonCancel.OnClick:= {$ifdef FPC}@{$endif}ClickCancel;

  AutoSizeToChildren:= True;
end;

procedure TViewConversationMenu.TViewConversationDialog.ClickNext(Sender: TObject);
begin
  Container.PopView(View);
end;

procedure TViewConversationMenu.TViewConversationDialog.ClickCancel(Sender: TObject);
begin
  Container.PopView(View);
end;

{ ========= ------------------------------------------------------------------ }
{ TViewConversationMenu ------------------------------------------------------ }
{ ========= ------------------------------------------------------------------ }

constructor TViewConversationMenu.CreateUntilStopped;
begin
  inherited CreateUntilStopped;
  DesignUrl := 'castle-data:/bgdialog.castle-user-interface';
end;

procedure TViewConversationMenu.Start;
begin
  inherited;

  { Do not allow clicks to pass to ViewPlay underneath.
    We are transparent (show the ViewPlay underneath),
    but we don't want to allow user to interact with it (e.g. by causing
    another ViewAskDialog by clicking, or by pressing on
    ViewPlay.ButtonBack). }
  InterceptInput:= True;

  Dialog:= TViewConversationDialog.Create(FreeAtStop);
  Dialog.View:= Self;
  Dialog.Anchor(hpMiddle);
  Dialog.Anchor(vpBottom);
  InsertFront(Dialog);
end;

end.
