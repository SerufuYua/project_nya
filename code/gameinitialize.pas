{ Game initialization.
  This unit is cross-platform.
  It will be used by the platform-specific program or library file.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameInitialize;

interface

implementation

uses SysUtils, CustApp,
  CastleWindow, CastleLog, CastleUIControls, GameAdjustLoadingScreen
  {$region 'Castle Initialization Uses'}
  // The content here may be automatically updated by CGE editor.
  , GameViewMain
  , GameViewPlayGirl
  , GameViewPlaySolo
  , GameViewPlayTogether
  , GameViewDressingMenu
  , GameViewLoading
  , GameViewTravelContainerRoom
  , GameViewTravelRoadAsteroid
  , GameViewConversationMenu
  {$endregion 'Castle Initialization Uses'};

var
  Window: TCastleWindow;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { Create views (see https://castle-engine.io/views ). }
  {$region 'Castle View Creation'}
  // The content here may be automatically updated by CGE editor.
  ViewMain:= TViewMain.Create(Application);
  ViewPlayGirl:= TViewPlayGirl.Create(Application);
  ViewPlaySolo:= TViewPlaySolo.Create(Application);
  ViewPlayTogether:= TViewPlayTogether.Create(Application);
  ViewDressingMenu:= TViewDressingMenu.Create(Application);
  ViewLoading:= TViewLoading.Create(Application);
  ViewTravelContainerRoom:= TViewTravelContainerRoom.Create(Application);
  ViewTravelRoadAsteroid:= TViewTravelRoadAsteroid.Create(Application);
  {$endregion 'Castle View Creation'}

  if Application.hasOption('y', 'PlayToy') then
    Window.Container.View:= ViewPlayGirl
  else if Application.hasOption('s', 'PlaySolo') then
    Window.Container.View:= ViewPlaySolo
  else if Application.hasOption('t', 'PlayTogether') then
    Window.Container.View:= ViewPlayTogether
  else if Application.hasOption('h', 'Home') then
    Window.Container.View:= ViewTravelContainerRoom
  else if Application.hasOption('a', 'Asteroid') then
    Window.Container.View:= ViewTravelRoadAsteroid
  else
    Window.Container.View:= ViewMain;

end;

initialization
  { This initialization section configures:
    - Application.OnInitialize
    - Application.MainWindow
    - determines initial window size

    You should not need to do anything more in this initialization section.
    Most of your actual application initialization (in particular, any file reading)
    should happen inside ApplicationInitialize. }

  Application.OnInitialize:= @ApplicationInitialize;

  Window:= TCastleWindow.Create(Application);
  Window.FullScreen:= false; // default
  Window.Width:= 1024;
  Window.Height:= 768;
  Window.AntiAliasing:= aa4SamplesNicer;
  Application.MainWindow:= Window;

  { Optionally, adjust window fullscreen state and size at this point.
    Examples:

    Run fullscreen:

      Window.FullScreen := true;

    Run in a 600x400 window:

      Window.FullScreen := false; // default
      Window.Width:= 600;
      Window.Height:= 400;

    Run in a window taking 2/3 of screen (width and height):

      Window.FullScreen := false; // default
      Window.Width:= Application.ScreenWidth * 2 div 3;
      Window.Height:= Application.ScreenHeight * 2 div 3;

    Note that some platforms (like mobile) ignore these window sizes.
  }

  { Handle command-line parameters like --fullscreen and --window.
    By doing this last, you let user to override your fullscreen / mode setup. }
  Window.ParseParameters;
end.
