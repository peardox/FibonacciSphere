unit Unit1;

// Fibonacci Sphere

{$mode objfpc}{$H+}
// {$define usedeepcopy}
// {$define usebatching}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Math,
  CastleControl, CastleCameras, CastleApplicationProperties, CastleUIControls,
  CastleSceneCore, CastleVectors, CastleScene, CastleViewport,
  X3DNodes, X3DFields, X3DLoad, CastleNotifications, CastleTimeUtils, CastleLCLUtils, CastleKeysMouse;

type

  { TForm1 }

  TForm1 = class(TForm)
    CastleControlBase1: TCastleControlBase;
    procedure CastleControlBase1BeforeRender(Sender: TObject);
    procedure CastleControlBase1Press(Sender: TObject;
      const Event: TInputPressRelease);
    procedure FormCreate(Sender: TObject);
  private
    Viewport: TCastleViewport;
    Scene: TCastleScene;
    procedure LoadScene(filename: String);
    procedure CreateViewport;
  public
    FullScreen: Boolean;
    RotateSphere: Boolean;
    Scale: Single;
    SavedTheta: Single;
    RenderedFirstFrame: Boolean;
    LoadTimer: Int64;
    infoNotifications: TCastleNotifications;
    timeNotifications: TCastleNotifications;
    function BuildSphere(Filename: String; ObjCount: Integer = 1): TX3DRootNode;
    procedure ToggleFullScreen;
  end;

var
  Form1: TForm1;

const
  // How many seconds to take to rotate the scene (Number)
  SecsPerRot = 30;
  // Dupe declaration hack
  BorderNone = Controls.bsNone;
  // How many objects (Integer)
  ObjectsOnSphere = 540;
  // Scale of each object (Single)
  ScaleMultiplier = 1.0;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ToggleFullScreen;
begin
  FullScreen := not FullScreen;

  if FullScreen then
    begin
      // Go FullScreen
      {$if defined(linux)}
      WindowState := wsFullScreen;
      {$elseif defined(darwin)}
      ShowWindow(Handle, SW_SHOWFULLSCREEN);
      {$else}
      Menu := nil;
      BorderStyle := BorderNone;
      WindowState := wsFullScreen;
      FormStyle := fsSystemStayOnTop;
      {$endif}
    end
  else
    begin
      // Go Windowed
      {$if defined(linux)}
      WindowState := wsNormal;
      {$elseif defined(darwin)}
      ShowWindow(Handle, SW_SHOWNORMAL);
      {$else}
      WindowState := wsNormal; // wsMinimized, wsMaximized, wsFullScreen
      BorderStyle := bsSizeable; // bsDialog, bsNone, bsSingle
      FormStyle := fsNormal;
      ShowInTaskBar := stDefault; // stAlways, stNever
      {$endif}
    end;
end;

procedure TForm1.CastleControlBase1BeforeRender(Sender: TObject);
var
  theta: Single;
begin
  if not RenderedFirstFrame then
    begin
      // Time elapsed from FormCreate to now
      LoadTimer := CastleGetTickCount64 - LoadTimer;
      // This is a one-shot so set RenderedFirstFrame
      RenderedFirstFrame := True;
    end;

  if RotateSphere and not (Scene = nil) then
    begin
      // Set angle (theta) to revolve completely once every SecsPerRot
      theta := (((CastleGetTickCount64 mod
                 (SecsPerRot * 1000)) /
                 (SecsPerRot * 1000)) * (Pi * 2)) + SavedTheta;

      // Rotate the scene in Y
      // Change to Vector4(1, 0, 0, theta); to rotate in X
      Scene.Rotation := Vector4(0, 1, 0, theta);
    end;

  // Show the control keys and frame rate
  infoNotifications.Show('Control Keys' + LineEnding +
    'F = Toggle FullScreen' + LineEnding +
    'R = Rotate Sphere in Y Axis' + LineEnding +
    LineEnding +
    'FPS : ' +
    FormatFloat('####0.00',CastleControlBase1.Fps.RealFps) +
    ', ' +
    FormatFloat('####0.00',CastleControlBase1.Fps.OnlyRenderFps) +
    ' (OnlyRender)'
    );

  // Show the load time, number of objects and scale
  timeNotifications.Show('Time To First Frame : ' +
    FormatFloat('####0.000',LoadTimer / 1000) +
    ', Objects : ' +
    IntToStr(ObjectsOnSphere) +
    ', Scale : ' +
    FormatFloat('####0.00000',Scale)
    );

end;

procedure TForm1.CastleControlBase1Press(Sender: TObject;
  const Event: TInputPressRelease);
var
  CurrentTheta: Single;
begin
  // Toggle FullScreen mode
  if Event.IsKey(keyF) then
    begin
      ToggleFullScreen;
    end;

  // Toggle Rotation in the Y Axis
  if Event.IsKey(keyR) then
    begin
      RotateSphere := Not RotateSphere;

      // Only do anything with Scene if it's loaded
      if not (Scene = nil) then
        begin
          if RotateSphere then
            begin
              // If the scene is about to resume store an offset in SavedTheta
              CurrentTheta := ((CastleGetTickCount64 mod
                               (SecsPerRot * 1000)) /
                               (SecsPerRot * 1000)) * (Pi * 2);
              SavedTheta := SavedTheta - CurrentTheta;
            end
          else
            begin
              // Save the current rotation in SavedTheta
              SavedTheta := Scene.Rotation.W; // W is the angle of a Vector4
            end;
        end;
    end;

end;

function TForm1.BuildSphere(Filename: String; ObjCount: Integer = 1): TX3DRootNode;
var
  GroupNode: TGroupNode;
  TransformNode: TTransformNode;
  GridNode: TX3DRootNode;
  GridObject: TX3DRootNode;
  {$ifdef usedeepcopy}
  NewGridNode: TX3DRootNode;
  {$endif}
  XPos: Single;
  YPos: Single;
  ZPos: Single;
  Phi: Single;
  Theta: Single;
  Radius: Single;
  Idx: Integer;
begin
  // Create the required objects
  GridObject := LoadNode(FileName);
  GroupNode := TGroupNode.Create;
  GridNode := TX3DRootNode.Create;
  GridNode.AddChildren(GroupNode);

  // The scale of objects making up the Sphere is
  // the inverse square root of the number of objects
  // divided by 2 (Sphere is raduis 1, objects are radius 0.5)
  // ScaleMultiplier allows fine tuning of object size
  Scale := (1 / (sqrt(ObjCount) / 2)) * ScaleMultiplier;

  // Fibonnaci Sphere from StackOverflow answer with Python code + screenshot
  // https://stackoverflow.com/questions/9600801/evenly-distributing-n-points-on-a-sphere

  // Golden angle in radians
  Phi := pi * (3 - sqrt(5));

  for Idx := 0 to ObjCount -1 do
    begin
      // Adapted from the StackOverflow code
      YPos := 1 - (Idx / (ObjCount - 1)) * 2;
      Radius := sqrt(1 - (YPos * YPos));
      Theta := Phi * Idx;
      XPos := cos(Theta) * Radius;
      ZPos := sin(Theta) * Radius;
      // We not have X, Y and Z for the object so apply scale + translation
      TransformNode := TTransformNode.Create;
      TransformNode.Scale := Vector3(Scale, Scale, Scale);
      TransformNode.Translation := Vector3(XPos, YPos, ZPos);
      {$ifdef usedeepcopy}
      NewGridNode := GridObject.DeepCopy as TX3DRootNode;
      TransformNode.AddChildren(NewGridNode);
      {$else}
      // Add this object to it's transform node
      TransformNode.AddChildren(GridObject);
      {$endif}
      // Add the object + transform to the group
      GroupNode.AddChildren(TransformNode);
    end;

  {$ifdef usedeepcopy}
  GridObject.Free();
  {$endif}

  // The group now contains ObjCount objects in a Spherical arrangement
  Result := GridNode;
end;

procedure TForm1.CreateViewport;
begin
  // Set up the main viewport
  Viewport := TCastleViewport.Create(Application);
  // Use all the viewport
  Viewport.FullSize := true;
  // Automatically position the camera
  Viewport.AutoCamera := true;
  // Use default navigation keys
  Viewport.AutoNavigation := true;

  // Add the info notification area to the CGE control
  infoNotifications := TCastleNotifications.Create(Application);
  infoNotifications.MaxMessages := 5;
  infoNotifications.Anchor(hpLeft, 10);
  infoNotifications.Anchor(vpBottom, 10);

  // Add the time notification area to the CGE control
  timeNotifications := TCastleNotifications.Create(Application);
  timeNotifications.MaxMessages := 1;
  timeNotifications.Anchor(hpLeft, 10);
  timeNotifications.Anchor(vpTop, -10);

  // Add the viewport to the CGE control
  CastleControlBase1.Controls.InsertFront(Viewport);
  CastleControlBase1.Controls.InsertFront(infoNotifications);
  CastleControlBase1.Controls.InsertFront(timeNotifications);
end;

procedure TForm1.LoadScene(filename: String);
var
  SphereNode: TX3DRootNode;
begin
  // Create a scene
  Scene := TCastleScene.Create(Application);

  {$ifdef usebatching}
  DynamicBatching := True;
  {$endif}

  // Load a model into the scene
  SphereNode := BuildSphere(FileName, ObjectsOnSphere);
  Scene.Load(SphereNode, True);
  // Add the scene to the viewport
  Viewport.Items.Add(Scene);

  // Tell the control this is the main scene so it gets some lighting
  Viewport.Items.MainScene := Scene;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Set application title and make window 1/4 screen size
  Caption := 'Fibonacci Sphere';
  Width := Trunc(Screen.Width / 2);
  Height := Trunc(Screen.Height / 2);
  // Get time of form create
  LoadTimer := CastleGetTickCount64;
  // Trap first frame flag (set false cos it hasn't happened yet)
  RenderedFirstFrame := False;
  // SavedTheta - used to pause rotation in Y Axis
  SavedTheta := 0;
  // Not FullScreen
  FullScreen := False;
  // Don't rotate the sphere at start
  RotateSphere := False;
  // Create a Viewport
  CreateViewport;
  // Create and load the Fibonacci Sphere into the Viewport
  LoadScene('castle-data:/box_roty.x3dv');
end;

end.
