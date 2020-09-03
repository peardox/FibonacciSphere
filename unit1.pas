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
    CamPos, CamDir, CamUp: TVector3;
    CameraAtOrigin: Boolean;
    FullScreen: Boolean;
    RotateSphere: Boolean;
    RenderedFirstFrame: Boolean;
    LoadTimer: Int64;
    Scale: Single;
    SavedTheta: Single;
    procedure LoadScene(filename: String);
    procedure CreateViewport;
  public
    infoNotifications: TCastleNotifications;
    timeNotifications: TCastleNotifications;
    viewNotifications: TCastleNotifications;
    function BuildSphere(Filename: String; ObjCount: Integer = 1): TX3DRootNode;
    procedure ToggleFullScreen;
  end;

var
  Form1: TForm1;

const
  // How many seconds to take to rotate the scene (Number)
  SecsPerRot = 30;
  // How many objects (Integer)
  ObjectsOnSphere = 540;
  // The 3D model to use when building the sphere
  ModelFilename = 'castle-data:/box_roty.x3dv';
  // Scale of each object (Single)
  ScaleMultiplier = 1.0;

  // Dupe declaration hack - leave this one alone or it won't compile
  BorderNone = Controls.bsNone;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ToggleFullScreen;
begin
  FullScreen := not FullScreen;

  if FullScreen then
    begin
      // Go FullScreen
      {$if defined(linux) or defined(darwin)}
      WindowState := wsFullScreen;
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
      {$if defined(linux) or defined(darwin)}
      WindowState := wsNormal;
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
  Pos, Dir, Up: TVector3;
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
    'C = Position Camera inside Sphere' + LineEnding +
    LineEnding +
    'FPS : ' +
    FormatFloat('####0.00',CastleControlBase1.Fps.RealFps) +
    ', ' +
    FormatFloat('####0.00',CastleControlBase1.Fps.OnlyRenderFps) +
    ' (OnlyRender)');

  // Show the load time, number of objects and scale
  timeNotifications.Show('Time To First Frame : ' +
    FormatFloat('####0.000',LoadTimer / 1000) +
    ', Objects : ' +
    IntToStr(ObjectsOnSphere) +
    ', Scale : ' +
    FormatFloat('####0.00000',Scale)
    );

  // Get and show camera settings
  Viewport.Camera.GetView(Pos, Dir, Up);
  viewNotifications.Show('Camera Settings' + LineEnding + 'Pos : (' +
                      FormatFloat('####0.00', Pos.X) + ', ' +
                      FormatFloat('####0.00', Pos.Y) + ', ' +
                      FormatFloat('####0.00', Pos.Z) + ')' + LineEnding + 'Dir : (' +
                      FormatFloat('####0.00', Dir.X) + ', ' +
                      FormatFloat('####0.00', Dir.Y) + ', ' +
                      FormatFloat('####0.00', Dir.Z) + ')' + LineEnding + 'Up  : (' +
                      FormatFloat('####0.00', Up.X) + ', ' +
                      FormatFloat('####0.00', Up.Y) + ', ' +
                      FormatFloat('####0.00', Up.Z) + ')' + LineEnding);
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

  // Toggle Camera Position (0, 0, 0) / normal
  if Event.IsKey(keyC) then
    begin
      if CameraAtOrigin then
        begin
          Viewport.Camera.SetView(CamPos, CamDir, CamUp);
        end
      else
        begin
          // Save the camera settings
          Viewport.Camera.GetView(CamPos, CamDir, CamUp);
          Viewport.Camera.SetView(Vector3(0, 0, 0),
                                  Vector3(0, 0, 0),
                                  Vector3(0, 1, 0));
        end;
        // Switch CameraAtOrigin after saving settings!!!
        CameraAtOrigin := not CameraAtOrigin;
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
  GridNode: TX3DRootNode;
  GridObject: TX3DRootNode;
  TransformNode: TTransformNode;
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
  // divided by 2 (Sphere is radius 1, objects are radius 0.5)
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
  infoNotifications.MaxMessages := 6;
  infoNotifications.Anchor(hpLeft, 10);
  infoNotifications.Anchor(vpBottom, 10);

  // Add the time notification area to the CGE control
  timeNotifications := TCastleNotifications.Create(Application);
  timeNotifications.MaxMessages := 1;
  timeNotifications.Anchor(hpLeft, 10);
  timeNotifications.Anchor(vpTop, -10);

  // Add the view notification area to the CGE control
  viewNotifications := TCastleNotifications.Create(Application);
  viewNotifications.MaxMessages := 4;
  viewNotifications.Anchor(hpRight, -10);
  viewNotifications.Anchor(vpTop, -10);

  // Add the viewport to the CGE control
  CastleControlBase1.Controls.InsertFront(Viewport);
  CastleControlBase1.Controls.InsertFront(infoNotifications);
  CastleControlBase1.Controls.InsertFront(timeNotifications);
  CastleControlBase1.Controls.InsertFront(viewNotifications);
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

  // Save the default camera settings
  if not(CameraAtOrigin) then
    begin
      Viewport.Camera.GetView(CamPos, CamDir, CamUp);
    end;
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
  // Position Camera inside Sphere?
  CameraAtOrigin := False;
  // Don't rotate the sphere at start
  RotateSphere := False;
  // Create a Viewport
  CreateViewport;
  // Create and load the Fibonacci Sphere into the Viewport
  LoadScene(ModelFilename);
end;

end.

