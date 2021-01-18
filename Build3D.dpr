program Build3D;

uses
  Vcl.Forms,
  UnitMain in 'UnitMain.pas' {FormMain},
  Render in 'Render.pas',
  DrawTools in 'DrawTools.pas',
  DrawToolCube in 'DrawToolCube.pas',
  DrawToolCubeCube in 'DrawToolCubeCube.pas',
  DrawToolClear in 'DrawToolClear.pas',
  Model in 'Model.pas',
  DrawToolPar in 'DrawToolPar.pas',
  DrawToolSphere in 'DrawToolSphere.pas',
  BlockListHelper in 'BlockListHelper.pas',
  UnitSizeLimit in 'UnitSizeLimit.pas' {FormSizeLimit},
  CommonClasses in 'CommonClasses.pas',
  UnitSliceOpt in 'UnitSliceOpt.pas' {FormSliceOpt},
  Constants in 'Constants.pas',
  CommonProc in 'CommonProc.pas',
  DrawToolPip in 'DrawToolPip.pas',
  Measure in 'Measure.pas',
  Monitor in 'Monitor.pas',
  ModelMap in 'ModelMap.pas',
  DebugClient in 'DebugClient.pas',
  DrawToolSmooth in 'DrawToolSmooth.pas',
  DrawToolGrow in 'DrawToolGrow.pas',
  ModelExport in 'ModelExport.pas',
  DrawToolSel in 'DrawToolSel.pas',
  CopyParser in 'CopyParser.pas',
  UnitEditPal in 'UnitEditPal.pas' {FormEditPal},
  Palette in 'Palette.pas',
  UnitTranspTexs in 'UnitTranspTexs.pas' {FormTranspTexs};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Трехмерный редактор';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
