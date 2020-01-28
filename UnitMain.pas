unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus,
  Generics.Collections,
  Render, DrawTools, Model, CommonClasses ;

type
  TFormMain = class(TForm)
    PanelTool: TPanel;
    PanelGL: TPanel;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    NExit: TMenuItem;
    comboTexs: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    EdCubeN: TEdit;
    Label4: TLabel;
    EdParX: TEdit;
    EdParY: TEdit;
    EdParZ: TEdit;
    Panel1: TPanel;
    stModelSize: TStaticText;
    N3: TMenuItem;
    NSetSizeLimit: TMenuItem;
    NNew: TMenuItem;
    NOpen: TMenuItem;
    NSave: TMenuItem;
    NSaveAs: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    NSlice: TMenuItem;
    N5: TMenuItem;
    NDecart: TMenuItem;
    NSphere: TMenuItem;
    NPreview: TMenuItem;
    NGreenSide: TMenuItem;
    NSetBackColor: TMenuItem;
    ColorDialog1: TColorDialog;
    N7: TMenuItem;
    NSetDefaultCenter: TMenuItem;
    N8: TMenuItem;
    NUndo: TMenuItem;
    N4: TMenuItem;
    NBlockReport: TMenuItem;
    Label5: TLabel;
    ComboTexsInModel: TComboBox;
    Timer1: TTimer;
    stFPS: TStaticText;
    NNoSelection: TMenuItem;
    LabR: TLabel;
    procedure NExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PanelGLMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure PanelGLClick(Sender: TObject);
    procedure PanelGLMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NSetSizeLimitClick(Sender: TObject);
    procedure NSaveClick(Sender: TObject);
    procedure NNewClick(Sender: TObject);
    procedure NOpenClick(Sender: TObject);
    procedure NSaveAsClick(Sender: TObject);
    procedure NSliceClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure NDecartClick(Sender: TObject);
    procedure NSphereClick(Sender: TObject);
    procedure NPreviewClick(Sender: TObject);
    procedure NGreenSideClick(Sender: TObject);
    procedure NSetBackColorClick(Sender: TObject);
    procedure N7Click(Sender: TObject);
    procedure NSetDefaultCenterClick(Sender: TObject);
    procedure NUndoClick(Sender: TObject);
    procedure NBlockReportClick(Sender: TObject);
    procedure ComboTexsInModelChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure NNoSelectionClick(Sender: TObject);
  private
     dc : HDC; //контекст устройства
     hRC : HGLRC; //контекст рендеринга
     render:TRender ;
     texturecodes:TStringList ;
     model:TModel ;
     buts: TList<TButton> ;
     ActiveTool:TDrawTool ;
     ActiveFileName:string ;
     OldMousePos:TPoint ;
     OldCtrlPress:Boolean ;
     oldblockgid:string ;
     olddir:TBlockDir ;
     procedure IdleHandler(Sender : TObject; var Done : Boolean);
     procedure ToolClick(Sender: TObject) ;
     procedure UpdateInfo() ;
     procedure LoadModel(FileName:string) ;
     procedure doSaveDialog() ;
     procedure UpdateToolParams(tool:TDrawTool=nil) ;
     procedure MeasureTopLevelItem(Sender: TObject; ACanvas: TCanvas; var Width,
       Height: Integer);
     procedure UpdateCombo() ;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation
uses OpenGL, IOUtils,
  dglOpenGL,
  DrawToolCubeCube,DrawToolPar,DrawToolSphere, DrawToolSmooth, UnitSizeLimit,
    UnitSliceOpt, Constants, DrawToolPip, CommonProc, Monitor, Measure ;

{$R *.dfm}

procedure TFormMain.ComboTexsInModelChange(Sender: TObject);
begin
  ComboTexs.ItemIndex:=ComboTexs.Items.IndexOf
    (ComboTexsInModel.Items[ComboTexsInModel.ItemIndex]) ;
end;

procedure TFormMain.doSaveDialog;
begin
  if SaveDialog1.Execute() then begin
    if TPath.GetExtension(SaveDialog1.FileName)<>DEFAULTEXT then
      SaveDialog1.FileName:=SaveDialog1.FileName+DEFAULTEXT ;
    model.SaveToFile(SaveDialog1.FileName);
    ActiveFileName:=SaveDialog1.FileName ;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var tool:TDrawTool ;
    p,k:Integer ;
    but:TButton ;
    x,y,z:Integer ;
    texcode:string ;
    errmsg:string ;

procedure setMeasureProc(root:TMenuItem);
var mi:TMenuItem ;
begin
  for mi in root do begin
    mi.OnMeasureItem:=MeasureTopLevelItem ;
    mi.Caption:=mi.Caption+' ';
    setMeasureProc(mi) ;
  end;
end;

begin

  TEXDIR := AppPath()+'\textures' ;

  dc := GetDC(PanelGL.Handle);  //получаем контекст устройства по форме Form1

  if  not InitOpenGL then begin
    ShowMessage('Что-то пошло не так. Не удается включить OpenGL') ;
    Exit ;
  end;

  model:=TModel.Create ;
  render:=TRender.Create(dc,model) ;

  hrc := CreateRenderingContext(dc,[opDoubleBuffered],32,24,0,0,0,0);

  ActivateRenderingContext(dc,hrc); //активируем контекст рендеринга
  render.SetupGL();
  texturecodes:=render.LoadRes(TEXDIR) ;
  //установка режимов OpenGL
  Application.OnIdle := IdleHandler;

  buts:=TList<TButton>.Create() ;
  p:=30 ;
  k:=0 ;
  for tool in TDrawTool.getToolList() do begin
    but:=TButton.Create(panelTool);
    but.Font.Size:=14 ;
    but.Left:=10 ;
    but.Width:=panelTool.Width-but.Left*2 ;
    but.Top:=p ;
    but.Caption:=tool.Caption ;
    but.Parent:=panelTool ;
    but.OnClick:=ToolClick ;
    but.Tag:=k ;
    Inc(k) ;
    buts.Add(but) ;
    Inc(p,but.Height+5) ;
  end;
  buts[0].Click() ;

  for texcode in texturecodes do
    comboTexs.Items.Add(texcode);
  comboTexs.ItemIndex:=0 ;

  // Отладочный код
  LoadModel('defaults/demo.model') ;

  ActiveFileName:='' ;

  OldMousePos.X:=Low(Integer) ;
  OldMousePos.Y:=Low(Integer) ;
  OldCtrlPress:=False ;

  Caption:='Трехмерный редактор кубиков: '+render.GetPosInfo() ;
  UpdateInfo() ;

  Screen.MenuFont.Size:=16 ;
  setMeasureProc(MainMenu1.Items) ;

  Timer1.Enabled:=True ;

end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  render.Free ;
  DeactivateRenderingContext;
  DestroyRenderingContext(hrc);
  ReleaseDC(PanelGL.Handle,dc);
end;

const VK_W = 87 ;
const VK_S = 83 ;
const VK_A = 65 ;
const VK_D = 68 ;
const VK_Q = 81 ;
const VK_E = 69 ;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var strafe:Boolean ;
   block:TBlock ;
   dir:TBlockDir ;
begin
  strafe:=False ;//ssShift in Shift ;

  if Key=VK_F6 then render.SwitchToDecart() ;
  if Key=VK_F7 then render.SwitchToSphere() ;
  if Key=VK_F8 then begin
     if render.isBlockOver(block,dir) then
       render.SetCenterView(block);
  end;

  //Label1.Caption:=IntToStr(Key) ;
  if Key=VK_W then render.MoveByR(1);
  if Key=VK_S then render.MoveByR(-1);

  if Key=VK_Q then begin
    if strafe then
      render.StrafeVert(0.25)
    else
      render.MoveByTeta(+0.1);
  end;

  if Key=VK_E then begin
    if strafe then
      render.StrafeVert(-0.25)
    else
      render.MoveByTeta(-0.1);
  end;

  if Key=VK_A then begin
    if strafe then
      render.StrafeHorz(-0.25)
    else
      render.MoveByFi(-0.1);
  end;

  if Key=VK_D then begin
    if strafe then
      render.StrafeHorz(0.25)
    else
      render.MoveByFi(0.1);
  end;

  Caption:='Трехмерный редактор кубиков: '+render.GetPosInfo() ;
//  Refresh() ;
end;

procedure TFormMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin

  if WheelDelta>0 then render.MoveByR(1);
  if WheelDelta<0 then render.MoveByR(-1);

  Caption:='Трехмерный редактор кубиков: '+render.GetPosInfo() ;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  panelGL.SetFocus() ;
end;

procedure TFormMain.IdleHandler(Sender : TObject; var Done : Boolean);
var frequency, start, finish:Int64 ;
begin
  QueryPerformanceFrequency(frequency);
  QueryPerformanceCounter(start);

  render.Render(PanelGL.Width,PanelGL.Height);
  Sleep(1);

  QueryPerformanceCounter(finish);

  getMonitor().WriteDelay(Round((finish - start) * 1000 / frequency)) ;

  Done := False;
end;

procedure TFormMain.LoadModel(FileName: string);
var r:TLoadResult ;
    errmsg:string ;
begin
  r:=model.LoadFromFile(FileName,texturecodes,errmsg) ;
  if r<>TLoadResult.lrOK then ShowMessage(errmsg) ;
  UpdateInfo() ;
  UpdateCombo() ;
  //ShowMessage('ok') ;
end;

procedure TFormMain.MeasureTopLevelItem(Sender: TObject; ACanvas: TCanvas; var Width,
  Height: Integer);
begin
  ACanvas.Font.Size:=Screen.MenuFont.Size ;
  Width:=ACanvas.TextWidth(TMenuItem(Sender).Caption) ;
  Height:=ACanvas.TextHeight(TMenuItem(Sender).Caption) ;
end;

procedure TFormMain.N7Click(Sender: TObject);
begin
  ShowMessage('Нажмите F8, когда будет выделен кубик мышкой') ;
end;

procedure TFormMain.NBlockReportClick(Sender: TObject);
begin
  ShowMessage(model.buildBlockReport()) ;
end;

procedure TFormMain.NDecartClick(Sender: TObject);
begin
  render.SwitchToDecart() ;
end;

procedure TFormMain.NExitClick(Sender: TObject);
begin
  Close() ;
end;

procedure TFormMain.NGreenSideClick(Sender: TObject);
begin
  render.setSelectionMode(smGreenSide);
end;

procedure TFormMain.NNewClick(Sender: TObject);
begin
  model.Clear() ;
  ActiveFileName:='' ;
  UpdateInfo() ;
end;

procedure TFormMain.NNoSelectionClick(Sender: TObject);
begin
  render.NoSelection:=not   render.NoSelection;
  NNoSelection.Checked:=  render.NoSelection ;
end;

procedure TFormMain.NOpenClick(Sender: TObject);
begin
  if not OpenDialog1.Execute() then Exit ;

  LoadModel(OpenDialog1.FileName) ;
  ActiveFileName:=OpenDialog1.FileName ;
end;

procedure TFormMain.NPreviewClick(Sender: TObject);
begin
  render.setSelectionMode(smPreview);
end;

procedure TFormMain.NSaveAsClick(Sender: TObject);
begin
  doSaveDialog() ;
end;

procedure TFormMain.NSaveClick(Sender: TObject);
begin
  if ActiveFileName<>'' then
    model.SaveToFile(ActiveFileName)
  else
    doSaveDialog() ;
end;

procedure TFormMain.NSetBackColorClick(Sender: TObject);
begin
  ColorDialog1.Color:=render.BackColor ;
  if ColorDialog1.Execute() then begin
    render.BackColor:=ColorDialog1.Color ;
  end;

end;

procedure TFormMain.NSetDefaultCenterClick(Sender: TObject);
begin
  render.SetDefaultCenterView() ;
end;

procedure TFormMain.NSetSizeLimitClick(Sender: TObject);
var fm:TFormSizeLimit ;
begin
  fm:=TFormSizeLimit.Create(Self);
  fm.setLimits(model.getLimits()) ;
  if fm.ShowModal()=mrOK then begin
    model.setLimits(fm.getLimits());
    UpdateInfo() ;
  end;
end;

procedure TFormMain.NSliceClick(Sender: TObject);
var fm:TFormSliceOpt ;
begin
  fm:=TFormSliceOpt.Create(Self) ;
  if fm.ShowModal()=mrOK then begin
    model.SaveLayers(fm.Dir(),fm.FileTpl(),fm.FormatExt(),fm.Axis(),
      fm.ShowGrid(),fm.ShowPriorLayer(),fm.GridWidth(),fm.LayerBr()) ;
    ShowMessage('Нарезка выполнена') ;
  end;
end;

procedure TFormMain.NSphereClick(Sender: TObject);
begin
  render.SwitchToSphere() ;
end;

procedure TFormMain.NUndoClick(Sender: TObject);
begin
  if not model.PopBlocks() then
    ShowMessage('Нет сохраненных изменений')
  else
    UpdateInfo() ;
end;

procedure TFormMain.PanelGLClick(Sender: TObject);
begin
  panelGL.SetFocus() ;
end;

procedure TFormMain.PanelGLMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var q:Integer ;
    block:TBlock ;
    dir:TBlockDir ;
    list:TList<TBlock> ;
begin

  render.SetMouse(X,Y);

  if render.isBlockOver(block,dir) then begin
    if (oldblockgid<>block.gid)or(olddir<>dir) then begin
      oldblockgid:=block.gid ; olddir:=dir ;
      UpdateToolParams() ;
      list:=ActiveTool.PregetNewBlocks(block,dir) ;
      render.SetPreBlocks(list) ;
      list.Free ;
    end;
  end;

//  Caption:=IntToStr(X)+' '+IntToStr(y) ;
  if (not (ssCtrl in Shift)) then begin
    OldCtrlPress:=False ;
    Exit ;
  end;

  if not OldCtrlPress then begin
    OldCtrlPress:=True ;
  end
  else begin
    if render.getMovingMode()=mmDecart then q:=200 else q:=100 ;

    render.MoveByFi((X-OldMousePos.X)/q);
    render.MoveByTeta((Y-OldMousePos.Y)/q);
  end;
  OldMousePos.X:=X ;
  OldMousePos.Y:=Y ;

//  Refresh() ;
end;

procedure TFormMain.PanelGLMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var block:TBlock ;
    dir:TBlockDir ;
    apply:Boolean ;
    ms:Integer ;
begin
  if Button=TMouseButton.mbRight then begin

    if render.isBlockOver(block,dir) then begin
      UpdateToolParams(TDrawTool.getClearTool) ;
      TDrawTool.getClearTool().Apply(model,block,dir) ;
      render.EmitRebuild3D() ;
      UpdateInfo() ;
    end

  end;

  if Button=TMouseButton.mbLeft then begin

  if ActiveTool=nil then Exit ;

  apply:=False ;
  if render.isBlockOver(block,dir) then begin
     if ActiveTool is TDrawToolPip then
       comboTexs.ItemIndex:=comboTexs.Items.IndexOf(block.texcode)
     else
       apply:=True ;
  end
  else
  if model.IsEmpty() then begin
     apply:=True ;
     block.x:=-1 ;
     block.y:=0 ;
     block.z:=0 ;
     dir:=dirXgr ;
  end;

  if apply then begin
    UpdateToolParams() ;
    //StartMeasure() ;
    ActiveTool.Apply(model,block,dir) ;
    //ms:=FinishMeasure() ;
    //TFile.WriteAllText('app.log',ms.ToString());
    render.EmitRebuild3D() ;
    UpdateInfo() ;
    UpdateCombo() ;
  end;

  end;

end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  stFPS.Caption:=getMonitor().GetInfo() ;
end;

procedure TFormMain.ToolClick(Sender: TObject);
var t:TButton ;
begin
  ActiveTool:=TDrawTool.getToolList()[TButton(Sender).Tag] ;
  for t in buts do
    if t=sender then t.Font.Style:=[fsBold] else t.Font.Style:=[] ;

  Label3.Visible:=(ActiveTool is TDrawToolCubeCube)or
  (ActiveTool is TDrawToolSphere) ;
  LabR.Visible:=(ActiveTool is TDrawToolSmooth) ;
  EdCubeN.Visible:=(ActiveTool is TDrawToolCubeCube)or
  (ActiveTool is TDrawToolSphere) or
  (ActiveTool is TDrawToolSmooth) ;

  Label4.Visible:=(ActiveTool is TDrawToolPar) ;
  EdParX.Visible:=(ActiveTool is TDrawToolPar) ;
  EdParY.Visible:=(ActiveTool is TDrawToolPar) ;
  EdParZ.Visible:=(ActiveTool is TDrawToolPar) ;
end;

procedure TFormMain.UpdateToolParams(tool:TDrawTool=nil);
begin
  if tool=nil then tool:=ActiveTool ;

  if tool=nil then Exit ;

  try
  Tool.tmpTexName:=ComboTexs.Items[ComboTexs.ItemIndex] ;
  Tool.tmpCubeSize:=StrToInt(Trim(EdCubeN.Text)) ;
  Tool.tmpParX:=StrToInt(Trim(EdParX.Text)) ;
  Tool.tmpParY:=StrToInt(Trim(EdParY.Text)) ;
  Tool.tmpParZ:=StrToInt(Trim(EdParZ.Text)) ;
  except
  end;
end;

procedure TFormMain.UpdateCombo;
var seltex:string ;
begin
  if ComboTexsInModel.ItemIndex<>-1 then
    seltex:=ComboTexsInModel.Items[ComboTexsInModel.ItemIndex]
  else
    seltex:='' ;

  model.fillUsedTextures(ComboTexsInModel.Items);

  if seltex<>'' then ComboTexsInModel.ItemIndex:=ComboTexsInModel.Items.IndexOf(seltex) ;
end;

procedure TFormMain.UpdateInfo;
begin
  stModelSize.Caption:=' Размер: '+model.getSizeInfo() ;
  stModelSize.Color:=clWhite ;
  if model.isLimitOver() then begin
    stModelSize.Caption:=stModelSize.Caption+#13#10' превышен лимит!' ;
    stModelSize.Color:=clRed ;
  end ;
end;

end.
