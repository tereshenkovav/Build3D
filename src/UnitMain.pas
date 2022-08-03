unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus,
  Generics.Collections,
  Render, DrawTools, Model, CommonClasses, KeysConfig ;

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
    NShowBorder: TMenuItem;
    NBridght: TMenuItem;
    NRedo: TMenuItem;
    rbFullBlock: TRadioButton;
    rbUpper: TRadioButton;
    rbLower: TRadioButton;
    cbSetToEdge: TCheckBox;
    cbAutoTexGrow: TCheckBox;
    Label6: TLabel;
    ComboPal: TComboBox;
    N6: TMenuItem;
    NAddToPal: TMenuItem;
    NEditPal: TMenuItem;
    NSavePalToFile: TMenuItem;
    NLoadPalFromFile: TMenuItem;
    SaveDialog2: TSaveDialog;
    OpenDialog2: TOpenDialog;
    ImageTex: TImage;
    NTranspTextures: TMenuItem;
    NSetKeys: TMenuItem;
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
    procedure NShowBorderClick(Sender: TObject);
    procedure NBridghtClick(Sender: TObject);
    procedure NRedoClick(Sender: TObject);
    procedure ComboPalChange(Sender: TObject);
    procedure NAddToPalClick(Sender: TObject);
    procedure NEditPalClick(Sender: TObject);
    procedure NSavePalToFileClick(Sender: TObject);
    procedure NLoadPalFromFileClick(Sender: TObject);
    procedure comboTexsChange(Sender: TObject);
    procedure NTranspTexturesClick(Sender: TObject);
    procedure NSetKeysClick(Sender: TObject);
  private
     dc : HDC; //контекст устройства
     hRC : HGLRC; //контекст рендеринга
     render:TRender ;
     texturecodes:TStringList ;
     textureicons:TDictionary<string,TBitmap> ;
     model:TModel ;
     buts: TList<TButton> ;
     ActiveTool:TDrawTool ;
     ActiveFileName:string ;
     OldMousePos:TPoint ;
     OldCtrlPress:Boolean ;
     oldblockgid:string ;
     olddir:TBlockDir ;
     keysconfig:TKeysConfig ;
     procedure IdleHandler(Sender : TObject; var Done : Boolean);
     procedure ToolClick(Sender: TObject) ;
     procedure UpdateInfo() ;
     procedure LoadModel(FileName:string) ;
     procedure doSaveDialog() ;
     procedure UpdateToolParams(tool:TDrawTool=nil) ;
     procedure MeasureTopLevelItem(Sender: TObject; ACanvas: TCanvas; var Width,
       Height: Integer);
     procedure UpdateCombo() ;
     procedure setTextureIndex(index:Integer) ;
     procedure updateMenuKeys() ;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation
uses OpenGL, IOUtils, IniFiles,
  dglOpenGL,
  DrawToolCubeCube,DrawToolPar,DrawToolSphere, DrawToolSmooth,DrawToolGrow,
    UnitSizeLimit, DrawToolCube, DrawToolSel,
    UnitSliceOpt, Constants, DrawToolPip, CommonProc, Monitor, Measure,
    ModelExport, CopyParser, UnitEditPal, UnitTranspTexs, UnitSetKeys ;

{$R *.dfm}

procedure TFormMain.setTextureIndex(index: Integer);
begin
  ComboTexs.ItemIndex:=index ;
  comboTexsChange(comboTexs) ;
end;

procedure TFormMain.ComboPalChange(Sender: TObject);
begin
  setTextureIndex(ComboTexs.Items.IndexOf
    (ComboPal.Items[ComboPal.ItemIndex])) ;
end;

procedure TFormMain.comboTexsChange(Sender: TObject);
begin
  ImageTex.Picture.Assign(textureicons[ComboTexs.Items[ComboTexs.ItemIndex]]);
end;

procedure TFormMain.ComboTexsInModelChange(Sender: TObject);
begin
  setTextureIndex(ComboTexs.Items.IndexOf
    (ComboTexsInModel.Items[ComboTexsInModel.ItemIndex])) ;
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

  TEXDIR := AppDataPath()+'\textures' ;
  if not TDirectory.Exists(TEXDIR) then TDirectory.CreateDirectory(TEXDIR) ;

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
  textureicons:=render.LoadTexIcons(TEXDIR,64,64) ;
  //установка режимов OpenGL
  Application.OnIdle := IdleHandler;

  buts:=TList<TButton>.Create() ;
  p:=30 ;
  k:=0 ;
  for tool in TDrawTool.getToolList() do begin
    but:=TButton.Create(panelTool);
    but.Font.Size:=12 ;
    but.Left:=10 ;
    but.Width:=panelTool.Width-but.Left*2 ;
    but.Top:=p ;
    but.Caption:=tool.Caption ;
    but.Parent:=panelTool ;
    but.OnClick:=ToolClick ;
    but.Tag:=k ;
    Inc(k) ;
    buts.Add(but) ;
    Inc(p,but.Height+3) ;
  end;
  buts[0].Click() ;

  for texcode in texturecodes do
    comboTexs.Items.Add(texcode);
  if comboTexs.Items.Count=0 then
    ShowMessage('Нет ни одного файла в каталоге '+TEXDIR)
  else
    setTextureIndex(0) ;

  with TIniFile.Create(AppDataPath+'\config.ini') do begin
    render.setTranspTexs(ReadString('Transparent','Textures','')) ;
    Free ;
  end ;

  // Отладочный код
  if TFile.Exists('defaults/demo.model') then
    LoadModel('defaults/demo.model') ;

  ActiveFileName:='' ;

  OldMousePos.X:=Low(Integer) ;
  OldMousePos.Y:=Low(Integer) ;
  OldCtrlPress:=False ;

  Caption:='Трехмерный редактор кубиков: '+render.GetPosInfo() ;
  UpdateInfo() ;

  Screen.MenuFont.Size:=16 ;
  setMeasureProc(MainMenu1.Items) ;

  keysconfig:=TKeysConfig.Create ;
  keysconfig.addKey(KEY_SWITCH_DECART,'Переключение в декартовые координаты',sNo,VK_F6);
  keysconfig.addKey(KEY_SWITCH_SPHERE,'Переключение в полярные координаты',sNo,VK_F7);
  keysconfig.addKey(KEY_SET_CENTERVIEW,'Установить центр вращения',sNo,VK_F8);
  keysconfig.addKey(KEY_GO_NEAR,'Приближение камеры',sNo,VK_W);
  keysconfig.addKey(KEY_GO_FAR,'Удаление камеры',sNo,VK_S);
  keysconfig.addKey(KEY_GO_DOWN,'Камера ниже',sNo,VK_Q);
  keysconfig.addKey(KEY_GO_UP,'Камера выше',sNo,VK_E);
  keysconfig.addKey(KEY_GO_LEFT,'Камера влево',sNo,VK_A);
  keysconfig.addKey(KEY_GO_RIGHT,'Камера вправо',sNo,VK_D);
  keysconfig.addKey(KEY_CANCEL_SELECT,'Сброс выделенного',sNo,VK_ESCAPE);
  keysconfig.addKey(KEY_COPY_SELECT,'Копия выделенного',sCtrl,VK_INSERT);
  keysconfig.addKey(KEY_ROTFILL_SELECT,'Заливка вращением выделенного',sCtrl,VK_F);
  keysconfig.addKey(KEY_DELETE_SELECT,'Очистка выделенного',sCtrl,VK_DELETE);
  keysconfig.loadFromFile() ;
  updateMenuKeys() ;

  Timer1.Enabled:=True ;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  render.Free ;
  DeactivateRenderingContext;
  DestroyRenderingContext(hrc);
  ReleaseDC(PanelGL.Handle,dc);
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var strafe:Boolean ;
   block:TBlock ;
   dir:TBlockDir ;
   str:string ;
   parser:TCopyParser ;
   zone:TZone3I ;
begin
  strafe:=False ;//ssShift in Shift ;

  if keysconfig.isKeyMatch(KEY_SWITCH_DECART,Shift,Key) then render.SwitchToDecart() ;
  if keysconfig.isKeyMatch(KEY_SWITCH_SPHERE,Shift,Key) then render.SwitchToSphere() ;
  if keysconfig.isKeyMatch(KEY_SET_CENTERVIEW,Shift,Key) then begin
     if render.isBlockOver(block,dir) then
       render.SetCenterView(block);
  end;

  //Label1.Caption:=IntToStr(Key) ;
  if keysconfig.isKeyMatch(KEY_GO_NEAR,Shift,Key) then render.MoveByR(1);
  if keysconfig.isKeyMatch(KEY_GO_FAR,Shift,Key) then render.MoveByR(-1);

  if keysconfig.isKeyMatch(KEY_GO_DOWN,Shift,Key) then begin
    if strafe then
      render.StrafeVert(0.25)
    else
      render.MoveByTeta(+0.1);
  end;

  if keysconfig.isKeyMatch(KEY_GO_UP,Shift,Key) then begin
    if strafe then
      render.StrafeVert(-0.25)
    else
      render.MoveByTeta(-0.1);
  end;

  if keysconfig.isKeyMatch(KEY_GO_LEFT,Shift,Key) then begin
    if strafe then
      render.StrafeHorz(-0.25)
    else
      render.MoveByFi(-0.1);
  end;

  if keysconfig.isKeyMatch(KEY_GO_RIGHT,Shift,Key) then begin
    if strafe then
      render.StrafeHorz(0.25)
    else
      render.MoveByFi(0.1);
  end;

  if keysconfig.isKeyMatch(KEY_CANCEL_SELECT,Shift,Key) then render.resetSelect() ;

  if keysconfig.isKeyMatch(KEY_COPY_SELECT,Shift,Key) then begin

    if not render.isZoneSelection() then begin
      ShowMessage('Не выделены блоки') ;
      Exit ;
    end ;

    if not InputQuery('Копирование','Введите сдвиг копирования (x y z) и установку отражений',str) then Exit ;

    parser:=TCopyParser.Create(str) ;
    if not parser.isCommandOk() then
      ShowMessage('Неверный ввод координат сдвига')
    else begin
      model.CopyZoneTo(render.getZoneSelection(),
        parser.getTarget(),
        parser.getMirrors()) ;
      render.EmitRebuild3D() ;
    end ;
  end;

  if keysconfig.isKeyMatch(KEY_ROTFILL_SELECT,Shift,Key) then begin

    if not render.isZoneSelection() then begin
      ShowMessage('Не выделены блоки') ;
      Exit ;
    end ;

    zone:=render.getZoneSelection() ;
    if not isZoneReadyForRotFill(zone) then begin
      ShowMessage('Некорректная зона для заливки вращением - нужна толщина 1 блок') ;
      Exit ;
    end ;

    str:='0' ;
    if not InputQuery('Заливка вращение','Введите ось вращения (0 для оси слева, 1 для оси справа)',str) then Exit ;

    model.RotFillZone(zone,str='0') ;
    render.resetSelect() ;
    render.EmitRebuild3D() ;

  end;

  if keysconfig.isKeyMatch(KEY_DELETE_SELECT,Shift,Key) then begin
    if not render.isZoneSelection() then begin
      ShowMessage('Не выделены блоки') ;
      Exit ;
    end ;

    model.ClearZone(render.getZoneSelection()) ;
    render.EmitRebuild3D() ;
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

procedure TFormMain.NAddToPalClick(Sender: TObject);
begin
  if (comboTexs.ItemIndex<>-1) then
    if ComboPal.Items.IndexOf(comboTexs.Items[comboTexs.ItemIndex])=-1 then
      ComboPal.Items.Add(comboTexs.Items[comboTexs.ItemIndex]) ;
end;

procedure TFormMain.NBlockReportClick(Sender: TObject);
begin
  ShowMessage(model.buildBlockReport()) ;
end;

procedure TFormMain.NBridghtClick(Sender: TObject);
begin
  NBridght.Checked:=not NBridght.Checked ;
  render.UsePlanesBright:=NBridght.Checked ;
end;

procedure TFormMain.NDecartClick(Sender: TObject);
begin
  render.SwitchToDecart() ;
end;

procedure TFormMain.NEditPalClick(Sender: TObject);
begin
  with TFormEditPal.Create(Self) do begin
    setPal(ComboPal.Items) ;
    if ShowModal()=mrOK then
      ComboPal.Items.Assign(getPal());
    Free ;
  end;
end;

procedure TFormMain.NExitClick(Sender: TObject);
begin
  Close() ;
end;

procedure TFormMain.NGreenSideClick(Sender: TObject);
begin
  render.setSelectionMode(smGreenSide);
end;

procedure TFormMain.NLoadPalFromFileClick(Sender: TObject);
begin
  if OpenDialog2.Execute() then ComboPal.Items.LoadFromFile(OpenDialog2.FileName);
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

procedure TFormMain.NRedoClick(Sender: TObject);
begin
 if not model.Redo() then
    ShowMessage('Нет отмененных действий')
  else
    UpdateInfo() ;
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

procedure TFormMain.NSavePalToFileClick(Sender: TObject);
begin
  if comboPal.Items.Count=0 then begin
    ShowMessage('Палитра пуста') ;
    Exit ;
  end;

  if SaveDialog2.Execute() then comboPal.Items.SaveToFile(SaveDialog2.FileName);
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

procedure TFormMain.NSetKeysClick(Sender: TObject);
begin
  with TFormSetKeys.Create(Self) do begin
    setKeysConfig(keysconfig) ;
    if ShowModal()=mrOk then begin
      keysconfig.saveToFile() ;
      updateMenuKeys() ;
    end;
    Free ;
  end;
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

procedure TFormMain.NShowBorderClick(Sender: TObject);
begin
  NShowBorder.Checked:=not NShowBorder.Checked ;
  render.ShowBorders:=NShowBorder.Checked ;
end;

procedure TFormMain.NSliceClick(Sender: TObject);
var fm:TFormSliceOpt ;
begin
  fm:=TFormSliceOpt.Create(Self) ;
  if fm.ShowModal()=mrOK then begin
    with TModelExport.Create(model) do begin
      SaveLayers(fm.Dir(),fm.FileTpl(),fm.FormatExt(),fm.Axis(),
        fm.ShowGrid(),fm.ShowPriorLayer(),fm.GridWidth(),fm.LayerBr()) ;
      Free ;
    end;
    ShowMessage('Нарезка выполнена') ;
  end;
end;

procedure TFormMain.NSphereClick(Sender: TObject);
begin
  render.SwitchToSphere() ;
end;

procedure TFormMain.NTranspTexturesClick(Sender: TObject);
var ini:TIniFile ;
begin
  ini:=TIniFile.Create(AppDataPath+'\config.ini') ;
  with TFormTranspTexs.Create(Self) do begin
    setTexs(ini.ReadString('Transparent','Textures','')) ;
    if ShowModal()=mrOK then begin
      ini.WriteString('Transparent','Textures',getTexs());
      render.setTranspTexs(getTexs());
      render.EmitRebuild3D() ;
    end;
    Free ;
  end;
  ini.Free ;
end;

procedure TFormMain.NUndoClick(Sender: TObject);
begin
  if not model.Undo() then
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
  end
  else begin
    render.ClearPreBlocks() ;
    oldblockgid:='' ;
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
       setTextureIndex(comboTexs.Items.IndexOf(block.texcode))
     else
     if ActiveTool is TDrawToolSel then
       render.doSelect(block)
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
  LabR.Visible:=(ActiveTool is TDrawToolSmooth)or(ActiveTool is TDrawToolGrow) ;
  EdCubeN.Visible:=(ActiveTool is TDrawToolCubeCube)or
  (ActiveTool is TDrawToolSphere) or
  (ActiveTool is TDrawToolSmooth) or
  (ActiveTool is TDrawToolGrow) ;

  Label4.Visible:=(ActiveTool is TDrawToolPar) ;
  EdParX.Visible:=(ActiveTool is TDrawToolPar) ;
  EdParY.Visible:=(ActiveTool is TDrawToolPar) ;
  EdParZ.Visible:=(ActiveTool is TDrawToolPar) ;

  cbSetToEdge.Visible:=(ActiveTool is TDrawToolSphere) ;
  cbAutoTexGrow.Visible:=(ActiveTool is TDrawToolGrow) ;

  rbFullBlock.Visible:=(ActiveTool is TDrawToolCube) ;
  rbUpper.Visible:=(ActiveTool is TDrawToolCube) ;
  rbLower.Visible:=(ActiveTool is TDrawToolCube) ;
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
  Tool.tmpSetToEdge:=cbSetToEdge.Checked ;
  Tool.tmpAutoTexGrow:=cbAutoTexGrow.Checked ;
  if rbFullBlock.Checked then Tool.tmpBlockType:=btFull ;
  if rbUpper.Checked then Tool.tmpBlockType:=btUpper ;
  if rbLower.Checked then Tool.tmpBlockType:=btLower ;
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

procedure TFormMain.updateMenuKeys;
begin
  NDecart.Caption:='Свободное движение камеры (декартовы координаты) '+
    keysconfig.getKeyView(KEY_SWITCH_DECART) ;
  NSphere.Caption:='Вращение камеры вокруг точки (сферические координаты) '+
    keysconfig.getKeyView(KEY_SWITCH_SPHERE) ;
  N7.Caption:='Установить новый центр вращения '+
    keysconfig.getKeyView(KEY_SET_CENTERVIEW) ;
end;

end.
