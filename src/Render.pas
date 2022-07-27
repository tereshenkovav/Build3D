unit Render;

interface
uses Windows, OpenGL, Generics.Collections, Classes, Graphics,
  dglOpenGL,
  Model, CommonClasses ;

type
  TRender = class
  private
     dc:HDC ;
     model:TModel ;
     Rsphere:Double ;
     Teta:Double ;
     Fi:Double ;
     Texs:TDictionary<string,UINT> ;
     TekX,TekY,TekZ:Double ;
     PosX,PosY,PosZ:Double ;
     OverBlock:TBlock ;
     OverBlockDir:TBlockDir ;
     FisBlockOver:Boolean ;
     Eye:TPoint3D ;
     mmode:TMovingMode ;
     smode:TSelectionMode ;
     preblocks:TList<TBlock> ;
     renderedpreblocks:Boolean ;
     D2X:Integer ;
     D2Y:Integer ;
     FBackColor:TColor ;
     Rebuild3DPos:Boolean ;
     CenterView:TPoint3D ;
     rback,gback,bback:Single ;
     bhm,bhp:GLfloat ;
     texp0,texp1:GLfloat ;
     selectionstate:TSelectionState ;
     FirstSelection:TPoint3I ;
     LastSelection:TPoint3I ;
     transptexs:TStringList ;
     procedure DrawCube(block:TBlock) ;
     procedure DrawPreCube(block:TBlock) ;
     procedure SetCubePos(pos:TPoint3D) ;
     function isTekAtSideX(x:Double):Boolean ;
     function isTekAtSideY(y:Double):Boolean ;
     function isTekAtSideZ(z:Double):Boolean ;
     function GetEyePoint():TPoint3D ;
     function GetViewPoint():TPoint3D ;
     function block2point3d(block:TBlock):TPoint3D ;
     procedure setCubeVarsByBlock(block:TBlock) ;
     function isBlockInSelection(const b:TBlock):Boolean ;
  public
     NoSelection:Boolean ;
     ShowBorders:Boolean ;
     UsePlanesBright:Boolean ;
     constructor Create(Adc:HDC; Amodel:TModel) ;
     procedure SetupGL;
     function GetPosInfo():string ;
     function LoadRes(Texdir:string):TStringList ;
     function LoadTexIcons(Texdir:string; w,h:Integer):TDictionary<string,TBitmap>;
     procedure Render(width,height:Integer);
     procedure MoveByR(dr:Double) ;
     procedure MoveByTeta(da:Double) ;
     procedure MoveByFi(da:Double) ;
     procedure StrafeHorz(dr:Double) ;
     procedure StrafeVert(dr:Double) ;
     procedure Get3DPos( WindowX, WindowY: Integer; var ReturnX, ReturnY, ReturnZ: GLDouble );
     procedure SetMouse( X, Y:Integer) ;
     function isBlockOver(out block:TBlock; out dir:TBlockDir):Boolean ;
     procedure SwitchToDecart() ;
     procedure SwitchToSphere() ;
     function getMovingMode():TMovingMode ;
     procedure SetPreBlocks(list:TList<TBlock>) ;
     procedure ClearPreBlocks() ;
     procedure setSelectionMode(value:TSelectionMode) ;
     property BackColor:TColor read FBackColor write FBackColor ;
     procedure EmitRebuild3D() ;
     procedure SetCenterView(block:TBlock) ;
     procedure SetDefaultCenterView() ;
     procedure doSelect(const b:TBlock) ;
     procedure resetSelect() ;
     function getZoneSelection():TZone3I;
     function isZoneSelection():Boolean ;
     procedure setTranspTexs(texs:string) ;
  end;

implementation
uses IOUtils, SysUtils, Math,
  Textures,
  CommonProc, Monitor ;

function TRender.block2point3d(block: TBlock): TPoint3D;
begin
  Result.x:=2*block.x ;
  Result.y:=2*block.y ;
  Result.z:=2*block.z ;
end;

constructor TRender.Create(Adc: HDC; Amodel:TModel);
begin
  dc:=Adc ;
  mmode:=mmDecart ;
  model:=Amodel ;
  Rsphere:=20 ;
  Teta:=PI/2 ;
  Fi:=0 ;
  Texs:=TDictionary<string,UINT>.Create() ;
  preblocks:=TList<TBlock>.Create() ;
  smode:=smPreview ;
  FBackColor:=clBlack ;
  Rebuild3DPos:=False ;
  SetDefaultCenterView() ;
  transptexs:=TStringList.Create ;

  Eye.x:=20.0*sin(teta)*cos(fi) ;
  Eye.y:=20.0*cos(teta) ;
  Eye.z:=20.0*sin(teta)*sin(fi) ;

  NoSelection:=False ;
  selectionstate:=stNone ;
end;

procedure TRender.doSelect(const b: TBlock);
begin
  if selectionstate=stNone then begin
    selectionstate:=stFirstBlock ;
    firstselection.setFromBlock(b) ;
  end
  else
  if selectionstate=stFirstBlock then begin
    selectionstate:=stLastBlock ;
    lastselection.setFromBlock(b) ;
  end
  else
  if selectionstate=stLastBlock then begin
    selectionstate:=stFirstBlock ;
    firstselection.setFromBlock(b) ;
  end ;
end;

procedure TRender.Get3DPos( WindowX, WindowY: Integer; var ReturnX, ReturnY, ReturnZ: GLDouble );
var
  viewport:TVector4i;
  model,proj:TGLMatrixd4;
  RealY: GLint;
  ZVal: GLfloat;
begin
  glGetIntegerv( GL_VIEWPORT, @viewport );
  glGetDoublev( GL_MODELVIEW_MATRIX, @model );
  glGetDoublev( GL_PROJECTION_MATRIX, @proj );
  RealY := Viewport[3] - WindowY - 1;
  glReadPixels( WindowX, RealY, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, @ZVal );
  gluUnProject( WindowX, RealY, ZVal, model, proj, viewport, @ReturnX, @ReturnY, @ReturnZ );
  //ReturnX:=0 ;
  //ReturnY:=0 ;
  //ReturnZ:=0 ;
end;

function TRender.getZoneSelection():TZone3I;
begin
  case selectionstate of
    stNone: begin
      Result.x1:=0 ; Result.y1:=0 ; Result.z1:=0 ;
      Result.x2:=-1 ; Result.y2:=-1 ; Result.z2:=-1 ;
    end;
    stFirstBlock: begin
      Result.x1:=FirstSelection.x ; Result.y1:=FirstSelection.y ; Result.z1:=FirstSelection.z ;
      Result.x1:=FirstSelection.x ; Result.y1:=FirstSelection.y ; Result.z1:=FirstSelection.z ;
    end ;
    stLastBlock: begin
      Result.x1:=Min(FirstSelection.x,LastSelection.x) ;
      Result.x2:=Max(FirstSelection.x,LastSelection.x) ;
      Result.y1:=Min(FirstSelection.y,LastSelection.y) ;
      Result.y2:=Max(FirstSelection.y,LastSelection.y) ;
      Result.z1:=Min(FirstSelection.z,LastSelection.z) ;
      Result.z2:=Max(FirstSelection.z,LastSelection.z) ;
    end ;
  end;

end;

function TRender.getMovingMode: TMovingMode;
begin
  Result:=mmode ;
end;

procedure TRender.setSelectionMode(value: TSelectionMode);
begin
  smode:=value ;
end;

procedure TRender.setTranspTexs(texs: string);
begin
  transptexs.CommaText:=texs ;
end;

function TRender.GetEyePoint: TPoint3D;
begin
  if mmode=mmSphere then begin
    Result.x:=CenterView.x+Rsphere*sin(teta)*cos(fi) ;
    Result.y:=CenterView.y+Rsphere*cos(teta) ;
    Result.z:=CenterView.z+Rsphere*sin(teta)*sin(fi) ;
  end
  else
    Result:=Eye ;
end;

function TRender.GetViewPoint: TPoint3D;
begin
  if mmode=mmSphere then begin
    Result.x:=CenterView.x ;
    Result.y:=CenterView.y ;
    Result.z:=CenterView.z ;
  end
  else begin
    Result.x:=Eye.x-1.0*sin(teta)*cos(fi) ;
    Result.y:=Eye.y-1.0*cos(teta) ;
    Result.z:=Eye.z-1.0*sin(teta)*sin(fi) ;
  end;
end;

function TRender.GetPosInfo: string;
var eye:TPoint3D ;
begin
  eye:=GetEyePoint() ;
 // Result:=Format('R=%f Fi=%f Teta=%f',[R,Fi,Teta]) ;
  Result:=Format('x=%f y=%f z=%f Fi=%f Teta=%f',[eye.x,eye.y,eye.z,fi,Teta]) ;
  if mmode=mmSphere then
    Result:=Result+' полет вокруг объекта'
  else
    Result:=Result+' свободное движение' ;
end;

procedure TRender.SetCenterView(block: TBlock);
begin
  CenterView:=block2point3d(block) ;
end;

procedure TRender.SetCubePos(pos:TPoint3D);
begin
  glTranslatef(pos.x,pos.y,pos.z);
  PosX:=pos.x ; posY:=pos.y ; PosZ:=pos.z ;
end;

procedure TRender.setCubeVarsByBlock(block: TBlock);
begin
  if block.bt=btFull then begin
    bhm:=-1.0 ; bhp:=1.0 ;
    texp0:=0.0 ; texp1:=1.0 ;
  end
  else
  if block.bt=btUpper then begin
    bhm:=0.0 ; bhp:=1.0 ;
    texp0:=0.5 ; texp1:=1.0 ;
  end
  else begin
    bhm:=-1.0 ; bhp:=0.0 ;
    texp0:=0.0 ; texp1:=0.5 ;
  end;
end;

procedure TRender.SetDefaultCenterView;
begin
  CenterView.x:=0 ;
  CenterView.y:=0 ;
  CenterView.z:=0 ;
end;

procedure TRender.SetMouse(X,Y: Integer);
begin
  D2X:=X ;
  D2Y:=Y ;
  Rebuild3DPos:=True ;
  //Get3DPos(X,Y,TekX,TekY,TekZ) ;
end;

procedure TRender.SetPreBlocks(list: TList<TBlock>);
begin
  preblocks.Clear ;
  preblocks.AddRange(list);
end;

procedure TRender.ClearPreBlocks();
begin
  preblocks.Clear ;
end;

procedure TRender.SetupGL;
begin
  glClearColor(0.0,0.0,0.0,0.0); // цвет фона
  glEnable(GL_DEPTH_TEST); //включить режим тест глубины
  glEnable(GL_CULL_FACE); //включить режим отображени€ только передних поверхностей
  glEnable(GL_TEXTURE_2D);
  glTexParameter (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameter (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
end;

const EPS = 0.1 ;

function TRender.isBlockInSelection(const b: TBlock): Boolean;
begin
  case selectionstate of
    stNone: Result:=False ;
    stFirstBlock: Result:=(b.x=FirstSelection.x)and
      (b.y=FirstSelection.y)and(b.z=FirstSelection.z) ;
    stLastBlock: Result:=isValueBetween(b.x,FirstSelection.x,LastSelection.x)and
      isValueBetween(b.y,FirstSelection.y,LastSelection.y)and
      isValueBetween(b.z,FirstSelection.z,LastSelection.z) ;
    else
      Result:=False ;
  end;
end;

function TRender.isBlockOver(out block:TBlock; out dir: TBlockDir): Boolean;
begin
  Result:=FisBlockOver ;
  block:=OverBlock ;
  dir:=OverBlockDir ;
end;

var rcnt:Integer ;

function TRender.isTekAtSideX(x:Double):Boolean ;
begin
  Result:=(Abs(PosX+x-TekX)<EPS)and(Abs(PosY-TekY)<1-EPS)and(Abs(PosZ-TekZ)<1-EPS) ;
end;

function TRender.isTekAtSideY(y:Double):Boolean ;
begin
  Result:=(Abs(PosY+y-TekY)<EPS)and(Abs(PosX-TekX)<1-EPS)and(Abs(PosZ-TekZ)<1-EPS) ;
end;

function TRender.isTekAtSideZ(z:Double):Boolean ;
begin
  Result:=(Abs(PosZ+z-TekZ)<EPS)and(Abs(PosY-TekY)<1-EPS)and(Abs(PosX-TekX)<1-EPS) ;
end;

function TRender.isZoneSelection: Boolean;
begin
  Result:=selectionstate<>stNone ;
end;

const POS:array[0..1] of GLfloat = (-1.0,1.0);

procedure TRender.DrawCube(block:TBlock);
var sel:Boolean ;
    i1,i2:Integer ;
    POSy:array[0..1] of GLfloat ;
    alpha:GLfloat ;
begin
   sel:=False ;

   setCubeVarsByBlock(block) ;

   if (transptexs.IndexOf(block.texcode)<>-1) then alpha:=0.5 else alpha:=1.0 ;

   glColor4F(1.0,1.0,1.0,alpha) ;

   if UsePlanesBright then glColor4F(0.80,0.80,0.80,alpha) ;

   if isBlockInSelection(block) then glColor4F(0.0,0.0,1.0,1.0) ;

   sel:=isTekAtSideZ(1.0) ;
   if sel and (smode=smGreenSide) then glColor4F(0.0,1.0,0.0,1.0) ;
   if not model.isPlaneSkiped(block,dirZgr) then begin
   glBegin(GL_QUADS);
    glNormal3f( 0.0, 0.0, 1.0);
    glTexCoord2f(0.0, texp0); glVertex3f(-1.0, bhm,  1.0);
    glTexCoord2f(1.0, texp0); glVertex3f( 1.0, bhm,  1.0);
    glTexCoord2f(1.0, texp1); glVertex3f( 1.0,  bhp,  1.0);
    glTexCoord2f(0.0, texp1); glVertex3f(-1.0,  bhp,  1.0);
   glEnd;
   end;
   if sel then begin FisBlockOver:=True ; OverBlock:=block; OverBlockDir:=dirZgr ;
      glColor4F(1.0,1.0,1.0,alpha) ; Inc(Rcnt) ; end ;

   sel:=isTekAtSideZ(-1.0) ;
   if sel and (smode=smGreenSide) then glColor4F(0.0,1.0,0.0,1.0) ;
   if not model.isPlaneSkiped(block,dirZle) then begin
   glBegin(GL_QUADS);
    glNormal3f( 0.0, 0.0,-1.0);
    glTexCoord2f(1.0, texp0); glVertex3f(-1.0, bhm, -1.0);
    glTexCoord2f(1.0, texp1); glVertex3f(-1.0,  bhp, -1.0);
    glTexCoord2f(0.0, texp1); glVertex3f( 1.0,  bhp, -1.0);
    glTexCoord2f(0.0, texp0); glVertex3f( 1.0, bhm, -1.0);
   glEnd;
   end;
   if sel then begin FisBlockOver:=True ; OverBlock:=block; OverBlockDir:=dirZle ;
      glColor4F(1.0,1.0,1.0,alpha) ; Inc(Rcnt) ; end ;

   if UsePlanesBright then glColor4F(1.0,1.0,1.0,alpha) ;

   sel:=isTekAtSideY(1.0)  ;
   if sel and (smode=smGreenSide) then glColor4F(0.0,1.0,0.0,1.0) ;
   if not model.isPlaneSkiped(block,dirYgr) then begin
   glBegin(GL_QUADS);
    glNormal3f( 0.0, 1.0, 0.0);
    glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  bhp, -1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f(-1.0,  bhp,  1.0);
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.0,  bhp,  1.0);
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  bhp, -1.0);
   glEnd();
   end;
   if sel then begin FisBlockOver:=True ; OverBlock:=block; OverBlockDir:=dirYgr ;
      glColor4F(1.0,1.0,1.0,alpha) ; Inc(Rcnt) ; end ;

   sel:=isTekAtSideY(-1.0) ;
   if sel and (smode=smGreenSide) then glColor4F(0.0,1.0,0.0,1.0) ;
   if not model.isPlaneSkiped(block,dirYle) then begin
   glBegin(GL_QUADS);
    glNormal3f( 0.0,-1.0, 0.0);
    glTexCoord2f(1.0, 1.0); glVertex3f(-1.0, bhm, -1.0);
    glTexCoord2f(0.0, 1.0); glVertex3f( 1.0, bhm, -1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, bhm,  1.0);
    glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, bhm,  1.0);
   glEnd();
   end;
   if sel then begin FisBlockOver:=True ; OverBlock:=block; OverBlockDir:=dirYle;
      glColor4F(1.0,1.0,1.0,alpha) ; Inc(Rcnt) ; end ;

   if UsePlanesBright then glColor4F(0.6,0.6,0.6,alpha) ;

   sel:=isTekAtSideX(1.0) ;
   if sel and (smode=smGreenSide) then glColor4F(0.0,1.0,0.0,1.0) ;
   if not model.isPlaneSkiped(block,dirXgr) then begin
   glBegin(GL_QUADS);
    glNormal3f( 1.0, 0.0, 0.0);
    glTexCoord2f(1.0, texp0); glVertex3f( 1.0, bhm, -1.0);
    glTexCoord2f(1.0, texp1); glVertex3f( 1.0,  bhp, -1.0);
    glTexCoord2f(0.0, texp1); glVertex3f( 1.0,  bhp,  1.0);
    glTexCoord2f(0.0, texp0); glVertex3f( 1.0, bhm,  1.0);
   glEnd();
   end;
   if sel then begin FisBlockOver:=True ; OverBlock:=block; OverBlockDir:=dirXgr ;
      glColor4F(1.0,1.0,1.0,alpha) ; Inc(Rcnt) ; end ;

   sel:=isTekAtSideX(-1.0) ;
   if sel and (smode=smGreenSide) then glColor4F(0.0,1.0,0.0,1.0) ;
   if not model.isPlaneSkiped(block,dirXle) then begin
   glBegin(GL_QUADS);
    glNormal3f(-1.0, 0.0, 0.0);
    glTexCoord2f(0.0, texp0); glVertex3f(-1.0, bhm, -1.0);
    glTexCoord2f(1.0, texp0); glVertex3f(-1.0, bhm,  1.0);
    glTexCoord2f(1.0, texp1); glVertex3f(-1.0,  bhp,  1.0);
    glTexCoord2f(0.0, texp1); glVertex3f(-1.0,  bhp, -1.0);
  glEnd();
   end;
   if sel then begin FisBlockOver:=True ; OverBlock:=block; OverBlockDir:=dirXle ;
      glColor4F(1.0,1.0,1.0,alpha) ; Inc(Rcnt) ; end ;

   if ShowBorders then begin
   POSy[0]:=bhm ; POSy[1]:=bhp ;
   glColor3d(rback,gback,bback);
   glLineWidth(2.0);
   for i1 := 0 to 1 do
     for i2 := 0 to 1 do begin
       glBegin(GL_LINES);
       glVertex3f(-1.0, POSy[i1],  POS[i2]);
       glVertex3f( 1.0, POSy[i1],  POS[i2]);
       glEnd();
       glBegin(GL_LINES);
       glVertex3f(POS[i1], bhm, POS[i2]);
       glVertex3f(POS[i1], bhp, POS[i2]);
       glEnd();
       glBegin(GL_LINES);
       glVertex3f(POS[i1], POSy[i2], -1.0);
       glVertex3f(POS[i1], POSy[i2], 1.0);
       glEnd();
     end;
   glColor4f(1.0,1.0,1.0,1.0);
   end;

end;

procedure TRender.DrawPreCube(block:TBlock);
begin
   setCubeVarsByBlock(block) ;

   glBegin(GL_QUADS);
    glNormal3f( 0.0, 0.0, 1.0);
    glTexCoord2f(0.0, texp0); glVertex3f(-1.0, bhm,  1.0);
    glTexCoord2f(1.0, texp0); glVertex3f( 1.0, bhm,  1.0);
    glTexCoord2f(1.0, texp1); glVertex3f( 1.0,  bhp,  1.0);
    glTexCoord2f(0.0, texp1); glVertex3f(-1.0,  bhp,  1.0);
   glEnd;

   glBegin(GL_QUADS);
    glNormal3f( 0.0, 0.0,-1.0);
    glTexCoord2f(1.0, texp0); glVertex3f(-1.0, bhm, -1.0);
    glTexCoord2f(1.0, texp1); glVertex3f(-1.0,  bhp, -1.0);
    glTexCoord2f(0.0, texp1); glVertex3f( 1.0,  bhp, -1.0);
    glTexCoord2f(0.0, texp0); glVertex3f( 1.0, bhm, -1.0);
   glEnd;

   glBegin(GL_QUADS);
    glNormal3f( 0.0, 1.0, 0.0);
    glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  bhp, -1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f(-1.0,  bhp,  1.0);
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.0,  bhp,  1.0);
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  bhp, -1.0);
   glEnd();

   glBegin(GL_QUADS);
    glNormal3f( 0.0,-1.0, 0.0);
    glTexCoord2f(1.0, 1.0); glVertex3f(-1.0, bhm, -1.0);
    glTexCoord2f(0.0, 1.0); glVertex3f( 1.0, bhm, -1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, bhm,  1.0);
    glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, bhm,  1.0);
   glEnd();

   glBegin(GL_QUADS);
    glNormal3f( 1.0, 0.0, 0.0);
    glTexCoord2f(1.0, texp0); glVertex3f( 1.0, bhm, -1.0);
    glTexCoord2f(1.0, texp1); glVertex3f( 1.0,  bhp, -1.0);
    glTexCoord2f(0.0, texp1); glVertex3f( 1.0,  bhp,  1.0);
    glTexCoord2f(0.0, texp0); glVertex3f( 1.0, bhm,  1.0);
   glEnd();

   glBegin(GL_QUADS);
    glNormal3f(-1.0, 0.0, 0.0);
    glTexCoord2f(0.0, texp0); glVertex3f(-1.0, bhm, -1.0);
    glTexCoord2f(1.0, texp0); glVertex3f(-1.0, bhm,  1.0);
    glTexCoord2f(1.0, texp1); glVertex3f(-1.0,  bhp,  1.0);
    glTexCoord2f(0.0, texp1); glVertex3f(-1.0,  bhp, -1.0);
   glEnd();
end;


procedure TRender.EmitRebuild3D;
begin
  Rebuild3DPos:=True ;
end;

function TRender.LoadRes(Texdir:string):TStringList;
var tex:UINT ;
    filename,texcode:string ;
begin
  Result:=TStringList.Create ;
  for filename in TDirectory.GetFiles(Texdir) do begin
    LoadTexture(filename,Tex,false);
    texcode:=TPath.GetFileName(filename).ToLower ;
    texs.Add(texcode,Tex);
    Result.Add(texcode) ;
  end;
end;

function TRender.LoadTexIcons(Texdir:string; w,h:Integer):TDictionary<string,TBitmap>;
var filename,texcode:string ;
    bmp:TBitmap ;
    picture:TPicture;
begin
  Result:=TDictionary<string,TBitmap>.Create() ;
  for filename in TDirectory.GetFiles(Texdir) do begin
    texcode:=TPath.GetFileName(filename).ToLower ;

    picture:=TPicture.Create() ;
    picture.LoadFromFile(filename);
    bmp:=TBitmap.Create ;
    bmp.SetSize(w,h);
    bmp.Canvas.StretchDraw(Rect(0,0,w,h),picture.Graphic);
    picture.Free ;

    Result.Add(texcode,bmp);
  end;
end;

procedure TRender.MoveByR(dr: Double);
begin
  if mmode=mmSphere then
    Rsphere:=Rsphere+dr
  else begin
    Eye.x:=Eye.x-dr*sin(teta)*cos(fi) ;
    Eye.y:=Eye.y-dr*cos(teta) ;
    Eye.z:=Eye.z-dr*sin(teta)*sin(fi) ;
  end;
end;

procedure TRender.StrafeHorz(dr: Double);
begin
end;

procedure TRender.StrafeVert(dr: Double);
begin
end;

procedure TRender.SwitchToDecart;
begin
  if mmode=mmDecart then Exit ;

  mmode:=mmDecart ;
end;

procedure TRender.SwitchToSphere;
begin
  if mmode=mmSphere then Exit ;

  mmode:=mmSphere ;
  Rsphere:=20 ;
  Teta:=PI/2 ;
  Fi:=0 ;
end;

procedure TRender.MoveByTeta(da: Double);
begin
  teta:=teta-da ;
  if teta<0 then teta:=PI/2-0.1*15 ;
  if teta>PI then teta:=PI/2+0.1*15 ;
end;

procedure TRender.MoveByFi(da: Double);
begin
  fi:=fi+da ;
end;

procedure TRender.Render(width,height:Integer);
const
  NearClipping = 0.1;    //Ѕлижн€€ плоскость отсечени€
  FarClipping  = 10000;  //ƒальн€€ плоскость отсечени€
var
  x,y,z: Integer;
  block:TBlock ;
  Eye,View:TPoint3D ;
  f:text ;
begin
  glViewport(0,0,width,height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(60.0,width/height,NearClipping,FarClipping);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  unRGBOpenGL(BackColor,rback,gback,bback) ;
  glClearColor(rback,gback,bback,1.0) ;

  Eye:=GetEyePoint() ;
  View:=GetViewPoint() ;
  gluLookAt(Eye.x,Eye.y,Eye.z,View.x,View.y,View.z,0.0,1.0,0.0) ;

  {
  x:=2 ;
  y:=2 ;
  z:=2 ;
  glPushMatrix() ;
      setCubePos(-2*x,-2*y,2*z);
      DrawCube() ;
      glPopMatrix() ;
  }
   glColor4F(1.0,1.0,1.0,1.0) ;

   Rcnt:=0 ;
   FIsBlockOver:=False ;
   for block in model.getBlocks() do
   if not model.isBlockSkiped(block) then begin
      glPushMatrix() ;
      glBindTexture(GL_TEXTURE_2D, Texs[block.texcode]);
      //glTranslatef(-2*x+4,-2*y+4,2*z);
      setCubePos(block2point3d(block));
      DrawCube(block) ;
      glPopMatrix() ;
    end;

    getMonitor().SetVar('Cnt',Rcnt) ;

    if not NoSelection then begin


    if Rebuild3DPos then begin
      Get3DPos(D2X,D2Y,TekX,TekY,TekZ) ;
      Rebuild3DPos:=False ;
    end;

    if (smode=smPreview) then begin
   glColor4F(1.0,1.0,1.0,0.5) ;
   for block in preblocks do begin
      glPushMatrix() ;
      glBindTexture(GL_TEXTURE_2D, Texs[block.texcode]);
      //setCubePos(2*block.x,2*block.y,2*block.z);
      glTranslatef(2*block.x,2*block.y,2*block.z);
      DrawPreCube(block) ;
      glPopMatrix() ;
    end;

    end;

    end;

  glDisable(GL_BLEND);

  SwapBuffers(dc);
end;

procedure TRender.resetSelect;
begin
  selectionstate:=stNone ;
end;

end.
