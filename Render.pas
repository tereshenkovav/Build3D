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
     procedure DrawCube(block:TBlock) ;
     procedure DrawPreCube(block:TBlock) ;
     procedure SetCubePos(pos:TPoint3D) ;
     function isTekAtSideX(x:Double):Boolean ;
     function isTekAtSideY(y:Double):Boolean ;
     function isTekAtSideZ(z:Double):Boolean ;
     function GetEyePoint():TPoint3D ;
     function GetViewPoint():TPoint3D ;
     function block2point3d(block:TBlock):TPoint3D ;
  public
     NoSelection:Boolean ;
     constructor Create(Adc:HDC; Amodel:TModel) ;
     procedure SetupGL;
     function GetPosInfo():string ;
     function LoadRes(Texdir:string):TStringList ;
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
     procedure setSelectionMode(value:TSelectionMode) ;
     property BackColor:TColor read FBackColor write FBackColor ;
     procedure EmitRebuild3D() ;
     procedure SetCenterView(block:TBlock) ;
     procedure SetDefaultCenterView() ;
  end;

implementation
uses IOUtils, SysUtils,
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

  Eye.x:=20.0*sin(teta)*cos(fi) ;
  Eye.y:=20.0*cos(teta) ;
  Eye.z:=20.0*sin(teta)*sin(fi) ;

  NoSelection:=False ;
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

function TRender.getMovingMode: TMovingMode;
begin
  Result:=mmode ;
end;

procedure TRender.setSelectionMode(value: TSelectionMode);
begin
  smode:=value ;
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
    Result:=Result+' ����� ������ �������'
  else
    Result:=Result+' ��������� ��������' ;
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

procedure TRender.SetupGL;
begin
  glClearColor(0.0,0.0,0.0,0.0); // ���� ����
  glEnable(GL_DEPTH_TEST); //�������� ����� ���� �������
  glEnable(GL_CULL_FACE); //�������� ����� ����������� ������ �������� ������������
  glEnable(GL_TEXTURE_2D);
  glTexParameter (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameter (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
end;

const EPS = 0.1 ;

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

procedure TRender.DrawCube(block:TBlock);
var sel:Boolean ;
begin
   sel:=False ;

   sel:=isTekAtSideZ(1.0) ;
   glBegin(GL_QUADS);
    glNormal3f( 0.0, 0.0, 1.0);
    if sel and (smode=smGreenSide) then glColor3F(0.0,1.0,0.0) ;
    glTexCoord2f(0.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0,  1.0);
    glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  1.0,  1.0);
   glEnd;
   if sel then begin FisBlockOver:=True ; OverBlock:=block; OverBlockDir:=dirZgr ;
      glColor3F(1.0,1.0,1.0) ; Inc(Rcnt) ; end ;

   sel:=isTekAtSideZ(-1.0) ;
   glBegin(GL_QUADS);
    glNormal3f( 0.0, 0.0,-1.0);
    if sel and (smode=smGreenSide) then glColor3F(0.0,1.0,0.0) ;
    glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, -1.0, -1.0);
    glTexCoord2f(1.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);
    glTexCoord2f(0.0, 1.0); glVertex3f( 1.0,  1.0, -1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, -1.0, -1.0);
   glEnd;
   if sel then begin FisBlockOver:=True ; OverBlock:=block; OverBlockDir:=dirZle ;
      glColor3F(1.0,1.0,1.0) ; Inc(Rcnt) ; end ;

   sel:=isTekAtSideY(1.0)  ;
   glBegin(GL_QUADS);
    glNormal3f( 0.0, 1.0, 0.0);
    if sel and (smode=smGreenSide) then glColor3F(0.0,1.0,0.0) ;
    glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f(-1.0,  1.0,  1.0);
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.0,  1.0,  1.0);
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0, -1.0);
   glEnd();
   if sel then begin FisBlockOver:=True ; OverBlock:=block; OverBlockDir:=dirYgr ;
      glColor3F(1.0,1.0,1.0) ; Inc(Rcnt) ; end ;

   sel:=isTekAtSideY(-1.0) ;
   glBegin(GL_QUADS);
    glNormal3f( 0.0,-1.0, 0.0);
    if sel and (smode=smGreenSide) then glColor3F(0.0,1.0,0.0) ;
    glTexCoord2f(1.0, 1.0); glVertex3f(-1.0, -1.0, -1.0);
    glTexCoord2f(0.0, 1.0); glVertex3f( 1.0, -1.0, -1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);
    glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);
   glEnd();
   if sel then begin FisBlockOver:=True ; OverBlock:=block; OverBlockDir:=dirYle;
      glColor3F(1.0,1.0,1.0) ; Inc(Rcnt) ; end ;

   sel:=isTekAtSideX(1.0) ;
   glBegin(GL_QUADS);
    glNormal3f( 1.0, 0.0, 0.0);
    if sel and (smode=smGreenSide) then glColor3F(0.0,1.0,0.0) ;
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.0, -1.0, -1.0);
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0, -1.0);
    glTexCoord2f(0.0, 1.0); glVertex3f( 1.0,  1.0,  1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);
   glEnd();
   if sel then begin FisBlockOver:=True ; OverBlock:=block; OverBlockDir:=dirXgr ;
      glColor3F(1.0,1.0,1.0) ; Inc(Rcnt) ; end ;

   sel:=isTekAtSideX(-1.0) ;
   if sel and (smode=smGreenSide) then glColor3F(0.0,1.0,0.0) ;
   glBegin(GL_QUADS);
    glNormal3f(-1.0, 0.0, 0.0);
    glTexCoord2f(0.0, 0.0); glVertex3f(-1.0, -1.0, -1.0);
    glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);
    glTexCoord2f(1.0, 1.0); glVertex3f(-1.0,  1.0,  1.0);
    glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);
  glEnd();
   if sel then begin FisBlockOver:=True ; OverBlock:=block; OverBlockDir:=dirXle ;
      glColor3F(1.0,1.0,1.0) ; Inc(Rcnt) ; end ;

end;

procedure TRender.DrawPreCube(block:TBlock);
begin
   glBegin(GL_QUADS);
    glNormal3f( 0.0, 0.0, 1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0,  1.0);
    glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  1.0,  1.0);
   glEnd;

   glBegin(GL_QUADS);
    glNormal3f( 0.0, 0.0,-1.0);
    glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, -1.0, -1.0);
    glTexCoord2f(1.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);
    glTexCoord2f(0.0, 1.0); glVertex3f( 1.0,  1.0, -1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, -1.0, -1.0);
   glEnd;

   glBegin(GL_QUADS);
    glNormal3f( 0.0, 1.0, 0.0);
    glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f(-1.0,  1.0,  1.0);
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.0,  1.0,  1.0);
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0, -1.0);
   glEnd();

   glBegin(GL_QUADS);
    glNormal3f( 0.0,-1.0, 0.0);
    glTexCoord2f(1.0, 1.0); glVertex3f(-1.0, -1.0, -1.0);
    glTexCoord2f(0.0, 1.0); glVertex3f( 1.0, -1.0, -1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);
    glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);
   glEnd();

   glBegin(GL_QUADS);
    glNormal3f( 1.0, 0.0, 0.0);
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.0, -1.0, -1.0);
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0, -1.0);
    glTexCoord2f(0.0, 1.0); glVertex3f( 1.0,  1.0,  1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);
   glEnd();

   glBegin(GL_QUADS);
    glNormal3f(-1.0, 0.0, 0.0);
    glTexCoord2f(0.0, 0.0); glVertex3f(-1.0, -1.0, -1.0);
    glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);
    glTexCoord2f(1.0, 1.0); glVertex3f(-1.0,  1.0,  1.0);
    glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);
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
  NearClipping = 0.1;    //������� ��������� ���������
  FarClipping  = 200;  //������� ��������� ���������
var
  x,y,z: Integer;
  block:TBlock ;
  Eye,View:TPoint3D ;
  f:text ;
  r,g,b:Single ;
begin
  glViewport(0,0,width,height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(60.0,width/height,NearClipping,FarClipping);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  unRGBOpenGL(BackColor,r,g,b) ;
  glClearColor(r,g,b,1.0) ;

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
   glEnable(GL_BLEND);
   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   for block in preblocks do begin
      glPushMatrix() ;
      glBindTexture(GL_TEXTURE_2D, Texs[block.texcode]);
      //setCubePos(2*block.x,2*block.y,2*block.z);
      glTranslatef(2*block.x,2*block.y,2*block.z);
      DrawPreCube(block) ;
      glPopMatrix() ;
    end;
   glDisable(GL_BLEND);
    end;

    end;

  SwapBuffers(dc);
end;

end.