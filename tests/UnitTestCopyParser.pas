unit UnitTestCopyParser;

interface

uses
  DUnitX.TestFramework,
  CopyParser ;

type

  [TestFixture]
  Test_CopyParser = class(TObject)
  private
    copyparser:TCopyParser;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestNoMirror;
    [Test]
    procedure TestMirrors;
  end;

implementation
uses CommonClasses ;

{ Test_CopyParser }

procedure Test_CopyParser.Setup;
begin

end;

procedure Test_CopyParser.TearDown;
begin

end;

procedure Test_CopyParser.TestMirrors;
begin
  copyparser:=TCopyParser.Create('-1 2 -3 x');
  Assert.IsTrue(copyparser.isCommandOk());
  Assert.AreEqual(copyparser.getTarget().x,-1);
  Assert.AreEqual(copyparser.getTarget().y,2);
  Assert.AreEqual(copyparser.getTarget().z,-3);
  Assert.IsTrue(copyparser.getMirrors()=[axisX]);
  copyparser.Free ;

  copyparser:=TCopyParser.Create('-1 2 -3 xy');
  Assert.IsTrue(copyparser.isCommandOk());
  Assert.AreEqual(copyparser.getTarget().x,-1);
  Assert.AreEqual(copyparser.getTarget().y,2);
  Assert.AreEqual(copyparser.getTarget().z,-3);
  Assert.IsTrue(copyparser.getMirrors()=[axisX,axisY]);
  copyparser.Free ;

  copyparser:=TCopyParser.Create('-1 2 -3 xyz');
  Assert.IsTrue(copyparser.isCommandOk());
  Assert.AreEqual(copyparser.getTarget().x,-1);
  Assert.AreEqual(copyparser.getTarget().y,2);
  Assert.AreEqual(copyparser.getTarget().z,-3);
  Assert.IsTrue(copyparser.getMirrors()=[axisX,axisY,axisZ]);
  copyparser.Free ;

end;

procedure Test_CopyParser.TestNoMirror;
begin
  copyparser:=TCopyParser.Create(' ');
  Assert.IsFalse(copyparser.isCommandOk());

  copyparser:=TCopyParser.Create('1');
  Assert.IsFalse(copyparser.isCommandOk());

  copyparser:=TCopyParser.Create('1 -2');
  Assert.IsFalse(copyparser.isCommandOk());

  copyparser:=TCopyParser.Create('1 -2 +3');
  Assert.IsTrue(copyparser.isCommandOk());
  Assert.AreEqual(copyparser.getTarget().x,1);
  Assert.AreEqual(copyparser.getTarget().y,-2);
  Assert.AreEqual(copyparser.getTarget().z,3);
  Assert.IsTrue(copyparser.getMirrors()=[]);
  copyparser.Free ;
end;

end.
