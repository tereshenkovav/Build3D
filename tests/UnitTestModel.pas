unit UnitTestModel;

interface
uses
  DUnitX.TestFramework,
  Model ;

type

  [TestFixture]
  Test_Model = class(TObject)
  private
    model:TModel ;
    function isBlockAt(x, y, z: Integer): Boolean;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestUndoRedo1;
    [Test]
    procedure TestUndoRedo2;
    [Test]
    procedure TestUndoRedoNewBranch;
    [Test]
    procedure TestUndoRedoClearing;
  end;

implementation
uses CommonClasses, BlockListHelper, Constants ;

procedure Test_Model.Setup;
begin
  model:=TModel.Create ;
  model.AddBlock(1,2,3,'default') ;
end;

procedure Test_Model.TearDown;
begin
  model.Free ;
end;

function Test_Model.isBlockAt(x, y, z: Integer): Boolean;
var tmp:TBlock ;
begin
  Result:=model.getBlocks().Find(function(b:TBlock):Boolean
  begin
    Result:=(b.x=x)and(b.y=y)and(b.z=z) ;
  end,tmp) ;
end ;

procedure Test_Model.TestUndoRedo1;
var r:Boolean ;
begin
  model.PushBlocks() ;
  model.AddBlock(1,2,4,'default') ;
  Assert.AreEqual(2,model.getBlocks().Count) ;
  Assert.IsTrue(isBlockAt(1,2,3));
  Assert.IsTrue(isBlockAt(1,2,4));

  r:=model.Undo() ;
  Assert.IsTrue(r);
  Assert.AreEqual(1,model.getBlocks().Count) ;
  Assert.IsTrue(isBlockAt(1,2,3));
  Assert.IsFalse(isBlockAt(1,2,4));

  r:=model.Undo() ;
  Assert.IsFalse(r);

  r:=model.Redo() ;
  Assert.IsTrue(r);
  Assert.AreEqual(2,model.getBlocks().Count) ;
  Assert.IsTrue(isBlockAt(1,2,3));
  Assert.IsTrue(isBlockAt(1,2,4));

  r:=model.Redo() ;
  Assert.IsFalse(r);
end;

procedure Test_Model.TestUndoRedo2;
var r:Boolean ;
begin
  model.PushBlocks() ;
  model.AddBlock(1,2,4,'default') ;
  model.PushBlocks() ;
  model.AddBlock(1,2,5,'default') ;
  Assert.AreEqual(3,model.getBlocks().Count) ;
  Assert.IsTrue(isBlockAt(1,2,3));
  Assert.IsTrue(isBlockAt(1,2,4));
  Assert.IsTrue(isBlockAt(1,2,5));

  r:=model.Undo() ;
  Assert.IsTrue(r);
  Assert.AreEqual(2,model.getBlocks().Count) ;
  Assert.IsTrue(isBlockAt(1,2,3));
  Assert.IsTrue(isBlockAt(1,2,4));
  Assert.IsFalse(isBlockAt(1,2,5));

  r:=model.Undo() ;
  Assert.IsTrue(r);
  Assert.AreEqual(1,model.getBlocks().Count) ;
  Assert.IsTrue(isBlockAt(1,2,3));
  Assert.IsFalse(isBlockAt(1,2,4));
  Assert.IsFalse(isBlockAt(1,2,5));

  r:=model.Undo() ;
  Assert.IsFalse(r);

  r:=model.Redo() ;
  Assert.IsTrue(r);
  Assert.AreEqual(2,model.getBlocks().Count) ;
  Assert.IsTrue(isBlockAt(1,2,3));
  Assert.IsTrue(isBlockAt(1,2,4));
  Assert.IsFalse(isBlockAt(1,2,5));

  r:=model.Redo() ;
  Assert.IsTrue(r);
  Assert.AreEqual(3,model.getBlocks().Count) ;
  Assert.IsTrue(isBlockAt(1,2,3));
  Assert.IsTrue(isBlockAt(1,2,4));
  Assert.IsTrue(isBlockAt(1,2,5));

  r:=model.Redo() ;
  Assert.IsFalse(r);

end;

procedure Test_Model.TestUndoRedoNewBranch;
var r:Boolean ;
begin
  model.PushBlocks() ;
  model.AddBlock(1,2,4,'default') ;
  model.PushBlocks() ;
  model.AddBlock(1,2,5,'default') ;
  Assert.AreEqual(3,model.getBlocks().Count) ;
  Assert.IsTrue(isBlockAt(1,2,3));
  Assert.IsTrue(isBlockAt(1,2,4));
  Assert.IsTrue(isBlockAt(1,2,5));

  r:=model.Undo() ;
  Assert.IsTrue(r);
  Assert.AreEqual(2,model.getBlocks().Count) ;
  Assert.IsTrue(isBlockAt(1,2,3));
  Assert.IsTrue(isBlockAt(1,2,4));
  Assert.IsFalse(isBlockAt(1,2,5));

  // Добавление в середине очереди
  model.PushBlocks() ;
  model.AddBlock(1,2,6,'default') ;
  Assert.AreEqual(3,model.getBlocks().Count) ;
  Assert.IsTrue(isBlockAt(1,2,3));
  Assert.IsTrue(isBlockAt(1,2,4));
  Assert.IsFalse(isBlockAt(1,2,5));
  Assert.IsTrue(isBlockAt(1,2,6));

  r:=model.Redo() ;
  Assert.IsFalse(r);

  r:=model.Undo() ;
  Assert.IsTrue(r);
  Assert.AreEqual(2,model.getBlocks().Count) ;
  Assert.IsTrue(isBlockAt(1,2,3));
  Assert.IsTrue(isBlockAt(1,2,4));
  Assert.IsFalse(isBlockAt(1,2,5));
  Assert.IsFalse(isBlockAt(1,2,6));

  r:=model.Redo() ;
  Assert.IsTrue(r);
  Assert.AreEqual(3,model.getBlocks().Count) ;
  Assert.IsTrue(isBlockAt(1,2,3));
  Assert.IsTrue(isBlockAt(1,2,4));
  Assert.IsFalse(isBlockAt(1,2,5));
  Assert.IsTrue(isBlockAt(1,2,6));

  r:=model.Redo() ;
  Assert.IsFalse(r);

end;

procedure Test_Model.TestUndoRedoClearing;
var r:Boolean ;
    i:Integer ;
begin
  for i := 0 to 100 do begin
    model.PushBlocks() ;
    model.AddBlock(1,2,4+i,'default') ;
  end;

  for i := 1 to MAX_HISTORY_SIZE-1 do begin
    r:=model.Undo() ;
    Assert.IsTrue(r);
  end;

  r:=model.Undo() ;
  Assert.IsFalse(r);

end ;

initialization
  TDUnitX.RegisterTestFixture(Test_Model);

end.
