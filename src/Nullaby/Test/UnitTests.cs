using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using TestHelper;
using Nullaby;

namespace Test
{
    [TestClass]
    public class UnitTest : CodeFixVerifier
    {
        [TestMethod]
        public void TestDereferenceOnKnownNull()
        {
            var code =
@"
public class C
{
  private string x;

  public void M()
  {
     if (x == null)
     {
        var y = x.Length;
     }
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.NullDeferenceId, dx[0].Id);
        }

        [TestMethod]
        public void TestDereferenceOnKnownNotNull()
        {
            var code =
@"
public class C
{
  private string x;

  public void M()
  {
     if (x != null)
     {
        var y = x.Length;
     }
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestDereferenceOnKnownNotNullAfterAnd()
        {
            var code =
@"
public class C
{
  private string x;

  public void M()
  {
     if (x != null && x.Length == 2)
     {
     }
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestDereferenceOnKnownNullAfterAnd()
        {
            var code =
@"
public class C
{
  private string x;

  public void M()
  {
     if (x == null && x.Length == 2)
     {
     }
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.NullDeferenceId, dx[0].Id);
        }

        [TestMethod]
        public void TestDereferenceCouldBeNullAfterOr()
        {
            var code =
@"
public class C
{
  public void M([CouldBeNull] string x)
  {
     if (x == null || x.Length == 2)
     {
     }
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestDereferenceOnUnknownNull()
        {
            var code =
@"
public class C
{
  private string x;

  public void M()
  {
     var = y = x.Length;
  }
}
";
            // we should have no error because variable is in unknown state
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestDereferenceOnKnownNullAfterOr()
        {
            var code =
@"
public class C
{
  private string x;

  public void M()
  {
     if (x == null || x.Length == 2)
     {
     }
  }
}
";
            // we should have no error because variable is in unknown state
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestDereferenceWithAssignmentTestedNotNull()
        {
            var code =
@"
public class C
{
  public void M([CouldBeNull] string x, [CouldBeNull] string y)
  {    
     if ((x = y) != null)
     {
        var n = x.Length;
     }
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestDereferenceOnKnownNullLocal()
        {
            var code =
@"
public class C
{
  public void M()
  {
     string x = null;
     if (x.Length == 2)
     {
     }
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.NullDeferenceId, dx[0].Id);
        }

        [TestMethod]
        public void TestDereferenceOnKnownNotNullLocal()
        {
            var code =
@"
public class C
{
  public void M()
  {
     string x = ""message"";
     if (x.Length == 2)
     {
     }
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestDereferenceOnShouldNotBeNullField()
        {
            var code =
@"
public class C
{
  [ShouldNotBeNull] string x;

  public void M()
  {
     if (x.Length == 2)
     {
     }
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestDereferenceOnCouldBeNullField()
        {
            var code =
@"
public class C
{
  [CouldBeNull] string x;

  public void M()
  {
     if (x.Length == 2)
     {
     }
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.PossibleNullDeferenceId, dx[0].Id);
        }

        [TestMethod]
        public void TestDereferenceOnCouldBeNullFieldAfterGuard()
        {
            var code =
@"
public class C
{
  [CouldBeNull] string x;

  public void M()
  {
     if (x != null && x.Length == 2)
     {
     }
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestDereferenceOnCouldBeNullFieldAfterGuardElse()
        {
            var code =
@"
public class C
{
  [CouldBeNull] string x;

  public void M()
  {
    if (x != null)
    {
    }
    else 
    {
       var y = x.Length;
    }
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.NullDeferenceId, dx[0].Id);
        }

        [TestMethod]
        public void TestAssignNullLiteralToShouldNotBeNullField()
        {
            var code =
@"
public class C
{
  [ShouldNotBeNull] string x;

  public void M()
  {
    x = null;
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.NullAssignmentId, dx[0].Id);
        }

        [TestMethod]
        public void TestAssignKnownNullToShouldNotBeNullField()
        {
            var code =
@"
public class C
{
  [ShouldNotBeNull] string x;
  string y;

  public void M()
  {
    if (y == null)
    {
        x = y;
    }
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.NullAssignmentId, dx[0].Id);
        }

        [TestMethod]
        public void TestAssignUnknownNullToShouldNotBeNullField()
        {
            var code =
@"
public class C
{
  [ShouldNotBeNull] string x;
  string y;

  public void M()
  {
    x = y;
  }
}
";
            // unknown are assignable to NotNull variables pay-for-play?
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestAssignCouldBeNullToShouldNotBeNullField()
        {
            var code =
@"
public class C
{
  [ShouldNotBeNull] string x;
  [CouldBeNull] string y;

  public void M()
  {
    x = y;
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.PossibleNullAssignmentId, dx[0].Id);
        }

        [TestMethod]
        public void TestAssignShouldNotBeNullMethodReturnToShouldNotBeNullField()
        {
            var code =
@"
public class C
{
  [ShouldNotBeNull] string x;

  public void M()
  {
    x = GetIt();
  }

  [return: NotNull]
  public string GetIt() { return ""string""; }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestAssignCouldBeNullMethodReturnToShouldNotBeNullField()
        {
            var code =
@"
public class C
{
  [ShouldNotBeNull] string x;

  public void M()
  {
    x = GetIt();
  }

  [return: CouldBeNull]
  public string GetIt() { return ""string""; }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.PossibleNullAssignmentId, dx[0].Id);
        }

        [TestMethod]
        public void TestAssignCouldBeNullToShouldNotNullParameter()
        {
            var code =
@"
public class C
{
  [CouldBeNull] string x;

  public void M()
  {
    DoIt(x);
  }

  public void DoIt([ShouldNotBeNull] string p) { }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.PossibleNullAssignmentId, dx[0].Id);
        }

        [TestMethod]
        public void TestAssignShouldNotBeNullToShouldNotBeNullParameter()
        {
            var code =
@"
public class C
{
  [ShouldNotBeNull] string x;

  public void M()
  {
    DoIt(x);
  }

  public void DoIt([ShouldNotBeNull] string p) { }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestAssignUnknownNullToShouldNotBeNullParameter()
        {
            var code =
@"
public class C
{
  string x;

  public void M()
  {
    DoIt(x);
  }

  public void DoIt([ShouldNotBeNull] string p) { }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestReturnNullFromShouldNotBeNullMethod()
        {
            var code =
@"
public class C
{
  [return: ShouldNotBeNull]
  public string M()
  {
    return null;
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.NullAssignmentId, dx[0].Id);
        }

        [TestMethod]
        public void TestReturnCouldBeNullFromShouldNotBeNullMethod()
        {
            var code =
@"
public class C
{
  [CouldBeNull] string x;
  
  [return: ShouldNotBeNull]
  public string M()
  {
    return x;
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.PossibleNullAssignmentId, dx[0].Id);
        }

        [TestMethod]
        public void TestReturnUnknownNullFromShouldNotBeNullMethod()
        {
            var code =
@"
public class C
{
  string x;
  
  [return: NotNull]
  public string M()
  {
    return x;
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestIfLeaksNullStateToBlockWithThrow()
        {
            var code =
@"
public class C
{
  [return: NotNull]
  public int M([CouldBeNull] string x)
  {
    if (x == null)
    {
        throw new ArgumentNullException(nameof(x));
    }

    return x.Length;
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestEmptyIfOnCouldBeNullState()
        {
            var code =
@"
public class C
{
  public int M([CouldBeNull] string x)
  {
    if (x == null)
    {
    }

    return x.Length;
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.PossibleNullDeferenceId, dx[0].Id);
        }

        [TestMethod]
        public void TestEmptyIfOnUnknownNullState()
        {
            var code =
@"
public class C
{
  public int M(string x)
  {
    if (x == null)
    {
    }

    return x.Length;
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestNotNullAssignmentIfOnNull()
        {
            var code =
@"
public class C
{
  public int M([CouldBeNull] string x)
  {
    if (x == null)
    {
       x = ""Something"";
    }

    return x.Length;
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestNullAssignmentInIf()
        {
            var code =
@"
public class C
{
  private bool y;

  public int M(string x)
  {
    if (y)
    {
       x = null;
    }

    return x.Length;
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.PossibleNullDeferenceId, dx[0].Id);
        }

        [TestMethod]
        public void TestWhileConditionNotNullAccessedInLoop()
        {
            var code =
@"
public class C
{
  public int M([CouldBeNull] string x)
  {
    while (x != null)
    {
        return x.Length;
    }

    return 0;
  }
}
";
            // should behave like if statement here
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestWhileConditionNullAccessedOutsideLoop()
        {
            var code =
@"
public class C
{
  public int M([CouldBeNull] string x)
  {
    while (x == null)
    {
    }

    return x.Length;
  }
}
";
            // should behave like if statement here
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestWhileAssignmentInLoopAccessAfter()
        {
            var code =
@"
public class C
{
  private bool p;

  public int M([CouldBeNull] string x)
  {
    while (p)
    {
       x = ""value"";
    }

    return x.Length;
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.PossibleNullDeferenceId, dx[0].Id);
        }

        [TestMethod]
        public void TestWhileAssignmentInNullConditionedLoop()
        {
            var code =
@"
public class C
{
  private bool p;

  public int M([CouldBeNull] string x)
  {
    while (x == null)
    {
       x = ""value"";
    }

    return x.Length;
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestWhileAssignmentInLoopBeforeBreak()
        {
            var code =
@"
public class C
{
  private bool p;

  public int M([CouldBeNull] string x)
  {
    while (x == null)
    {
       x = ""value"";
       if (p) break;
    }

    return x.Length;
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestWhileAssignmentInLoopAfterBreak()
        {
            var code =
@"
public class C
{
  private bool p;

  public int M([CouldBeNull] string x)
  {
    while (x == null)
    {
       if (p) break;
       x = ""value"";
    }

    return x.Length;
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.PossibleNullDeferenceId, dx[0].Id);
        }

        [TestMethod]
        public void TestGotoBranchForward()
        {
            var code =
@"
public class C
{
  public int M([CouldBeNull] string x)
  {
    if (x == null)
    {
       x = ""something"";
       goto label;
    }

  label:
    return x.Length;
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestGotoBranchBackward()
        {
            var code =
@"
public class C
{
  public int M([CouldBeNull] string x)
  {
    label:

    if (x == null)
    {
       x = ""something"";
       goto label;
    }

    return x.Length;
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestGotoBranchBackwardNullAssignment()
        {
            var code =
@"
public class C
{
  public void M()
  {
    var x = ""something"";
    label:
    var n = x.Length;
    x = null;
    goto label;
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.PossibleNullDeferenceId, dx[0].Id);
        }

        [TestMethod]
        public void TestFieldExpressionAcquiresNullState()
        {
            var code =
@"
public class C
{
  public C C;

  [CouldBeNull]
  public string F;

  public void M()
  {
    if (C.F != null)
    {
        var y = C.F.Length;
    }
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestComplexFieldExpressionAcquiresNullState()
        {
            var code =
@"
public class C
{
  public C C;
  public C[] A;

  [CouldBeNull]
  public string F;

  public void M()
  {
    if (C.A[0].F != null)
    {
        var y = C.A[0].F.Length;
    }
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestComplexFieldExpressionKnownNull()
        {
            var code =
@"
public class C
{
  public C C;
  public C[] A;

  [CouldBeNull]
  public string F;

  public void M()
  {
    if (C.A[0].F == null)
    {
        var y = C.A[0].F.Length;
    }
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.NullDeferenceId, dx[0].Id);
        }

        [TestMethod]
        public void TestParensDontInterfere()
        {
            var code =
@"
public class C
{
  [CouldBeNull]
  public string F;

  public void M()
  {
    if ((F) != null)
    {
        var y = F.Length;
    }
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(0, dx.Length);
        }

        [TestMethod]
        public void TestLogicalNotReversesState()
        {
            var code =
@"
public class C
{
  [CouldBeNull]
  public string F;

  public void M()
  {
    if (!(F != null))
    {
        var y = F.Length;
    }
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.NullDeferenceId, dx[0].Id);
        }

        [TestMethod]
        public void TestConditionalAccessImpliesCouldBeNull()
        {
            var code =
@"
public class C
{
  public C C;
  public string F;

  [ShouldNotBeNull]
  public object X;

  public void M()
  {
    X = C?.F.Length;
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.PossibleNullAssignmentId, dx[0].Id);
        }

        [TestMethod]
        public void TestCoalesceImpartsRightHandSideState()
        {
            var code =
@"
public class C
{
  public string F;
 
  [CouldBeNull]
  public string G;

  public void M([ShouldNotBeNull] string s)
  {
    M(F ?? G);
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.PossibleNullAssignmentId, dx[0].Id);
        }

        [TestMethod]
        public void TestNullVariableWorksAsNullTest()
        {
            var code =
@"
public class C
{
  public void M()
  {
    string x = null;
    string y;

    if (y == x)
    {
        var z = y.Length;
    }
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.NullDeferenceId, dx[0].Id);
        }

        [TestMethod]
        public void TestNullVariableWorksAsNullTestInElse()
        {
            var code =
@"
public class C
{
  public void M()
  {
    string x = null;
    string y;

    if (y != x)
    {
    }
    else 
    {
        var z = y.Length;
    }
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.NullDeferenceId, dx[0].Id);
        }

        [TestMethod]
        public void TestFieldInitializedToNull()
        {
            var code =
@"
public class C
{
  [ShouldNotBeNull] 
  string F = null;
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.NullAssignmentId, dx[0].Id);
        }

        [TestMethod]
        public void TestParameterInitializedToNull()
        {
            var code =
@"
public class C
{
  public void M([ShouldNotBeNull] string p = null) { }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.NullAssignmentId, dx[0].Id);
        }

        [TestMethod]
        public void TestObjectCreation()
        {
            var code =
@"
public class C
{
  public C([ShouldNotBeNull] string s) { }

  public static C Create([CouldBeNull] string s)
  {
    return new C(s);
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.PossibleNullAssignmentId, dx[0].Id);
        }

        [TestMethod]
        public void TestConstructorInitializer()
        {
            var code =
@"

public class B
{
   public B([ShouldNotBeNull] string s)
   {
   }
}

public class C : B
{
  public C([CouldBeNull] string s) 
    : base(s)
  { 
  }
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.PossibleNullAssignmentId, dx[0].Id);
        }

        [TestMethod]
        public void TestPropertyInitializer()
        {
            var code =
@"
public class C
{
  [ShouldNotBeNull]
  public string P { get; set; } = null;
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.NullAssignmentId, dx[0].Id);
        }

        [TestMethod]
        public void TestExpressionBodiedProperty()
        {
            var code =
@"
public class C
{
  [ShouldNotBeNull]
  public string P => null;
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(NullAnalyzer.NullAssignmentId, dx[0].Id);
        }

        protected Diagnostic[] GetAnalyzerDiagnostics(string code)
        {
            code = code + @"
public class CouldBeNullAttribute : System.Attribute { }
public class ShouldNotBeNullAttribute : System.Attribute { }
";
            var document = CreateDocument(code, LanguageNames.CSharp);
            var analyzer = new NullAnalyzer();
            return GetSortedDiagnosticsFromDocuments(analyzer, new[] { document });
        }

        protected override CodeFixProvider GetCSharpCodeFixProvider()
        {
            throw new NotImplementedException();
        }

        protected override DiagnosticAnalyzer GetCSharpDiagnosticAnalyzer()
        {
            return new NullAnalyzer();
        }
    }
}