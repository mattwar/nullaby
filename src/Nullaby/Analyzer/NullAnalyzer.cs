using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

[assembly: ShouldNotBeNull]

namespace Nullaby
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class NullAnalyzer : DiagnosticAnalyzer
    {
        public const string PossibleNullDeferenceId = "NN0001";
        public const string PossibleNullAssignmentId = "NN0002";

        internal static DiagnosticDescriptor PossibleNullDereference =
            new DiagnosticDescriptor(
                id: PossibleNullDeferenceId,
                title: "Possible null dereference",
                messageFormat: "Possible dereference of null value",
                category: "Nulls",
                defaultSeverity: DiagnosticSeverity.Warning,
                isEnabledByDefault: true);

        internal static DiagnosticDescriptor PossibleNullAssignment =
           new DiagnosticDescriptor(
                id: PossibleNullAssignmentId,
                title: "Possible null assignment",
                messageFormat: "Possible assignment of null value to variable or parameter that should not be null.",
                category: "Nulls",
                defaultSeverity: DiagnosticSeverity.Warning,
                isEnabledByDefault: true);

        private static readonly ImmutableArray<DiagnosticDescriptor> s_supported =
            ImmutableArray.Create(PossibleNullDereference, PossibleNullAssignment);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
        {
            get { return s_supported; }
        }

        public override void Initialize(AnalysisContext context)
        {
            //context.RegisterCodeBlockAction(AnalyzeCodeBlock);
            context.RegisterCodeBlockStartAction<SyntaxKind>(Start);
        }

        private static void Start(CodeBlockStartAnalysisContext<SyntaxKind> context)
        {
            context.RegisterCodeBlockEndAction(AnalyzeCodeBlock);
        }

        private static void AnalyzeCodeBlock(CodeBlockAnalysisContext context)
        {
            new CodeBlockAnalyzer(context).Analyze(context.CodeBlock);
        }

        private class CodeBlockAnalyzer : CSharpSyntaxWalker
        {
            private readonly CodeBlockAnalysisContext context;
            private readonly ImmutableDictionary<object, NullState> empty;

            // used to track observed null states lexically 
            private ImmutableDictionary<object, NullState> lexicalStates;

            // use to remember states for break style exits that branch to end of statement/etc
            private ImmutableDictionary<object, NullState> exitStates;

            // a map from label name to states from incoming branch points (goto statements)
            private ImmutableDictionary<string, ImmutableDictionary<object, NullState>> incomingStates
                = ImmutableDictionary<string, ImmutableDictionary<object, NullState>>.Empty;

            // true if the analzyer should report diagnostics on the current pass
            private bool reportDiagnostics;

            private enum NullState
            {
                Unknown,
                Null,
                NotNull,
                CouldBeNull,
                ShouldNotBeNull
            }

            public CodeBlockAnalyzer(CodeBlockAnalysisContext context)
            {
                this.context = context;
                this.empty = ImmutableDictionary.Create<object, NullState>(new VariableComparer(this));
                this.lexicalStates = empty;
                this.exitStates = empty;
            }

            public void Analyze(SyntaxNode node)
            {
                if (node.Parent != null)
                {
                    // for some nodes, start analysis at parent node instead
                    switch (node.Parent.Kind())
                    {
                        case SyntaxKind.EqualsValueClause:
                        case SyntaxKind.NameEquals:
                        case SyntaxKind.VariableDeclarator:
                        case SyntaxKind.Parameter:
                        case SyntaxKind.ArrowExpressionClause:
                            this.Analyze(node.Parent);
                            break;
                    }
                }

                // do initial visit to find branch points
                this.DoAnalysisPass(node);

                // if we have branch points, repeat visits until we have reached steady-state
                int revists = 0;
                while (this.incomingStates.Count > 0)
                {
                    var lastIncomingStates = this.incomingStates;
                    this.DoAnalysisPass(node);
                    if (lastIncomingStates == this.incomingStates || (revists++) < 10)
                    {
                        break;
                    }
                }

                // one more time to report diagnostics
                this.reportDiagnostics = true;
                this.DoAnalysisPass(node);
            }

            private void DoAnalysisPass(SyntaxNode node)
            {
                this.lexicalStates = this.empty;
                this.exitStates = this.empty;
                this.Visit(node);
            }

            public override void VisitVariableDeclarator(VariableDeclaratorSyntax node)
            {
                base.VisitVariableDeclarator(node);

                if (node.Initializer != null)
                {
                    var symbol = context.SemanticModel.GetDeclaredSymbol(node);
                    if (symbol != null)
                    {
                        CheckAssignment(symbol, node.Initializer.Value);
                    }
                }
            }

            public override void VisitEqualsValueClause(EqualsValueClauseSyntax node)
            {
                base.VisitEqualsValueClause(node);

                if (node.Parent != null && node.Parent.IsKind(SyntaxKind.PropertyDeclaration))
                {
                    var symbol = context.SemanticModel.GetDeclaredSymbol((PropertyDeclarationSyntax)node.Parent);
                    if (symbol != null)
                    {
                        CheckAssignment(symbol, node.Value);
                    }
                }
            }

            public override void VisitArrowExpressionClause(ArrowExpressionClauseSyntax node)
            {
                if (node.Parent != null && node.Parent.IsKind(SyntaxKind.PropertyDeclaration))
                {
                    switch (node.Parent.Kind())
                    {
                        case SyntaxKind.PropertyDeclaration:
                        case SyntaxKind.IndexerDeclaration:
                            var symbol = context.SemanticModel.GetDeclaredSymbol(node.Parent);
                            if (symbol != null)
                            {
                                CheckAssignment(symbol, node.Expression);
                            }
                            break;
                    }
                }

                base.VisitArrowExpressionClause(node);
            }

            public override void VisitParameter(ParameterSyntax node)
            {
                if (node.Default != null)
                {
                    var symbol = context.SemanticModel.GetDeclaredSymbol(node);
                    if (symbol != null)
                    {
                        CheckAssignment(symbol, node.Default.Value, isInvocationParameter: true);
                    }
                }

                base.VisitParameter(node);
            }

            public override void VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
            {
                base.VisitMemberAccessExpression(node);

                if (reportDiagnostics)
                {
                    switch (GetReferenceState(node.Expression))
                    {
                        case NullState.Null:
                        case NullState.CouldBeNull:
                            context.ReportDiagnostic(Diagnostic.Create(PossibleNullDereference, node.Name.GetLocation()));
                            break;
                    }
                }
            }

            public override void VisitAssignmentExpression(AssignmentExpressionSyntax node)
            {
                base.VisitAssignmentExpression(node);
                CheckAssignment(node.Left, node.Right);
            }

            private void CheckAssignment(ExpressionSyntax variable, ExpressionSyntax expression)
            {
                var exprState = GetReferenceState(expression);
                CheckAssignment(GetAssignmentState(variable), exprState, expression);
                SetVariableState(variable, exprState);
            }

            private void CheckAssignment(ISymbol symbol, ExpressionSyntax expression, bool isInvocationParameter = false)
            {
                var exprState = GetReferenceState(expression);
                CheckAssignment(GetAssignmentState(symbol, isInvocationParameter), exprState, expression);
                SetVariableState(symbol, exprState);
            }

            private void CheckAssignment(NullState variableState, NullState expressionState, ExpressionSyntax expression)
            {
                if (reportDiagnostics && variableState == NullState.ShouldNotBeNull)
                {
                    switch (expressionState)
                    {
                        case NullState.Null:
                        case NullState.CouldBeNull:
                            context.ReportDiagnostic(Diagnostic.Create(PossibleNullAssignment, expression.GetLocation()));
                            break;
                    }
                }
            }

            public override void VisitReturnStatement(ReturnStatementSyntax node)
            {
                if (node.Expression != null)
                {
                    var symbol = context.SemanticModel.GetEnclosingSymbol(node.SpanStart);
                    if (symbol != null)
                    {
                        CheckAssignment(symbol, node.Expression);
                    }
                }

                base.VisitReturnStatement(node);
            }

            public override void VisitInvocationExpression(InvocationExpressionSyntax node)
            {
                base.VisitInvocationExpression(node);

                var method = context.SemanticModel.GetSymbolInfo(node).Symbol as IMethodSymbol;
                if (method != null)
                {
                    CheckArguments(node.ArgumentList.Arguments, method.Parameters);
                }
            }

            public override void VisitObjectCreationExpression(ObjectCreationExpressionSyntax node)
            {
                base.VisitObjectCreationExpression(node);

                var method = context.SemanticModel.GetSymbolInfo(node).Symbol as IMethodSymbol;
                if (method != null)
                {
                    CheckArguments(node.ArgumentList.Arguments, method.Parameters);
                }
            }

            public override void VisitConstructorInitializer(ConstructorInitializerSyntax node)
            {
                base.VisitConstructorInitializer(node);

                var method = context.SemanticModel.GetSymbolInfo(node).Symbol as IMethodSymbol;
                if (method != null)
                {
                    CheckArguments(node.ArgumentList.Arguments, method.Parameters);
                }
            }

            private void CheckArguments(SeparatedSyntaxList<ArgumentSyntax> arguments, ImmutableArray<IParameterSymbol> parameters)
            {
                // check parameter assignments from arguments
                if (reportDiagnostics && arguments.Count <= parameters.Length)
                {
                    for (int i = 0; i < arguments.Count; i++)
                    {
                        CheckAssignment(parameters[i], arguments[i].Expression, isInvocationParameter: true);
                    }
                }
            }

            public override void VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
            {
                base.VisitLocalDeclarationStatement(node);

                // local variables acquire state from initializer
                foreach (var v in node.Declaration.Variables)
                {
                    if (v.Initializer != null)
                    {
                        var symbol = context.SemanticModel.GetDeclaredSymbol(v);
                        if (symbol != null)
                        {
                            var state = GetReferenceState(v.Initializer.Value);
                            SetVariableState(symbol, state);
                        }
                    }
                }
            }

            public override void VisitIfStatement(IfStatementSyntax node)
            {
                var initialStates = this.lexicalStates;

                this.Visit(node.Condition);

                var trueBranch = this.lexicalStates;
                var falseBranch = NegateChanges(initialStates, trueBranch);

                this.lexicalStates = trueBranch;
                this.Visit(node.Statement);
                trueBranch = this.lexicalStates;

                if (node.Else != null)
                {
                    this.lexicalStates = falseBranch;
                    this.Visit(node.Else);
                    falseBranch = this.lexicalStates;
                }

                if (!Exits(node.Statement))
                {
                    // if statement does not exit then false branch states hold after statement
                    // because the only want to reach that point is if the condition was false
                    this.lexicalStates = falseBranch;
                }
                else if (node.Else != null && !Exits(node.Else.Statement))
                {
                    // if else statement does not exit then true branch states hold after statement
                    // because the only way to reach that point is if the condition was true
                    this.lexicalStates = trueBranch;
                }
                else
                {
                    // can't say anything about true/false condition
                    // final state is a join between both branches
                    this.lexicalStates = Join(trueBranch, falseBranch);
                }
            }

            public override void VisitWhileStatement(WhileStatementSyntax node)
            {
                var initialStates = this.lexicalStates;
                var oldExits = this.exitStates;

                this.Visit(node.Condition);

                var trueBranch = this.lexicalStates;
                var falseBranch = NegateChanges(initialStates, trueBranch);

                // body of loop is evaluated if condition is true
                this.lexicalStates = trueBranch;
                this.exitStates = this.empty;
                this.Visit(node.Statement);

                this.lexicalStates = Join(falseBranch, Join(this.lexicalStates, this.exitStates));

                if (!BranchesOut(node.Statement))
                {
                    // if we don't otherwise exit from inside the loop, then the false condition holds at the end
                    // regardless what happened inside the loop, so add it back.
                    this.lexicalStates = AddChanges(initialStates, falseBranch, this.lexicalStates);
                }

                this.exitStates = oldExits;
            }

            public override void VisitBreakStatement(BreakStatementSyntax node)
            {
                base.VisitBreakStatement(node);

                // combine all exit states together
                // note: don't join if no exit states yet, since empty won't include the loop/branch condition, etc.
                this.exitStates = this.exitStates == this.empty
                    ? this.lexicalStates
                    : Join(this.exitStates, this.lexicalStates);
            }

            public override void VisitLabeledStatement(LabeledStatementSyntax node)
            {
                // join incoming branch states (if they exists) into the lexical state
                var labelName = node.Identifier.ValueText;
                ImmutableDictionary<object, NullState> labelState;
                if (incomingStates.TryGetValue(labelName, out labelState))
                {
                    this.lexicalStates = Join(this.lexicalStates, labelState);
                }

                base.VisitLabeledStatement(node);
            }

            public override void VisitGotoStatement(GotoStatementSyntax node)
            {
                // join current lexical state to label's external branch states
                if (node.CaseOrDefaultKeyword == default(SyntaxToken)
                    && node.Expression.IsKind(SyntaxKind.IdentifierName))
                {
                    var labelName = ((IdentifierNameSyntax)node.Expression).Identifier.ValueText;
                    ImmutableDictionary<object, NullState> labelState = this.empty;
                    if (this.incomingStates.TryGetValue(labelName, out labelState))
                    {
                        var joined = Join(labelState, this.lexicalStates);
                        this.incomingStates = this.incomingStates.SetItem(labelName, joined);
                    }
                    else
                    {
                        this.incomingStates = this.incomingStates.SetItem(labelName, this.lexicalStates);
                    }
                }

                base.VisitGotoStatement(node);
            }

            private bool Exits(StatementSyntax statement)
            {
                var flow = context.SemanticModel.AnalyzeControlFlow(statement);
                return flow.EndPointIsReachable; // || flow.ExitPoints.Any();
            }

            private bool BranchesOut(StatementSyntax statement)
            {
                var flow = context.SemanticModel.AnalyzeControlFlow(statement);
                return flow.ExitPoints.Any();
            }

            public override void VisitPrefixUnaryExpression(PrefixUnaryExpressionSyntax node)
            {
                switch (node.Kind())
                {
                    case SyntaxKind.LogicalNotExpression:
                        this.VisitLogicalNot(node);
                        break;

                    default:
                        base.VisitPrefixUnaryExpression(node);
                        break;
                }
            }

            private void VisitLogicalNot(PrefixUnaryExpressionSyntax node)
            {
                var initialStates = this.lexicalStates;
                this.Visit(node.Operand);
                this.lexicalStates = NegateChanges(initialStates, this.lexicalStates);
            }

            public override void VisitBinaryExpression(BinaryExpressionSyntax node)
            {
                switch (node.Kind())
                {
                    case SyntaxKind.EqualsExpression:
                    case SyntaxKind.NotEqualsExpression:
                        this.VisitEquality(node);
                        break;

                    case SyntaxKind.LogicalOrExpression:
                        this.VisitLogicalOr(node);
                        break;

                    default:
                        base.VisitBinaryExpression(node);
                        break;
                }
            }

            private void VisitLogicalOr(BinaryExpressionSyntax binop)
            {
                var initialStates = this.lexicalStates;
                this.Visit(binop.Left);

                var leftBranch = this.lexicalStates;

                // right evaluation starts with negation of left-side changes because
                // right side only executes if left side is false.
                this.lexicalStates = NegateChanges(initialStates, leftBranch);
                this.Visit(binop.Right);
                var rightBranch = this.lexicalStates;

                this.lexicalStates = Join(leftBranch, rightBranch);
            }

            private void VisitEquality(BinaryExpressionSyntax binop)
            {
                base.VisitBinaryExpression(binop);

                ExpressionSyntax influencedExpr = null;

                if (binop.Right.IsKind(SyntaxKind.NullLiteralExpression))
                {
                    influencedExpr = binop.Left;
                }
                else if (binop.Left.IsKind(SyntaxKind.NullLiteralExpression))
                {
                    influencedExpr = binop.Right;
                }

                if (influencedExpr != null)
                {
                    influencedExpr = WithoutParens(influencedExpr);

                    switch (binop.Kind())
                    {
                        case SyntaxKind.EqualsExpression:
                            this.SetVariableState(influencedExpr, NullState.Null);
                            break;

                        case SyntaxKind.NotEqualsExpression:
                            this.SetVariableState(influencedExpr, NullState.NotNull);
                            break;
                    }
                }
            }

            private ExpressionSyntax WithoutParens(ExpressionSyntax expr)
            {
                while (expr.IsKind(SyntaxKind.ParenthesizedExpression))
                {
                    expr = ((ParenthesizedExpressionSyntax)expr).Expression;
                }

                return expr;
            }

            private NullState GetAssignmentState(ExpressionSyntax variable, bool isInvocationParameter = false)
            {
                var symbol = context.SemanticModel.GetSymbolInfo(variable).Symbol;
                if (symbol != null)
                {
                    return GetAssignmentState(symbol, isInvocationParameter);
                }
                else
                {
                    return NullState.Unknown;
                }
            }

            private NullState GetAssignmentState(ISymbol symbol, bool isInvocationParameter = false)
            {
                switch (symbol.Kind)
                {
                    case SymbolKind.Local:
                        return NullState.Unknown;
                    case SymbolKind.Parameter:
                        if (!isInvocationParameter)
                        {
                            // method body parameters get their state assigned just like locals
                            return NullState.Unknown;
                        }
                        else
                        {
                            goto default;
                        }
                    default:
                        return GetDeclaredState(symbol);
                }
            }

            private void SetVariableState(ExpressionSyntax expr, NullState state)
            {
                expr = GetVariableExpression(expr);
                if (expr != null)
                {
                    this.lexicalStates = this.lexicalStates.SetItem(expr, state);
                }
            }

            /// <summary>
            /// Returns the portion of the expression that represents the variable
            /// that can be tracked, or null if the expression is not trackable.
            /// </summary>
            [return: CouldBeNull]
            private ExpressionSyntax GetVariableExpression(ExpressionSyntax expr)
            {
                expr = WithoutParens(expr);

                switch (expr.Kind())
                {
                    // assignment expressions yield their LHS variable for tracking
                    // this comes into play during null checks: (x = y) != null
                    // in this case x can be assigned tested-not-null state.. (what about y?)
                    case SyntaxKind.SimpleAssignmentExpression:
                        return GetVariableExpression(((BinaryExpressionSyntax)expr).Left);

                    // all dotted names are trackable.
                    case SyntaxKind.SimpleMemberAccessExpression:
                    case SyntaxKind.PointerMemberAccessExpression:
                    case SyntaxKind.QualifiedName:
                    case SyntaxKind.IdentifierName:
                    case SyntaxKind.AliasQualifiedName:
                        return expr;

                    default:
                        return null;
                }
            }

            private void SetVariableState(ISymbol symbol, NullState state)
            {
                switch (symbol.Kind)
                {
                    case SymbolKind.Local:
                    case SymbolKind.Parameter:
                    case SymbolKind.RangeVariable:
                        this.lexicalStates = this.lexicalStates.SetItem(symbol, state);
                        break;
                }
            }

            private NullState GetReferenceState(ExpressionSyntax expression)
            {
                if (expression != null)
                {
                    expression = WithoutParens(expression);

                    NullState state;
                    if (this.lexicalStates.TryGetValue(expression, out state))
                    {
                        return state;
                    }

                    switch (expression.Kind())
                    {
                        case SyntaxKind.NullLiteralExpression:
                            return NullState.Null;

                        case SyntaxKind.StringLiteralExpression:
                        case SyntaxKind.ObjectCreationExpression:
                        case SyntaxKind.ArrayCreationExpression:
                            return NullState.NotNull;

                        case SyntaxKind.ConditionalAccessExpression:
                            var ca = (ConditionalAccessExpressionSyntax)expression;
                            var exprState = GetReferenceState(ca.Expression);
                            switch (GetReferenceState(ca.Expression))
                            {
                                case NullState.Null:
                                    return NullState.Null;
                                case NullState.CouldBeNull:
                                case NullState.Unknown:
                                    return NullState.CouldBeNull;
                                default:
                                    return GetDeclaredState(ca.WhenNotNull);
                            }

                        case SyntaxKind.CoalesceExpression:
                            var co = (BinaryExpressionSyntax)expression;
                            return GetReferenceState(co.Right);
                    }

                    var symbol = context.SemanticModel.GetSymbolInfo(expression).Symbol;
                    if (symbol != null)
                    {
                        return GetReferenceState(symbol);
                    }
                }

                return NullState.Unknown;
            }

            private NullState GetReferenceState(ISymbol symbol)
            {
                NullState state;
                if (lexicalStates.TryGetValue(symbol, out state))
                {
                    return state;
                }

                return GetDeclaredState(symbol);
            }

            private NullState GetDeclaredState(object symbolOrSyntax)
            {
                var syntax = symbolOrSyntax as ExpressionSyntax;
                if (syntax != null)
                {
                    return GetDeclaredState(syntax);
                }

                var symbol = symbolOrSyntax as ISymbol;
                if (symbol != null)
                {
                    return GetDeclaredState(symbol);
                }

                return NullState.Unknown;
            }

            private NullState GetDeclaredState(ExpressionSyntax syntax)
            {
                var symbol = context.SemanticModel.GetSymbolInfo(syntax).Symbol;
                if (symbol != null)
                {
                    return GetDeclaredState(symbol);
                }
                else
                {
                    return NullState.Unknown;
                }
            }

            private static NullState GetDeclaredState(ISymbol symbol)
            {
                switch (symbol.Kind)
                {
                    case SymbolKind.Local:
                        return NullState.Unknown;

                    default:
                        return GetSymbolInfo(symbol).NullState;
                }
            }

            private static bool TryGetAttributedState(ImmutableArray<AttributeData> attrs, out NullState state)
            {
                foreach (var a in attrs)
                {
                    if (a.AttributeClass.Name == "ShouldNotBeNullAttribute")
                    {
                        state = NullState.ShouldNotBeNull;
                        return true;
                    }
                    else if (a.AttributeClass.Name == "CouldBeNullAttribute")
                    {
                        state = NullState.CouldBeNull;
                        return true;
                    }
                }

                state = NullState.Unknown;
                return false;

            }

            private class SymbolInfo
            {
                public readonly NullState NullState;

                public SymbolInfo(NullState defaultState)
                {
                    this.NullState = defaultState;
                }
            }

            private static ConditionalWeakTable<ISymbol, SymbolInfo> symbolInfos
                = new ConditionalWeakTable<ISymbol, SymbolInfo>();

            private static SymbolInfo GetSymbolInfo(ISymbol symbol)
            {
                SymbolInfo info;
                if (!symbolInfos.TryGetValue(symbol, out info))
                {
                    info = CreateSymbolInfo(symbol);
                    info = symbolInfos.GetValue(symbol, _ => info);
                }

                return info;
            }

            private static SymbolInfo CreateSymbolInfo(ISymbol symbol)
            {
                // check if it can possibly be null
                var type = GetVariableType(symbol);
                if (type != null)
                {
                    var possibleToBeNull = type.IsReferenceType 
                        || type.OriginalDefinition.SpecialType == SpecialType.System_Nullable_T;
                    if (!possibleToBeNull)
                    {
                        return new SymbolInfo(NullState.NotNull);
                    }
                }

                // check any explicit attributes
                ImmutableArray<AttributeData> attrs;
                switch (symbol.Kind)
                {
                    case SymbolKind.Method:
                        attrs = ((IMethodSymbol)symbol).GetReturnTypeAttributes();
                        break;

                    default:
                        attrs = symbol.GetAttributes();
                        break;
                }

                NullState state;
                if (!TryGetAttributedState(attrs, out state))
                {
                    // if defaulted to null, then obviously it could be null!
                    if (symbol.Kind == SymbolKind.Parameter)
                    {
                        var ps = (IParameterSymbol)symbol;
                        if (ps.HasExplicitDefaultValue && ps.ExplicitDefaultValue == null)
                        {
                            return new SymbolInfo(NullState.CouldBeNull);
                        }
                    }

                    // otherwise try to pickup default from assembly level attribute
                    if (symbol.Kind != SymbolKind.Assembly)
                    {
                        state = GetSymbolInfo(symbol.ContainingAssembly).NullState;
                    }
                }

                return new SymbolInfo(state);
            }

            [return: CouldBeNull]
            private static ITypeSymbol GetVariableType(ISymbol symbol)
            {
                switch (symbol.Kind)
                {
                    case SymbolKind.Parameter:
                        return ((IParameterSymbol)symbol).Type;

                    case SymbolKind.Local:
                        return ((ILocalSymbol)symbol).Type;

                    case SymbolKind.Field:
                        return ((IFieldSymbol)symbol).Type;

                    case SymbolKind.Property:
                        return ((IPropertySymbol)symbol).Type;

                    case SymbolKind.Method:
                        return ((IMethodSymbol)symbol).ReturnType;

                    default:
                        return null;
                }
            }

            /// <summary>
            /// returns the changed states with the specific changes negated
            /// </summary>
            private ImmutableDictionary<object, NullState> NegateChanges(
                ImmutableDictionary<object, NullState> original,
                ImmutableDictionary<object, NullState> changed)
            {
                var inverted = original;

                foreach (var kvp in changed)
                {
                    NullState os;
                    if (!original.TryGetValue(kvp.Key, out os) || os != kvp.Value)
                    {
                        inverted = inverted.SetItem(kvp.Key, Negate(kvp.Value));
                    }
                }

                return inverted;
            }

            private static NullState Negate(NullState state)
            {
                switch (state)
                {
                    case NullState.Null:
                        return NullState.NotNull;
                    case NullState.NotNull:
                        return NullState.Null;
                    case NullState.CouldBeNull:
                        return NullState.ShouldNotBeNull;
                    case NullState.ShouldNotBeNull:
                        return NullState.CouldBeNull;
                    default:
                        return NullState.Unknown;
                }
            }

            /// <summary>
            /// returns the target with the changes added to it.
            /// </summary>
            private ImmutableDictionary<object, NullState> AddChanges(
                ImmutableDictionary<object, NullState> original,
                ImmutableDictionary<object, NullState> changed,
                ImmutableDictionary<object, NullState> target)
            {
                foreach (var kvp in changed)
                {
                    NullState os;
                    if (!original.TryGetValue(kvp.Key, out os) || os != kvp.Value)
                    {
                        target = target.SetItem(kvp.Key, kvp.Value);
                    }
                }

                return target;
            }

            /// <summary>
            /// Returns the states from code path A and code path B joined together.
            /// </summary>
            /// <remarks>
            /// If a state on both branches in the same, then that is the final state.
            /// If a state on one of the branches is possibly null, then the resulting state is CouldBeNull.
            /// If a state on one side is ShouldNotBeNull and the other is any other not-null state, the joined state is ShouldNotBeNull.
            /// </remarks>
            private ImmutableDictionary<object, NullState> Join(
                ImmutableDictionary<object, NullState> branchA, 
                ImmutableDictionary<object, NullState> branchB)
            {
                var joined = branchA;
                Join(branchA, branchB, ref joined);
                Join(branchB, branchA, ref joined);
                return joined;
            }

            private void Join(
                ImmutableDictionary<object, NullState> branchA, 
                ImmutableDictionary<object, NullState> branchB, 
                ref ImmutableDictionary<object, NullState> joined)
            {
                // for all items in a
                foreach (var kvp in branchA)
                {
                    NullState bs;
                    if (!branchB.TryGetValue(kvp.Key, out bs))
                    {
                        bs = GetDeclaredState(kvp.Key);
                    }

                    var w = Join(kvp.Value, bs);

                    joined = joined.SetItem(kvp.Key, w);
                }
            }

            private NullState Join(NullState a, NullState b)
            {
                switch (a)
                {
                    case NullState.Unknown:
                        switch (b)
                        {
                            case NullState.Unknown:
                            case NullState.NotNull:
                            case NullState.ShouldNotBeNull:
                                return NullState.Unknown;
                            case NullState.Null:
                            case NullState.CouldBeNull:
                                return NullState.CouldBeNull;
                        }
                        break;

                    case NullState.CouldBeNull:
                        return NullState.CouldBeNull;

                    case NullState.ShouldNotBeNull:
                        switch (b)
                        {
                            case NullState.ShouldNotBeNull:
                            case NullState.NotNull:
                                return NullState.ShouldNotBeNull;

                            case NullState.Unknown:
                                return NullState.Unknown;

                            case NullState.CouldBeNull:
                            case NullState.Null:
                                return NullState.CouldBeNull;
                        }
                        break;

                    case NullState.Null:
                        switch (b)
                        {
                            case NullState.Unknown:
                            case NullState.CouldBeNull:
                            case NullState.ShouldNotBeNull:
                            case NullState.NotNull:
                                return NullState.CouldBeNull;

                            case NullState.Null:
                                return NullState.Null;
                        }
                        break;

                    case NullState.NotNull:
                        switch (b)
                        {
                            case NullState.Unknown:
                                return NullState.Unknown;
                            case NullState.ShouldNotBeNull:
                                return NullState.ShouldNotBeNull;
                            case NullState.NotNull:
                                return NullState.NotNull;
                            case NullState.CouldBeNull:
                            case NullState.Null:
                                return NullState.CouldBeNull;
                        }
                        break;
                }

                return NullState.Unknown;
            }

            private class VariableComparer : IEqualityComparer<object>
            {
                private readonly CodeBlockAnalyzer analzyer;

                public VariableComparer(CodeBlockAnalyzer analzyer)
                {
                    this.analzyer = analzyer;
                }

                public new bool Equals(object x, object y)
                {
                    if (x == y)
                    {
                        return true;
                    }

                    if (x == null || y == null)
                    {
                        return false;
                    }

                    var xs = x as ISymbol;
                    var ys = y as ISymbol;

                    var xn = x as ExpressionSyntax;
                    var yn = y as ExpressionSyntax;

                    if (xs == null && xn != null)
                    {
                        xs = analzyer.context.SemanticModel.GetSymbolInfo(xn).Symbol;
                    }

                    if (ys == null && yn != null)
                    {
                        ys = analzyer.context.SemanticModel.GetSymbolInfo(yn).Symbol;
                    }

                    if (xs.Equals(ys))
                    {
                        // don't need to compare syntax to match these (or static symbols)
                        if (xs.Kind == SymbolKind.Local || xs.Kind == SymbolKind.Parameter || xs.Kind == SymbolKind.RangeVariable || xs.IsStatic)
                        {
                            return true;
                        }

                        // syntax must be similar to be confident this reaches the same instance
                        return xn != null && yn != null && SyntaxFactory.AreEquivalent(xn, yn, topLevel: false);
                    }

                    return false;
                }

                public int GetHashCode(object obj)
                {
                    // hash code is based on symbol's hash code
                    var sym = obj as ISymbol;
                    var exp = obj as ExpressionSyntax;

                    if (sym == null && exp != null)
                    {
                        sym = analzyer.context.SemanticModel.GetSymbolInfo(exp).Symbol;
                    }

                    if (sym != null)
                    {
                        return sym.GetHashCode();
                    }

                    return obj.GetHashCode();
                }
            }
        }
    }
}
