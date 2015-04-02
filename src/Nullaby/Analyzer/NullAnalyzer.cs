using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace Nullaby
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class NullAnalyzer : DiagnosticAnalyzer
    {
        public const string NullDeferenceId = "NN0001";
        public const string PossibleNullDeferenceId = "NN0002";
        public const string NullAssignmentId = "NN0003";
        public const string PossibleNullAssignmentId = "NN0004";

        internal static DiagnosticDescriptor NullDeference =
            new DiagnosticDescriptor(
                id: NullDeferenceId,
                title: "Null dereference",
                messageFormat: "Dereference of null value",
                category: "Nulls",
                defaultSeverity: DiagnosticSeverity.Error,
                isEnabledByDefault: true);

        internal static DiagnosticDescriptor PossibleNullDereference =
            new DiagnosticDescriptor(
                id: PossibleNullDeferenceId,
                title: "Possible null deference",
                messageFormat: "Possible dereference of null value",
                category: "Nulls",
                defaultSeverity: DiagnosticSeverity.Error,
                isEnabledByDefault: true);

        internal static DiagnosticDescriptor NullAssignment =
           new DiagnosticDescriptor(
                id: NullAssignmentId,
                title: "Null assignment",
                messageFormat: "Assignment of null to not-null variable XXX",
                category: "Nulls",
                defaultSeverity: DiagnosticSeverity.Error,
                isEnabledByDefault: true);

        internal static DiagnosticDescriptor PossibleNullAssignment =
           new DiagnosticDescriptor(
                id: PossibleNullAssignmentId,
                title: "Possible null assignment",
                messageFormat: "Possible assignment of null to not-null variable",
                category: "Nulls",
                defaultSeverity: DiagnosticSeverity.Error,
                isEnabledByDefault: true);

        private static readonly ImmutableArray<DiagnosticDescriptor> s_supported =
            ImmutableArray.Create(NullDeference, PossibleNullDereference, NullAssignment, PossibleNullAssignment);

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
            private ImmutableDictionary<object, NullState> states;

            private enum NullState
            {
                Unknown,
                Null,
                NotNull,
                TestedNull,
                TestedNotNull,
                CouldBeNull,
                ShouldNotBeNull
            }

            public CodeBlockAnalyzer(CodeBlockAnalysisContext context)
            {
                this.context = context;
                this.empty = ImmutableDictionary.Create<object, NullState>(new VariableComparer(this));
                this.states = empty;
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

                this.Visit(node);
            }

            public override void VisitVariableDeclarator(VariableDeclaratorSyntax node)
            {
                if (node.Initializer != null)
                {
                    var symbol = context.SemanticModel.GetDeclaredSymbol(node);
                    if (symbol != null)
                    {
                        CheckAssignment(symbol, node.Initializer.Value);
                    }

                    this.Visit(node.Initializer.Value);
                }
                else
                {
                    base.VisitVariableDeclarator(node);
                }
            }

            public override void VisitEqualsValueClause(EqualsValueClauseSyntax node)
            {
                if (node.Parent != null && node.Parent.IsKind(SyntaxKind.PropertyDeclaration))
                {
                    var symbol = context.SemanticModel.GetDeclaredSymbol((PropertyDeclarationSyntax)node.Parent);
                    if (symbol != null)
                    {
                        CheckAssignment(symbol, node.Value);
                    }
                }
                else
                {
                    base.VisitEqualsValueClause(node);
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
                        CheckAssignment(symbol, node.Default.Value);
                    }
                }

                base.VisitParameter(node);
            }

            public override void VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
            {
                switch (GetNullState(node.Expression))
                {
                    case NullState.Null:
                    case NullState.TestedNull:
                        context.ReportDiagnostic(Diagnostic.Create(NullDeference, node.Name.GetLocation()));
                        break;
                    case NullState.CouldBeNull:
                        context.ReportDiagnostic(Diagnostic.Create(PossibleNullDereference, node.Name.GetLocation()));
                        break;
                }

                base.VisitMemberAccessExpression(node);
            }

            public override void VisitAssignmentExpression(AssignmentExpressionSyntax node)
            {
                CheckAssignment(node.Left, node.Right);
                base.VisitAssignmentExpression(node);
            }

            private void CheckAssignment(ExpressionSyntax variable, ExpressionSyntax expression)
            {
                var exprState = GetNullState(expression);
                CheckAssignment(GetDeclaredNullState(variable), exprState, expression);
                SetNullState(variable, exprState);
            }

            private void CheckAssignment(ISymbol symbol, ExpressionSyntax expression)
            {
                var exprState = GetNullState(expression);
                CheckAssignment(GetDeclaredNullState(symbol), exprState, expression);
                SetNullState(symbol, exprState);
            }

            private void CheckAssignment(NullState variableState, NullState expressionState, ExpressionSyntax expression)
            {
                if (variableState == NullState.ShouldNotBeNull)
                {
                    switch (expressionState)
                    {
                        case NullState.Null:
                        case NullState.TestedNull:
                            context.ReportDiagnostic(Diagnostic.Create(NullAssignment, expression.GetLocation()));
                            break;

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
                var method = context.SemanticModel.GetSymbolInfo(node).Symbol as IMethodSymbol;
                if (method != null)
                {
                    CheckArguments(node.ArgumentList.Arguments, method.Parameters);
                }

                base.VisitInvocationExpression(node);
            }

            public override void VisitObjectCreationExpression(ObjectCreationExpressionSyntax node)
            {
                var method = context.SemanticModel.GetSymbolInfo(node).Symbol as IMethodSymbol;
                if (method != null)
                {
                    CheckArguments(node.ArgumentList.Arguments, method.Parameters);
                }

                base.VisitObjectCreationExpression(node);
            }

            public override void VisitConstructorInitializer(ConstructorInitializerSyntax node)
            {
                var method = context.SemanticModel.GetSymbolInfo(node).Symbol as IMethodSymbol;
                if (method != null)
                {
                    CheckArguments(node.ArgumentList.Arguments, method.Parameters);
                }

                base.VisitConstructorInitializer(node);
            }

            private void CheckArguments(SeparatedSyntaxList<ArgumentSyntax> arguments, ImmutableArray<IParameterSymbol> parameters)
            {
                // check parameter assignments from arguments
                if (arguments.Count <= parameters.Length)
                {
                    for (int i = 0; i < parameters.Length; i++)
                    {
                        CheckAssignment(parameters[i], arguments[i].Expression);
                    }
                }
            }

            public override void VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
            {
                // local variables acquire state from initializer
                foreach (var v in node.Declaration.Variables)
                {
                    if (v.Initializer != null)
                    {
                        var symbol = context.SemanticModel.GetDeclaredSymbol(v);
                        if (symbol != null)
                        {
                            var state = GetNullState(v.Initializer.Value);
                            SetNullState(symbol, state);
                        }
                    }
                }

                base.VisitLocalDeclarationStatement(node);
            }

            public override void VisitIfStatement(IfStatementSyntax node)
            {
                var initialStates = this.states;

                this.Visit(node.Condition);

                var trueBranch = this.states;
                var falseBranch = NegateChanges(initialStates, trueBranch);

                this.states = trueBranch;
                this.Visit(node.Statement);
                trueBranch = this.states; 

                if (node.Else != null)
                {
                    this.states = falseBranch;
                    this.Visit(node.Else);
                    falseBranch = this.states; 
                }

                if (!Exits(node.Statement))
                {
                    // if statement does not exit then false branch states hold after statement
                    // because the only want to reach that point is if the condition was false
                    this.states = falseBranch;
                }
                else if (node.Else != null && !Exits(node.Else.Statement))
                {
                    // if else statement does not exit then true branch states hold after statement
                    // because the only way to reach that point is if the condition was true
                    this.states = trueBranch;
                }
                else
                {
                    // can't say anything about true/false condition
                    // final state is a join between both branches
                    this.states = Join(initialStates, trueBranch, falseBranch);
                }
            }

            public override void VisitWhileStatement(WhileStatementSyntax node)
            {
                var initialStates = this.states;

                this.Visit(node.Condition);

                var trueBranch = this.states;
                var falseBranch = NegateChanges(initialStates, trueBranch);

                // body of loop is evaluated if condition is true
                this.states = trueBranch;
                this.Visit(node.Statement);

                var loopEnd = this.states;
                this.states = Join(initialStates, falseBranch, loopEnd);

                if (!BranchesOut(node.Statement))
                {
                    // if we don't otherwise exit from inside the loop, then the false condition holds at the end
                    // regardless what happened inside the loop, so add it back.
                    this.states = AddChanges(initialStates, falseBranch, this.states);
                }
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
                var initialStates = this.states;
                this.Visit(node.Operand);
                this.states = NegateChanges(initialStates, this.states);
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
                var initialStates = this.states;
                this.Visit(binop.Left);

                var leftBranch = this.states;

                // right evaluation starts with negation of left-side changes because
                // right side only executes if left side is false.
                this.states = NegateChanges(initialStates, leftBranch);
                this.Visit(binop.Right);
                var rightBranch = this.states;

                this.states = Join(initialStates, leftBranch, rightBranch);
            }

            private void VisitEquality(BinaryExpressionSyntax binop)
            {
                ExpressionSyntax influencedExpr = null;

                var leftState = GetNullState(binop.Left);
                var rightState = GetNullState(binop.Right);

                if (leftState == NullState.Null && rightState != NullState.Null)
                {
                    influencedExpr = binop.Right;
                }
                else if (rightState == NullState.Null && leftState != NullState.Null)
                {
                    influencedExpr = binop.Left;
                }

                if (influencedExpr != null)
                {
                    influencedExpr = WithoutParens(influencedExpr);

                    switch (binop.Kind())
                    {
                        case SyntaxKind.EqualsExpression:
                            this.SetNullState(influencedExpr, NullState.TestedNull);
                            break;

                        case SyntaxKind.NotEqualsExpression:
                            this.SetNullState(influencedExpr, NullState.TestedNotNull);
                            break;
                    }
                }

                base.VisitBinaryExpression(binop);
            }

            private void SetNullState(ExpressionSyntax expr, NullState state)
            {
                expr = WithoutParens(expr);
                this.states = this.states.SetItem(expr, state);
            }

            private ExpressionSyntax WithoutParens(ExpressionSyntax expr)
            {
                while (expr.IsKind(SyntaxKind.ParenthesizedExpression))
                {
                    expr = ((ParenthesizedExpressionSyntax)expr).Expression;
                }

                return expr;
            }

            private void SetNullState(ISymbol symbol, NullState state)
            {
                switch (symbol.Kind)
                {
                    case SymbolKind.Local:
                    case SymbolKind.Parameter:
                    case SymbolKind.RangeVariable:
                        this.states = this.states.SetItem(symbol, state);
                        break;
                }
            }

            private NullState GetNullState(ExpressionSyntax expression)
            {
                if (expression != null)
                {
                    expression = WithoutParens(expression);

                    NullState state;
                    if (this.states.TryGetValue(expression, out state))
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
                            var exprState = GetNullState(ca.Expression);
                            switch (GetNullState(ca.Expression))
                            {
                                case NullState.Null:
                                    return NullState.Null;
                                case NullState.CouldBeNull:
                                case NullState.Unknown:
                                    return NullState.CouldBeNull;
                                default:
                                    return GetDeclaredNullState(ca.WhenNotNull);
                            }

                        case SyntaxKind.CoalesceExpression:
                            var co = (BinaryExpressionSyntax)expression;
                            return GetNullState(co.Right);
                    }

                    var symbol = context.SemanticModel.GetSymbolInfo(expression).Symbol;
                    if (symbol != null)
                    {
                        return GetNullState(symbol);
                    }
                }

                return NullState.Unknown;
            }

            private NullState GetNullState(ISymbol symbol)
            {
                NullState state;
                if (states.TryGetValue(symbol, out state))
                {
                    return state;
                }

                return GetDeclaredNullState(symbol);
            }

            private NullState GetDeclaredNullState(object symbolOrSyntax)
            {
                var syntax = symbolOrSyntax as ExpressionSyntax;
                if (syntax != null)
                {
                    return GetDeclaredNullState(syntax);
                }

                var symbol = symbolOrSyntax as ISymbol;
                if (symbol != null)
                {
                    return GetDeclaredNullState(symbol);
                }

                return NullState.Unknown;
            }

            private NullState GetDeclaredNullState(ExpressionSyntax syntax)
            {
                var symbol = context.SemanticModel.GetSymbolInfo(syntax).Symbol;
                if (symbol != null)
                {
                    return GetDeclaredNullState(symbol);
                }
                else
                {
                    return NullState.Unknown;
                }
            }

            private static NullState GetDeclaredNullState(ISymbol symbol)
            {
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

                foreach (var a in attrs)
                {
                    if (a.AttributeClass.Name == "ShouldNotBeNullAttribute")
                    {
                        return NullState.ShouldNotBeNull;
                    }
                    else if (a.AttributeClass.Name == "CouldBeNullAttribute")
                    {
                        return NullState.CouldBeNull;
                    }
                }

                return NullState.Unknown;
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
                    case NullState.TestedNull:
                        return NullState.TestedNotNull;
                    case NullState.TestedNotNull:
                        return NullState.TestedNull;
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
            /// Tested states are promoted to absolute states, unless they counter each other, and then the original state prevails.
            /// </remarks>
            private ImmutableDictionary<object, NullState> Join(
                ImmutableDictionary<object, NullState> original, 
                ImmutableDictionary<object, NullState> branchA, 
                ImmutableDictionary<object, NullState> branchB)
            {
                var joined = original;
                Join(original, branchA, branchB, ref joined);
                Join(original, branchB, branchA, ref joined);
                return RealizeChanges(original, joined);
            }

            private void Join(
                ImmutableDictionary<object, NullState> original,
                ImmutableDictionary<object, NullState> branchA, 
                ImmutableDictionary<object, NullState> branchB, 
                ref ImmutableDictionary<object, NullState> joined)
            {
                // for all items in a
                foreach (var kvp in branchA)
                {
                    NullState bs;
                    if (!branchB.TryGetValue(kvp.Key, out bs)
                        && !original.TryGetValue(kvp.Key, out bs))
                    {
                        bs = GetDeclaredNullState(kvp.Key);
                    }

                    var w = Join(kvp.Value, bs);

                    // any unknown result means to keep the original state
                    if (w == NullState.Unknown
                        && !original.TryGetValue(kvp.Key, out w))
                    {
                        w = GetDeclaredNullState(kvp.Key);
                    }

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
                            case NullState.TestedNotNull:
                            case NullState.ShouldNotBeNull:
                                return NullState.Unknown;
                            case NullState.Null:
                            case NullState.TestedNull:
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
                            case NullState.TestedNotNull:
                                return NullState.ShouldNotBeNull;

                            case NullState.Unknown:
                                return NullState.Unknown;

                            case NullState.CouldBeNull:
                            case NullState.Null:
                            case NullState.TestedNull:
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
                            case NullState.TestedNotNull:
                                return NullState.CouldBeNull;

                            case NullState.Null:
                            case NullState.TestedNull:
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
                            case NullState.TestedNotNull:
                                return NullState.NotNull;
                            case NullState.CouldBeNull:
                            case NullState.Null:
                            case NullState.TestedNull:
                                return NullState.CouldBeNull;
                        }
                        break;

                    case NullState.TestedNull:
                        switch (b)
                        {
                            case NullState.TestedNotNull:
                                return NullState.Unknown;

                            case NullState.Unknown:
                            case NullState.CouldBeNull:
                            case NullState.ShouldNotBeNull:
                            case NullState.NotNull:
                                return NullState.CouldBeNull;

                            case NullState.Null:
                            case NullState.TestedNull:
                                return NullState.Null;
                        }
                        break;

                    case NullState.TestedNotNull:
                        switch (b)
                        {
                            case NullState.Unknown:
                                return NullState.Unknown;
                            case NullState.ShouldNotBeNull:
                                return NullState.ShouldNotBeNull;
                            case NullState.NotNull:
                            case NullState.TestedNotNull:
                                return NullState.NotNull;
                            case NullState.CouldBeNull:
                            case NullState.Null:
                                return NullState.CouldBeNull;
                            case NullState.TestedNull:
                                return NullState.Unknown;
                        }
                        break;
                }

                return NullState.Unknown;
            }

            /// <summary>
            /// Converts tested states into aboslute states (if they differ from original)
            /// </summary>
            private ImmutableDictionary<object, NullState> RealizeChanges(
                ImmutableDictionary<object, NullState> original,
                ImmutableDictionary<object, NullState> changed)
            {
                var realized = changed;

                foreach (var kvp in changed)
                {
                    NullState os;
                    if (!original.TryGetValue(kvp.Key, out os) || os != kvp.Value)
                    {
                        switch (kvp.Value)
                        {
                            case NullState.TestedNull:
                                realized = realized.SetItem(kvp.Key, NullState.Null);
                                break;
                            case NullState.TestedNotNull:
                                realized = realized.SetItem(kvp.Key, NullState.NotNull);
                                break;
                        }
                    }
                }

                return realized;
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
