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
            private ImmutableDictionary<object, NullState> current;
            private ImmutableDictionary<object, NullState> assigned;

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
                this.current = empty;
                this.assigned = empty;
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
                var oldStates = this.current;

                ImmutableDictionary<object, NullState> trueStates;
                ImmutableDictionary<object, NullState> falseStates;

                VisitCondition(node.Condition, out trueStates, out falseStates);

                var oldAssigned = this.assigned;
                this.assigned = this.empty;

                this.current = trueStates;
                this.Visit(node.Statement);

                var trueAssigned = this.assigned;
                var falseAssigned = this.empty;

                if (node.Else != null)
                {
                    this.current = falseStates;
                    this.assigned = this.empty;
                    this.Visit(node.Else);
                    falseAssigned = this.assigned;
                }

                if (!Exits(node.Statement))
                {
                    this.current = falseStates;
                    this.assigned = Add(oldAssigned, falseAssigned);
                }
                else if (node.Else != null && !Exits(node.Else.Statement))
                {
                    this.current = trueStates;
                    this.assigned = Add(oldAssigned, trueAssigned);
                }
                else if (node.Else == null)
                {
                    var bothAssigned = Weaker(trueAssigned, falseAssigned);
                    this.assigned = Add(oldAssigned, bothAssigned);

                    var trueCurrent = Add(oldStates, trueAssigned);
                    var falseCurrent = falseStates;
                    this.current = Weaker(trueCurrent, falseCurrent);
                }
                else
                {
                    var bothAssigned = Weaker(trueAssigned, falseAssigned);
                    this.assigned = Add(oldAssigned, bothAssigned);
                    this.current = Add(oldStates, bothAssigned);
                }
            }

            public override void VisitWhileStatement(WhileStatementSyntax node)
            {
                var oldStates = this.current;

                ImmutableDictionary<object, NullState> trueStates;
                ImmutableDictionary<object, NullState> falseStates;

                VisitCondition(node.Condition, out trueStates, out falseStates);

                var oldAssigned = this.assigned;
                this.current = trueStates;
                this.Visit(node.Statement);

                var blockAssigned = this.assigned;

                if (!BranchesOut(node.Statement))
                {
                    // only exit is the actual loop condition, so we can infer 
                    // if we have exited the loop then the opposite of the loop condition holds
                    var whileAssigned = Weaker(oldAssigned, blockAssigned); // might not have entered loop
                    this.assigned = whileAssigned;
                    this.current = Add(falseStates, whileAssigned);
                }
                else
                {
                    // branch might terminate loop, so can't rely on anything about loop condition
                    var whileAssigned = Weaker(oldAssigned, blockAssigned); // might not have entered loop
                    this.assigned = whileAssigned;
                    this.current = Add(oldStates, blockAssigned);
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

            private void VisitCondition(ExpressionSyntax expr,
                out ImmutableDictionary<object, NullState> trueStates,
                out ImmutableDictionary<object, NullState> falseStates)
            {
                trueStates = this.current;
                falseStates = this.current;

                switch (expr.Kind())
                {
                    case SyntaxKind.EqualsExpression:
                    case SyntaxKind.NotEqualsExpression:
                    case SyntaxKind.LogicalAndExpression:
                        this.VisitBinaryExpression((BinaryExpressionSyntax)expr, out trueStates, out falseStates);
                        break;

                    case SyntaxKind.ParenthesizedExpression:
                        this.VisitParenthesizedExpression((ParenthesizedExpressionSyntax)expr, out trueStates, out falseStates);
                        break;

                    case SyntaxKind.LogicalNotExpression:
                        this.VisitLogicalNot((PrefixUnaryExpressionSyntax)expr, out trueStates, out falseStates);
                        break;

                    default:
                        this.Visit(expr);
                        break;
                }
            }

            private void VisitLogicalNot(PrefixUnaryExpressionSyntax unop,
                out ImmutableDictionary<object, NullState> trueStates,
                out ImmutableDictionary<object, NullState> falseStates)
            {
                // intentionally reverses out parameters
                this.VisitCondition(unop.Operand, out falseStates, out trueStates);
            }

            private void VisitParenthesizedExpression(ParenthesizedExpressionSyntax expr,
                out ImmutableDictionary<object, NullState> trueStates,
                out ImmutableDictionary<object, NullState> falseStates)
            {
                this.VisitCondition(expr.Expression, out trueStates, out falseStates);
            }

            public override void VisitBinaryExpression(BinaryExpressionSyntax node)
            {
                ImmutableDictionary<object, NullState> trueStates;
                ImmutableDictionary<object, NullState> falseStates;
                this.VisitBinaryExpression(node, out trueStates, out falseStates);
            }

            private void VisitBinaryExpression(BinaryExpressionSyntax binop,
                out ImmutableDictionary<object, NullState> trueStates,
                out ImmutableDictionary<object, NullState> falseStates)
            {
                trueStates = this.current;
                falseStates = this.current;

                switch (binop.Kind())
                {
                    case SyntaxKind.EqualsExpression:
                    case SyntaxKind.NotEqualsExpression:
                        this.VisitEquality(binop, out trueStates, out falseStates);
                        break;

                    case SyntaxKind.LogicalAndExpression:
                        this.VisitLogicalAnd(binop, out trueStates, out falseStates);
                        break;

                    case SyntaxKind.LogicalOrExpression:
                        this.VisitLogicalOr(binop, out trueStates, out falseStates);
                        break;

                    default:
                        base.VisitBinaryExpression(binop);
                        break;
                }
            }

            private void VisitLogicalAnd(BinaryExpressionSyntax binop,
                out ImmutableDictionary<object, NullState> trueStates,
                out ImmutableDictionary<object, NullState> falseStates)
            {
                VisitCondition(binop.Left, out trueStates, out falseStates);

                var oldStates = this.current;

                this.current = trueStates;
                VisitCondition(binop.Right, out trueStates, out falseStates);

                this.current = oldStates;
            }

            private void VisitLogicalOr(BinaryExpressionSyntax binop,
                out ImmutableDictionary<object, NullState> trueStates,
                out ImmutableDictionary<object, NullState> falseStates)
            {
                VisitCondition(binop.Left, out trueStates, out falseStates);

                var oldStates = this.current;

                this.current = falseStates;
                VisitCondition(binop.Right, out trueStates, out falseStates);

                this.current = oldStates;
                trueStates = oldStates;
                falseStates = oldStates;
            }

            private void VisitEquality(BinaryExpressionSyntax binop,
                out ImmutableDictionary<object, NullState> trueStates,
                out ImmutableDictionary<object, NullState> falseStates)
            {
                trueStates = current;
                falseStates = current;
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
                            trueStates = trueStates.SetItem(influencedExpr, NullState.Null);
                            falseStates = falseStates.SetItem(influencedExpr, NullState.NotNull);
                            break;

                        case SyntaxKind.NotEqualsExpression:
                            trueStates = trueStates.SetItem(influencedExpr, NullState.NotNull);
                            falseStates = falseStates.SetItem(influencedExpr, NullState.Null);
                            break;
                    }
                }

                base.VisitBinaryExpression(binop);
            }

            private void SetNullState(ExpressionSyntax expr, NullState state)
            {
                expr = WithoutParens(expr);
                this.current = this.current.SetItem(expr, state);
                this.assigned = this.assigned.SetItem(expr, state);
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
                        this.current = this.current.SetItem(symbol, state);
                        this.assigned = this.assigned.SetItem(symbol, state);
                        break;
                }
            }

            private NullState GetNullState(ExpressionSyntax expression)
            {
                if (expression != null)
                {
                    expression = WithoutParens(expression);

                    NullState state;
                    if (this.current.TryGetValue(expression, out state))
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
                if (current.TryGetValue(symbol, out state))
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

            private NullState GetDeclaredNullState(ISymbol symbol)
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

            private ImmutableDictionary<object, NullState> Add(ImmutableDictionary<object, NullState> a, ImmutableDictionary<object, NullState> b)
            {
                foreach (var kvp in b)
                {
                    a = a.SetItem(kvp.Key, kvp.Value);
                }

                return a;
            }

            private ImmutableDictionary<object, NullState> Weaker(ImmutableDictionary<object, NullState> a, ImmutableDictionary<object, NullState> b)
            {
                var result = a;
                Weaker(a, b, ref result);
                Weaker(b, a, ref result);
                return result;
            }

            /// <summary>
            /// Merges null states accumulated across separate code paths
            /// </summary>
            private void Weaker(ImmutableDictionary<object, NullState> a, ImmutableDictionary<object, NullState> b, ref ImmutableDictionary<object, NullState> result)
            {
                // for all items in a
                foreach (var kvp in a)
                {
                    NullState bs;
                    if (!b.TryGetValue(kvp.Key, out bs))
                    {
                        bs = GetDeclaredNullState(kvp.Key);
                    }

                    var w = Weaker(kvp.Value, bs);

                    if (kvp.Value != w)
                    {
                        result = result.SetItem(kvp.Key, w);
                    }
                }
            }

            private NullState Weaker(NullState a, NullState b)
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
                            case NullState.CouldBeNull:
                            case NullState.Null:
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
