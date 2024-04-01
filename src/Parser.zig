const std = @import("std");

const Token = @import("Token.zig");
const TokenType = Token.TokenType;
const Lexer = @import("Lexer.zig");

const Ast = struct {
    usingnamespace @import("Ast.zig");
    usingnamespace @import("Expression.zig");
    usingnamespace @import("Statement.zig");
};

const Parser = @This();

pub const ParserError = error{
    UnknownConstruct,
    UnexpectedToken,
    NoPrefixParseFunction,
    ConversionFailed,
};

const OperatorPrecedence = enum(u8) {
    lowest,
    equals, // ==
    lessGreater, // > or <
    sum, // +
    product, // *
    prefix, // -x or !x
    call, // foo(x)
    index, // array[index]
};

const prefixParseFn = *const fn (*Parser) *Ast.Expression;
const infixParseFn = *const fn (*Parser, *Ast.Expression) *Ast.Expression;

allocator: std.mem.Allocator,
lexer: Lexer,
cur_token: Token = undefined,
next_token: Token = undefined,
errors: std.ArrayList([]const u8),
prefix_parse_fns: std.AutoHashMap(TokenType, prefixParseFn),
infix_parse_fns: std.AutoHashMap(TokenType, infixParseFn),

pub fn init(allocator: std.mem.Allocator, lexer: Lexer) !Parser {
    var parser = Parser{
        .allocator = allocator,
        .lexer = lexer,
        .errors = std.ArrayList([]const u8).init(allocator),
        .prefix_parse_fns = std.AutoHashMap(TokenType, prefixParseFn).init(allocator),
        .infix_parse_fns = std.AutoHashMap(TokenType, infixParseFn).init(allocator),
    };

    try parser.registerPrefix(.ident, parseIdentifier);
    try parser.registerPrefix(.int, parseIntegerLiteral);
    try parser.registerPrefix(.bang, parsePrefixExpression);
    try parser.registerPrefix(.minus, parsePrefixExpression);
    try parser.registerPrefix(.true, parseBooleanExpression);
    try parser.registerPrefix(.false, parseBooleanExpression);
    try parser.registerPrefix(.lparen, parseGroupedExpression);
    try parser.registerPrefix(.@"if", parseIfExpression);
    try parser.registerPrefix(.function, parseFunctionLiteral);
    try parser.registerPrefix(.string, parseStringLiteral);
    try parser.registerPrefix(.lbracket, parseArrayLiteral);
    try parser.registerPrefix(.lbrace, parseHashLiteral);

    try parser.registerInfix(.plus, parseInfixExpression);
    try parser.registerInfix(.minus, parseInfixExpression);
    try parser.registerInfix(.slash, parseInfixExpression);
    try parser.registerInfix(.asterisk, parseInfixExpression);
    try parser.registerInfix(.eq, parseInfixExpression);
    try parser.registerInfix(.noteq, parseInfixExpression);
    try parser.registerInfix(.lt, parseInfixExpression);
    try parser.registerInfix(.gt, parseInfixExpression);
    try parser.registerInfix(.lparen, parseCallExpression);
    try parser.registerInfix(.lbracket, parseIndexExpression);

    parser.nextToken();
    parser.nextToken();
    return parser;
}

pub fn deinit(self: *Parser) void {
    self.errors.deinit();
    self.prefix_parse_fns.deinit();
    self.infix_parse_fns.deinit();
}

pub fn registerPrefix(self: *Parser, token_type: TokenType, func: prefixParseFn) !void {
    try self.prefix_parse_fns.put(token_type, func);
}

pub fn registerInfix(self: *Parser, token_type: TokenType, func: infixParseFn) !void {
    try self.infix_parse_fns.put(token_type, func);
}

pub fn nextToken(self: *Parser) void {
    self.cur_token = self.next_token;
    self.next_token = self.lexer.nextToken();
}

fn curTokenIs(self: Parser, token_type: TokenType) bool {
    return self.cur_token.type == token_type;
}

fn nextTokenIs(self: Parser, token_type: TokenType) bool {
    return self.next_token.type == token_type;
}

fn curPrecedence(self: Parser) OperatorPrecedence {
    return tokenPrecedence(self.cur_token.type);
}

fn peekPrecedence(self: Parser) OperatorPrecedence {
    return tokenPrecedence(self.next_token.type);
}

fn tokenPrecedence(token_type: TokenType) OperatorPrecedence {
    return switch (token_type) {
        .eq => .equals,
        .noteq => .equals,
        .lt => .lessGreater,
        .gt => .lessGreater,
        .plus => .sum,
        .minus => .sum,
        .asterisk => .product,
        .slash => .product,
        .lparen => .call,
        .lbracket => .index,
        else => .lowest,
    };
}

pub fn parseProgram(self: *Parser) *Ast.Node {
    var node = Ast.Program.init(self.allocator);

    while (!self.curTokenIs(.eof)) {
        const stmt = self.parseStatement();
        node.program.statements.append(stmt) catch @panic("OOM");
        self.nextToken();
    }

    return node;
}

fn parseStatement(self: *Parser) *Ast.Statement {
    return switch (self.cur_token.type) {
        .let => self.parseLetStatement(),
        .@"return" => self.parseReturnStatement(),
        else => self.parseExpressionStatement(),
    };
}

fn parseLetStatement(self: *Parser) *Ast.Statement {
    const ls = Ast.LetStatement.init(self.allocator, self.cur_token);

    if (self.nextTokenIs(.ident)) {
        self.nextToken();
    } else {
        return self.peekStatementError(.ident);
    }

    const name = self.allocator.create(Ast.Identifier) catch @panic("OOM");
    name.token = self.cur_token;
    name.value = self.cur_token.literal;

    ls.name = name;

    if (self.nextTokenIs(.assign)) {
        self.nextToken();
    } else {
        return self.peekStatementError(.assign);
    }

    self.nextToken();

    ls.value = self.parseExpression(.lowest);

    if (self.nextTokenIs(.semicolon)) {
        self.nextToken();
    }

    const s = self.allocator.create(Ast.Statement) catch @panic("OOM");
    s.* = Ast.Statement{ .let_statement = ls };
    return s;
}

fn parseReturnStatement(self: *Parser) *Ast.Statement {
    const rs = Ast.ReturnStatement.init(self.allocator, self.cur_token);

    self.nextToken();

    rs.return_value = self.parseExpression(.lowest);

    if (self.nextTokenIs(.semicolon)) {
        self.nextToken();
    }

    const s = self.allocator.create(Ast.Statement) catch @panic("OOM");
    s.* = Ast.Statement{ .return_statement = rs };
    return s;
}

fn parseExpressionStatement(self: *Parser) *Ast.Statement {
    const es = Ast.ExpressionStatement.init(self.allocator, self.cur_token);
    es.expression = self.parseExpression(.lowest);

    if (self.nextTokenIs(.semicolon)) {
        self.nextToken();
    }

    const s = self.allocator.create(Ast.Statement) catch @panic("OOM");
    s.* = Ast.Statement{ .expression_statement = es };
    return s;
}

fn parseExpression(self: *Parser, precedence: OperatorPrecedence) *Ast.Expression {
    const prefixFn = self.prefix_parse_fns.get(self.cur_token.type) orelse {
        return self.noPrefixParseFnError(self.cur_token.type);
    };
    var leftExpr = prefixFn(self);

    while (!self.nextTokenIs(.semicolon) and @intFromEnum(precedence) < @intFromEnum(self.peekPrecedence())) {
        const infixFn = self.infix_parse_fns.get(self.next_token.type) orelse {
            return leftExpr;
        };

        self.nextToken();
        leftExpr = infixFn(self, leftExpr);
    }

    return leftExpr;
}

fn parseIdentifier(self: *Parser) *Ast.Expression {
    const ident = self.allocator.create(Ast.Identifier) catch @panic("OOM");
    ident.token = self.cur_token;
    ident.value = self.cur_token.literal;

    const e = self.allocator.create(Ast.Expression) catch @panic("OOM");
    e.* = Ast.Expression{ .identifier = ident };
    return e;
}

fn parseIntegerLiteral(self: *Parser) *Ast.Expression {
    const ilit = self.allocator.create(Ast.IntegerLiteral) catch @panic("OOM");
    ilit.token = self.cur_token;

    ilit.value = std.fmt.parseInt(i64, self.cur_token.literal, 10) catch {
        return self.conversionError(self.cur_token.literal);
    };

    const e = self.allocator.create(Ast.Expression) catch @panic("OOM");
    e.* = Ast.Expression{ .integer_literal = ilit };
    return e;
}

fn parsePrefixExpression(self: *Parser) *Ast.Expression {
    const pe = self.allocator.create(Ast.PrefixExpression) catch @panic("OOM");
    pe.token = self.cur_token;
    pe.operator = self.cur_token.literal;

    self.nextToken();

    pe.right = self.parseExpression(.prefix);

    const e = self.allocator.create(Ast.Expression) catch @panic("OOM");
    e.* = Ast.Expression{ .prefix_expression = pe };
    return e;
}

fn parseInfixExpression(self: *Parser, left: *Ast.Expression) *Ast.Expression {
    const ie = self.allocator.create(Ast.InfixExpression) catch @panic("OOM");
    ie.token = self.cur_token;
    ie.operator = self.cur_token.literal;
    ie.left = left;

    const precedence = self.curPrecedence();

    self.nextToken();

    ie.right = self.parseExpression(precedence);

    const e = self.allocator.create(Ast.Expression) catch @panic("OOM");
    e.* = Ast.Expression{ .infix_expression = ie };
    return e;
}

fn parseBooleanExpression(self: *Parser) *Ast.Expression {
    const b = self.allocator.create(Ast.BooleanExpression) catch @panic("OOM");
    b.token = self.cur_token;
    b.value = self.curTokenIs(.true);

    const e = self.allocator.create(Ast.Expression) catch @panic("OOM");
    e.* = Ast.Expression{ .boolean = b };
    return e;
}

fn parseGroupedExpression(self: *Parser) *Ast.Expression {
    self.nextToken();

    const ge = self.parseExpression(.lowest);

    if (self.nextTokenIs(.rparen)) {
        self.nextToken();
    } else {
        return self.peekExpressionError(.rparen);
    }

    return ge;
}

fn parseIfExpression(self: *Parser) *Ast.Expression {
    const ie = self.allocator.create(Ast.IfExpression) catch @panic("OOM");
    ie.token = self.cur_token;
    ie.alternative = null;

    if (self.nextTokenIs(.lparen)) {
        self.nextToken();
    } else {
        return self.peekExpressionError(.lparen);
    }

    self.nextToken();

    ie.condition = self.parseExpression(.lowest);

    if (self.nextTokenIs(.rparen)) {
        self.nextToken();
    } else {
        return self.peekExpressionError(.rparen);
    }

    if (self.nextTokenIs(.lbrace)) {
        self.nextToken();
    } else {
        return self.peekExpressionError(.lbrace);
    }

    ie.consequence = self.parseBlockStatement();

    if (self.nextTokenIs(.@"else")) {
        self.nextToken();

        if (self.nextTokenIs(.lbrace)) {
            self.nextToken();
        } else {
            return self.peekExpressionError(.lbrace);
        }

        ie.alternative = self.parseBlockStatement();
    }

    const e = self.allocator.create(Ast.Expression) catch @panic("OOM");
    e.* = Ast.Expression{ .if_expression = ie };
    return e;
}

fn parseBlockStatement(self: *Parser) *Ast.BlockStatement {
    var bs = Ast.BlockStatement.init(self.allocator, self.cur_token);
    self.nextToken();

    while (!self.curTokenIs(.rbrace) and !self.curTokenIs(.eof)) {
        const stmt = self.parseStatement();
        bs.statements.append(stmt) catch @panic("OOM");
        self.nextToken();
    }
    return bs;
}

fn parseFunctionLiteral(self: *Parser) *Ast.Expression {
    var fl = Ast.FunctionLiteral.init(self.allocator, self.cur_token);

    if (self.nextTokenIs(.lparen)) {
        self.nextToken();
    } else {
        return self.peekExpressionError(.lparen);
    }

    self.parseFunctionParameters(&fl.parameters);

    if (self.nextTokenIs(.lbrace)) {
        self.nextToken();
    } else {
        return self.peekExpressionError(.lbrace);
    }

    fl.body = self.parseBlockStatement();

    const e = self.allocator.create(Ast.Expression) catch @panic("OOM");
    e.* = Ast.Expression{ .function_literal = fl };
    return e;
}

fn parseFunctionParameters(self: *Parser, params: *std.ArrayList(*Ast.Identifier)) void {
    if (self.nextTokenIs(.rparen)) {
        self.nextToken();
        return;
    }

    self.nextToken();

    const ident1 = self.allocator.create(Ast.Identifier) catch @panic("OOM");
    ident1.token = self.cur_token;
    ident1.value = self.cur_token.literal;
    params.append(ident1) catch @panic("OOM");

    while (self.nextTokenIs(.comma)) {
        self.nextToken();
        self.nextToken();

        const ident2 = self.allocator.create(Ast.Identifier) catch @panic("OOM");
        ident2.token = self.cur_token;
        ident2.value = self.cur_token.literal;
        params.append(ident2) catch @panic("OOM");
    }

    if (self.nextTokenIs(.rparen)) {
        self.nextToken();
    } else {
        _ = self.peekExpressionError(.rparen);
    }
}

fn parseCallExpression(self: *Parser, function: *Ast.Expression) *Ast.Expression {
    var call_expr = Ast.CallExpression.init(self.allocator, self.cur_token, function);
    call_expr.arguments = self.parseExpressionList(.rparen);

    const e = self.allocator.create(Ast.Expression) catch @panic("OOM");
    e.* = Ast.Expression{ .call_expression = call_expr };
    return e;
}

fn parseCallArguments(self: *Parser, args: *std.ArrayList(*Ast.Expression)) void {
    if (self.nextTokenIs(.rparen)) {
        self.nextToken();
        return;
    }

    self.nextToken();
    var expr = self.parseExpression(.lowest);
    args.append(expr) catch @panic("OOM");

    while (self.nextTokenIs(.comma)) {
        self.nextToken();
        self.nextToken();
        expr = self.parseExpression(.lowest);
        args.append(expr) catch @panic("OOM");
    }

    if (self.nextTokenIs(.rparen)) {
        self.nextToken();
    } else {
        return self.peekExpressionError(.rparen);
    }
}

fn parseStringLiteral(self: *Parser) *Ast.Expression {
    const s = self.allocator.create(Ast.StringLiteral) catch @panic("OOM");
    s.token = self.cur_token;
    s.value = self.cur_token.literal;

    const e = self.allocator.create(Ast.Expression) catch @panic("OOM");
    e.* = Ast.Expression{ .string_literal = s };
    return e;
}

fn parseArrayLiteral(self: *Parser) *Ast.Expression {
    const a = self.allocator.create(Ast.ArrayLiteral) catch @panic("OOM");
    a.token = self.cur_token;

    a.elements = self.parseExpressionList(.rbracket);

    const e = self.allocator.create(Ast.Expression) catch @panic("OOM");
    e.* = Ast.Expression{ .array_literal = a };
    return e;
}

fn parseExpressionList(self: *Parser, end: TokenType) std.ArrayList(*Ast.Expression) {
    var list = std.ArrayList(*Ast.Expression).init(self.allocator);

    if (self.nextTokenIs(end)) {
        self.nextToken();
        return list;
    }

    self.nextToken();
    list.append(self.parseExpression(.lowest)) catch @panic("OOM");

    while (self.nextTokenIs(.comma)) {
        self.nextToken();
        self.nextToken();
        list.append(self.parseExpression(.lowest)) catch @panic("OOM");
    }

    if (self.nextTokenIs(end)) {
        self.nextToken();
    } else {
        _ = self.peekExpressionError(end);
    }
    return list;
}

fn parseIndexExpression(self: *Parser, left: *Ast.Expression) *Ast.Expression {
    const ie = self.allocator.create(Ast.IndexExpression) catch @panic("OOM");
    ie.token = self.cur_token;
    ie.left = left;

    self.nextToken();
    ie.index = self.parseExpression(.lowest);

    if (self.nextTokenIs(.rbracket)) {
        self.nextToken();
    } else {
        return self.peekExpressionError(.rbracket);
    }

    const e = self.allocator.create(Ast.Expression) catch @panic("OOM");
    e.* = Ast.Expression{ .index_expression = ie };
    return e;
}

fn parseHashLiteral(self: *Parser) *Ast.Expression {
    const h = self.allocator.create(Ast.HashLiteral) catch @panic("OOM");
    h.token = self.cur_token;
    h.pairs = std.AutoHashMap(*Ast.Expression, *Ast.Expression).init(self.allocator);

    while (!self.nextTokenIs(.rbrace)) {
        self.nextToken();
        const key = self.parseExpression(.lowest);

        if (self.nextTokenIs(.colon)) {
            self.nextToken();
        } else {
            return self.peekExpressionError(.colon);
        }

        self.nextToken();
        const value = self.parseExpression(.lowest);

        h.pairs.put(key, value) catch @panic("OOM");

        if (!self.nextTokenIs(.rbrace)) {
            if (!self.nextTokenIs(.comma)) {
                return self.peekExpressionError(.comma);
            } else {
                self.nextToken();
            }
        }
    }

    if (self.nextTokenIs(.rbrace)) {
        self.nextToken();
    } else {
        return self.peekExpressionError(.rbrace);
    }

    const e = self.allocator.create(Ast.Expression) catch @panic("OOM");
    e.* = Ast.Expression{ .hash_literal = h };
    return e;
}

// Errors

fn peekStatementError(self: *Parser, expected: TokenType) *Ast.Statement {
    const fmt = "expected next token to be {any}, got {any} instead";
    const msg = std.fmt.allocPrint(self.allocator, fmt, .{ expected, self.next_token.type }) catch @panic("OOM");
    self.errors.append(msg) catch @panic("OOM");

    const s = self.allocator.create(Ast.Statement) catch @panic("OOM");
    s.* = Ast.Statement{ .@"error" = ParserError.UnexpectedToken };
    return s;
}

fn peekExpressionError(self: *Parser, expected: TokenType) *Ast.Expression {
    const fmt = "expected next token to be {any}, got {any} instead";
    const msg = std.fmt.allocPrint(self.allocator, fmt, .{ expected, self.next_token.type }) catch @panic("OOM");
    self.errors.append(msg) catch @panic("OOM");

    const e = self.allocator.create(Ast.Expression) catch @panic("OOM");
    e.* = Ast.Expression{ .@"error" = ParserError.UnexpectedToken };
    return e;
}

fn noPrefixParseFnError(self: *Parser, token_type: TokenType) *Ast.Expression {
    const fmt = "no prefix parse function for {any} found";
    const msg = std.fmt.allocPrint(self.allocator, fmt, .{token_type}) catch @panic("OOM");
    self.errors.append(msg) catch @panic("OOM");

    const e = self.allocator.create(Ast.Expression) catch @panic("OOM");
    e.* = Ast.Expression{ .@"error" = ParserError.NoPrefixParseFunction };
    return e;
}

fn conversionError(self: *Parser, literal: []const u8) *Ast.Expression {
    const fmt = "could not parse {s} as integer";
    const msg = std.fmt.allocPrint(self.allocator, fmt, .{literal}) catch @panic("OOM");
    self.errors.append(msg) catch @panic("OOM");

    const e = self.allocator.create(Ast.Expression) catch @panic("OOM");
    e.* = Ast.Expression{ .@"error" = ParserError.ConversionFailed };
    return e;
}

// tests

pub fn checkParserErrors(parser: Parser) void {
    if (parser.errors.items.len == 0) {
        return;
    }
    std.debug.print("\nparser has {d} errors\n", .{parser.errors.items.len});
    for (parser.errors.items) |msg| {
        std.debug.print("parser error: {s}\n", .{msg});
    }
    @panic("some error occurred");
}

test "TestLetStatement" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;
    const expected_idents = [_][]const u8{
        "x",
        "y",
        "foobar",
    };

    const expected_vals = [_]i64{ 5, 10, 838383 };

    const lexer = Lexer.init(input);
    var parser = try Parser.init(std.testing.allocator, lexer);
    defer parser.deinit();
    var node = parser.parseProgram();
    defer node.deinit();

    checkParserErrors(parser);

    try std.testing.expectEqual(@as(usize, 3), node.program.statements.items.len);

    for (expected_idents, expected_vals, node.program.statements.items) |ident, val, stmt| {
        try std.testing.expectEqualStrings("let", stmt.tokenLiteral());
        try std.testing.expect(.let == stmt.let_statement.token.type);
        try std.testing.expectEqualStrings("let", stmt.let_statement.token.literal);
        try std.testing.expectEqualStrings(ident, stmt.let_statement.name.value);
        try std.testing.expectEqualStrings(ident, stmt.let_statement.name.token.literal);
        try std.testing.expectEqual(val, stmt.let_statement.value.integer_literal.value);
    }
}

test "TestReturnStatement" {
    const input =
        \\return 5;
        \\return 10;
        \\return 993322;
    ;
    const expected_vals = [_]i64{ 5, 10, 993322 };

    const lexer = Lexer.init(input);
    var parser = try Parser.init(std.testing.allocator, lexer);
    defer parser.deinit();
    var node = parser.parseProgram();
    defer node.deinit();

    checkParserErrors(parser);

    try std.testing.expectEqual(@as(usize, 3), node.program.statements.items.len);

    for (expected_vals, node.program.statements.items) |val, stmt| {
        try std.testing.expectEqualStrings("return", stmt.tokenLiteral());
        try std.testing.expect(.@"return" == stmt.return_statement.token.type);
        try std.testing.expectEqualStrings("return", stmt.return_statement.token.literal);
        try std.testing.expectEqual(val, stmt.return_statement.return_value.integer_literal.value);
    }
}

test "TestIndentiferExpression" {
    const input = "foobar;";

    const lexer = Lexer.init(input);
    var parser = try Parser.init(std.testing.allocator, lexer);
    defer parser.deinit();
    var node = parser.parseProgram();
    defer node.deinit();

    checkParserErrors(parser);

    try std.testing.expectEqual(@as(usize, 1), node.program.statements.items.len);

    const stmt = node.program.statements.items[0];

    try std.testing.expectEqualStrings("foobar", stmt.tokenLiteral());
    try std.testing.expectEqualStrings("foobar", stmt.expression_statement.tokenLiteral());
    try std.testing.expectEqualStrings("foobar", stmt.expression_statement.expression.tokenLiteral());
    try std.testing.expectEqualStrings("foobar", stmt.expression_statement.expression.identifier.tokenLiteral());
    try std.testing.expectEqualStrings("foobar", stmt.expression_statement.expression.identifier.value);
}

test "TestIntegerLiteralExpression" {
    const input = "5;";

    const lexer = Lexer.init(input);
    var parser = try Parser.init(std.testing.allocator, lexer);
    defer parser.deinit();
    var node = parser.parseProgram();
    defer node.deinit();

    checkParserErrors(parser);

    try std.testing.expectEqual(@as(usize, 1), node.program.statements.items.len);

    const stmt = node.program.statements.items[0];
    const expr = stmt.expression_statement.expression;
    {
        const expected = "5";
        const actual = expr.tokenLiteral();
        try std.testing.expectEqualStrings(expected, actual);
    }
    {
        const expected: i64 = 5;
        const actual = expr.integer_literal.value;
        try std.testing.expectEqual(expected, actual);
    }
}

test "TestPrefixExpression" {
    const Test = struct { []const u8, []const u8, i64 };
    const tests = [_]Test{
        .{ "!5;", "!", 5 },
        .{ "-15;", "-", 15 },
    };

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        var node = parser.parseProgram();
        defer node.deinit();

        checkParserErrors(parser);

        try std.testing.expectEqual(@as(usize, 1), node.program.statements.items.len);

        const stmt = node.program.statements.items[0];
        const prefixExpr = stmt.expression_statement.expression.prefix_expression;
        {
            const expected = t[1];
            const actual = prefixExpr.operator;
            try std.testing.expectEqualStrings(expected, actual);
        }
        {
            const expected = t[2];
            const actual = prefixExpr.right.integer_literal.value;
            try std.testing.expectEqual(expected, actual);
        }
    }
}

test "TestParsingInfixExpressions" {
    const Test = struct { []const u8, i64, []const u8, i64 };
    const tests = [_]Test{
        .{ "5 + 5;", 5, "+", 5 },
        .{ "5 - 5;", 5, "-", 5 },
        .{ "5 * 5;", 5, "*", 5 },
        .{ "5 / 5;", 5, "/", 5 },
        .{ "5 > 5;", 5, ">", 5 },
        .{ "5 < 5;", 5, "<", 5 },
        .{ "5 == 5;", 5, "==", 5 },
        .{ "5 != 5;", 5, "!=", 5 },
    };

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        var node = parser.parseProgram();
        defer node.deinit();

        checkParserErrors(parser);

        try std.testing.expectEqual(@as(usize, 1), node.program.statements.items.len);

        const stmt = node.program.statements.items[0];
        const infix_expr = stmt.expression_statement.expression.infix_expression;
        {
            const expected = t[1];
            const actual = infix_expr.left.integer_literal.value;
            try std.testing.expectEqual(expected, actual);
        }
        {
            const expected = t[2];
            const actual = infix_expr.operator;
            try std.testing.expectEqualStrings(expected, actual);
        }
        {
            const expected = t[3];
            const actual = infix_expr.right.integer_literal.value;
            try std.testing.expectEqual(expected, actual);
        }
    }
}

test "TestOperatorPrecedenceParsing" {
    const Test = struct { []const u8, []const u8 };
    const tests = [_]Test{
        .{ "-a * b", "((-a) * b)" },
        .{ "!-a", "(!(-a))" },
        .{ "a + b + c", "((a + b) + c)" },
        .{ "a + b - c", "((a + b) - c)" },
        .{ "a * b * c", "((a * b) * c)" },
        .{ "a * b / c", "((a * b) / c)" },
        .{ "a + b / c", "(a + (b / c))" },
        .{ "a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)" },
        .{ "3 + 4; -5 * 5", "(3 + 4)((-5) * 5)" },
        .{ "5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))" },
        .{ "5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))" },
        .{ "3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))" },

        .{ "true", "true" },
        .{ "false", "false" },
        .{ "3 > 5 == false", "((3 > 5) == false)" },
        .{ "3 < 5 == true", "((3 < 5) == true)" },

        .{
            "1 + (2 + 3) + 4",
            "((1 + (2 + 3)) + 4)",
        },
        .{
            "(5 + 5) * 2",
            "((5 + 5) * 2)",
        },
        .{
            "2 / (5 + 5)",
            "(2 / (5 + 5))",
        },
        .{
            "-(5 + 5)",
            "(-(5 + 5))",
        },
        .{
            "!(true == true)",
            "(!(true == true))",
        },

        .{
            "a + add(b * c) + d",
            "((a + add((b * c))) + d)",
        },
        .{
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        },
        .{
            "add(a + b + c * d / f + g)",
            "add((((a + b) + ((c * d) / f)) + g))",
        },

        .{
            "a * [1, 2, 3, 4][b * c] * d",
            "((a * ([1, 2, 3, 4][(b * c)])) * d)",
        },
        .{
            "add(a * b[2], b[1], 2 * [1, 2][1])",
            "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
        },
    };

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        var node = parser.parseProgram();
        defer node.deinit();

        checkParserErrors(parser);

        var buffer = std.ArrayList(u8).init(std.testing.allocator);
        defer buffer.deinit();

        try node.program.string(buffer.writer());
        try std.testing.expectEqualStrings(t[1], buffer.items);
    }
}

test "TestParsingInfixExpressionsWithBool" {
    const Test = struct {
        []const u8,
        bool,
        []const u8,
        bool,
    };
    const tests = [_]Test{
        .{ "true == true", true, "==", true },
        .{ "true != false", true, "!=", false },
        .{ "false == false", false, "==", false },
    };

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        var node = parser.parseProgram();
        defer node.deinit();

        checkParserErrors(parser);

        try std.testing.expectEqual(@as(usize, 1), node.program.statements.items.len);

        const stmt = node.program.statements.items[0];
        const infix_expr = stmt.expression_statement.expression.infix_expression;
        {
            const expected = t[1];
            const actual = infix_expr.left.boolean.value;
            try std.testing.expectEqual(expected, actual);
        }
        {
            const expected = t[2];
            const actual = infix_expr.operator;
            try std.testing.expectEqualStrings(expected, actual);
        }
        {
            const expected = t[3];
            const actual = infix_expr.right.boolean.value;
            try std.testing.expectEqual(expected, actual);
        }
    }
}

test "TestParsingPrefixExpressions" {
    const Test = struct {
        []const u8,
        []const u8,
        bool,
    };
    const tests = [_]Test{
        .{ "!true;", "!", true },
        .{ "!false;", "!", false },
    };

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        var node = parser.parseProgram();
        defer node.deinit();

        checkParserErrors(parser);

        try std.testing.expectEqual(@as(usize, 1), node.program.statements.items.len);

        const stmt = node.program.statements.items[0];
        const prefix_expr = stmt.expression_statement.expression.prefix_expression;
        {
            const expected = t[1];
            const actual = prefix_expr.operator;
            try std.testing.expectEqualStrings(expected, actual);
        }
        {
            const expected = t[2];
            const actual = prefix_expr.right.boolean.value;
            try std.testing.expectEqual(expected, actual);
        }
    }
}

test "TestIfExpression" {
    const input = "if (x < y) { x }";

    const lexer = Lexer.init(input);
    var parser = try Parser.init(std.testing.allocator, lexer);
    defer parser.deinit();
    var node = parser.parseProgram();
    defer node.deinit();

    checkParserErrors(parser);

    try std.testing.expectEqual(@as(usize, 1), node.program.statements.items.len);

    const stmt = node.program.statements.items[0];
    const if_expr = stmt.expression_statement.expression.if_expression;
    const condition_expr = if_expr.condition.infix_expression;
    {
        try std.testing.expectEqualStrings("if", stmt.tokenLiteral());
    }
    {
        const expected = "x";
        const actual = condition_expr.left.identifier.value;
        try std.testing.expectEqualStrings(expected, actual);
    }
    {
        const expected = "<";
        const actual = condition_expr.operator;
        try std.testing.expectEqualStrings(expected, actual);
    }
    {
        const expected = "y";
        const actual = condition_expr.right.identifier.value;
        try std.testing.expectEqualStrings(expected, actual);
    }
    try std.testing.expectEqual(@as(usize, 1), if_expr.consequence.statements.items.len);
    {
        const expected = "x";
        const actual = if_expr.consequence.statements.items[0].tokenLiteral();
        try std.testing.expectEqualStrings(expected, actual);
    }
    {
        try std.testing.expect(null == if_expr.alternative);
    }
}

test "TestIfElseExpression" {
    const input = "if (x < y) { x } else { y }";

    const lexer = Lexer.init(input);
    var parser = try Parser.init(std.testing.allocator, lexer);
    defer parser.deinit();
    var node = parser.parseProgram();
    defer node.deinit();

    checkParserErrors(parser);

    try std.testing.expectEqual(@as(usize, 1), node.program.statements.items.len);

    const stmt = node.program.statements.items[0];
    const if_expr = stmt.expression_statement.expression.if_expression;
    const condition_expr = if_expr.condition.infix_expression;
    {
        try std.testing.expectEqualStrings("if", stmt.tokenLiteral());
    }
    {
        const expected = "x";
        const actual = condition_expr.left.identifier.value;
        try std.testing.expectEqualStrings(expected, actual);
    }
    {
        const expected = "<";
        const actual = condition_expr.operator;
        try std.testing.expectEqualStrings(expected, actual);
    }
    {
        const expected = "y";
        const actual = condition_expr.right.identifier.value;
        try std.testing.expectEqualStrings(expected, actual);
    }
    try std.testing.expectEqual(@as(usize, 1), if_expr.consequence.statements.items.len);
    {
        const expected = "x";
        const actual = if_expr.consequence.statements.items[0].tokenLiteral();
        try std.testing.expectEqualStrings(expected, actual);
    }
    if (if_expr.alternative) |alternative| {
        try std.testing.expectEqual(@as(usize, 1), alternative.statements.items.len);
        const alternativeStmt = alternative.statements.items[0];
        try std.testing.expectEqualStrings("y", alternativeStmt.tokenLiteral());
    }
}

test "TestFunctionLiteralParsing" {
    const input = "fn(x, y) { x + y; }";

    const lexer = Lexer.init(input);
    var parser = try Parser.init(std.testing.allocator, lexer);
    defer parser.deinit();
    var node = parser.parseProgram();
    defer node.deinit();

    checkParserErrors(parser);

    try std.testing.expectEqual(@as(usize, 1), node.program.statements.items.len);

    const stmt = node.program.statements.items[0];
    const fn_lit = stmt.expression_statement.expression.function_literal;
    const params = fn_lit.parameters;

    try std.testing.expectEqual(@as(usize, 2), params.items.len);
    {
        const expected = "x";
        const actual = params.items[0].value;
        try std.testing.expectEqualStrings(expected, actual);
    }
    {
        const expected = "y";
        const actual = params.items[1].value;
        try std.testing.expectEqualStrings(expected, actual);
    }
    try std.testing.expectEqual(@as(usize, 1), fn_lit.body.statements.items.len);
    const infix_expr = fn_lit.body.statements.items[0].expression_statement.expression.infix_expression;
    {
        const expected = "x";
        const actual = infix_expr.left.tokenLiteral();
        try std.testing.expectEqualStrings(expected, actual);
    }
    {
        const expected = "+";
        const actual = infix_expr.operator;
        try std.testing.expectEqualStrings(expected, actual);
    }
    {
        const expected = "y";
        const actual = infix_expr.right.tokenLiteral();
        try std.testing.expectEqualStrings(expected, actual);
    }
}

test "TestFunctionParameterParsing" {
    const Test = struct {
        []const u8,
        usize,
    };
    const tests = [_]Test{
        .{ "fn() {};", 0 },
        .{ "fn(x) {};", 1 },
        .{ "fn(x, y, z) {};", 3 },
    };

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        var node = parser.parseProgram();
        defer node.deinit();

        checkParserErrors(parser);

        const stmt = node.program.statements.items[0];
        const fn_lit = stmt.expression_statement.expression.function_literal;
        {
            const expected = t[1];
            const actual = fn_lit.parameters.items.len;
            try std.testing.expectEqual(expected, actual);
        }
    }
}

test "TestCallExpressionParsing" {
    const input = "add(1, 2 * 3, 4 + 5);";

    const lexer = Lexer.init(input);
    var parser = try Parser.init(std.testing.allocator, lexer);
    defer parser.deinit();
    var node = parser.parseProgram();
    defer node.deinit();

    checkParserErrors(parser);

    try std.testing.expectEqual(@as(usize, 1), node.program.statements.items.len);

    const stmt = node.program.statements.items[0];
    const call_expr = stmt.expression_statement.expression.call_expression;
    {
        const expected = "add";
        const actual = call_expr.function.tokenLiteral();
        try std.testing.expectEqualStrings(expected, actual);
    }
    try std.testing.expectEqual(@as(usize, 3), call_expr.arguments.items.len);
    {
        {
            const expected = "1";
            const actual = call_expr.arguments.items[0].tokenLiteral();
            try std.testing.expectEqualStrings(expected, actual);
        }
    }
    {
        const infix_expr = call_expr.arguments.items[1].infix_expression;
        {
            const expected = "2";
            const actual = infix_expr.left.tokenLiteral();
            try std.testing.expectEqualStrings(expected, actual);
        }
        {
            const expected = "*";
            const actual = infix_expr.operator;
            try std.testing.expectEqualStrings(expected, actual);
        }
        {
            const expected = "3";
            const actual = infix_expr.right.tokenLiteral();
            try std.testing.expectEqualStrings(expected, actual);
        }
    }
    {
        const infix_expr = call_expr.arguments.items[2].infix_expression;
        {
            const expected = "4";
            const actual = infix_expr.left.tokenLiteral();
            try std.testing.expectEqualStrings(expected, actual);
        }
        {
            const expected = "+";
            const actual = infix_expr.operator;
            try std.testing.expectEqualStrings(expected, actual);
        }
        {
            const expected = "5";
            const actual = infix_expr.right.tokenLiteral();
            try std.testing.expectEqualStrings(expected, actual);
        }
    }
}

test "TestStringLiteralExpression" {
    const input = "\"hello world\"";

    const lexer = Lexer.init(input);
    var parser = try Parser.init(std.testing.allocator, lexer);
    defer parser.deinit();
    var node = parser.parseProgram();
    defer node.deinit();

    checkParserErrors(parser);

    const stmt = node.program.statements.items[0];
    const literal = stmt.expression_statement.expression.string_literal;
    {
        const expected = "hello world";
        const actual = literal.value;
        try std.testing.expectEqualStrings(expected, actual);
    }
}

test "TestParsingArrayLiterals" {
    const S = struct {
        fn testInfixExpression(expr: *Ast.Expression, a: i64, b: []const u8, c: i64) !void {
            const e = expr.infix_expression;
            try std.testing.expectEqual(e.left.integer_literal.value, a);
            try std.testing.expectEqualStrings(e.operator, b);
            try std.testing.expectEqual(e.right.integer_literal.value, c);
        }
    };
    const input = "[1, 2 * 2, 3 + 3]";

    const lexer = Lexer.init(input);
    var parser = try Parser.init(std.testing.allocator, lexer);
    defer parser.deinit();
    var node = parser.parseProgram();
    defer node.deinit();

    checkParserErrors(parser);

    const stmt = node.program.statements.items[0];
    const array = stmt.expression_statement.expression.array_literal;

    try std.testing.expectEqual(@as(usize, 3), array.elements.items.len);

    {
        try std.testing.expectEqual(1, array.elements.items[0].integer_literal.value);
        try S.testInfixExpression(array.elements.items[1], 2, "*", 2);
        try S.testInfixExpression(array.elements.items[2], 3, "+", 3);
    }
}

test "TestParsingIndexExpressions" {
    const S = struct {
        fn testInfixExpression(expr: *Ast.Expression, a: i64, b: []const u8, c: i64) !void {
            const e = expr.infix_expression;
            try std.testing.expectEqual(e.left.integer_literal.value, a);
            try std.testing.expectEqualStrings(e.operator, b);
            try std.testing.expectEqual(e.right.integer_literal.value, c);
        }
    };
    const input = "myArray[1 + 1]";

    const lexer = Lexer.init(input);
    var parser = try Parser.init(std.testing.allocator, lexer);
    defer parser.deinit();
    var node = parser.parseProgram();
    defer node.deinit();

    checkParserErrors(parser);

    const stmt = node.program.statements.items[0];
    const index_exp = stmt.expression_statement.expression.index_expression;

    {
        try std.testing.expectEqualStrings(index_exp.left.identifier.value, "myArray");
        try S.testInfixExpression(index_exp.index, 1, "+", 1);
    }
}

test "TestParsingHashLiteralsStringKeys" {
    const input =
        \\{"one": 1, "two": 2, "three": 3}
    ;

    const lexer = Lexer.init(input);
    var parser = try Parser.init(std.testing.allocator, lexer);
    defer parser.deinit();
    var node = parser.parseProgram();
    defer node.deinit();

    checkParserErrors(parser);

    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    const stmt = node.program.statements.items[0];
    const hash = stmt.expression_statement.expression.hash_literal;

    try std.testing.expectEqual(@as(usize, 3), hash.pairs.count());

    const expected = std.ComptimeStringMap(i64, .{
        .{ "one", 1 },
        .{ "two", 2 },
        .{ "three", 3 },
    });

    {
        var iterator = hash.pairs.iterator();
        while (iterator.next()) |entry| {
            const literal = entry.key_ptr.*.string_literal;
            try literal.string(buffer.writer());
            const expected_value = expected.get(buffer.items).?;
            const actual = entry.value_ptr.*.integer_literal.value;
            try std.testing.expectEqual(expected_value, actual);
            buffer.clearRetainingCapacity();
        }
    }
}

test "TestParsingEmptyHashLiteral" {
    const input = "{}";

    const lexer = Lexer.init(input);
    var parser = try Parser.init(std.testing.allocator, lexer);
    defer parser.deinit();
    var node = parser.parseProgram();
    defer node.deinit();

    checkParserErrors(parser);

    const stmt = node.program.statements.items[0];
    const hash = stmt.expression_statement.expression.hash_literal;

    try std.testing.expectEqual(@as(usize, 0), hash.pairs.count());
}

test "TestParsingHashLiteralsWithExpressions" {
    const S = struct {
        fn testInfixExpression(expr: *Ast.Expression, a: i64, b: []const u8, c: i64) !void {
            const e = expr.infix_expression;
            try std.testing.expectEqual(e.left.integer_literal.value, a);
            try std.testing.expectEqualStrings(e.operator, b);
            try std.testing.expectEqual(e.right.integer_literal.value, c);
        }
        fn f1(expr: *Ast.Expression) !void {
            try testInfixExpression(expr, 0, "+", 1);
        }
        fn f2(expr: *Ast.Expression) !void {
            try testInfixExpression(expr, 10, "-", 8);
        }
        fn f3(expr: *Ast.Expression) !void {
            try testInfixExpression(expr, 15, "/", 5);
        }
    };
    const input =
        \\{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}
    ;

    const lexer = Lexer.init(input);
    var parser = try Parser.init(std.testing.allocator, lexer);
    defer parser.deinit();
    var node = parser.parseProgram();
    defer node.deinit();

    checkParserErrors(parser);

    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    const stmt = node.program.statements.items[0];
    const hash = stmt.expression_statement.expression.hash_literal;

    try std.testing.expectEqual(@as(usize, 3), hash.pairs.count());

    const tests = std.ComptimeStringMap(*const fn (expr: *Ast.Expression) anyerror!void, .{
        .{ "one", S.f1 },
        .{ "two", S.f2 },
        .{ "three", S.f3 },
    });

    {
        var iterator = hash.pairs.iterator();
        while (iterator.next()) |entry| {
            const literal = entry.key_ptr.*.string_literal;
            try literal.string(buffer.writer());
            const test_func = tests.get(buffer.items).?;
            try test_func(entry.value_ptr.*);
            buffer.clearRetainingCapacity();
        }
    }
}
