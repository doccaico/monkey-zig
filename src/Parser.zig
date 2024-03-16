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
};

const prefixParseFn = *const fn (*Parser) anyerror!Ast.Expression;
const infixParseFn = *const fn (*Parser, *Ast.Expression) anyerror!Ast.Expression;

allocator: std.mem.Allocator,
lexer: Lexer,
cur_token: Token = undefined,
next_token: Token = undefined,
errors: std.ArrayList([]const u8),
prefix_parse_fns: std.AutoHashMap(TokenType, prefixParseFn),
infix_parse_fns: std.AutoHashMap(TokenType, infixParseFn),

pub fn init(allocator: std.mem.Allocator, lexer: Lexer) anyerror!Parser {
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

    try parser.registerInfix(.plus, parseInfixExpression);
    try parser.registerInfix(.minus, parseInfixExpression);
    try parser.registerInfix(.slash, parseInfixExpression);
    try parser.registerInfix(.asterisk, parseInfixExpression);
    try parser.registerInfix(.eq, parseInfixExpression);
    try parser.registerInfix(.noteq, parseInfixExpression);
    try parser.registerInfix(.lt, parseInfixExpression);
    try parser.registerInfix(.gt, parseInfixExpression);
    try parser.registerInfix(.lparen, parseCallExpression);

    parser.nextToken();
    parser.nextToken();
    return parser;
}

pub fn deinit(self: *Parser) void {
    // for (self.errors.items) |err| {
    //     self.allocator.free(err);
    // }
    // _ = self;

    self.errors.deinit();
    self.prefix_parse_fns.deinit();
    self.infix_parse_fns.deinit();
}

pub fn registerPrefix(self: *Parser, token_type: TokenType, func: prefixParseFn) anyerror!void {
    try self.prefix_parse_fns.put(token_type, func);
}

pub fn registerInfix(self: *Parser, token_type: TokenType, func: infixParseFn) anyerror!void {
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
        else => .lowest,
    };
}

pub fn parseProgram(self: *Parser) anyerror!Ast.Program {
    var program = Ast.Program.init(self.allocator);

    while (!self.curTokenIs(.eof)) {
        const stmt = try self.parseStatement();
        try program.statements.append(stmt);
        self.nextToken();
    }

    return program;
}

fn parseStatement(self: *Parser) anyerror!Ast.Statement {
    return switch (self.cur_token.type) {
        .let => try self.parseLetStatement(),
        .@"return" => try self.parseReturnStatement(),
        else => try self.parseExpressionStatement(),
    };
}

fn parseLetStatement(self: *Parser) anyerror!Ast.Statement {
    var ls = Ast.LetStatement{
        .token = self.cur_token,
        .name = undefined,
        .value = undefined,
    };

    if (self.nextTokenIs(.ident)) {
        self.nextToken();
    } else {
        return self.peekStatementError(.ident);
    }

    ls.name = Ast.Identifier{
        .token = self.cur_token,
        .value = self.cur_token.literal,
    };

    if (self.nextTokenIs(.assign)) {
        self.nextToken();
    } else {
        return self.peekStatementError(.assign);
    }

    self.nextToken();

    ls.value = try self.parseExpression(.lowest);

    if (self.nextTokenIs(.semicolon)) {
        self.nextToken();
    }

    return Ast.Statement{ .let_statement = ls };
}

fn parseReturnStatement(self: *Parser) anyerror!Ast.Statement {
    var rs = Ast.ReturnStatement{
        .token = self.cur_token,
        .value = undefined,
    };

    self.nextToken();

    rs.value = try self.parseExpression(.lowest);

    if (self.nextTokenIs(.semicolon)) {
        self.nextToken();
    }

    return Ast.Statement{ .return_statement = rs };
}

fn parseExpressionStatement(self: *Parser) anyerror!Ast.Statement {
    const es = Ast.ExpressionStatement{
        .token = self.cur_token,
        .expression = try self.parseExpression(.lowest),
    };

    if (self.nextTokenIs(.semicolon)) {
        self.nextToken();
    }

    return Ast.Statement{ .expression_statement = es };
}

fn parseExpression(self: *Parser, precedence: OperatorPrecedence) anyerror!Ast.Expression {
    const prefixFn = self.prefix_parse_fns.get(self.cur_token.type) orelse {
        return self.noPrefixParseFnError(self.cur_token.type);
    };
    var leftExpr = try prefixFn(self);

    while (!self.nextTokenIs(.semicolon) and @intFromEnum(precedence) < @intFromEnum(self.peekPrecedence())) {
        const infixFn = self.infix_parse_fns.get(self.next_token.type) orelse {
            return leftExpr;
        };

        self.nextToken();

        const ptr = try self.allocator.create(Ast.Expression);
        ptr.* = leftExpr;

        leftExpr = try infixFn(self, ptr);
    }

    return leftExpr;
}

fn parseIdentifier(self: *Parser) anyerror!Ast.Expression {
    return Ast.Expression{ .identifier = Ast.Identifier{
        .token = self.cur_token,
        .value = self.cur_token.literal,
    } };
}

fn parseIntegerLiteral(self: *Parser) anyerror!Ast.Expression {
    var ilit = Ast.IntegerLiteral{
        .token = self.cur_token,
        .value = undefined,
    };

    ilit.value = std.fmt.parseInt(i64, self.cur_token.literal, 10) catch {
        return self.conversionError(self.cur_token.literal);
    };

    return Ast.Expression{
        .integer_literal = ilit,
    };
}

fn parsePrefixExpression(self: *Parser) anyerror!Ast.Expression {
    var pe = Ast.PrefixExpression{
        .token = self.cur_token,
        .operator = self.cur_token.literal,
        .right = undefined,
    };

    self.nextToken();

    const ptr = try self.allocator.create(Ast.Expression);
    ptr.* = try self.parseExpression(.prefix);

    pe.right = ptr;

    return Ast.Expression{
        .prefix_expression = pe,
    };
}

fn parseInfixExpression(self: *Parser, left: *Ast.Expression) anyerror!Ast.Expression {
    var ie = Ast.InfixExpression{
        .token = self.cur_token,
        .operator = self.cur_token.literal,
        .left = left,
        .right = undefined,
    };

    const precedence = self.curPrecedence();

    self.nextToken();

    const ptr = try self.allocator.create(Ast.Expression);
    ptr.* = try self.parseExpression(precedence);

    ie.right = ptr;

    return Ast.Expression{
        .infix_expression = ie,
    };
}

fn parseBooleanExpression(self: *Parser) anyerror!Ast.Expression {
    return Ast.Expression{
        .boolean = Ast.BooleanExpression{
            .token = self.cur_token,
            .value = self.curTokenIs(.true),
        },
    };
}

fn parseGroupedExpression(self: *Parser) anyerror!Ast.Expression {
    self.nextToken();

    const ge = try self.parseExpression(.lowest);

    if (self.nextTokenIs(.rparen)) {
        self.nextToken();
    } else {
        return self.peekExpressionError(.rparen);
    }

    return ge;
}

fn parseIfExpression(self: *Parser) anyerror!Ast.Expression {
    var ie = Ast.IfExpression{
        .token = self.cur_token,
        .condition = undefined,
        .consequence = undefined,
        .alternative = null,
    };

    if (self.nextTokenIs(.lparen)) {
        self.nextToken();
    } else {
        return self.peekExpressionError(.lparen);
    }

    self.nextToken();

    const ptr = try self.allocator.create(Ast.Expression);
    ptr.* = try self.parseExpression(.lowest);
    ie.condition = ptr;

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

    ie.consequence = try self.parseBlockStatement();

    if (self.nextTokenIs(.@"else")) {
        self.nextToken();

        if (self.nextTokenIs(.lbrace)) {
            self.nextToken();
        } else {
            return self.peekExpressionError(.lbrace);
        }

        ie.alternative = try self.parseBlockStatement();
    }

    return Ast.Expression{
        .if_expression = ie,
    };
}

fn parseBlockStatement(self: *Parser) anyerror!Ast.BlockStatement {
    var bs = Ast.BlockStatement.init(self.allocator, self.cur_token);
    self.nextToken();

    while (!self.curTokenIs(.rbrace) and !self.curTokenIs(.eof)) {
        const stmt = try self.parseStatement();
        try bs.statements.append(stmt);
        self.nextToken();
    }
    return bs;
}

fn parseFunctionLiteral(self: *Parser) anyerror!Ast.Expression {
    var fl = Ast.FunctionLiteral.init(self.allocator, self.cur_token);

    if (self.nextTokenIs(.lparen)) {
        self.nextToken();
    } else {
        return self.peekExpressionError(.lparen);
    }

    try self.parseFunctionParameters(&fl.parameters);

    if (self.nextTokenIs(.lbrace)) {
        self.nextToken();
    } else {
        return self.peekExpressionError(.lbrace);
    }

    fl.body = try self.parseBlockStatement();

    return Ast.Expression{
        .function_literal = fl,
    };
}

fn parseFunctionParameters(self: *Parser, params: *std.ArrayList(Ast.Identifier)) anyerror!void {
    if (self.nextTokenIs(.rparen)) {
        self.nextToken();
        return;
    }

    self.nextToken();

    var ident = Ast.Identifier{
        .token = self.cur_token,
        .value = self.cur_token.literal,
    };
    try params.append(ident);

    while (self.nextTokenIs(.comma)) {
        self.nextToken();
        self.nextToken();

        ident = Ast.Identifier{
            .token = self.cur_token,
            .value = self.cur_token.literal,
        };
        try params.append(ident);
    }

    if (self.nextTokenIs(.rparen)) {
        self.nextToken();
    } else {
        _ = self.peekExpressionError(.rparen) catch {};
    }
}

fn parseCallExpression(self: *Parser, function: *Ast.Expression) anyerror!Ast.Expression {
    var call_expr = Ast.CallExpression.init(self.allocator, self.cur_token, function);
    try self.parseCallArguments(&call_expr.arguments);
    return Ast.Expression{
        .call_expression = call_expr,
    };
}

fn parseCallArguments(self: *Parser, args: *std.ArrayList(Ast.Expression)) anyerror!void {
    if (self.nextTokenIs(.rparen)) {
        self.nextToken();
        return;
    }

    self.nextToken();
    var expr = try self.parseExpression(.lowest);
    try args.append(expr);

    while (self.nextTokenIs(.comma)) {
        self.nextToken();
        self.nextToken();
        expr = try self.parseExpression(.lowest);
        try args.append(expr);
    }

    if (self.nextTokenIs(.rparen)) {
        self.nextToken();
    } else {
        _ = self.peekExpressionError(.rparen) catch {};
    }
}

// Errors

fn peekStatementError(self: *Parser, expected: TokenType) anyerror!Ast.Statement {
    const fmt = "expected next token to be {any}, got {any} instead";
    const msg = try std.fmt.allocPrint(self.allocator, fmt, .{ expected, self.next_token.type });
    try self.errors.append(msg);
    return Ast.Statement{ .@"error" = ParserError.UnexpectedToken };
}

fn peekExpressionError(self: *Parser, expected: TokenType) anyerror!Ast.Expression {
    const fmt = "expected next token to be {any}, got {any} instead";
    const msg = try std.fmt.allocPrint(self.allocator, fmt, .{ expected, self.next_token.type });
    try self.errors.append(msg);
    return Ast.Expression{ .@"error" = ParserError.UnexpectedToken };
}

fn noPrefixParseFnError(self: *Parser, token_type: TokenType) anyerror!Ast.Expression {
    const fmt = "no prefix parse function for {any} found";
    const msg = try std.fmt.allocPrint(self.allocator, fmt, .{token_type});
    try self.errors.append(msg);
    return Ast.Expression{ .@"error" = ParserError.NoPrefixParseFunction };
}

fn conversionError(self: *Parser, literal: []const u8) anyerror!Ast.Expression {
    const fmt = "could not parse {s} as integer";
    const msg = try std.fmt.allocPrint(self.allocator, fmt, .{literal});
    try self.errors.append(msg);
    return Ast.Expression{ .@"error" = ParserError.ConversionFailed };
}

// Test Utils

// pub fn checkParserErrors(parser: Parser) !void {
//     const errors = parser.errors;
//     if (errors.items.len == 0) {
//         return;
//     }
//
//     std.debug.print("parser has {d} errors\n", .{errors.items.len});
//     for (errors.items) |msg| {
//         std.debug.print("parser error: {s}\n", .{msg});
//     }
//     return error.ParserError;
//     // std.process.exit(255);
// }

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
    var program = try parser.parseProgram();
    defer program.deinit();

    checkParserErrors(parser);

    try std.testing.expectEqual(@as(usize, 3), program.statements.items.len);

    for (expected_idents, expected_vals, program.statements.items) |ident, val, stmt| {
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
    var program = try parser.parseProgram();
    defer program.deinit();

    checkParserErrors(parser);

    try std.testing.expectEqual(@as(usize, 3), program.statements.items.len);

    for (expected_vals, program.statements.items) |val, stmt| {
        try std.testing.expectEqualStrings("return", stmt.tokenLiteral());
        try std.testing.expect(.@"return" == stmt.return_statement.token.type);
        try std.testing.expectEqualStrings("return", stmt.return_statement.token.literal);
        try std.testing.expectEqual(val, stmt.return_statement.value.integer_literal.value);
    }
}

test "TestIndentiferExpression" {
    const input = "foobar;";

    const lexer = Lexer.init(input);
    var parser = try Parser.init(std.testing.allocator, lexer);
    defer parser.deinit();
    var program = try parser.parseProgram();
    defer program.deinit();

    checkParserErrors(parser);

    try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);

    const stmt = program.statements.items[0];

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
    var program = try parser.parseProgram();
    defer program.deinit();

    checkParserErrors(parser);

    try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);

    const stmt = program.statements.items[0];
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
        var program = try parser.parseProgram();
        defer program.deinit();

        checkParserErrors(parser);

        try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);

        const stmt = program.statements.items[0];
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
        var program = try parser.parseProgram();
        defer program.deinit();

        checkParserErrors(parser);

        try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);

        const stmt = program.statements.items[0];
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
    };

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        var program = try parser.parseProgram();
        defer program.deinit();

        checkParserErrors(parser);

        var buffer = std.ArrayList(u8).init(std.testing.allocator);
        defer buffer.deinit();

        try program.string(buffer.writer());
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
        var program = try parser.parseProgram();
        defer program.deinit();

        checkParserErrors(parser);

        try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);

        const stmt = program.statements.items[0];
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
        var program = try parser.parseProgram();
        defer program.deinit();

        checkParserErrors(parser);

        try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);

        const stmt = program.statements.items[0];
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
    var program = try parser.parseProgram();
    defer program.deinit();

    checkParserErrors(parser);

    try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);

    const stmt = program.statements.items[0];
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
    var program = try parser.parseProgram();
    defer program.deinit();

    checkParserErrors(parser);

    try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);

    const stmt = program.statements.items[0];
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
    var program = try parser.parseProgram();
    defer program.deinit();

    checkParserErrors(parser);

    try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);

    const stmt = program.statements.items[0];
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
        var program = try parser.parseProgram();
        defer program.deinit();

        checkParserErrors(parser);

        const stmt = program.statements.items[0];
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
    var program = try parser.parseProgram();
    defer program.deinit();

    checkParserErrors(parser);

    try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);

    const stmt = program.statements.items[0];
    const call_expr = stmt.expression_statement.expression.call_expression;
    {
        const expected = "add";
        const actual = call_expr.function.tokenLiteral();
        try std.testing.expectEqualStrings(expected, actual);
    }
    try std.testing.expectEqual(@as(usize, 3), call_expr.arguments.items.len);
    {
        const expected = "1";
        const actual = call_expr.arguments.items[0].tokenLiteral();
        try std.testing.expectEqualStrings(expected, actual);
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
