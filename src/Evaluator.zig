const std = @import("std");

const TokenType = @import("Token.zig").TokenType;
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Object = @import("Object.zig");
// const Object = struct {
//     usingnamespace @import("Object.zig");
// };
const Ast = struct {
    usingnamespace @import("Ast.zig");
    usingnamespace @import("Expression.zig");
    usingnamespace @import("Statement.zig");
};
const checkParserErrors = Parser.checkParserErrors;

// pub fn eval(program: Ast.Program) Object.Object {
//     for (program.statements.items) |stmt| {
//         const expr = stmt.expression_statement.expression;
//         switch (expr) {
//             .integer_literal => |obj| return Object.Object{ .integer = Object.Integer{ .value = obj.value } },
//             else => {},
//         }
//     }
//     return Object.Object{ .null = Object.Null{} };
// }

pub fn eval(program: Ast.Program) Object.Object {
    const ret = Object.Object{ .null = Object.Null{} };
    for (program.statements.items) |stmt| {
        return evalStatement(stmt);
        // const expr = stmt.expression_statement.expression;
        // switch (expr) {
        //     // .integer_literal => |obj| return Object.Object{ .integer = Object.Integer{ .value = obj.value } },
        //     .integer_literal => |obj| return Object.Object{ .integer = Object.Integer{ .value = obj.value } },
        //     else => {},
        // }
    }
    return ret;
}

fn evalStatement(stmt: Ast.Statement) Object.Object {
    return switch (stmt) {
        .let_statement => |s| evalLetStatement(s),
        .return_statement => |s| evalReturnStatement(s),
        .block_statement => |s| evalBlockStatement(s),
        .expression_statement => |s| evalExpressionStatement(s.expression),
        else => unreachable,
    };
}

fn evalLetStatement(ls: Ast.LetStatement) Object.Object {
    _ = ls;
    return Object.Object{ .null = Object.Null{} };
}

fn evalReturnStatement(rs: Ast.ReturnStatement) Object.Object {
    _ = rs;
    return Object.Object{ .null = Object.Null{} };
}

fn evalBlockStatement(bs: Ast.BlockStatement) Object.Object {
    const ret = Object.Object{ .null = Object.Null{} };
    for (bs.statements.items) |stmt| {
        const evaluated = evalStatement(stmt);
        return evaluated;
    }
    return ret;
}

fn evalExpressionStatement(expr: Ast.Expression) Object.Object {
    return switch (expr) {
        .integer_literal => |e| evalIntegerLiteral(e),
        .boolean => |e| evalBoolean(e),
        .prefix_expression => |e| {
            const right = evalExpressionStatement(e.right.*);
            return evalPrefixExpression(e.token.type, right);
        },
        .infix_expression => |e| {
            const left = evalExpressionStatement(e.left.*);
            const right = evalExpressionStatement(e.right.*);
            return evalInfixExpression(e.operator, left, right);
        },
        .if_expression => |e| return evalIfExpression(e),
        else => unreachable,
    };
}

fn evalIntegerLiteral(il: Ast.IntegerLiteral) Object.Object {
    return Object.Object{ .integer = Object.Integer{ .value = il.value } };
}

fn evalBoolean(be: Ast.BooleanExpression) Object.Object {
    return Object.Object{ .boolean = Object.Boolean{ .value = be.value } };
}

fn evalPrefixExpression(token_type: TokenType, right: Object.Object) Object.Object {
    return switch (token_type) {
        .bang => evalPrefixBangExpression(right),
        .minus => evalPrefixMinusExpression(right),
        else => Object.Object{ .null = Object.Null{} },
    };
}

fn evalPrefixBangExpression(right: Object.Object) Object.Object {
    return switch (right) {
        .boolean => |obj| Object.Object{ .boolean = Object.Boolean{ .value = !obj.value } },
        .null => Object.Object{ .boolean = Object.Boolean{ .value = true } },
        else => Object.Object{ .boolean = Object.Boolean{ .value = false } },
    };
}

fn evalPrefixMinusExpression(right: Object.Object) Object.Object {
    return switch (right) {
        .integer => |obj| Object.Object{ .integer = Object.Integer{ .value = -obj.value } },
        else => Object.Object{ .null = Object.Null{} },
    };
}

fn evalInfixExpression(op: []const u8, left: Object.Object, right: Object.Object) Object.Object {
    // const left_is_integer = std.mem.eql(u8, left.getType(), Object.INTEGER_OBJ);
    // const right_is_integer = std.mem.eql(u8, right.getType(), Object.INTEGER_OBJ);
    if (std.mem.eql(u8, left.getType(), Object.INTEGER_OBJ) and
        std.mem.eql(u8, right.getType(), Object.INTEGER_OBJ))
    {
        return evalIntegerInfixExpression(op, left.integer.value, right.integer.value);
    } else if (std.mem.eql(u8, op, "==")) {
        return Object.Object{ .boolean = Object.Boolean{ .value = left.boolean.value == right.boolean.value } };
    } else if (std.mem.eql(u8, op, "!=")) {
        return Object.Object{ .boolean = Object.Boolean{ .value = left.boolean.value != right.boolean.value } };
    } else {
        return Object.Object{ .null = Object.Null{} };
    }
}

// fn evalIntegerInfixExpression(op: []const u8, left: Object.Object, right: Object.Object) Object.Object {
fn evalIntegerInfixExpression(op: []const u8, left_val: i64, right_val: i64) Object.Object {
    // const left_val = left.integer.value;
    // const right_val = right.integer.value;
    return switch (op[0]) {
        '+' => Object.Object{ .integer = Object.Integer{ .value = left_val + right_val } },
        '-' => Object.Object{ .integer = Object.Integer{ .value = left_val - right_val } },
        '*' => Object.Object{ .integer = Object.Integer{ .value = left_val * right_val } },
        '/' => Object.Object{ .integer = Object.Integer{ .value = @divTrunc(left_val, right_val) } },
        '<' => Object.Object{ .boolean = Object.Boolean{ .value = left_val < right_val } },
        '>' => Object.Object{ .boolean = Object.Boolean{ .value = left_val > right_val } },
        else => blk: {
            if (std.mem.eql(u8, op, "==")) {
                break :blk Object.Object{ .boolean = Object.Boolean{ .value = left_val == right_val } };
            }

            if (std.mem.eql(u8, op, "!=")) {
                break :blk Object.Object{ .boolean = Object.Boolean{ .value = left_val != right_val } };
            }
            break :blk Object.Object{ .null = Object.Null{} };
        },
    };
}

fn evalIfExpression(ie: Ast.IfExpression) Object.Object {
    const condition = evalExpressionStatement(ie.condition.*);

    if (isTruthy(condition)) {
        return evalBlockStatement(ie.consequence);
    } else if (ie.alternative) |alt| {
        return evalBlockStatement(alt);
    } else {
        return Object.Object{ .null = Object.Null{} };
    }
}

fn isTruthy(obj: Object.Object) bool {
    return switch (obj) {
        .null => false,
        .boolean => |o| {
            if (o.value) {
                return true;
            } else {
                return false;
            }
        },
        else => true,
    };
}

// Test Utils

test "TestEvalIntegerExpression" {
    const Test = struct {
        []const u8,
        i64,
    };
    const tests = [_]Test{
        .{ "5", 5 },
        .{ "10;", 10 },

        .{ "-5", -5 },
        .{ "-10", -10 },

        .{ "5 + 5 + 5 + 5 - 10", 10 },
        .{ "2 * 2 * 2 * 2 * 2", 32 },
        .{ "-50 + 100 + -50", 0 },
        .{ "5 * 2 + 10", 20 },
        .{ "5 + 2 * 10", 25 },
        .{ "20 + 2 * -10", 0 },
        .{ "50 / 2 * 2 + 10", 60 },
        .{ "2 * (5 + 10)", 30 },
        .{ "3 * 3 * 3 + 10", 37 },
        .{ "3 * (3 * 3) + 10", 37 },
        .{ "(5 + 10 * 2 + 15 / 3) * 2 + -10", 50 },
    };

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        var program = try parser.parseProgram();
        defer program.deinit();

        checkParserErrors(parser);

        const obj = eval(program);

        {
            const expected = t[1];
            const actual = obj.integer.value;
            try std.testing.expectEqual(expected, actual);
        }
    }
}

test "TestEvalBooleanExpression" {
    const Test = struct {
        []const u8,
        bool,
    };
    const tests = [_]Test{
        .{ "true", true },
        .{ "false;", false },

        .{ "1 < 2", true },
        .{ "1 > 2", false },
        .{ "1 < 1", false },
        .{ "1 > 1", false },
        .{ "1 == 1", true },
        .{ "1 != 1", false },
        .{ "1 == 2", false },
        .{ "1 != 2", true },

        .{ "true == true", true },
        .{ "false == false", true },
        .{ "true == false", false },
        .{ "true != false", true },
        .{ "false != true", true },
        .{ "(1 < 2) == true", true },
        .{ "(1 < 2) == false", false },
        .{ "(1 > 2) == true", false },
        .{ "(1 > 2) == false", true },
    };

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        var program = try parser.parseProgram();
        defer program.deinit();

        checkParserErrors(parser);

        const obj = eval(program);

        {
            const expected = t[1];
            const actual = obj.boolean.value;
            try std.testing.expectEqual(expected, actual);
        }
    }
}

test "TestBangOperator" {
    const Test = struct {
        []const u8,
        bool,
    };
    const tests = [_]Test{
        .{ "!true", false },
        .{ "!false", true },
        .{ "!5", false },
        .{ "!!true", true },
        .{ "!!false", false },
        .{ "!!5", true },
    };

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        var program = try parser.parseProgram();
        defer program.deinit();

        checkParserErrors(parser);

        const obj = eval(program);

        {
            const expected = t[1];
            const actual = obj.boolean.value;
            try std.testing.expectEqual(expected, actual);
        }
    }
}

test "TestIfElseExpressions" {
    const Test = struct {
        []const u8,
        Object.Object,
    };
    const tests = [_]Test{
        .{ "if (true) { 10 }", .{ .integer = Object.Integer{ .value = 10 } } },
        .{ "if (false) { 10 }", .{ .null = Object.Null{} } },
        .{ "if (1) { 10 }", .{ .integer = Object.Integer{ .value = 10 } } },
        .{ "if (1 < 2) { 10 }", .{ .integer = Object.Integer{ .value = 10 } } },
        .{ "if (1 > 2) { 10 }", .{ .null = Object.Null{} } },
        .{ "if (1 > 2) { 10 } else { 20 }", .{ .integer = Object.Integer{ .value = 20 } } },
        .{ "if (1 < 2) { 10 } else { 20 }", .{ .integer = Object.Integer{ .value = 10 } } },
    };

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        var program = try parser.parseProgram();
        defer program.deinit();

        checkParserErrors(parser);

        const obj = eval(program);

        {
            const expected = t[1];
            const actual = obj;

            switch (actual) {
                .integer => try std.testing.expectEqual(expected, actual),
                else => {
                    const null_obj = Object.Object{ .null = Object.Null{} };
                    try std.testing.expectEqual(null_obj, actual);
                },
            }
        }
    }
}

test "TestReturnStatements" {
    const Test = struct {
        []const u8,
        i64,
    };
    const tests = [_]Test{
        .{ "return 10;", 10 },
        .{ "return 10; 9;", 10 },
        .{ "return 2 * 5; 9;", 10 },
        .{ "9; return 2 * 5; 9;", 10 },
    };

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        var program = try parser.parseProgram();
        defer program.deinit();

        checkParserErrors(parser);

        const obj = eval(program);

        {
            const expected = t[1];
            const actual = obj.integer.value;
            try std.testing.expectEqual(expected, actual);
        }
    }
}
