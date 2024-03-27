const std = @import("std");

const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Object = @import("Object.zig");

const Ast = struct {
    usingnamespace @import("Ast.zig");
    usingnamespace @import("Expression.zig");
    usingnamespace @import("Statement.zig");
};

const Evaluator = @This();
const checkParserErrors = Parser.checkParserErrors;

var TRUE: *Object.Object = undefined;
var FALSE: *Object.Object = undefined;
var NULL: *Object.Object = undefined;

allocator: std.mem.Allocator,
node: *Ast.Node,
object_list: std.ArrayList(*Object.Object),
node_list: std.ArrayList(*Ast.Node),

pub fn init(node: *Ast.Node) Evaluator {
    var self = Evaluator{
        .allocator = node.program.allocator,
        .node = node,
        .object_list = std.ArrayList(*Object.Object).init(node.program.allocator),
        .node_list = std.ArrayList(*Ast.Node).init(node.program.allocator),
    };

    TRUE = self.createObjectBoolean(true);
    FALSE = self.createObjectBoolean(false);
    NULL = self.createObjectNull();

    return self;
}

pub fn deinit(self: Evaluator) void {
    for (self.object_list.items) |item| {
        switch (item.*) {
            .integer => |v| self.allocator.destroy(v),
            .boolean => |v| self.allocator.destroy(v),
            .null => |v| self.allocator.destroy(v),
            .return_value => |v| self.allocator.destroy(v),
            .@"error" => |v| {
                self.allocator.free(v.message);
                self.allocator.destroy(v);
            },
        }
        self.allocator.destroy(item);
    }
    self.object_list.deinit();

    for (self.node_list.items) |item| {
        self.allocator.destroy(item);
    }
    self.node_list.deinit();
}

pub fn eval(self: *Evaluator) ?*Object.Object {
    switch (self.node.*) {
        .program => {
            return self.evalProgram();
        },
        .statement => |x| {
            switch (x.*) {
                .return_statement => |y| {
                    const new_node = self.createNode();
                    new_node.* = Ast.Node{ .expression = y.return_value };

                    self.prepareEval(new_node);
                    const result = self.eval().?;

                    const new_return_value_obj = self.createObjectReturnValue();
                    new_return_value_obj.value = result;

                    const new_obj = self.createObject();
                    new_obj.* = Object.Object{ .return_value = new_return_value_obj };

                    return new_obj;
                },
                .expression_statement => |y| {
                    const new_node = self.createNode();
                    new_node.* = Ast.Node{ .expression = y.expression };

                    self.prepareEval(new_node);
                    return self.eval();
                },
                else => unreachable,
            }
        },
        .expression => |x| {
            switch (x.*) {
                .integer_literal => |y| {
                    const new_integer_obj = self.createObjectInteger();
                    new_integer_obj.value = y.value;

                    const new_obj = self.createObject();
                    new_obj.* = Object.Object{ .integer = new_integer_obj };

                    return new_obj;
                },
                .prefix_expression => |y| {
                    const new_right_node = self.createNode();
                    new_right_node.* = Ast.Node{ .expression = y.right };

                    self.prepareEval(new_right_node);
                    const right = self.eval().?;

                    return self.evalPrefixExpression(y.operator, right);
                },
                .infix_expression => |y| {
                    // left
                    const new_left_node = self.createNode();
                    new_left_node.* = Ast.Node{ .expression = y.left };

                    self.prepareEval(new_left_node);
                    const left = self.eval().?;
                    // rigth
                    const new_right_node = self.createNode();
                    new_right_node.* = Ast.Node{ .expression = y.right };

                    self.prepareEval(new_right_node);
                    const right = self.eval().?;

                    return self.evalInfixExpression(y.operator, left, right);
                },
                .boolean => |y| {
                    return nativeBoolToBooleanObject(y.value);
                },
                .if_expression => |y| {
                    return self.evalIfExpression(y);
                },
                else => unreachable,
            }
        },
    }
    // std.debug.print("{s}\n", .{"reached 'null'"});
    return null;
}

fn evalProgram(self: *Evaluator) ?*Object.Object {
    var result: ?*Object.Object = null;

    for (self.node.program.statements.items) |stmt| {
        const new_node = self.createNode();
        new_node.* = .{ .statement = stmt };

        self.prepareEval(new_node);
        result = self.eval();

        if (result == null) continue;

        switch (result.?.*) {
            .return_value => |v| return v.value,
            .@"error" => return result.?,
            else => {},
        }
    }

    return result;
}

fn evalBlockStatement(self: *Evaluator, bs: *Ast.BlockStatement) *Object.Object {
    var result: ?*Object.Object = undefined;
    for (bs.statements.items) |stmt| {
        const new_node = self.createNode();
        new_node.* = .{ .statement = stmt };

        self.prepareEval(new_node);
        result = self.eval();

        if (result) |r| {
            const rt = r.getType();
            if (std.mem.eql(u8, rt, Object.RETURN_VALUE_OBJ) or std.mem.eql(u8, rt, Object.ERROR_OBJ)) {
                return r;
            }
        }
    }

    return result.?;
}

fn evalExpressionStatement(self: *Evaluator, expr: *Ast.Expression) *Object.Object {
    return switch (expr.*) {
        .integer_literal => |e| self.evalIntegerLiteral(e),
        .boolean => |e| nativeBoolToBooleanObject(e.value),
        .prefix_expression => |e| {
            const right = self.evalExpressionStatement(e.right);

            return self.evalPrefixExpression(e.operator, right);
        },
        .infix_expression => |e| {
            const left = self.evalExpressionStatement(e.left);
            const right = self.evalExpressionStatement(e.right);
            return self.evalInfixExpression(e.operator, left, right);
        },
        .if_expression => |e| return self.evalIfExpression(e),
        else => unreachable,
    };
}

fn evalIntegerLiteral(self: *Evaluator, il: *Ast.IntegerLiteral) *Object.Object {
    const new_integer_obj = self.createObjectInteger();
    new_integer_obj.value = il.value;

    const new_obj = self.createObject();
    new_obj.* = Object.Object{ .integer = new_integer_obj };

    return new_obj;
}

fn evalPrefixExpression(self: *Evaluator, operator: []const u8, right: *Object.Object) *Object.Object {
    if (std.mem.eql(u8, operator, "!")) {
        return evalPrefixBangExpression(right);
    } else if (std.mem.eql(u8, operator, "-")) {
        return self.evalPrefixMinusExpression(right);
    } else {
        return self.createError("unknown operator: {s}{s}", .{ operator, right.getType() });
    }
}

fn evalPrefixBangExpression(right: *Object.Object) *Object.Object {
    if (right == TRUE) return FALSE;
    if (right == FALSE) return TRUE;
    if (right == NULL) return TRUE;
    return FALSE;
}

fn evalPrefixMinusExpression(self: *Evaluator, right: *Object.Object) *Object.Object {
    if (!std.mem.eql(u8, right.getType(), Object.INTEGER_OBJ)) {
        return self.createError("unknown operator: -{s}", .{right.getType()});
    }

    const value = right.integer.value;

    const new_integer_obj = self.createObjectInteger();
    new_integer_obj.value = -value;

    const new_obj = self.createObject();
    new_obj.* = Object.Object{ .integer = new_integer_obj };

    return new_obj;
}

fn evalInfixExpression(self: *Evaluator, op: []const u8, left: *Object.Object, right: *Object.Object) *Object.Object {
    if (std.mem.eql(u8, left.getType(), Object.INTEGER_OBJ) and
        std.mem.eql(u8, right.getType(), Object.INTEGER_OBJ))
    {
        return self.evalIntegerInfixExpression(op, left, right);
    } else if (std.mem.eql(u8, op, "==")) {
        return nativeBoolToBooleanObject(left.boolean.value == right.boolean.value);
    } else if (std.mem.eql(u8, op, "!=")) {
        return nativeBoolToBooleanObject(left.boolean.value != right.boolean.value);
    } else if (!std.mem.eql(u8, left.getType(), right.getType())) {
        return self.createError("type mismatch: {s} {s} {s}", .{ left.getType(), op, right.getType() });
    } else {
        return self.createError("unknown operator: {s} {s} {s}", .{ left.getType(), op, right.getType() });
    }
}

fn evalIntegerInfixExpression(self: *Evaluator, op: []const u8, left: *Object.Object, right: *Object.Object) *Object.Object {
    switch (op[0]) {
        '+' => {
            const new_integer_obj = self.createObjectInteger();
            new_integer_obj.value = left.integer.value + right.integer.value;

            const new_obj = self.createObject();
            new_obj.* = Object.Object{ .integer = new_integer_obj };

            return new_obj;
        },
        '-' => {
            const new_integer_obj = self.createObjectInteger();
            new_integer_obj.value = left.integer.value - right.integer.value;

            const new_obj = self.createObject();
            new_obj.* = Object.Object{ .integer = new_integer_obj };

            return new_obj;
        },
        '*' => {
            const new_integer_obj = self.createObjectInteger();
            new_integer_obj.value = left.integer.value * right.integer.value;

            const new_obj = self.createObject();
            new_obj.* = Object.Object{ .integer = new_integer_obj };

            return new_obj;
        },
        '/' => {
            const new_integer_obj = self.createObjectInteger();
            new_integer_obj.value = @divTrunc(left.integer.value, right.integer.value);

            const new_obj = self.createObject();
            new_obj.* = Object.Object{ .integer = new_integer_obj };

            return new_obj;
        },
        '<' => {
            return nativeBoolToBooleanObject(left.integer.value < right.integer.value);
        },
        '>' => {
            return nativeBoolToBooleanObject(left.integer.value > right.integer.value);
        },
        else => {
            if (std.mem.eql(u8, op, "==")) {
                return nativeBoolToBooleanObject(left.integer.value == right.integer.value);
            }
            if (std.mem.eql(u8, op, "!=")) {
                return nativeBoolToBooleanObject(left.integer.value != right.integer.value);
            }
            return self.createError("unknown operator: {s} {s} {s}", .{ left.getType(), op, right.getType() });
        },
    }
}

fn evalIfExpression(self: *Evaluator, ie: *Ast.IfExpression) *Object.Object {
    const condition = self.evalExpressionStatement(ie.condition);

    if (isTruthy(condition)) {
        return self.evalBlockStatement(ie.consequence);
    } else if (ie.alternative) |alt| {
        return self.evalBlockStatement(alt);
    } else {
        return NULL;
    }
}

fn prepareEval(self: *Evaluator, node: *Ast.Node) void {
    self.node = node;
}

fn isTruthy(obj: *Object.Object) bool {
    if (obj == NULL) {
        return false;
    }
    if (obj == TRUE) {
        return true;
    }
    if (obj == FALSE) {
        return false;
    }
    return true;
}

fn nativeBoolToBooleanObject(input: bool) *Object.Object {
    if (input) {
        return TRUE;
    } else {
        return FALSE;
    }
}

fn createObjectBoolean(self: *Evaluator, value: bool) *Object.Object {
    const new_boolean_obj = self.allocator.create(Object.Boolean) catch @panic("OOM");
    new_boolean_obj.value = value;

    const new_obj = self.createObject();
    new_obj.* = Object.Object{ .boolean = new_boolean_obj };

    return new_obj;
}

fn createObjectNull(self: *Evaluator) *Object.Object {
    const new_null_obj = self.allocator.create(Object.Null) catch @panic("OOM");

    const new_obj = self.createObject();
    new_obj.* = Object.Object{ .null = new_null_obj };

    return new_obj;
}

fn createNode(self: *Evaluator) *Ast.Node {
    const new_node = self.allocator.create(Ast.Node) catch @panic("OOM");
    self.node_list.append(new_node) catch @panic("OOM");
    return new_node;
}

fn createObject(self: *Evaluator) *Object.Object {
    const new_obj = self.allocator.create(Object.Object) catch @panic("OOM");
    self.object_list.append(new_obj) catch @panic("OOM");
    return new_obj;
}

fn createObjectInteger(self: Evaluator) *Object.Integer {
    const new_obj = self.allocator.create(Object.Integer) catch @panic("OOM");

    return new_obj;
}

pub fn createObjectReturnValue(self: Evaluator) *Object.ReturnValue {
    const new_obj = self.allocator.create(Object.ReturnValue) catch @panic("OOM");

    return new_obj;
}

fn createError(self: *Evaluator, comptime format: []const u8, args: anytype) *Object.Object {
    const message = std.fmt.allocPrint(self.allocator, format, args) catch @panic("OOM");

    const new_error_obj = self.allocator.create(Object.Error) catch @panic("OOM");
    new_error_obj.message = message;

    const new_obj = self.createObject();
    new_obj.* = Object.Object{ .@"error" = new_error_obj };

    return new_obj;
}

// tests

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
        var node = parser.parseProgram();
        defer node.deinit();

        checkParserErrors(parser);

        var evaluator = Evaluator.init(node);
        defer evaluator.deinit();

        const result = evaluator.eval();

        {
            const expected = t[1];
            const actual = result.?.integer.value;
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
        var node = parser.parseProgram();
        defer node.deinit();

        checkParserErrors(parser);

        var evaluator = Evaluator.init(node);
        defer evaluator.deinit();

        const result = evaluator.eval();

        {
            const expected = t[1];
            const actual = result.?.boolean.value;
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
        var node = parser.parseProgram();
        defer node.deinit();

        checkParserErrors(parser);

        var evaluator = Evaluator.init(node);
        defer evaluator.deinit();

        const result = evaluator.eval();
        {
            const expected = t[1];
            const actual = result.?.boolean.value;
            try std.testing.expectEqual(expected, actual);
        }
    }
}

test "TestIfElseExpressions" {
    const Types1 = union(enum) {
        null: *Object.Object,
        integer: i64,
    };
    const Test = struct {
        []const u8,
        Types1,
    };
    const tests = [_]Test{
        .{ "if (true) { 10 }", .{ .integer = 10 } },
        .{ "if (false) { 10 }", .{ .null = NULL } },
        .{ "if (1) { 10 }", .{ .integer = 10 } },
        .{ "if (1 < 2) { 10 }", .{ .integer = 10 } },
        .{ "if (1 > 2) { 10 }", .{ .null = NULL } },
        .{ "if (1 > 2) { 10 } else { 20 }", .{ .integer = 20 } },
        .{ "if (1 < 2) { 10 } else { 20 }", .{ .integer = 10 } },
    };

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        var node = parser.parseProgram();
        defer node.deinit();

        checkParserErrors(parser);

        var evaluator = Evaluator.init(node);
        defer evaluator.deinit();

        const result = evaluator.eval();

        {
            const expected = t[1];
            switch (result.?.*) {
                .integer => |x| {
                    const actual = x.value;
                    try std.testing.expectEqual(expected.integer, actual);
                },
                .null => |x| {
                    const actual = x;
                    try std.testing.expectEqual(NULL.null, actual);
                },
                else => unreachable,
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
        var node = parser.parseProgram();
        defer node.deinit();

        checkParserErrors(parser);

        var evaluator = Evaluator.init(node);
        defer evaluator.deinit();

        const result = evaluator.eval();

        {
            const expected = t[1];
            const actual = result.?.integer.value;
            try std.testing.expectEqual(expected, actual);
        }
    }
}

test "TestErrorHandling" {
    const Test = struct {
        []const u8,
        []const u8,
    };
    const tests = [_]Test{
        .{
            "5 + true;",
            "type mismatch: INTEGER + BOOLEAN",
        },
        .{
            "5 + true; 5;",
            "type mismatch: INTEGER + BOOLEAN",
        },
        .{
            "-true",
            "unknown operator: -BOOLEAN",
        },
        .{
            "true + false;",
            "unknown operator: BOOLEAN + BOOLEAN",
        },
        .{
            "5; true + false; 5",
            "unknown operator: BOOLEAN + BOOLEAN",
        },
        .{
            "if (10 > 1) { true + false; }",
            "unknown operator: BOOLEAN + BOOLEAN",
        },
        .{
            \\if (10 > 1) {
            \\  if (10 > 1) {
            \\    return true + false;
            \\  }
            \\  return 1;
            \\}
            ,
            "unknown operator: BOOLEAN + BOOLEAN",
        },
    };

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        var node = parser.parseProgram();
        defer node.deinit();

        checkParserErrors(parser);

        var evaluator = Evaluator.init(node);
        defer evaluator.deinit();

        const result = evaluator.eval();

        {
            const expected = t[1];
            const actual = result.?.@"error".message;
            try std.testing.expectEqualStrings(expected, actual);
        }
    }
}
