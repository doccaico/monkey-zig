const std = @import("std");

const Environment = @import("Environment.zig");
const Globals = @import("Globals.zig");
const Lexer = @import("Lexer.zig");
const Object = @import("Object.zig");
const Parser = @import("Parser.zig");

const Ast = struct {
    usingnamespace @import("Ast.zig");
    usingnamespace @import("Expression.zig");
    usingnamespace @import("Statement.zig");
};

const Evaluator = @This();

const checkParserErrors = Parser.checkParserErrors;

allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) Evaluator {
    return Evaluator{
        .allocator = allocator,
    };
}

pub fn eval(self: *Evaluator, node: *Ast.Node, env: *Environment) ?*Object.Object {
    switch (node.*) {
        .program => {
            return self.evalProgram(node, env);
        },
        .statement => |x| {
            switch (x.*) {
                .return_statement => |y| {
                    const new_node = self.createNode();
                    new_node.* = Ast.Node{ .expression = y.return_value };

                    const result = self.eval(new_node, env);
                    if (isError(result)) {
                        return result;
                    }

                    const new_return_value_obj = self.allocator.create(Object.ReturnValue) catch @panic("OOM");
                    new_return_value_obj.value = result.?;

                    const new_obj = self.createObject();
                    new_obj.* = Object.Object{ .return_value = new_return_value_obj };

                    return new_obj;
                },
                .expression_statement => |y| {
                    const new_node = self.createNode();
                    new_node.* = Ast.Node{ .expression = y.expression };

                    return self.eval(new_node, env);
                },
                .let_statement => |y| {
                    const new_node = self.createNode();
                    new_node.* = Ast.Node{ .expression = y.value };

                    const result = self.eval(new_node, env);
                    if (isError(result)) {
                        return result;
                    }
                    env.set(y.name.value, result.?);
                },
                else => unreachable,
            }
        },
        .expression => |x| {
            switch (x.*) {
                .integer_literal => |y| {
                    const new_integer_obj = self.allocator.create(Object.Integer) catch @panic("OOM");
                    new_integer_obj.value = y.value;

                    const new_obj = self.createObject();
                    new_obj.* = Object.Object{ .integer = new_integer_obj };

                    return new_obj;
                },
                .prefix_expression => |y| {
                    const new_right_node = self.createNode();
                    new_right_node.* = Ast.Node{ .expression = y.right };

                    const right = self.eval(new_right_node, env);
                    if (isError(right)) {
                        return right;
                    }
                    return self.evalPrefixExpression(y.operator, right.?);
                },
                .infix_expression => |y| {
                    // left
                    const new_left_node = self.createNode();
                    new_left_node.* = Ast.Node{ .expression = y.left };

                    const left = self.eval(new_left_node, env);
                    if (isError(left)) {
                        return left;
                    }
                    // rigth
                    const new_right_node = self.createNode();
                    new_right_node.* = Ast.Node{ .expression = y.right };

                    const right = self.eval(new_right_node, env);
                    if (isError(right)) {
                        return right;
                    }

                    return self.evalInfixExpression(y.operator, left.?, right.?);
                },
                .boolean => |y| {
                    return nativeBoolToBooleanObject(y.value);
                },
                .if_expression => |y| {
                    return self.evalIfExpression(y, env);
                },
                .identifier => |y| {
                    return self.evalIdentifier(y, env);
                },
                .function_literal => |y| {
                    const new_function_obj = self.allocator.create(Object.Function) catch @panic("OOM");
                    new_function_obj.parameters = y.parameters;
                    new_function_obj.body = y.body;
                    new_function_obj.env = env;

                    const new_obj = self.createObject();
                    new_obj.* = Object.Object{ .function = new_function_obj };
                    return new_obj;
                },
                .call_expression => |y| {
                    const new_node = self.createNode();
                    new_node.* = Ast.Node{ .expression = y.function };

                    const result = self.eval(new_node, env);
                    if (isError(result)) {
                        return result;
                    }
                    const args = self.evalExpressions(y.arguments, env);
                    Globals.argsAppend(args);
                    if (args.items.len == 1 and isError(args.items[0])) {
                        return args.items[0];
                    }
                    return self.applyFunction(env, result.?, args);
                },
                .string_literal => |y| {
                    const new_string_obj = self.allocator.create(Object.String) catch @panic("OOM");
                    new_string_obj.value = y.value;

                    const new_obj = self.createObject();
                    new_obj.* = Object.Object{ .string = new_string_obj };

                    return new_obj;
                },
                .array_literal => |y| {
                    const elements = self.evalExpressions(y.elements, env);
                    if (elements.items.len == 1 and isError(elements.items[0])) {
                        return elements.items[0];
                    }

                    const new_array_obj = self.allocator.create(Object.Array) catch @panic("OOM");
                    new_array_obj.elements = elements;

                    const new_obj = self.createObject();
                    new_obj.* = Object.Object{ .array = new_array_obj };

                    return new_obj;
                },
                .index_expression => |y| {
                    // left
                    const new_left_node = self.createNode();
                    new_left_node.* = Ast.Node{ .expression = y.left };

                    const left = self.eval(new_left_node, env);
                    if (isError(left)) {
                        return left;
                    }
                    // index
                    const new_index_node = self.createNode();
                    new_index_node.* = Ast.Node{ .expression = y.index };

                    const index = self.eval(new_index_node, env);
                    if (isError(index)) {
                        return index;
                    }

                    return self.evalIndexExpression(left.?, index.?);
                },
                else => {},
            }
        },
    }
    return null;
}

fn evalProgram(self: *Evaluator, node: *Ast.Node, env: *Environment) ?*Object.Object {
    var result: ?*Object.Object = null;

    for (node.program.statements.items) |stmt| {
        const new_node = self.createNode();
        new_node.* = Ast.Node{ .statement = stmt };

        result = self.eval(new_node, env);

        if (result == null) continue;

        switch (result.?.*) {
            .return_value => |x| return x.value,
            .@"error" => return result.?,
            else => {},
        }
    }
    return result;
}

fn evalBlockStatement(self: *Evaluator, bs: *Ast.BlockStatement, env: *Environment) *Object.Object {
    var result: ?*Object.Object = undefined;
    for (bs.statements.items) |stmt| {
        const new_node = self.createNode();
        new_node.* = .{ .statement = stmt };

        result = self.eval(new_node, env);

        if (result) |r| {
            const rt = r.getType();
            if (std.mem.eql(u8, rt, Object.RETURN_VALUE_OBJ) or std.mem.eql(u8, rt, Object.ERROR_OBJ)) {
                return r;
            }
        }
    }

    return result.?;
}

fn evalIntegerLiteral(self: *Evaluator, il: *Ast.IntegerLiteral) *Object.Object {
    const new_integer_obj = self.allocator.create(Object.Integer) catch @panic("OOM");
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
    if (right == Environment.TRUE) return Environment.FALSE;
    if (right == Environment.FALSE) return Environment.TRUE;
    if (right == Environment.NULL) return Environment.TRUE;
    return Environment.FALSE;
}

fn evalPrefixMinusExpression(self: *Evaluator, right: *Object.Object) *Object.Object {
    if (!std.mem.eql(u8, right.getType(), Object.INTEGER_OBJ)) {
        return self.createError("unknown operator: -{s}", .{right.getType()});
    }

    const value = right.integer.value;

    const new_integer_obj = self.allocator.create(Object.Integer) catch @panic("OOM");
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
    } else if (std.mem.eql(u8, left.getType(), Object.STRING_OBJ) and
        std.mem.eql(u8, right.getType(), Object.STRING_OBJ))
    {
        return self.evalStringInfixExpression(op, left, right);
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
            const new_integer_obj = self.allocator.create(Object.Integer) catch @panic("OOM");
            new_integer_obj.value = left.integer.value + right.integer.value;

            const new_obj = self.createObject();
            new_obj.* = Object.Object{ .integer = new_integer_obj };

            return new_obj;
        },
        '-' => {
            const new_integer_obj = self.allocator.create(Object.Integer) catch @panic("OOM");
            new_integer_obj.value = left.integer.value - right.integer.value;

            const new_obj = self.createObject();
            new_obj.* = Object.Object{ .integer = new_integer_obj };

            return new_obj;
        },
        '*' => {
            const new_integer_obj = self.allocator.create(Object.Integer) catch @panic("OOM");
            new_integer_obj.value = left.integer.value * right.integer.value;

            const new_obj = self.createObject();
            new_obj.* = Object.Object{ .integer = new_integer_obj };

            return new_obj;
        },
        '/' => {
            const new_integer_obj = self.allocator.create(Object.Integer) catch @panic("OOM");
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

fn evalStringInfixExpression(self: *Evaluator, op: []const u8, left: *Object.Object, right: *Object.Object) *Object.Object {
    if (!std.mem.eql(u8, op, "+")) {
        return self.createError("unknown operator: {s} {s} {s}", .{ left.getType(), op, right.getType() });
    }

    const slice = &[_][]const u8{ left.string.value, right.string.value };
    const s = std.mem.concat(self.allocator, u8, slice) catch @panic("OOM");
    Globals.stringAppend(s);

    const new_string_obj = self.allocator.create(Object.String) catch @panic("OOM");
    new_string_obj.value = s;

    const new_obj = self.createObject();
    new_obj.* = Object.Object{ .string = new_string_obj };

    return new_obj;
}

fn evalIfExpression(self: *Evaluator, ie: *Ast.IfExpression, env: *Environment) *Object.Object {
    const new_node = self.createNode();
    new_node.* = Ast.Node{ .expression = ie.condition };

    const condition = self.eval(new_node, env);
    if (isError(condition)) {
        return condition.?;
    }
    if (isTruthy(condition.?)) {
        return self.evalBlockStatement(ie.consequence, env);
    } else if (ie.alternative) |alt| {
        return self.evalBlockStatement(alt, env);
    } else {
        return Environment.NULL;
    }
}

fn evalIdentifier(self: *Evaluator, ident: *Ast.Identifier, env: *Environment) *Object.Object {
    const value = env.get(ident.value);
    if (value != null) {
        return value.?;
    }
    const builtin = Environment.getBuiltinFunction(ident.value);
    if (builtin != null) {
        return builtin.?;
    }
    return self.createError("identifier not found: {s}", .{ident.value});
}

fn evalExpressions(self: *Evaluator, exps: std.ArrayList(*Ast.Expression), env: *Environment) std.ArrayList(*Object.Object) {
    var result = std.ArrayList(*Object.Object).init(self.allocator);
    for (exps.items) |e| {
        const new_node = self.createNode();
        new_node.* = Ast.Node{ .expression = e };

        const evaluated = self.eval(new_node, env).?;
        if (isError(evaluated)) {
            result.append(evaluated) catch @panic("OOM");
            return result;
        }
        result.append(evaluated) catch @panic("OOM");
    }
    return result;
}

fn applyFunction(self: *Evaluator, env: *Environment, func: *Object.Object, args: std.ArrayList(*Object.Object)) *Object.Object {
    switch (func.*) {
        .function => |x| {
            const extended_env = extendFunctionEnv(env, x, args);
            const evaluated = self.evalBlockStatement(x.body, extended_env);
            return unwrapReturnValue(evaluated);
        },
        .builtin => |x| {
            return x.function(args);
        },
        else => return self.createError("not a function: {s}", .{func.getType()}),
    }
}

fn extendFunctionEnv(env: *Environment, func: *Object.Function, args: std.ArrayList(*Object.Object)) *Environment {
    const new_env = env.newEnclosedEnvironment(func.env);
    for (func.parameters.items, 0..) |param, param_idx| {
        new_env.set(param.value, args.items[param_idx]);
    }
    return new_env;
}

fn unwrapReturnValue(obj: *Object.Object) *Object.Object {
    return switch (obj.*) {
        .return_value => |x| x.value,
        else => obj,
    };
}

fn evalIndexExpression(self: *Evaluator, left: *Object.Object, index: *Object.Object) *Object.Object {
    if (std.mem.eql(u8, left.getType(), Object.ARRAY_OBJ) and
        std.mem.eql(u8, index.getType(), Object.INTEGER_OBJ))
    {
        return evalArrayIndexExpression(left, index);
    } else {
        return self.createError("index operator not supported: {s}", .{left.getType()});
    }
}

fn evalArrayIndexExpression(array: *Object.Object, index: *Object.Object) *Object.Object {
    const array_obj = array.array;
    const idx = index.integer.value;
    const max: i64 = @intCast(array_obj.elements.items.len - 1);

    if (idx < 0 or idx > max) {
        return Environment.NULL;
    }

    return array_obj.elements.items[@intCast(idx)];
    // return array_obj.elements.items[idx];
}

fn isTruthy(obj: *Object.Object) bool {
    if (obj == Environment.NULL) {
        return false;
    }
    if (obj == Environment.TRUE) {
        return true;
    }
    if (obj == Environment.FALSE) {
        return false;
    }
    return true;
}

fn nativeBoolToBooleanObject(input: bool) *Object.Object {
    if (input) {
        return Environment.TRUE;
    } else {
        return Environment.FALSE;
    }
}

fn isError(obj: ?*Object.Object) bool {
    if (obj == null) {
        return false;
    }
    return std.mem.eql(u8, obj.?.getType(), Object.ERROR_OBJ);
}

fn createNode(self: *Evaluator) *Ast.Node {
    const new_node = self.allocator.create(Ast.Node) catch @panic("OOM");
    Globals.nodeAppend(new_node);
    return new_node;
}

fn createObject(self: *Evaluator) *Object.Object {
    const new_obj = self.allocator.create(Object.Object) catch @panic("OOM");
    Globals.objectAppend(new_obj);
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

    Globals.init(std.testing.allocator);
    defer Globals.deinit();

    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        const node_program = parser.parseProgram();
        defer Globals.nodeProgramAppend(node_program);

        checkParserErrors(parser);

        var evaluator = Evaluator.init(std.testing.allocator);

        const result = evaluator.eval(node_program, env);

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

    Globals.init(std.testing.allocator);
    defer Globals.deinit();

    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        const node_program = parser.parseProgram();
        defer Globals.nodeProgramAppend(node_program);

        checkParserErrors(parser);

        var evaluator = Evaluator.init(std.testing.allocator);

        const result = evaluator.eval(node_program, env);

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

    Globals.init(std.testing.allocator);
    defer Globals.deinit();

    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        const node_program = parser.parseProgram();
        defer Globals.nodeProgramAppend(node_program);

        checkParserErrors(parser);

        var evaluator = Evaluator.init(std.testing.allocator);

        const result = evaluator.eval(node_program, env);

        {
            const expected = t[1];
            const actual = result.?.boolean.value;
            try std.testing.expectEqual(expected, actual);
        }
    }
}

test "TestIfElseExpressions" {
    const null_value = -256;
    const Test = struct {
        []const u8,
        i64,
    };
    const tests = [_]Test{
        .{ "if (true) { 10 }", 10 },
        .{ "if (false) { 10 }", null_value },
        .{ "if (1) { 10 }", 10 },
        .{ "if (1 < 2) { 10 }", 10 },
        .{ "if (1 > 2) { 10 }", null_value },
        .{ "if (1 > 2) { 10 } else { 20 }", 20 },
        .{ "if (1 < 2) { 10 } else { 20 }", 10 },
    };

    Globals.init(std.testing.allocator);
    defer Globals.deinit();

    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        const node_program = parser.parseProgram();
        defer Globals.nodeProgramAppend(node_program);

        checkParserErrors(parser);

        var evaluator = Evaluator.init(std.testing.allocator);

        const result = evaluator.eval(node_program, env);

        {
            const expected = t[1];
            switch (result.?.*) {
                .integer => |x| {
                    const actual = x.value;
                    try std.testing.expectEqual(expected, actual);
                },
                .null => {
                    try std.testing.expectEqual(expected, null_value);
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

    Globals.init(std.testing.allocator);
    defer Globals.deinit();

    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        const node_program = parser.parseProgram();
        defer Globals.nodeProgramAppend(node_program);

        checkParserErrors(parser);

        var evaluator = Evaluator.init(std.testing.allocator);

        const result = evaluator.eval(node_program, env);

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
        .{
            "foobar",
            "identifier not found: foobar",
        },
        .{
            "\"Hello\" - \"World\"",
            "unknown operator: STRING - STRING",
        },
    };

    Globals.init(std.testing.allocator);
    defer Globals.deinit();

    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        const node_program = parser.parseProgram();
        defer Globals.nodeProgramAppend(node_program);

        checkParserErrors(parser);

        var evaluator = Evaluator.init(std.testing.allocator);

        const result = evaluator.eval(node_program, env);

        {
            const expected = t[1];
            const actual = result.?.@"error".message;
            try std.testing.expectEqualStrings(expected, actual);
        }
    }
}

test "TestLetStatements" {
    const Test = struct {
        []const u8,
        i64,
    };
    const tests = [_]Test{
        .{ "let a = 5; a;", 5 },
        .{ "let a = 5 * 5; a;", 25 },
        .{ "let a = 5; let b = a; b;", 5 },
        .{ "let a = 5; let b = a; let c = a + b + 5; c;", 15 },
    };

    Globals.init(std.testing.allocator);
    defer Globals.deinit();

    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        const node_program = parser.parseProgram();
        defer Globals.nodeProgramAppend(node_program);

        checkParserErrors(parser);

        var evaluator = Evaluator.init(std.testing.allocator);

        const result = evaluator.eval(node_program, env);

        {
            const expected = t[1];
            const actual = result.?.integer.value;
            try std.testing.expectEqual(expected, actual);
        }
    }
}

test "TestFunctionObject" {
    const input = "fn(x) { x + 2; };";

    Globals.init(std.testing.allocator);
    defer Globals.deinit();

    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    const lexer = Lexer.init(input);
    var parser = try Parser.init(std.testing.allocator, lexer);
    defer parser.deinit();
    const node_program = parser.parseProgram();
    defer Globals.nodeProgramAppend(node_program);

    checkParserErrors(parser);

    var evaluator = Evaluator.init(std.testing.allocator);

    const result = evaluator.eval(node_program, env);

    {
        const expected = 1;
        const actual = result.?.function.parameters.items.len;
        try std.testing.expectEqual(expected, actual);
    }

    {
        const expected = "x";
        const actual = result.?.function.parameters.items[0].value;
        try std.testing.expectEqualStrings(expected, actual);
    }

    {
        const expected = "(x + 2)";
        var buffer = std.ArrayList(u8).init(std.testing.allocator);
        defer buffer.deinit();
        try result.?.function.body.string(buffer.writer());
        const actual = buffer.items;
        try std.testing.expectEqualStrings(expected, actual);
    }
}

test "TestFunctionApplication" {
    const Test = struct {
        []const u8,
        i64,
    };
    const tests = [_]Test{
        .{ "let identity = fn(x) { x; }; identity(5);", 5 },
        .{ "let identity = fn(x) { return x; }; identity(5);", 5 },
        .{ "let double = fn(x) { x * 2; }; double(5);", 10 },
        .{ "let add = fn(x, y) { x + y; }; add(5, 5);", 10 },
        .{ "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20 },
        .{ "fn(x) { x; }(5)", 5 },
    };

    Globals.init(std.testing.allocator);
    defer Globals.deinit();

    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        const node_program = parser.parseProgram();
        defer Globals.nodeProgramAppend(node_program);

        checkParserErrors(parser);

        var evaluator = Evaluator.init(std.testing.allocator);

        const result = evaluator.eval(node_program, env);

        {
            const expected = t[1];
            const actual = result.?.integer.value;
            try std.testing.expectEqual(expected, actual);
        }
    }
}

test "TestClosures" {
    const input =
        \\let newAdder = fn(x) {
        \\  fn(y) { x + y };
        \\};
        \\
        \\let addTwo = newAdder(2);
        \\addTwo(2);
    ;

    Globals.init(std.testing.allocator);
    defer Globals.deinit();

    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    const lexer = Lexer.init(input);
    var parser = try Parser.init(std.testing.allocator, lexer);
    defer parser.deinit();
    const node_program = parser.parseProgram();
    defer Globals.nodeProgramAppend(node_program);

    checkParserErrors(parser);

    var evaluator = Evaluator.init(std.testing.allocator);

    const result = evaluator.eval(node_program, env);

    {
        const expected: i64 = 4;
        const actual = result.?.integer.value;
        try std.testing.expectEqual(expected, actual);
    }
}

test "TestStringLiteral" {
    const input =
        \\"Hello World!"
    ;

    Globals.init(std.testing.allocator);
    defer Globals.deinit();

    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    const lexer = Lexer.init(input);
    var parser = try Parser.init(std.testing.allocator, lexer);
    defer parser.deinit();
    const node_program = parser.parseProgram();
    defer Globals.nodeProgramAppend(node_program);

    checkParserErrors(parser);

    var evaluator = Evaluator.init(std.testing.allocator);

    const result = evaluator.eval(node_program, env);

    {
        const expected = "Hello World!";
        const actual = result.?.string.value;
        try std.testing.expectEqualStrings(expected, actual);
    }
}

test "TestStringConcatenation" {
    const input =
        \\"Hello" + " " + "World!";
    ;

    Globals.init(std.testing.allocator);
    defer Globals.deinit();

    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    const lexer = Lexer.init(input);
    var parser = try Parser.init(std.testing.allocator, lexer);
    defer parser.deinit();
    const node_program = parser.parseProgram();
    defer Globals.nodeProgramAppend(node_program);

    checkParserErrors(parser);

    var evaluator = Evaluator.init(std.testing.allocator);

    const result = evaluator.eval(node_program, env);

    {
        const expected = "Hello World!";
        const actual = result.?.string.value;
        try std.testing.expectEqualStrings(expected, actual);
    }
}

test "TestBuiltinFunctions" {
    const Types1 = union(enum) {
        string: []const u8,
        integer: i64,
    };
    const Test = struct {
        []const u8,
        Types1,
    };
    const tests = [_]Test{
        .{ "len(\"\")", .{ .integer = 0 } },
        .{ "len(\"four\")", .{ .integer = 4 } },
        .{ "len(\"hello world\")", .{ .integer = 11 } },
        .{ "len(1)", .{ .string = "argument to `len` not supported, got INTEGER" } },
        .{ "len(\"one\", \"two\")", .{ .string = "wrong number of arguments. got=2, want=1" } },
    };

    Globals.init(std.testing.allocator);
    defer Globals.deinit();

    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        const node_program = parser.parseProgram();
        defer Globals.nodeProgramAppend(node_program);

        checkParserErrors(parser);

        var evaluator = Evaluator.init(std.testing.allocator);

        const result = evaluator.eval(node_program, env);

        {
            switch (result.?.*) {
                .integer => |x| {
                    const expected = t[1].integer;
                    const actual = x.value;
                    try std.testing.expectEqual(expected, actual);
                },
                .@"error" => |x| {
                    const expected = t[1].string;
                    const actual = x.message;
                    try std.testing.expectEqualStrings(expected, actual);
                },
                else => unreachable,
            }
        }
    }
}

test "TestArrayLiterals" {
    const input = "[1, 2 * 2, 3 + 3]";

    Globals.init(std.testing.allocator);
    defer Globals.deinit();

    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    const lexer = Lexer.init(input);
    var parser = try Parser.init(std.testing.allocator, lexer);
    defer parser.deinit();
    const node_program = parser.parseProgram();
    defer Globals.nodeProgramAppend(node_program);

    checkParserErrors(parser);

    var evaluator = Evaluator.init(std.testing.allocator);

    const result = evaluator.eval(node_program, env);

    {
        const expected: i64 = 1;
        const actual = result.?.array.elements.items[0].integer.value;
        try std.testing.expectEqual(expected, actual);
    }
    {
        const expected: i64 = 4;
        const actual = result.?.array.elements.items[1].integer.value;
        try std.testing.expectEqual(expected, actual);
    }
    {
        const expected: i64 = 6;
        const actual = result.?.array.elements.items[2].integer.value;
        try std.testing.expectEqual(expected, actual);
    }
}

test "TestArrayIndexExpressions" {
    const null_value = -256;
    const Test = struct {
        []const u8,
        i64,
    };
    const tests = [_]Test{
        .{
            "[1, 2, 3][0]",
            1,
        },
        .{
            "[1, 2, 3][1]",
            2,
        },
        .{
            "[1, 2, 3][2]",
            3,
        },
        .{
            "let i = 0; [1][i];",
            1,
        },
        .{
            "[1, 2, 3][1 + 1];",
            3,
        },
        .{
            "let myArray = [1, 2, 3]; myArray[2];",
            3,
        },
        .{
            "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
            6,
        },
        .{
            "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
            2,
        },
        .{
            "[1, 2, 3][3]",
            null_value,
        },
        .{
            "[1, 2, 3][-1]",
            null_value,
        },
    };

    Globals.init(std.testing.allocator);
    defer Globals.deinit();

    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        const node_program = parser.parseProgram();
        defer Globals.nodeProgramAppend(node_program);

        checkParserErrors(parser);

        var evaluator = Evaluator.init(std.testing.allocator);

        const result = evaluator.eval(node_program, env);

        {
            const expected = t[1];
            switch (result.?.*) {
                .integer => |x| {
                    const actual = x.value;
                    try std.testing.expectEqual(expected, actual);
                },
                .null => {
                    try std.testing.expectEqual(expected, null_value);
                },
                else => unreachable,
            }
        }
    }
}
