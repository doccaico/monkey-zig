const std = @import("std");

const TokenType = @import("Token.zig").TokenType;
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Object = @import("Object.zig");

const Ast = struct {
    usingnamespace @import("Ast.zig");
    usingnamespace @import("Expression.zig");
    usingnamespace @import("Statement.zig");
};

const checkParserErrors = Parser.checkParserErrors;

var TRUE: *Object.Object = undefined;
var FALSE: *Object.Object = undefined;
var NULL: *Object.Object = undefined;

pub var eval_allocator: std.mem.Allocator = undefined;
pub var object_list: std.ArrayList(*Object.Object) = undefined;
pub var node_list: std.ArrayList(*Ast.Node) = undefined;

pub const EvalWorld = struct {
    pub fn init(allocator: std.mem.Allocator) void {
        eval_allocator = allocator;
        object_list = std.ArrayList(*Object.Object).init(allocator);
        node_list = std.ArrayList(*Ast.Node).init(allocator);

        TRUE = Object.createObjectBoolean(true);
        FALSE = Object.createObjectBoolean(false);
        NULL = Object.createObjectNull();
    }

    pub fn deinit() void {
        // object_list
        for (object_list.items) |item| {
            switch (item.*) {
                .integer => |v| eval_allocator.destroy(v),
                .boolean => |v| eval_allocator.destroy(v),
                .null => |v| eval_allocator.destroy(v),
                else => {},
            }
            eval_allocator.destroy(item);
        }
        object_list.deinit();

        // node_list
        for (node_list.items) |item| {
            // switch (item.*) {
            //     .statement => |v| {
            //         switch (v.*) {
            //             .block_statement => |bs| {
            //                 bs.deinit();
            //                 eval_allocator.destroy(bs);
            //             },
            //             else => {},
            //         }
            //     },
            //     //eval_allocator.destroy(v),
            //     else => {},
            // }
            eval_allocator.destroy(item);
        }
        node_list.deinit();
    }
};

pub fn createNode() *Ast.Node {
    const node = eval_allocator.create(Ast.Node) catch @panic("OOM");
    node_list.append(node) catch @panic("OOM");
    return node;
}

pub fn createObject() *Object.Object {
    const obj = eval_allocator.create(Object.Object) catch @panic("OOM");
    object_list.append(obj) catch @panic("OOM");
    return obj;
}

pub fn createExpression(comptime T: type) *T {
    const expr = eval_allocator.create(T) catch @panic("OOM");
    // node_list.append(node) catch @panic("OOM");
    return expr;
}

pub fn eval(node: *Ast.Node) ?*Object.Object {
    switch (node.*) {
        .program => |v| {
            return evalProgram(v);
        },
        .statement => |v| {
            switch (v.*) {
                .return_statement => |self| {
                    const new_node = createNode();
                    new_node.* = Ast.Node{ .expression = self.return_value };

                    const n = eval(new_node).?;

                    const new_obj = createObject();
                    new_obj.return_value.value = n.return_value.value;

                    return new_obj;
                },
                .expression_statement => |self| {
                    const new_node = createNode();
                    new_node.* = Ast.Node{ .expression = self.expression };

                    return eval(new_node);
                },
                else => unreachable,
            }
        },
        .expression => |v| {
            switch (v.*) {
                .integer_literal => |self| {
                    const new_integer_obj = Object.createObjectInteger();
                    new_integer_obj.value = self.value;

                    const new_obj = createObject();
                    new_obj.* = Object.Object{ .integer = new_integer_obj };

                    return new_obj;
                },
                .prefix_expression => |self| {
                    const new_right_node = createNode();
                    new_right_node.* = Ast.Node{ .expression = self.right };

                    const right = eval(new_right_node).?;

                    return evalPrefixExpression(self.operator, right);
                },
                .infix_expression => |self| {
                    // left
                    const new_left_node = createNode();
                    new_left_node.* = Ast.Node{ .expression = self.left };

                    const left = eval(new_left_node).?;
                    // rigth
                    const new_right_node = createNode();
                    new_right_node.* = Ast.Node{ .expression = self.right };

                    const right = eval(new_right_node).?;

                    return evalInfixExpression(self.operator, left, right);
                },
                .boolean => |self| {
                    return nativeBoolToBooleanObject(self.value);
                },
                .if_expression => |self| {
                    // const new_expr = createExpression(Ast.IfExpression);
                    // // new_expr.* = Ast.IfExpression{ .if_expression = self };
                    // new_expr.* = self;

                    return evalIfExpression(self);
                    // return evalIfExpression(new_expr);
                },
                else => unreachable,
            }
        },
    }

    // std.debug.print("{s}\n", .{"reached 'null'"});

    return null;
}

fn evalProgram(program: *Ast.Program) ?*Object.Object {
    var result: ?*Object.Object = null;

    for (program.statements.items) |stmt| {
        const new_node = createNode();
        new_node.* = .{ .statement = stmt };

        result = eval(new_node);

        if (result == null) continue;

        switch (result.?.*) {
            .return_value => |v| return v.value,
            else => {},
        }
    }

    return result;
}

// fn evalStatement(stmt: Ast.Statement) Object.Object {
//     return switch (stmt) {
//         .let_statement => |s| evalLetStatement(s),
//         .return_statement => |s| evalReturnStatement(s),
//         .block_statement => |s| evalBlockStatement(s),
//         .expression_statement => |s| evalExpressionStatement(s.expression),
//         else => unreachable,
//     };
// }

fn evalLetStatement(ls: Ast.LetStatement) Object.Object {
    _ = ls;
    return Object.Object{ .null = Object.Null{} };
}

fn evalReturnStatement(rs: Ast.ReturnStatement) Object.Object {
    _ = rs;
    return Object.Object{ .null = Object.Null{} };
}

fn evalBlockStatement(bs: *Ast.BlockStatement) *Object.Object {
    var result: ?*Object.Object = undefined;
    for (bs.statements.items) |stmt| {
        const new_node = createNode();
        new_node.* = .{ .statement = stmt };

        result = eval(new_node);

        if (result != null) {
            const rt = result.?.getType();
            // if (std.mem.eql(u8, rt, Object.RETURN_VALUE_OBJ) or std.mem.eql(u8, rt, Object.ERROR_OBJ)) {}
            if (std.mem.eql(u8, rt, Object.RETURN_VALUE_OBJ)) {
                return result.?;
            }
        }
    }

    return result.?;
}

fn evalExpressionStatement(expr: *Ast.Expression) *Object.Object {
    return switch (expr.*) {
        .integer_literal => |e| evalIntegerLiteral(e),
        .boolean => |e| evalBoolean(e),
        .prefix_expression => |e| {
            const right = evalExpressionStatement(e.right);

            return evalPrefixExpression(e.operator, right);
        },
        .infix_expression => |e| {
            const left = evalExpressionStatement(e.left);
            const right = evalExpressionStatement(e.right);
            return evalInfixExpression(e.operator, left, right);
        },
        .if_expression => |e| return evalIfExpression(e),
        else => unreachable,
    };
}

fn evalIntegerLiteral(il: *Ast.IntegerLiteral) *Object.Object {
    const new_integer_obj = Object.createObjectInteger();
    new_integer_obj.value = il.value;

    const new_obj = createObject();
    new_obj.* = Object.Object{ .integer = new_integer_obj };

    return new_obj;
}

fn evalBoolean(be: *Ast.BooleanExpression) *Object.Object {
    return nativeBoolToBooleanObject(be.value);
    // return Object.Object{ .boolean = Object.Boolean{ .value = be.value } };
}

fn evalPrefixExpression(operator: []const u8, right: *Object.Object) *Object.Object {
    if (std.mem.eql(u8, operator, "!")) {
        return evalPrefixBangExpression(right);
    } else if (std.mem.eql(u8, operator, "-")) {
        return evalPrefixMinusExpression(right);
    } else {
        return NULL;
    }

    // return switch (token_type) {
    //     .bang => evalPrefixBangExpression(right),
    //     .minus => evalPrefixMinusExpression(right),
    //     else => Object.Object{ .null = Object.Null{} },
    // };
}

fn evalPrefixBangExpression(right: *Object.Object) *Object.Object {
    if (right == TRUE) return FALSE;
    if (right == FALSE) return TRUE;
    if (right == NULL) return TRUE;
    return FALSE;

    // return switch (right) {
    //     .boolean => |obj| Object.Object{ .boolean = Object.Boolean{ .value = !obj.value } },
    //     .null => Object.Object{ .boolean = Object.Boolean{ .value = true } },
    //     else => Object.Object{ .boolean = Object.Boolean{ .value = false } },
    // };
}

fn evalPrefixMinusExpression(right: *Object.Object) *Object.Object {
    switch (right.*) {
        .integer => |obj| {
            // Object.Object{ .integer = Object.Integer{ .value = -obj.value } };
            // new_obj.return_value.value = n.return_value.value;

            const new_integer_obj = Object.createObjectInteger();
            new_integer_obj.value = -obj.value;

            const new_obj = createObject();
            new_obj.* = Object.Object{ .integer = new_integer_obj };

            return new_obj;
        },
        else => {
            return NULL;
        },
    }
}

fn evalInfixExpression(op: []const u8, left: *Object.Object, right: *Object.Object) *Object.Object {
    if (std.mem.eql(u8, left.getType(), Object.INTEGER_OBJ) and
        std.mem.eql(u8, right.getType(), Object.INTEGER_OBJ))
    {
        return evalIntegerInfixExpression(op, left.integer.value, right.integer.value);
    } else if (std.mem.eql(u8, op, "==")) {
        return nativeBoolToBooleanObject(left.boolean.value == right.boolean.value);
    } else if (std.mem.eql(u8, op, "!=")) {
        return nativeBoolToBooleanObject(left.boolean.value != right.boolean.value);
    } else {
        return NULL;
    }
}

fn evalIntegerInfixExpression(op: []const u8, left_val: i64, right_val: i64) *Object.Object {
    switch (op[0]) {
        '+' => {
            const new_integer_obj = Object.createObjectInteger();
            new_integer_obj.value = left_val + right_val;

            const new_obj = createObject();
            new_obj.* = Object.Object{ .integer = new_integer_obj };

            return new_obj;
        },
        '-' => {
            const new_integer_obj = Object.createObjectInteger();
            new_integer_obj.value = left_val - right_val;

            const new_obj = createObject();
            new_obj.* = Object.Object{ .integer = new_integer_obj };

            return new_obj;
        },
        '*' => {
            const new_integer_obj = Object.createObjectInteger();
            new_integer_obj.value = left_val * right_val;

            const new_obj = createObject();
            new_obj.* = Object.Object{ .integer = new_integer_obj };

            return new_obj;
        },
        '/' => {
            const new_integer_obj = Object.createObjectInteger();
            new_integer_obj.value = @divTrunc(left_val, right_val);

            const new_obj = createObject();
            new_obj.* = Object.Object{ .integer = new_integer_obj };

            return new_obj;
        },
        '<' => {
            return nativeBoolToBooleanObject(left_val < right_val);
        },
        '>' => {
            return nativeBoolToBooleanObject(left_val > right_val);
        },
        else => {
            if (std.mem.eql(u8, op, "==")) {
                return nativeBoolToBooleanObject(left_val == right_val);
            }
            if (std.mem.eql(u8, op, "!=")) {
                return nativeBoolToBooleanObject(left_val != right_val);
            }
            return NULL;
        },
    }
}

fn evalIfExpression(ie: *Ast.IfExpression) *Object.Object {
    // const condition = evalExpressionStatement(ie.condition.*);
    const condition = evalExpressionStatement(ie.condition);

    // std.debug.print("{}", .{condition});

    if (isTruthy(condition)) {
        return evalBlockStatement(ie.consequence);
    } else if (ie.alternative) |alt| {
        return evalBlockStatement(alt);
    } else {
        return NULL;
    }
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

    // return switch (obj) {
    //     .null => false,
    //     .boolean => |o| {
    //         if (o.value) {
    //             return true;
    //         } else {
    //             return false;
    //         }
    //     },
    //     else => true,
    // };
}

fn nativeBoolToBooleanObject(input: bool) *Object.Object {
    if (input) {
        return TRUE;
    } else {
        return FALSE;
    }
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
        var program = parser.parseProgram();
        defer program.deinit();

        checkParserErrors(parser);

        EvalWorld.init(std.testing.allocator);
        defer EvalWorld.deinit();

        const obj = eval(program);

        {
            const expected = t[1];
            const actual = obj.?.integer.value;
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
        var program = parser.parseProgram();
        defer program.deinit();

        checkParserErrors(parser);

        EvalWorld.init(std.testing.allocator);
        defer EvalWorld.deinit();

        const obj = eval(program);

        {
            const expected = t[1];
            const actual = obj.?.boolean.value;
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
        var program = parser.parseProgram();
        defer program.deinit();

        checkParserErrors(parser);

        EvalWorld.init(std.testing.allocator);
        defer EvalWorld.deinit();

        const obj = eval(program);

        {
            const expected = t[1];
            const actual = obj.?.boolean.value;
            try std.testing.expectEqual(expected, actual);
        }
    }
}

pub const Types1 = union(enum) {
    null,
    integer: i64,
};

test "TestIfElseExpressions" {
    const Test = struct {
        []const u8,
        Types1,
    };

    const tests = [_]Test{
        .{ "if (true) { 10 }", .{ .integer = 10 } },
        .{ "if (false) { 10 }", .{ .null = void{} } },
        .{ "if (false) { 10 }", .{ .null = void{} } },
        .{ "if (false) { 10 }", .{ .null = void{} } },

        .{ "if (1) { 10 }", .{ .integer = 10 } },
        .{ "if (1 < 2) { 10 }", .{ .integer = 10 } },
        .{ "if (1 > 2) { 10 }", .{ .null = void{} } },
        .{ "if (1 > 2) { 10 } else { 20 }", .{ .integer = 20 } },
        .{ "if (1 < 2) { 10 } else { 20 }", .{ .integer = 10 } },
    };

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        var program_node = parser.parseProgram();
        defer program_node.deinit();

        checkParserErrors(parser);

        EvalWorld.init(std.testing.allocator);
        defer EvalWorld.deinit();

        const obj = eval(program_node);

        {
            const expected = t[1];
            switch (obj.?.*) {
                .integer => |v| try std.testing.expectEqual(expected.integer, v.value),
                .null => |_| try std.testing.expectEqual(expected.null, void{}),
                else => unreachable,
            }
        }
    }
}

// test "TestReturnStatements" {
//     const Test = struct {
//         []const u8,
//         i64,
//     };
//     const tests = [_]Test{
//         .{ "return 10;", 10 },
//         .{ "return 10; 9;", 10 },
//         .{ "return 2 * 5; 9;", 10 },
//         .{ "9; return 2 * 5; 9;", 10 },
//     };
//
//     for (tests) |t| {
//         const lexer = Lexer.init(t[0]);
//         var parser = try Parser.init(std.testing.allocator, lexer);
//         defer parser.deinit();
//         var program = try parser.parseProgram();
//         defer program.deinit();
//
//         checkParserErrors(parser);
//
//         const obj = eval(program);
//
//         {
//             const expected = t[1];
//             const actual = obj.integer.value;
//             try std.testing.expectEqual(expected, actual);
//         }
//     }
// }
