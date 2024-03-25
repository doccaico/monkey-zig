const std = @import("std");

const Parser = @import("Parser.zig");
const Evaluator = @import("Evaluator.zig");

const Ast = struct {
    usingnamespace @import("Expression.zig");
    usingnamespace @import("Statement.zig");
};

pub const Node = union(enum) {
    program: *Program,
    statement: *Ast.Statement,
    expression: *Ast.Expression,

    pub fn deinit(self: *Node) void {
        for (self.program.statements.items) |stmt| {
            switch (stmt.*) {
                .expression_statement => |exprStmt| {
                    freeExpressionPointer(self.program.allocator, exprStmt.expression);
                    self.program.allocator.destroy(exprStmt);
                },
                .let_statement => |letStmt| {
                    self.program.allocator.destroy(letStmt.name);
                    freeExpressionPointer(self.program.allocator, letStmt.value);
                },
                .return_statement => |returnStmt| {
                    freeExpressionPointer(self.program.allocator, returnStmt.return_value);
                },
                else => {},
            }
            self.program.allocator.destroy(stmt);
        }
        self.program.statements.deinit();
        const alloc = self.program.allocator;
        alloc.destroy(self.program);
        alloc.destroy(self);

        // self.program.allocator.destroy(self.program);
        // self.program.allocator.destroy(self);
        // self.program.allocator.destroy(self);
        // self.deinit();
    }

    fn freeExpressionPointer(allocator: std.mem.Allocator, expr: *Ast.Expression) void {
        switch (expr.*) {
            .prefix_expression => |prefixExpr| {
                // freeExpressionPointer(allocator, prefixExpr);
                // allocator.destroy(prefixExpr);
                // std.debug.print(">>>>>>>>   {*}\n", .{prefixExpr.right});
                // std.debug.print(">>>>>>>>   {*}\n", .{prefixExpr});
                // std.debug.print(">>>>>>>>   {*}\n", .{expr});

                // switch (prefixExpr.right.*) {
                //     .integer_literal => |v| freeExpressionPointer(allocator, v),
                //     else => {},
                // }
                freeExpressionPointer(allocator, prefixExpr.right);
                allocator.destroy(prefixExpr);
                allocator.destroy(expr);
                // freeExpressionPointer(allocator, prefixExpr);
                // freeExpressionPointer(allocator, expr);
                // allocator.destroy(prefixExpr.right);
                // allocator.destroy(prefixExpr);
                // freeExpressionPointer(allocator, expr);
                // std.debug.print(">>>>>>>>   {}\n", .{expr});
            },
            .infix_expression => |infixExpr| {
                freeExpressionPointer(allocator, infixExpr.left);
                // allocator.destroy(infixExpr.left);

                freeExpressionPointer(allocator, infixExpr.right);
                // allocator.destroy(infixExpr.right);
                allocator.destroy(infixExpr);
            },
            .if_expression => |ifExpr| {
                freeExpressionPointer(allocator, ifExpr.condition);
                allocator.destroy(ifExpr.condition);

                ifExpr.consequence.deinit();

                if (ifExpr.alternative) |alternative| {
                    alternative.deinit();
                }
            },
            .function_literal => |function_literal| {
                for (function_literal.body.statements.items) |stmt| {
                    switch (stmt.*) {
                        .expression_statement => |exprStmt| {
                            freeExpressionPointer(allocator, exprStmt.expression);
                        },
                        else => {},
                    }
                }
                function_literal.deinit();
            },
            .call_expression => |call_expression| {
                freeExpressionPointer(allocator, call_expression.function);
                allocator.destroy(call_expression.function);
                for (call_expression.arguments.items) |arg| {
                    freeExpressionPointer(allocator, arg);
                }

                call_expression.deinit();
            },
            .integer_literal => |intExpr| {
                allocator.destroy(intExpr);
                allocator.destroy(expr);
            },
            // .expression_statement => |_| {
            // std.debug.print(">>   {}\n", .{intExpr});
            // },
            else => {},
        }
    }

    pub fn string(self: Node, writer: anytype) !void {
        switch (self) {
            inline else => |s| try s.string(writer),
        }
    }
};

pub const Program = struct {
    allocator: std.mem.Allocator,
    statements: std.ArrayList(*Ast.Statement),

    pub fn init(allocator: std.mem.Allocator) *Node {
        var prg = allocator.create(Program) catch @panic("OOM");
        prg.allocator = allocator;
        prg.statements = std.ArrayList(*Ast.Statement).init(allocator);

        const node = allocator.create(Node) catch @panic("OOM");
        // std.debug.print("KKKK\n", .{});
        // std.debug.print("HHHHH\n", .{});
        node.* = .{ .program = prg };
        // node.program = prg;
        // node.statement = undefined;
        // node.expression = undefined;
        return node;
    }

    pub fn deinit(self: *Program) void {
        // for (self.errors.items) |err| {
        //     self.allocator.free(err);
        // }
        // _ = self;
        // for (self.statements.items) |item| {
        //     switch (item) {
        //         .left => |v| self.allocator.destroy(v),
        //         else => {},
        //     }
        // }
        // self.allocator.destroy(item);
        self.statements.deinit();
        self.allocator.destroy(self);
        // self.errors.deinit();
        // self.prefix_parse_fns.deinit();
        // self.infix_parse_fns.deinit();
    }

    // obj = Program{
    //     .allocator = allocator,
    //     .statements = std.ArrayList(Ast.Statement).init(allocator),
    // };

    // pub fn init(allocator: std.mem.Allocator) Program {
    //     return Program{
    //         .allocator = allocator,
    //         .statements = std.ArrayList(Ast.Statement).init(allocator),
    //     };
    // }

    pub fn tokenLiteral(self: Program) []const u8 {
        return if (self.statements.items.len > 0) self.statements[0].tokenLiteral() else "";
    }

    pub fn string(self: Program, writer: anytype) anyerror!void {
        for (self.statements.items) |*stmt| {
            switch (stmt.*) {
                .@"error" => {},
                inline else => |*s| try s.string(writer),
            }
        }
    }
};

// test "TestString()" {
//     const Token = @import("Token.zig");
//
//     var statements = std.ArrayList(Ast.Statement).init(std.testing.allocator);
//     try statements.append(Ast.Statement{
//         .let_statement = Ast.LetStatement{
//             .token = Token{ .type = .let, .literal = "let" },
//             .name = Ast.Identifier{
//                 .token = Token{ .type = .ident, .literal = "myVar" },
//                 .value = "myVar",
//             },
//             .value = Ast.Expression{
//                 .identifier = Ast.Identifier{
//                     .token = Token{ .type = .ident, .literal = "anotherVar" },
//                     .value = "anotherVar",
//                 },
//             },
//         },
//     });
//
//     var program = Program{ .allocator = std.testing.allocator, .statements = statements };
//     defer program.deinit();
//
//     try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);
//
//     var buffer = std.ArrayList(u8).init(std.testing.allocator);
//     defer buffer.deinit();
//     try program.string(buffer.writer());
//     try std.testing.expectEqualStrings("let myVar = anotherVar;", buffer.items);
// }
