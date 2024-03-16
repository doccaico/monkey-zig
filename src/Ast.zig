const std = @import("std");

const Parser = @import("Parser.zig");
const Ast = struct {
    usingnamespace @import("Expression.zig");
    usingnamespace @import("Statement.zig");
};

// pub const Node = union(enum) {
//     program: Program,
//     statement: Ast.Statement,
//     expression: Ast.Expression,
//
//     pub fn string(self: Node, writer: anytype) !void {
//         switch (self) {
//             inline else => |s| try s.string(writer),
//         }
//     }
// };

pub const Program = struct {
    allocator: std.mem.Allocator,
    statements: std.ArrayList(Ast.Statement),

    pub fn init(allocator: std.mem.Allocator) Program {
        return Program{
            .allocator = allocator,
            .statements = std.ArrayList(Ast.Statement).init(allocator),
        };
    }

    pub fn deinit(self: *Program) void {
        for (self.statements.items) |stmt| {
            switch (stmt) {
                .expression_statement => |exprStmt| {
                    freeExpressionPointer(self.allocator, exprStmt.expression);
                },
                else => {},
            }
        }
        self.statements.deinit();
    }

    fn freeExpressionPointer(allocator: std.mem.Allocator, expr: Ast.Expression) void {
        switch (expr) {
            .prefix_expression => |prefixExpr| {
                freeExpressionPointer(allocator, prefixExpr.right.*);
                allocator.destroy(prefixExpr.right);
            },
            .infix_expression => |infixExpr| {
                freeExpressionPointer(allocator, infixExpr.left.*);
                allocator.destroy(infixExpr.left);

                freeExpressionPointer(allocator, infixExpr.right.*);
                allocator.destroy(infixExpr.right);
            },
            .if_expression => |ifExpr| {
                freeExpressionPointer(allocator, ifExpr.condition.*);
                allocator.destroy(ifExpr.condition);

                ifExpr.consequence.deinit();

                if (ifExpr.alternative) |alternative| {
                    alternative.deinit();
                }
            },
            .function_literal => |function_literal| {
                for (function_literal.body.statements.items) |stmt| {
                    switch (stmt) {
                        .expression_statement => |exprStmt| freeExpressionPointer(allocator, exprStmt.expression),
                        else => {},
                    }
                }
                function_literal.deinit();
            },
            .call_expression => |call_expression| {
                freeExpressionPointer(allocator, call_expression.function.*);
                allocator.destroy(call_expression.function);
                for (call_expression.arguments.items) |arg| {
                    freeExpressionPointer(allocator, arg);
                }

                call_expression.deinit();
            },
            else => {},
        }
    }

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

test "TestString()" {
    const Token = @import("Token.zig");

    var statements = std.ArrayList(Ast.Statement).init(std.testing.allocator);
    try statements.append(Ast.Statement{
        .let_statement = Ast.LetStatement{
            .token = Token{ .type = .let, .literal = "let" },
            .name = Ast.Identifier{
                .token = Token{ .type = .ident, .literal = "myVar" },
                .value = "myVar",
            },
            .value = Ast.Expression{
                .identifier = Ast.Identifier{
                    .token = Token{ .type = .ident, .literal = "anotherVar" },
                    .value = "anotherVar",
                },
            },
        },
    });

    var program = Program{ .allocator = std.testing.allocator, .statements = statements };
    defer program.deinit();

    try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);

    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();
    try program.string(buffer.writer());
    try std.testing.expectEqualStrings("let myVar = anotherVar;", buffer.items);
}
