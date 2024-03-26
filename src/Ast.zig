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
                    exprStmt.deinit();
                },
                .let_statement => |letStmt| {
                    self.program.allocator.destroy(letStmt.name);
                    freeExpressionPointer(self.program.allocator, letStmt.value);
                    letStmt.deinit();
                },
                .return_statement => |returnStmt| {
                    freeExpressionPointer(self.program.allocator, returnStmt.return_value);
                    returnStmt.deinit();
                },
                else => {},
            }

            self.program.allocator.destroy(stmt);
        }
        self.program.statements.deinit();
        const allocator = self.program.allocator;
        allocator.destroy(self.program);
        allocator.destroy(self);
    }

    fn freeExpressionPointer(allocator: std.mem.Allocator, expr: *Ast.Expression) void {
        switch (expr.*) {
            .prefix_expression => |prefixExpr| {
                freeExpressionPointer(allocator, prefixExpr.right);
                allocator.destroy(prefixExpr);
                allocator.destroy(expr);
            },
            .infix_expression => |infixExpr| {
                freeExpressionPointer(allocator, infixExpr.left);
                freeExpressionPointer(allocator, infixExpr.right);
                allocator.destroy(infixExpr);
                allocator.destroy(expr);
            },
            .if_expression => |ifExpr| {
                freeExpressionPointer(allocator, ifExpr.condition);

                for (ifExpr.consequence.statements.items) |stmt| {
                    switch (stmt.*) {
                        .expression_statement => |v| {
                            freeExpressionPointer(allocator, v.expression);
                            v.deinit();
                        },
                        .return_statement => |v| freeExpressionPointer(allocator, v.return_value),
                        else => {},
                    }
                    allocator.destroy(stmt);
                }

                ifExpr.consequence.deinit();

                if (ifExpr.alternative) |alt| {
                    for (alt.statements.items) |stmt| {
                        switch (stmt.*) {
                            .expression_statement => |v| {
                                freeExpressionPointer(allocator, v.expression);
                                v.deinit();
                            },
                            .return_statement => |v| freeExpressionPointer(allocator, v.return_value),
                            else => {},
                        }
                        allocator.destroy(stmt);
                    }
                    alt.statements.deinit();
                    allocator.destroy(alt);
                }

                allocator.destroy(ifExpr);
                allocator.destroy(expr);
            },
            .function_literal => |funcLit| {
                for (funcLit.parameters.items) |param| {
                    allocator.destroy(param);
                }
                for (funcLit.body.statements.items) |stmt| {
                    switch (stmt.*) {
                        .expression_statement => |v| {
                            freeExpressionPointer(allocator, v.expression);
                            v.deinit();
                        },
                        else => {},
                    }
                    allocator.destroy(stmt);
                }
                funcLit.deinit();
                allocator.destroy(funcLit);
                allocator.destroy(expr);
            },
            .call_expression => |callExpr| {
                freeExpressionPointer(allocator, callExpr.function);
                for (callExpr.arguments.items) |arg| {
                    freeExpressionPointer(allocator, arg);
                }
                callExpr.deinit();

                allocator.destroy(callExpr);
                allocator.destroy(expr);
            },
            .integer_literal => |intExpr| {
                allocator.destroy(intExpr);
                allocator.destroy(expr);
            },
            .boolean => |booleanExpr| {
                allocator.destroy(booleanExpr);
                allocator.destroy(expr);
            },
            .identifier => |identifierExpr| {
                allocator.destroy(identifierExpr);
                allocator.destroy(expr);
            },
            // .expression_statement => |_| {
            //     std.debug.print(">>>>>>>>>>>>>>>>>>>>>>>>>>   \n", .{});
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
        node.* = .{ .program = prg };
        return node;
    }

    pub fn deinit(self: *Program) void {
        self.statements.deinit();
        self.allocator.destroy(self);
    }

    pub fn tokenLiteral(self: Program) []const u8 {
        return if (self.statements.items.len > 0) self.statements[0].tokenLiteral() else "";
    }

    pub fn string(self: Program, writer: anytype) anyerror!void {
        for (self.statements.items) |stmt| {
            switch (stmt.*) {
                .@"error" => {},
                inline else => |s| try s.string(writer),
            }
        }
    }
};

test "TestString" {
    const Token = @import("Token.zig");

    const allocator = std.testing.allocator;

    var statements = std.ArrayList(*Ast.Statement).init(std.testing.allocator);
    defer statements.deinit();

    const stmt = try allocator.create(Ast.Statement);
    defer allocator.destroy(stmt);

    const let_stmt = try allocator.create(Ast.LetStatement);
    defer allocator.destroy(let_stmt);

    const ident1 = try allocator.create(Ast.Identifier);
    defer allocator.destroy(ident1);
    ident1.token = Token{ .type = .ident, .literal = "myVar" };
    ident1.value = "myVar";

    const ident2 = try allocator.create(Ast.Identifier);
    defer allocator.destroy(ident2);
    ident2.token = Token{ .type = .ident, .literal = "anotherVar" };
    ident2.value = "anotherVar";

    const expr = try allocator.create(Ast.Expression);
    defer allocator.destroy(expr);

    expr.* = .{ .identifier = ident2 };

    let_stmt.token = Token{ .type = .let, .literal = "let" };
    let_stmt.name = ident1;
    let_stmt.value = expr;

    stmt.* = .{ .let_statement = let_stmt };

    try statements.append(stmt);

    const program = Program{ .allocator = allocator, .statements = statements };

    try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();
    try program.string(buffer.writer());
    try std.testing.expectEqualStrings("let myVar = anotherVar;", buffer.items);
}
