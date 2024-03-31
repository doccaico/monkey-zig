const std = @import("std");

const Ast = struct {
    usingnamespace @import("Expression.zig");
    usingnamespace @import("Statement.zig");
};

pub const Node = union(enum) {
    program: *Program,
    statement: *Ast.Statement,
    expression: *Ast.Expression,

    pub fn deinit(self: *Node) void {
        switch (self.*) {
            .program => |x| {
                for (x.statements.items) |stmt| {
                    switch (stmt.*) {
                        .expression_statement => |y| {
                            freeExpressionPointer(x.allocator, y.expression);
                            y.deinit();
                        },
                        .let_statement => |y| {
                            self.program.allocator.destroy(y.name);
                            freeExpressionPointer(x.allocator, y.value);
                            y.deinit();
                        },
                        .return_statement => |y| {
                            freeExpressionPointer(x.allocator, y.return_value);
                            y.deinit();
                        },
                        else => {},
                    }

                    self.program.allocator.destroy(stmt);
                }
                x.statements.deinit();
                const z = x.allocator;
                z.destroy(self.program);
                z.destroy(self);
            },
            .statement => {},
            .expression => {},
        }
    }

    fn freeExpressionPointer(allocator: std.mem.Allocator, expr: *Ast.Expression) void {
        switch (expr.*) {
            .prefix_expression => |x| {
                freeExpressionPointer(allocator, x.right);
                allocator.destroy(x);
                allocator.destroy(expr);
            },
            .infix_expression => |x| {
                freeExpressionPointer(allocator, x.left);
                freeExpressionPointer(allocator, x.right);
                allocator.destroy(x);
                allocator.destroy(expr);
            },
            .if_expression => |x| {
                freeExpressionPointer(allocator, x.condition);

                for (x.consequence.statements.items) |stmt| {
                    switch (stmt.*) {
                        .expression_statement => |y| {
                            freeExpressionPointer(allocator, y.expression);
                            y.deinit();
                        },
                        .return_statement => |y| {
                            freeExpressionPointer(allocator, y.return_value);
                            y.deinit();
                        },
                        else => {},
                    }
                    allocator.destroy(stmt);
                }

                x.consequence.deinit();

                if (x.alternative) |alt| {
                    for (alt.statements.items) |stmt| {
                        switch (stmt.*) {
                            .expression_statement => |y| {
                                freeExpressionPointer(allocator, y.expression);
                                y.deinit();
                            },
                            .return_statement => |y| {
                                freeExpressionPointer(allocator, y.return_value);
                                y.deinit();
                            },
                            else => {},
                        }
                        allocator.destroy(stmt);
                    }
                    alt.statements.deinit();
                    allocator.destroy(alt);
                }

                allocator.destroy(x);
                allocator.destroy(expr);
            },
            .function_literal => |x| {
                for (x.parameters.items) |param| {
                    allocator.destroy(param);
                }
                for (x.body.statements.items) |stmt| {
                    switch (stmt.*) {
                        .expression_statement => |y| {
                            freeExpressionPointer(allocator, y.expression);
                            y.deinit();
                        },
                        .return_statement => |y| {
                            freeExpressionPointer(allocator, y.return_value);
                            y.deinit();
                        },
                        else => {},
                    }
                    allocator.destroy(stmt);
                }
                x.deinit();
                allocator.destroy(x);
                allocator.destroy(expr);
            },
            .call_expression => |x| {
                freeExpressionPointer(allocator, x.function);
                for (x.arguments.items) |arg| {
                    freeExpressionPointer(allocator, arg);
                }
                x.deinit();

                allocator.destroy(x);
                allocator.destroy(expr);
            },
            .integer_literal => |x| {
                allocator.destroy(x);
                allocator.destroy(expr);
            },
            .boolean => |x| {
                allocator.destroy(x);
                allocator.destroy(expr);
            },
            .identifier => |x| {
                allocator.destroy(x);
                allocator.destroy(expr);
            },
            .string_literal => |x| {
                allocator.destroy(x);
                allocator.destroy(expr);
            },
            .array_literal => |x| {
                for (x.elements.items) |elem| {
                    freeExpressionPointer(allocator, elem);
                }
                x.elements.deinit();
                allocator.destroy(x);
                allocator.destroy(expr);
            },
            else => {},
        }
    }

    pub fn string(self: Node, writer: anytype) !void {
        switch (self) {
            inline else => |x| try x.string(writer),
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
        node.* = Node{ .program = prg };
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
                inline else => |x| try x.string(writer),
                .@"error" => {},
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

    expr.* = Ast.Expression{ .identifier = ident2 };

    let_stmt.token = Token{ .type = .let, .literal = "let" };
    let_stmt.name = ident1;
    let_stmt.value = expr;

    stmt.* = Ast.Statement{ .let_statement = let_stmt };

    try statements.append(stmt);

    const program = Program{ .allocator = allocator, .statements = statements };

    try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();
    try program.string(buffer.writer());
    try std.testing.expectEqualStrings("let myVar = anotherVar;", buffer.items);
}
