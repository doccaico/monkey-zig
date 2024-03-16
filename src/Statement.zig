const std = @import("std");

const Token = @import("Token.zig");
const ParserError = @import("Parser.zig").ParserError;
const Ast = struct {
    usingnamespace @import("Expression.zig");
};

pub const Statement = union(enum(u8)) {
    @"error": ParserError,
    let_statement: LetStatement,
    return_statement: ReturnStatement,
    expression_statement: ExpressionStatement,
    block_statement: BlockStatement,

    pub fn tokenLiteral(self: Statement) []const u8 {
        return switch (self) {
            .@"error" => "",
            inline else => |tag| tag.tokenLiteral(),
        };
    }

    pub fn string(self: Statement, writer: anytype) anyerror!void {
        return switch (self) {
            .@"error" => {},
            inline else => |tag| tag.string(writer),
        };
    }
};

pub const LetStatement = struct {
    token: Token,
    name: Ast.Identifier,
    value: Ast.Expression,

    pub fn tokenLiteral(self: LetStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: LetStatement, writer: anytype) !void {
        try writer.print("{s} ", .{self.tokenLiteral()});
        try self.name.string(writer);
        _ = try writer.write(" = ");
        try self.value.string(writer);
        _ = try writer.write(";");
    }
};

pub const ReturnStatement = struct {
    token: Token,
    value: Ast.Expression,

    pub fn tokenLiteral(self: ReturnStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: ReturnStatement, writer: anytype) !void {
        try writer.print("{s} ", .{self.tokenLiteral()});
        try self.value.string(writer);
        _ = try writer.write(";");
    }
};

pub const ExpressionStatement = struct {
    token: Token,
    expression: Ast.Expression,

    pub fn tokenLiteral(self: ExpressionStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: ExpressionStatement, writer: anytype) !void {
        try self.expression.string(writer);
    }
};

pub const BlockStatement = struct {
    token: Token,
    statements: std.ArrayList(Statement),

    pub fn init(allocator: std.mem.Allocator, token: Token) BlockStatement {
        return BlockStatement{ .token = token, .statements = std.ArrayList(Statement).init(allocator) };
    }

    pub fn deinit(self: BlockStatement) void {
        self.statements.deinit();
    }

    pub fn tokenLiteral(self: BlockStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: BlockStatement, writer: anytype) !void {
        for (self.statements.items) |stmt| {
            try stmt.string(writer);
        }
    }
};
