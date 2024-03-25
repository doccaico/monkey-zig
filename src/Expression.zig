const std = @import("std");

const Token = @import("Token.zig");
const ParserError = @import("Parser.zig").ParserError;
const Ast = struct {
    usingnamespace @import("Statement.zig");
};

pub const Expression = union(enum(u8)) {
    @"error": ParserError,
    identifier: *Identifier,
    integer_literal: *IntegerLiteral,
    prefix_expression: *PrefixExpression,
    infix_expression: *InfixExpression,
    boolean: *BooleanExpression,
    if_expression: *IfExpression,
    function_literal: *FunctionLiteral,
    call_expression: *CallExpression,

    pub fn tokenLiteral(self: Expression) []const u8 {
        return switch (self) {
            .@"error" => "",
            inline else => |tag| tag.tokenLiteral(),
        };
    }

    pub fn string(self: Expression, writer: anytype) anyerror!void {
        return switch (self) {
            .@"error" => {},
            inline else => |tag| tag.string(writer),
        };
    }
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,

    pub fn tokenLiteral(self: Identifier) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: Identifier, writer: anytype) anyerror!void {
        _ = try writer.write(self.value);
    }
};

pub const IntegerLiteral = struct {
    token: Token,
    allocator: std.mem.Allocator,
    value: i64,

    pub fn tokenLiteral(self: IntegerLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: IntegerLiteral, writer: anytype) anyerror!void {
        _ = try writer.write(self.token.literal);
    }
};

pub const PrefixExpression = struct {
    token: Token,
    operator: []const u8,
    right: *Expression,

    pub fn tokenLiteral(self: PrefixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: PrefixExpression, writer: anytype) anyerror!void {
        _ = try writer.write("(");
        _ = try writer.write(self.operator);
        try self.right.string(writer);
        _ = try writer.write(")");
    }
};

pub const InfixExpression = struct {
    token: Token,
    left: *Expression,
    operator: []const u8,
    right: *Expression,

    pub fn tokenLiteral(self: InfixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: InfixExpression, writer: anytype) anyerror!void {
        _ = try writer.write("(");
        try self.left.string(writer);
        try writer.print(" {s} ", .{self.operator});
        try self.right.string(writer);
        _ = try writer.write(")");
    }
};

pub const BooleanExpression = struct {
    token: Token,
    value: bool,

    pub fn tokenLiteral(self: BooleanExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: BooleanExpression, writer: anytype) anyerror!void {
        _ = try writer.write(self.token.literal);
    }
};

pub const IfExpression = struct {
    token: Token,
    condition: *Expression,
    consequence: *Ast.BlockStatement,
    alternative: ?*Ast.BlockStatement,

    pub fn tokenLiteral(self: IfExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: IfExpression, writer: anytype) anyerror!void {
        _ = try writer.write("if");
        try self.condition.string(writer);
        _ = try writer.write(" ");
        try self.consequence.string(writer);

        if (self.alternative) |alternative| {
            _ = try writer.write("else ");
            try alternative.string(writer);
        }
    }
};

pub const FunctionLiteral = struct {
    token: Token,
    parameters: std.ArrayList(Identifier),
    body: *Ast.BlockStatement,

    pub fn init(allocator: std.mem.Allocator, token: Token) *FunctionLiteral {
        const f = allocator.create(FunctionLiteral) catch @panic("OOM");
        f.token = token;
        f.parameters = std.ArrayList(Identifier).init(allocator);
        f.body = undefined;
        return f;
    }

    pub fn deinit(self: FunctionLiteral) void {
        self.parameters.deinit();
        self.body.deinit();
    }

    pub fn tokenLiteral(self: FunctionLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: FunctionLiteral, writer: anytype) anyerror!void {
        _ = try writer.write(self.tokenLiteral());
        _ = try writer.write("(");
        const size = self.parameters.items.len;
        for (self.parameters.items, 0..) |param, i| {
            try param.string(writer);
            if (i < size - 1) {
                _ = try writer.write(", ");
            }
        }
        _ = try writer.write(") ");
        try self.body.string(writer);
    }
};

pub const CallExpression = struct {
    token: Token,
    function: *Expression,
    arguments: std.ArrayList(*Expression),

    pub fn init(allocator: std.mem.Allocator, token: Token, function: *Expression) *CallExpression {
        const ce = allocator.create(CallExpression) catch @panic("OOM");
        ce.token = token;
        ce.function = function;
        ce.arguments = std.ArrayList(*Expression).init(allocator);
        return ce;
    }

    pub fn deinit(self: CallExpression) void {
        self.arguments.deinit();
    }

    pub fn tokenLiteral(self: CallExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: CallExpression, writer: anytype) !void {
        try self.function.string(writer);
        _ = try writer.write("(");
        const size = self.arguments.items.len;
        for (self.arguments.items, 0..) |arg, i| {
            try arg.string(writer);
            if (i < size - 1) {
                _ = try writer.write(", ");
            }
        }
        _ = try writer.write(")");
    }
};
