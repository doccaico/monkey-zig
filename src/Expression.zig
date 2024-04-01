const std = @import("std");

const Ast = @import("Statement.zig");
const ParserError = @import("Parser.zig").ParserError;
const Token = @import("Token.zig");

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
    string_literal: *StringLiteral,
    array_literal: *ArrayLiteral,
    index_expression: *IndexExpression,
    hash_literal: *HashLiteral,

    pub fn tokenLiteral(self: Expression) []const u8 {
        return switch (self) {
            .@"error" => "",
            inline else => |x| x.tokenLiteral(),
        };
    }

    pub fn string(self: Expression, writer: anytype) anyerror!void {
        return switch (self) {
            .@"error" => {},
            inline else => |x| x.string(writer),
        };
    }
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,

    pub fn tokenLiteral(self: Identifier) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: Identifier, writer: anytype) !void {
        try writer.writeAll(self.value);
    }
};

pub const IntegerLiteral = struct {
    token: Token,
    value: i64,

    pub fn tokenLiteral(self: IntegerLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: IntegerLiteral, writer: anytype) !void {
        try writer.writeAll(self.token.literal);
    }
};

pub const PrefixExpression = struct {
    token: Token,
    operator: []const u8,
    right: *Expression,

    pub fn tokenLiteral(self: PrefixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: PrefixExpression, writer: anytype) !void {
        try writer.writeAll("(");
        try writer.writeAll(self.operator);
        try self.right.string(writer);
        try writer.writeAll(")");
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

    pub fn string(self: InfixExpression, writer: anytype) !void {
        try writer.writeAll("(");
        try self.left.string(writer);
        try writer.print(" {s} ", .{self.operator});
        try self.right.string(writer);
        try writer.writeAll(")");
    }
};

pub const BooleanExpression = struct {
    token: Token,
    value: bool,

    pub fn tokenLiteral(self: BooleanExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: BooleanExpression, writer: anytype) !void {
        try writer.writeAll(self.token.literal);
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

    pub fn string(self: IfExpression, writer: anytype) !void {
        try writer.writeAll("if");
        try self.condition.string(writer);
        try writer.writeAll(" ");
        try self.consequence.string(writer);

        if (self.alternative) |alternative| {
            try writer.writeAll("else ");
            try alternative.string(writer);
        }
    }
};

pub const FunctionLiteral = struct {
    token: Token,
    parameters: std.ArrayList(*Identifier),
    body: *Ast.BlockStatement,

    pub fn init(allocator: std.mem.Allocator, token: Token) *FunctionLiteral {
        const f = allocator.create(FunctionLiteral) catch @panic("OOM");
        f.token = token;
        f.parameters = std.ArrayList(*Identifier).init(allocator);
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

    pub fn string(self: FunctionLiteral, writer: anytype) !void {
        try writer.writeAll("fn");
        try writer.writeAll("(");
        const size = self.parameters.items.len;
        for (self.parameters.items, 0..) |param, i| {
            try param.string(writer);
            if (i < size - 1) {
                try writer.writeAll(", ");
            }
        }
        try writer.writeAll(") {\n");
        try self.body.string(writer);
        try writer.writeAll("\n}");
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
        try writer.writeAll("(");
        const size = self.arguments.items.len;
        for (self.arguments.items, 0..) |arg, i| {
            try arg.string(writer);
            if (i < size - 1) {
                try writer.writeAll(", ");
            }
        }
        try writer.writeAll(")");
    }
};

pub const StringLiteral = struct {
    token: Token,
    value: []const u8,

    pub fn tokenLiteral(self: StringLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: StringLiteral, writer: anytype) !void {
        try writer.writeAll(self.token.literal);
    }
};

pub const ArrayLiteral = struct {
    token: Token,
    elements: std.ArrayList(*Expression),

    pub fn tokenLiteral(self: ArrayLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: ArrayLiteral, writer: anytype) !void {
        try writer.writeAll("[");
        const size = self.elements.items.len;
        for (self.elements.items, 0..) |arg, i| {
            try arg.string(writer);
            if (i < size - 1) {
                try writer.writeAll(", ");
            }
        }
        try writer.writeAll("]");
    }
};

pub const IndexExpression = struct {
    token: Token,
    left: *Expression,
    index: *Expression,

    pub fn tokenLiteral(self: IndexExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: IndexExpression, writer: anytype) !void {
        try writer.writeAll("(");
        try self.left.string(writer);
        try writer.writeAll("[");
        try self.index.string(writer);
        try writer.writeAll("])");
    }
};

pub const HashLiteral = struct {
    token: Token,
    pairs: std.AutoHashMap(*Expression, *Expression),

    pub fn tokenLiteral(self: HashLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: HashLiteral, writer: anytype) !void {
        try writer.writeAll("{");

        const size = self.pairs.count();
        var i: usize = 0;
        var iterator = self.pairs.iterator();
        while (iterator.next()) |entry| {
            try entry.key_ptr.*.string(writer);
            try writer.writeAll(":");
            try entry.value_ptr.*.string(writer);
            if (i < size - 1) {
                try writer.writeAll(", ");
            }
            i += 1;
        }

        try writer.writeAll("}");
    }
};
