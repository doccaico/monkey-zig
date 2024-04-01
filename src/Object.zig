const std = @import("std");

const Environment = @import("Environment.zig");

const Ast = struct {
    usingnamespace @import("Expression.zig");
    usingnamespace @import("Statement.zig");
};

const ObjectType = []const u8;
pub const BuiltinFunction = *const fn (args: std.ArrayList(*Object)) *Object;

pub const INTEGER_OBJ: ObjectType = "INTEGER";
pub const BOOLEAN_OBJ: ObjectType = "BOOLEAN";
pub const NULL_OBJ: ObjectType = "NULL";
pub const RETURN_VALUE_OBJ: ObjectType = "RETURN_VALUE";
pub const ERROR_OBJ: ObjectType = "ERROR";
pub const FUNCTION_OBJ: ObjectType = "FUNCTION";
pub const STRING_OBJ: ObjectType = "STRING";
pub const BUILTIN_OBJ: ObjectType = "BUILTIN";
pub const ARRAY_OBJ: ObjectType = "ARRAY";

pub const Object = union(enum(u8)) {
    integer: *Integer,
    boolean: *Boolean,
    null: *Null,
    return_value: *ReturnValue,
    @"error": *Error,
    function: *Function,
    string: *String,
    builtin: *Builtin,
    array: *Array,

    pub fn inspect(self: Object, writer: anytype) !void {
        switch (self) {
            inline else => |x| try x.inspect(writer),
        }
    }

    pub fn getType(self: Object) ObjectType {
        return switch (self) {
            inline else => |x| x.getType(),
        };
    }
};

pub const Integer = struct {
    value: i64,

    pub fn inspect(self: Integer, writer: anytype) !void {
        try writer.print("{d}", .{self.value});
    }

    pub fn getType(_: Integer) ObjectType {
        return INTEGER_OBJ;
    }
};

pub const Boolean = struct {
    value: bool,

    pub fn inspect(self: Boolean, writer: anytype) !void {
        try writer.print("{}", .{self.value});
    }

    pub fn getType(_: Boolean) ObjectType {
        return BOOLEAN_OBJ;
    }
};

pub const Null = struct {
    pub fn inspect(_: Null, writer: anytype) !void {
        try writer.writeAll("null");
    }

    pub fn getType(_: Null) ObjectType {
        return NULL_OBJ;
    }
};

pub const ReturnValue = struct {
    value: *Object,

    pub fn inspect(self: ReturnValue, writer: anytype) anyerror!void {
        switch (self.value.*) {
            inline else => |x| try x.inspect(writer),
        }
    }

    pub fn getType(_: ReturnValue) ObjectType {
        return RETURN_VALUE_OBJ;
    }
};

pub const Error = struct {
    message: []const u8,

    pub fn inspect(self: Error, writer: anytype) !void {
        try writer.print("ERROR: {s}", .{self.message});
    }

    pub fn getType(_: Error) ObjectType {
        return ERROR_OBJ;
    }
};

pub const Function = struct {
    parameters: std.ArrayList(*Ast.Identifier),
    body: *Ast.BlockStatement,
    env: *Environment,

    pub fn inspect(self: Function, writer: anytype) !void {
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

    pub fn getType(_: Function) ObjectType {
        return FUNCTION_OBJ;
    }
};

pub const String = struct {
    value: []const u8,

    pub fn inspect(self: String, writer: anytype) !void {
        try writer.writeAll(self.value);
    }

    pub fn getType(_: String) ObjectType {
        return STRING_OBJ;
    }
};

pub const Builtin = struct {
    function: BuiltinFunction,

    pub fn inspect(_: Builtin, writer: anytype) !void {
        try writer.writeAll("builtin function");
    }

    pub fn getType(_: Builtin) ObjectType {
        return BUILTIN_OBJ;
    }
};

pub const Array = struct {
    elements: std.ArrayList(*Object),

    pub fn inspect(self: Array, writer: anytype) anyerror!void {
        try writer.writeAll("[");

        const size = self.elements.items.len;
        for (self.elements.items, 0..) |elem, i| {
            switch (elem.*) {
                inline else => |x| try x.inspect(writer),
            }
            if (i < size - 1) {
                try writer.writeAll(", ");
            }
        }
        try writer.writeAll("]");
    }

    pub fn getType(_: Array) ObjectType {
        return ARRAY_OBJ;
    }
};
