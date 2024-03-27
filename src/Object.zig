const Evaluator = @import("Evaluator.zig");

const ObjectType = []const u8;

pub const INTEGER_OBJ: ObjectType = "INTEGER";
pub const BOOLEAN_OBJ: ObjectType = "BOOLEAN";
pub const NULL_OBJ: ObjectType = "NULL";
pub const RETURN_VALUE_OBJ: ObjectType = "RETURN_VALUE";
pub const ERROR_OBJ: ObjectType = "ERROR";

pub const Object = union(enum(u8)) {
    integer: *Integer,
    boolean: *Boolean,
    null: *Null,
    return_value: *ReturnValue,
    @"error": *Error,

    pub fn inspect(self: Object, writer: anytype) !void {
        return switch (self) {
            inline else => |tag| tag.inspect(writer),
        };
    }

    pub fn getType(self: Object) ObjectType {
        return switch (self) {
            inline else => |tag| tag.getType(),
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
        try writer.write("null");
    }

    pub fn getType(_: Null) ObjectType {
        return NULL_OBJ;
    }
};

pub const ReturnValue = struct {
    value: *Object,

    pub fn inspect(self: ReturnValue, writer: anytype) !void {
        try writer.write(self.value.inspect(writer));
    }

    pub fn getType(_: ReturnValue) ObjectType {
        return RETURN_VALUE_OBJ;
    }
};

pub const Error = struct {
    message: []const u8,

    pub fn inspect(self: Error, writer: anytype) !void {
        try writer.print("ERROR: {}", .{self.message});
    }

    pub fn getType(_: Error) ObjectType {
        return ERROR_OBJ;
    }
};
