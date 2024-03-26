const Evaluator = @import("Evaluator.zig");

const ObjectType = []const u8;

pub const INTEGER_OBJ: ObjectType = "INTEGER";
pub const BOOLEAN_OBJ: ObjectType = "BOOLEAN";
pub const NULL_OBJ: ObjectType = "NULL";
pub const RETURN_VALUE_OBJ: ObjectType = "RETURN_VALUE";

pub const Object = union(enum(u8)) {
    integer: *Integer,
    boolean: *Boolean,
    null: *Null,
    return_value: *ReturnValue,

    pub fn inspect(self: Object, writer: anytype) anyerror!void {
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

    pub fn inspect(self: Integer, writer: anytype) anyerror!void {
        try writer.print("{d}", .{self.value});
    }

    pub fn getType(self: Integer) ObjectType {
        _ = self;
        return INTEGER_OBJ;
    }
};

pub const Boolean = struct {
    value: bool,

    pub fn inspect(self: Boolean, writer: anytype) anyerror!void {
        try writer.print("{}", .{self.value});
    }

    pub fn getType(self: Boolean) ObjectType {
        _ = self;
        return BOOLEAN_OBJ;
    }
};

pub const Null = struct {
    pub fn inspect(self: Null, writer: anytype) anyerror!void {
        _ = self;
        _ = try writer.write("null");
    }

    pub fn getType(self: Null) ObjectType {
        _ = self;
        return NULL_OBJ;
    }
};

pub const ReturnValue = struct {
    value: *Object,

    pub fn inspect(self: ReturnValue, writer: anytype) anyerror!void {
        _ = try writer.write(self.value.inspect(writer));
    }

    pub fn getType(self: ReturnValue) ObjectType {
        _ = self;
        return RETURN_VALUE_OBJ;
    }
};

// functions

pub fn createObjectBoolean(value: bool) *Object {
    const new_boolean_obj = Evaluator.eval_allocator.create(Boolean) catch @panic("OOM");
    new_boolean_obj.value = value;

    const new_obj = Evaluator.createObject();
    new_obj.* = Object{ .boolean = new_boolean_obj };

    return new_obj;
}

pub fn createObjectNull() *Object {
    const new_null_obj = Evaluator.eval_allocator.create(Null) catch @panic("OOM");

    const new_obj = Evaluator.createObject();
    new_obj.* = Object{ .null = new_null_obj };

    return new_obj;
}

pub fn createObjectInteger() *Integer {
    const new_obj = Evaluator.eval_allocator.create(Integer) catch @panic("OOM");

    return new_obj;
}

pub fn createObjectReturnValue() *ReturnValue {
    const new_obj = Evaluator.eval_allocator.create(ReturnValue) catch @panic("OOM");

    return new_obj;
}
