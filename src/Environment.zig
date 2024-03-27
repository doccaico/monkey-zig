const std = @import("std");

const Object = @import("Object.zig");

const Environment = @This();

store: std.StringHashMap(*Object.Object),

pub fn init(allocator: std.mem.Allocator) Environment {
    return Environment{
        .store = std.StringHashMap(*Object.Object).init(allocator),
    };
}

pub fn deinit(self: *Environment) void {
    self.store.deinit();
}

pub fn get(self: Environment, key: []const u8) ?*Object.Object {
    return self.store.get(key);
}

pub fn set(self: *Environment, key: []const u8, value: *Object.Object) void {
    self.store.put(key, value) catch @panic("OOM");
}
