const std = @import("std");

const Globals = @import("Globals.zig");
const Object = @import("Object.zig");

const Environment = @This();

pub var TRUE: *Object.Object = undefined;
pub var FALSE: *Object.Object = undefined;
pub var NULL: *Object.Object = undefined;

allocator: std.mem.Allocator,
store: std.StringHashMap(*Object.Object),
outer: ?*Environment,

pub fn init(allocator: std.mem.Allocator) *Environment {
    TRUE = createObjectBoolean(allocator, true);
    FALSE = createObjectBoolean(allocator, false);
    NULL = createObjectNull(allocator);

    const env = allocator.create(Environment) catch @panic("OOM");
    env.* = Environment{
        .allocator = allocator,
        .store = std.StringHashMap(*Object.Object).init(allocator),
        .outer = null,
    };
    Globals.envAppend(env);
    return env;
}

pub fn deinit(self: *Environment) void {
    self.allocator.destroy(TRUE.boolean);
    self.allocator.destroy(TRUE);
    self.allocator.destroy(FALSE.boolean);
    self.allocator.destroy(FALSE);
    self.allocator.destroy(NULL.null);
    self.allocator.destroy(NULL);
}

pub fn newEnclosedEnvironment(self: *Environment, outer: ?*Environment) *Environment {
    const new_outer_env = self.allocator.create(Environment) catch @panic("OOM");
    new_outer_env.allocator = self.allocator;
    new_outer_env.store = std.StringHashMap(*Object.Object).init(self.allocator);
    if (outer) |x| {
        var iterator = x.store.keyIterator();
        while (iterator.next()) |key| {
            const obj = self.store.get(key.*).?;

            const clone_key = self.allocator.alloc(u8, key.len) catch @panic("OOM");
            @memcpy(clone_key, key.*);
            new_outer_env.store.put(clone_key, obj) catch @panic("OOM");
        }
    }
    new_outer_env.outer = null;

    Globals.envAppend(new_outer_env);

    const env = self.allocator.create(Environment) catch @panic("OOM");
    env.allocator = self.allocator;
    env.store = std.StringHashMap(*Object.Object).init(self.allocator);
    env.outer = new_outer_env;

    Globals.envAppend(env);

    return env;
}

pub fn get(self: Environment, key: []const u8) ?*Object.Object {
    var obj = self.store.get(key);
    if (obj == null and self.outer != null) {
        obj = self.outer.?.get(key);
    }
    return obj;
}

pub fn set(self: *Environment, key: []const u8, value: *Object.Object) void {
    if (self.store.contains(key)) {
        const key_ptr = self.store.getKeyPtr(key).?;
        self.allocator.free(key_ptr.*);
        self.store.removeByPtr(key_ptr);
    }
    const clone_key = self.allocator.alloc(u8, key.len) catch @panic("OOM");
    @memcpy(clone_key, key);
    self.store.put(clone_key, value) catch @panic("OOM");
}

fn createObjectBoolean(allocator: std.mem.Allocator, value: bool) *Object.Object {
    const new_boolean_obj = allocator.create(Object.Boolean) catch @panic("OOM");
    new_boolean_obj.value = value;

    const new_obj = allocator.create(Object.Object) catch @panic("OOM");
    new_obj.* = Object.Object{ .boolean = new_boolean_obj };
    return new_obj;
}

fn createObjectNull(allocator: std.mem.Allocator) *Object.Object {
    const new_null_obj = allocator.create(Object.Null) catch @panic("OOM");

    const new_obj = allocator.create(Object.Object) catch @panic("OOM");
    new_obj.* = Object.Object{ .null = new_null_obj };
    return new_obj;
}

test "TestEnvironment" {
    const Evaluator = @import("Evaluator.zig");
    const Lexer = @import("Lexer.zig");
    const Parser = @import("Parser.zig");

    const checkParserErrors = Parser.checkParserErrors;

    const Test = struct {
        []const u8,
        i64,
    };
    const tests = [_]Test{
        .{ "let a = 5; let b = 10; a + b;", 15 },
        .{ "b - a;", 5 },
        .{ "let c = a * b; a + b + c;", 65 },
        .{ "let a = 50; let b = a; a + b;", 100 },
    };

    Globals.init(std.testing.allocator);
    defer Globals.deinit();

    var env = init(std.testing.allocator);
    defer env.deinit();

    for (tests) |t| {
        const lexer = Lexer.init(t[0]);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        var node = parser.parseProgram();
        defer node.deinit();

        checkParserErrors(parser);

        var evaluator = Evaluator.init(std.testing.allocator);

        const result = evaluator.eval(node, env);

        {
            const expected = t[1];
            const actual = result.?.integer.value;
            try std.testing.expectEqual(expected, actual);
        }
    }
}