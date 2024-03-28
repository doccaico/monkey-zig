const std = @import("std");

const Object = @import("Object.zig");

const Environment = @This();

pub var TRUE: *Object.Object = undefined;
pub var FALSE: *Object.Object = undefined;
pub var NULL: *Object.Object = undefined;

var allocator: std.mem.Allocator = undefined;

store: std.StringHashMap(*Object.Object),

pub fn init(environment_allocator: std.mem.Allocator) Environment {
    allocator = environment_allocator;
    TRUE = createObjectBoolean(true);
    FALSE = createObjectBoolean(false);
    NULL = createObjectNull();
    return Environment{
        .store = std.StringHashMap(*Object.Object).init(allocator),
    };
}

pub fn deinit(self: *Environment) void {
    allocator.destroy(TRUE.boolean);
    allocator.destroy(TRUE);
    allocator.destroy(FALSE.boolean);
    allocator.destroy(FALSE);
    allocator.destroy(NULL.null);
    allocator.destroy(NULL);

    var iterator = self.store.keyIterator();
    while (iterator.next()) |key| {
        allocator.free(key.*);
    }
    self.store.deinit();
}

pub fn get(self: Environment, key: []const u8) ?*Object.Object {
    return self.store.get(key);
}

pub fn set(self: *Environment, key: []const u8, value: *Object.Object) void {
    if (self.store.contains(key)) {
        const key_ptr = self.store.getKeyPtr(key).?;
        allocator.free(key_ptr.*);
        self.store.removeByPtr(key_ptr);
    }
    const clone_key = allocator.alloc(u8, key.len) catch @panic("OOM");
    @memcpy(clone_key, key);
    self.store.put(clone_key, value) catch @panic("OOM");
}

fn createObjectBoolean(value: bool) *Object.Object {
    const new_boolean_obj = allocator.create(Object.Boolean) catch @panic("OOM");
    new_boolean_obj.value = value;

    const new_obj = allocator.create(Object.Object) catch @panic("OOM");
    new_obj.* = Object.Object{ .boolean = new_boolean_obj };
    return new_obj;
}

fn createObjectNull() *Object.Object {
    const new_null_obj = allocator.create(Object.Null) catch @panic("OOM");

    const new_obj = allocator.create(Object.Object) catch @panic("OOM");
    new_obj.* = Object.Object{ .null = new_null_obj };
    return new_obj;
}

test "TestEnvironment" {
    const Evaluator = @import("Evaluator.zig");
    const Globals = @import("Globals.zig");
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

        const result = evaluator.eval(node, &env);

        {
            const expected = t[1];
            const actual = result.?.integer.value;
            try std.testing.expectEqual(expected, actual);
        }
    }
}
