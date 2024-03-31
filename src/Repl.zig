const builtin = @import("builtin");
const std = @import("std");

const Environment = @import("Environment.zig");
const Evaluator = @import("Evaluator.zig");
const Globals = @import("Globals.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

const checkParserErrors = Parser.checkParserErrors;

const PROMPT = ">> ";
const DELIMITER = if (builtin.os.tag == .windows) '\r' else '\n';
const MONKEY_FACE =
    \\            __,__
    \\   .--.  .-"     "-.  .--.
    \\  / .. \/  .-. .-.  \/ .. \
    \\ | |  '|  /   Y   \  |'  | |
    \\ | \   \  \ 0 | 0 /  /   / |
    \\  \ '- ,\.-"""""""-./, -' /
    \\   ''-' /_   ^ ^   _\ '-''
    \\       |  \._   _./  |
    \\       \   \ '~' /   /
    \\        '._ '-=-' _.'
    \\           '-----'
;

pub fn start(stdin: anytype, stdout: anytype) !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = general_purpose_allocator.allocator();

    Globals.init(gpa);
    defer Globals.deinit();

    var env = Environment.init(gpa);
    defer env.deinit();

    loop: while (true) {
        try stdout.writeAll(PROMPT);

        var input = std.ArrayList(u8).init(gpa);

        stdin.streamUntilDelimiter(input.writer(), DELIMITER, null) catch |err| switch (err) {
            error.EndOfStream => {
                try stdout.writeAll("KeyboardInterrupt");
                input.deinit();
                break :loop;
            },
            else => |x| return x,
        };

        const space = if (builtin.os.tag == .windows) " \n" else " ";
        const line = std.mem.trim(u8, input.items, space);
        if (line.len == 0) {
            input.clearRetainingCapacity();
            continue;
        }

        const lexer = Lexer.init(line);
        defer Globals.lineAppend(line);
        var parser = try Parser.init(gpa, lexer);
        defer parser.deinit();
        const node_program = parser.parseProgram();
        defer Globals.nodeProgramAppend(node_program);

        if (parser.errors.items.len != 0) {
            try printParserErrors(stdout, parser.errors);
            input.deinit();
            continue;
        }

        var evaluator = Evaluator.init(gpa);

        if (evaluator.eval(node_program, env)) |result| {
            try result.inspect(stdout);
            try stdout.writeByte('\n');
        }

        input.clearRetainingCapacity();
    }
}

fn printParserErrors(writer: anytype, errors: std.ArrayList([]const u8)) !void {
    _ = try writer.print("{s}\n", .{MONKEY_FACE});
    _ = try writer.write("Woops! We ran into some monkey business here!\n");
    for (errors.items) |msg| {
        try writer.print("\t{s}\n", .{msg});
    }
}

test "TestRepl" {
    const Test = struct {
        []const u8,
        i64,
    };
    // use '0' for null value
    const tests = [_]Test{
        .{ "let addTwo = fn(x) { x + 2; }; 0;", 0 },
        .{ "addTwo(2);", 4 },
        // 3.10 - Functions & Function Calls
        .{ "let add = fn(a, b) { a + b }; 0", 0 },
        .{ "let sub = fn(a, b) { a - b }; 0", 0 },
        .{ "let applyFunc = fn(a, b, func) { func(a, b) }; 0;", 0 },
        .{ "applyFunc(2, 2, add);", 4 },
        .{ "applyFunc(10, 2, sub);", 8 },
    };

    Globals.init(std.testing.allocator);
    defer Globals.deinit();

    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    var line_list = std.ArrayList([]const u8).init(std.testing.allocator);

    for (tests) |t| {
        const line = try std.testing.allocator.alloc(u8, t[0].len);
        @memcpy(line, t[0]);
        try line_list.append(line);

        const lexer = Lexer.init(line);
        var parser = try Parser.init(std.testing.allocator, lexer);
        defer parser.deinit();
        const node_program = parser.parseProgram();
        defer Globals.nodeProgramAppend(node_program);

        checkParserErrors(parser);

        var evaluator = Evaluator.init(std.testing.allocator);

        const result = evaluator.eval(node_program, env);

        {
            const expected = t[1];
            const actual = result.?.integer.value;
            try std.testing.expectEqual(expected, actual);
        }
    }

    for (line_list.items) |item| {
        std.testing.allocator.free(item);
    }
    line_list.deinit();
}
