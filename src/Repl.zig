const builtin = @import("builtin");
const std = @import("std");

const Ast = @import("Ast.zig");
const Environment = @import("Environment.zig");
const Evaluator = @import("Evaluator.zig");
const Globals = @import("Globals.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

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

    var input = std.ArrayList(u8).init(gpa);

    Globals.init(gpa);
    defer Globals.deinit();

    var env = Environment.init(gpa);
    defer env.deinit();

    // var node: *Ast.Node = undefined;

    loop: while (true) {
        try stdout.writeAll(PROMPT);

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
        var parser = try Parser.init(gpa, lexer);
        defer parser.deinit();
        var node = parser.parseProgram();
        defer node.deinit();

        if (parser.errors.items.len != 0) {
            try printParserErrors(stdout, parser.errors);
            input.deinit();
            continue;
        }

        var evaluator = Evaluator.init(gpa);

        if (evaluator.eval(node, env)) |result| {
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
