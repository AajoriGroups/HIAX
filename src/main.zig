const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Compiler = @import("codegen.zig").Compiler;
const Evaluator = @import("evaluator.zig").Evaluator;
const Environment = @import("environment.zig").Environment;
const ast = @import("ast.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: hiax <file.hix> or hiax [options]\n", .{});
        std.debug.print("Options:\n", .{});
        std.debug.print("  -v, --version    Show version info\n", .{});
        std.debug.print("  -h, --help       Show help message\n", .{});
        std.debug.print("  --list           List bilingual keywords\n", .{});
        std.debug.print("  --build <file>   Run via Transpiler (requires Zig)\n", .{});
        return;
    }

    const command_or_file = args[1];

    if (std.mem.eql(u8, command_or_file, "--version") or std.mem.eql(u8, command_or_file, "-v")) {
        std.debug.print("Hiax Language v0.2.0 (Beta)\n", .{});
        std.debug.print("Powered by Hiax Engine (Standalone)\n", .{});
        std.debug.print("Developed By: Aajori Groups\n", .{});
        return;
    }

    if (std.mem.eql(u8, command_or_file, "--help") or std.mem.eql(u8, command_or_file, "-h")) {
        std.debug.print(
            \\Hiax Language CLI Help
            \\
            \\Usage: hiax <file.hix>
            \\
            \\Bilingual Keywords:
            \\  Hiax supports both English and Hindi/Assamese keywords.
            \\  Example: 'dhor x = 10;' is the same as 'var x = 10;'
            \\
            \\Options:
            \\  -v, --version    Show version info
            \\  -h, --help       Show this help message
            \\  --list           List all keywords
            \\  --build <file>   Run via Transpiler (requires Zig)
            \\
        , .{});
        return;
    }

    if (std.mem.eql(u8, command_or_file, "--list")) {
        std.debug.print(
            \\Hiax Bilingual Keywords List:
            \\-----------------------------------------
            \\ English       | Native (H/A) | Meaning
            \\-----------------------------------------
            \\ var           | dhor         | Variable
            \\ echo/print    | bol          | Print
            \\ if            | jodi         | Condition
            \\ else          | na           | Otherwise
            \\ while         | ghur         | Loop
            \\ for           | -            | Loop
            \\ function      | kaam         | Routine
            \\ class         | shreni       | Class
            \\ new           | noya         | Instantiate
            \\ this          | iswa         | Self Reference
            \\ int           | ank          | Number
            \\ string        | txt          | Text
            \\-----------------------------------------
            \\
        , .{});
        return;
    }

    var build_mode = false;
    var filename = command_or_file;

    if (std.mem.eql(u8, command_or_file, "--build")) {
        build_mode = true;
        if (args.len < 3) {
            std.debug.print("Usage: hiax --build <file.hix>\n", .{});
            return;
        }
        filename = args[2];
    }
    
    const input = std.fs.cwd().readFileAlloc(filename, allocator, @enumFromInt(10 * 1024 * 1024)) catch |err| {
        std.debug.print("Error: Could not read file '{s}' ({s})\n", .{filename, @errorName(err)});
        return;
    };

    // std.debug.print("Compiling {s}...\n", .{filename});

    var lexer = Lexer.init(input);
    var parser = Parser.init(allocator, &lexer);
    defer parser.deinit();
    
    const program = parser.parseProgram();
    
    if (parser.errors.items.len > 0) {
        std.debug.print("Found {d} errors during parsing:\n", .{parser.errors.items.len});
        for (parser.errors.items) |err| {
            std.debug.print("{s}\n", .{err});
        }
        std.process.exit(1);
    }

    if (build_mode) {
        var compiler = Compiler.init(allocator, program);
        const result = try compiler.compile(); // Call compile

        // Write to temporary file
        const out_filename = ".hiax_temp.zig";
        const out_file = try std.fs.cwd().createFile(out_filename, .{});
        defer {
            out_file.close();
            // std.fs.cwd().deleteFile(out_filename) catch {};
        }
        try out_file.writeAll(result);

        // std.debug.print("Transpiled to {s}. Running...\n\n", .{out_filename});

        // Run the generated zig file
        // Command: zig run out.zig
        
        const argv = [_][]const u8{"zig", "run", out_filename};
        
        var child = std.process.Child.init(&argv, allocator);
        child.stdout_behavior = .Inherit;
        child.stderr_behavior = .Inherit;
        
        const term = try child.spawnAndWait();
        
        switch (term) {
            .Exited => |code| {
                 if (code != 0) {
                     std.debug.print("\nExecution failed with code {d}\n", .{code});
                 }
            },
            else => {
                std.debug.print("\nExecution terminated abnormally\n", .{});
            }
        }
    } else {
        // Standalone Engine Mode
        const env = Environment.init(allocator, null);
        var evaluator = Evaluator.init(allocator);
        const result = try evaluator.eval(.{ .Program = program }, env);
        if (result == .Error) {
             std.debug.print("{s}\n", .{result.Error});
             std.process.exit(1);
        }
    }
}
