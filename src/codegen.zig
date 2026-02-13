const std = @import("std");
const ast = @import("ast.zig");

pub const CompileError = error{ OutOfMemory, WriteError };

pub const Compiler = struct {
    program: *ast.Program,
    allocator: std.mem.Allocator,
    output: std.ArrayList(u8),
    is_inside_method: bool,

    pub fn init(allocator: std.mem.Allocator, program: *ast.Program) Compiler {
        return Compiler{
            .program = program,
            .allocator = allocator,
            .output = .empty,
            .is_inside_method = false,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.output.deinit(self.allocator);
    }

    fn render(self: *Compiler, comptime fmt: []const u8, args: anytype) CompileError!void {
        const str = std.fmt.allocPrint(self.allocator, fmt, args) catch return CompileError.OutOfMemory;
        defer self.allocator.free(str);
        self.output.appendSlice(self.allocator, str) catch return CompileError.OutOfMemory;
    }

    pub fn compile(self: *Compiler) CompileError![]const u8 {
        // Add standard imports
        try self.render("const std = @import(\"std\");\n\n", .{});
        
        // HIAX Value type for dynamic behavior in Zig output
        try self.render(
            \\const Value = union(enum) {{
            \\    int: i64,
            \\    string: []const u8,
            \\    boolean: bool,
            \\    object: *anyopaque,
            \\
            \\    fn isTrue(self: Value) bool {{
            \\        return switch (self) {{
            \\            .boolean => |b| b,
            \\            .int => |i| i != 0,
            \\            else => false,
            \\        }};
            \\    }}
            \\
            \\    fn from(v: anytype) Value {{
            \\        const T = @TypeOf(v);
            \\        if (T == Value) return v;
            \\        if (T == i64 or T == comptime_int) return .{{ .int = v }};
            \\        if (T == bool) return .{{ .boolean = v }};
            \\        
            \\        // Handle strings (slices, arrays, and pointers to arrays)
            \\        const is_string = comptime switch (@typeInfo(T)) {{
            \\            .pointer => |ptr| blk: {{
            \\                if (ptr.size == .slice) break :blk ptr.child == u8;
            \\                if (ptr.size == .one) {{
            \\                    if (@typeInfo(ptr.child) == .array) break :blk @typeInfo(ptr.child).array.child == u8;
            \\                }}
            \\                break :blk false;
            \\            }},
            \\            else => false,
            \\        }};
            \\
            \\        if (is_string) return .{{ .string = v }};
            \\
            \\        return .{{ .object = @constCast(@ptrCast(v)) }};
            \\    }}
            \\}};
            \\
            \\fn _concat(allocator: std.mem.Allocator, a: Value, b: Value) Value {{
            \\    var s1: []const u8 = "";
            \\    var s2: []const u8 = "";
            \\    var buf1: [32]u8 = undefined;
            \\    var buf2: [32]u8 = undefined;
            \\    
            \\    switch(a) {{
            \\        .int => |v| s1 = std.fmt.bufPrint(&buf1, "{{d}}", .{{v}}) catch "",
            \\        .string => |v| s1 = v,
            \\        else => {{}},
            \\    }}
            \\    switch(b) {{
            \\        .int => |v| s2 = std.fmt.bufPrint(&buf2, "{{d}}", .{{v}}) catch "",
            \\        .string => |v| s2 = v,
            \\        else => {{}},
            \\    }}
            \\    const res = std.mem.concat(allocator, u8, &[_][]const u8{{s1, s2}}) catch "";
            \\    return .{{ .string = res }};
            \\}}
            \\
            \\fn _add(allocator: std.mem.Allocator, a: Value, b: Value) Value {{
            \\    // If both are ints, do arithmetic addition
            \\    if (a == .int and b == .int) {{
            \\        return .{{ .int = a.int + b.int }};
            \\    }}
            \\    // Otherwise, fall back to string concatenation
            \\    return _concat(allocator, a, b);
            \\}}
            \\
            \\// Print helper - properly escaped for std.fmt
            \\const _print = struct {{
            \\    fn p(val: anytype) void {{
            \\        const v = if (@TypeOf(val) == Value) val else Value.from(val);
            \\        var stdout_w = std.fs.File.stdout().writer(&.{{}});
            \\        const stdout = &stdout_w.interface;
            \\        switch(v) {{
            \\            .int => |i| stdout.print("{{d}}\n", .{{i}}) catch {{}},
            \\            .string => |s| stdout.print("{{s}}\n", .{{s}}) catch {{}},
            \\            .boolean => |b| stdout.print("{{}}\n", .{{b}}) catch {{}},
            \\            .object => |o| stdout.print("Object<{{*}}>\n", .{{o}}) catch {{}},
            \\        }}
            \\    }}
            \\}}.p;
            \\
        , .{});

        // Collect classes first (Zig needs them at top level or before main)
        for (self.program.statements.items) |stmt| {
            if (stmt.* == .Class) {
                try self.compileClassStatement(stmt.Class);
            }
        }

        try self.render("pub fn main() !void {{\n", .{});
        // We might need a small arena for 'noya' (new) allocations in pure Zig output
        try self.render("    var gpa = std.heap.GeneralPurposeAllocator(.{{}}){{}};\n", .{});
        try self.render("    const allocator = gpa.allocator();\n\n", .{});

        for (self.program.statements.items) |stmt| {
            if (stmt.* != .Class) {
                 try self.compileStatement(stmt);
            }
        }

        if (self.program.statements.items.len == 0) {
             try self.render("    _ = allocator;\n", .{});
             try self.render("    _ = gpa;\n", .{});
        }
        try self.render("}}\n", .{});
        return self.output.items;
    }

    fn compileClassStatement(self: *Compiler, stmt: *ast.ClassStatement) CompileError!void {
        try self.render("const {s} = struct {{\n", .{stmt.name.value});
        
        // Basic field discovery: look for 'iswa.member = ...' in all methods
        var fields = std.StringHashMap(void).init(self.allocator);
        defer fields.deinit();

        for (stmt.methods.items) |method| {
            self.discoverFieldsInBlock(method.body, &fields);
        }

        try self.render("    allocator: std.mem.Allocator,\n", .{});
        var f_it = fields.iterator();
        while (f_it.next()) |entry| {
            try self.render("    {s}: Value = .{{ .int = 0 }},\n", .{entry.key_ptr.*});
        }
        
        for (stmt.methods.items) |method| {
            try self.compileMethod(stmt.name.value, method);
        }

        // Add an init helper
        try self.render("    pub fn init(allocator: std.mem.Allocator) !*{s} {{\n", .{stmt.name.value});
        try self.render("        const instance = try allocator.create({s});\n", .{stmt.name.value});
        try self.render("        instance.* = .{{ .allocator = allocator }};\n", .{}); 
        try self.render("        return instance;\n", .{});
        try self.render("    }}\n", .{});

        try self.render("}};\n\n", .{});
    }

    fn discoverFieldsInBlock(self: *Compiler, block: *ast.BlockStatement, fields: *std.StringHashMap(void)) void {
        for (block.statements.items) |stmt| {
            self.discoverFieldsInStatement(stmt, fields);
        }
    }

    fn discoverFieldsInStatement(self: *Compiler, stmt: *ast.Statement, fields: *std.StringHashMap(void)) void {
        switch (stmt.*) {
            .Expression => |expr_stmt| {
                self.discoverFieldsInExpression(expr_stmt.expression, fields);
            },
            .Block => |block_stmt| {
                self.discoverFieldsInBlock(block_stmt, fields);
            },
            .Let => |let_stmt| {
                self.discoverFieldsInExpression(let_stmt.value, fields);
            },
            .For => |for_stmt| {
                if (for_stmt.init) |init_stmt| self.discoverFieldsInStatement(init_stmt, fields);
                if (for_stmt.condition) |cond| self.discoverFieldsInExpression(cond, fields);
                if (for_stmt.post) |post| self.discoverFieldsInStatement(post, fields);
                self.discoverFieldsInBlock(for_stmt.body, fields);
            },
            // Note: If and While are Expressions in this AST
            else => {},
        }
    }

    fn discoverFieldsInExpression(self: *Compiler, expr: *ast.Expression, fields: *std.StringHashMap(void)) void {
        switch (expr.*) {
            .Infix => |infix| {
                if (std.mem.eql(u8, infix.operator, "=")) {
                    const left = infix.left;
                    if (left.* == .Dot and left.Dot.left.* == .This) {
                        fields.put(left.Dot.member.value, {}) catch unreachable;
                    }
                }
                self.discoverFieldsInExpression(infix.left, fields);
                self.discoverFieldsInExpression(infix.right, fields);
            },
            .If => |if_expr| {
                self.discoverFieldsInExpression(if_expr.condition, fields);
                self.discoverFieldsInBlock(if_expr.consequence, fields);
                if (if_expr.alternative) |alt| {
                    self.discoverFieldsInBlock(alt, fields);
                }
            },
            .Call => |call_expr| {
                for (call_expr.arguments.items) |arg| {
                    self.discoverFieldsInExpression(arg, fields);
                }
            },
            .Dot => |dot_expr| {
                self.discoverFieldsInExpression(dot_expr.left, fields);
            },
            .Prefix => |prefix_expr| {
                self.discoverFieldsInExpression(prefix_expr.right, fields);
            },
            else => {},
        }
    }

    fn compileMethod(self: *Compiler, class_name: []const u8, method: *ast.FunctionLiteral) CompileError!void {
        self.is_inside_method = true;
        defer self.is_inside_method = false;
        
        try self.render("    pub fn {s}(self: *{s}", .{method.name, class_name});
        for (method.parameters.items) |param| {
            try self.render(", {s}: anytype", .{param.value});
        }
        try self.render(") !void {{\n", .{});
        
        for (method.body.statements.items) |stmt| {
             try self.compileStatementInsideMethod(stmt);
        }
        
        try self.render("    }}\n", .{});
    }

    fn compileStatementInExpression(self: *Compiler, stmt: *ast.Statement) CompileError!void {
        switch (stmt.*) {
            .Expression => |expr_stmt| {
                if (expr_stmt.expression.* == .If) {
                    try self.render("_ = ", .{});
                }
                try self.compileExpression(expr_stmt.expression);
                try self.render(";\n", .{});
            },
            else => {}, 
        }
    }

    fn compileStatementInsideMethod(self: *Compiler, stmt: *ast.Statement) CompileError!void {
        try self.render("        ", .{});
        switch (stmt.*) {
            .Let => |let_stmt| {
                try self.render("var {s}: Value = Value.from(", .{let_stmt.name.value});
                try self.compileExpression(let_stmt.value);
                try self.render(");\n", .{});
                try self.render("_ = &{s};\n", .{let_stmt.name.value});
            },
            .Expression => |expr_stmt| {
                // If the expression is an 'if', its result (Value) must be discarded
                if (expr_stmt.expression.* == .If) {
                    try self.render("_ = ", .{});
                }
                try self.compileExpression(expr_stmt.expression);
                try self.render(";\n", .{});
            },
            .While => |while_stmt| {
                try self.render("while ( (", .{});
                try self.compileExpression(while_stmt.condition);
                try self.render(").isTrue()) {{\n", .{});
                for (while_stmt.body.statements.items) |s| {
                    try self.compileStatementInsideMethod(s);
                }
                try self.render("        }}\n", .{});
            },
            .For => |for_stmt| {
                // Inside method, same logic but call compileStatementInsideMethod
                try self.render("        {{\n", .{}); 
                if (for_stmt.init) |init_stmt| try self.compileStatementInsideMethod(init_stmt);
                try self.render("        while (", .{});
                if (for_stmt.condition) |cond| {
                     try self.render(" (", .{});
                     try self.compileExpression(cond);
                     try self.render(").isTrue()", .{});
                } else {
                     try self.render("true", .{});
                }
                try self.render(") : ({{\n", .{});
                if (for_stmt.post) |post| try self.compileStatementInsideMethod(post);
                try self.render("        }}) {{\n", .{});
                for (for_stmt.body.statements.items) |s| {
                    try self.compileStatementInsideMethod(s);
                }
                try self.render("        }}\n", .{});
                try self.render("        }}\n", .{});
            },
            else => {},
        }
    }

    fn compileStatement(self: *Compiler, stmt: *ast.Statement) CompileError!void {
        switch (stmt.*) {
            .Let => |let_stmt| {
                // Use type inference for local variables to allow easier OOP method calls
                // The Zig compiler will infer the type (i64, *Struct, etc.)
                try self.render("    var {s} = ", .{let_stmt.name.value});
                try self.compileExpression(let_stmt.value);
                try self.render(";\n", .{});
                try self.render("    _ = &{s};\n", .{let_stmt.name.value});
            },
            .Expression => |expr_stmt| {
                try self.render("    ", .{});
                if (expr_stmt.expression.* == .If) {
                    try self.render("_ = ", .{});
                }
                try self.compileExpression(expr_stmt.expression);
                try self.render(";\n", .{});
            },
            .While => |while_stmt| {
                try self.render("    while ( (", .{});
                try self.compileExpression(while_stmt.condition);
                try self.render(").isTrue()) {{\n", .{});
                for (while_stmt.body.statements.items) |s| {
                    try self.compileStatement(s);
                }
                try self.render("    }}\n", .{});
            },
            .For => |for_stmt| try self.compileForStatement(for_stmt),
            else => {},
        }
    }

    fn compileForStatement(self: *Compiler, stmt: *ast.ForStatement) CompileError!void {
        try self.render("    {{\n", .{}); // New block scope
        
        if (stmt.init) |init_stmt| {
             try self.compileStatement(init_stmt);
        }

        try self.render("    while (", .{});
        if (stmt.condition) |cond| {
             try self.render(" (", .{});
             try self.compileExpression(cond);
             try self.render(").isTrue()", .{});
        } else {
             try self.render("true", .{});
        }
        
        try self.render(") : (", .{});
        if (stmt.post) |post| {
             // Post is a statement (ExpressionStatement wrapper usually)
             // compileStatement adds indentation and newline.
             // We need inline compilation or just handle expression statement logic here.
             // But compileStatement adds semicolons. 
             // Zig while loop continue expression: `while(...) : (expr)`.
             // It expects an expression, or a block.
             try self.render("{{\n", .{});
             try self.compileStatement(post);
             try self.render("    }}", .{});
        } else {
             try self.render("{{}}", .{});
        }
        try self.render(") {{\n", .{});
        
        for (stmt.body.statements.items) |s| {
            try self.compileStatement(s);
        }
        try self.render("    }}\n", .{});
        try self.render("    }}\n", .{}); // Close block scope
    }

    fn compileExpression(self: *Compiler, expr: *ast.Expression) CompileError!void {
        switch (expr.*) {
            .Identifier => |ident| {
                try self.render("{s}", .{ident.value});
            },
            .IntegerLiteral => |lit| {
                try self.render("@as(i64, {d})", .{lit.value});
            },
            .StringLiteral => |lit| {
                try self.render("\"{s}\"", .{lit.value});
            },
            .Call => |call| {
                // Handle 'echo' specifically
                if (call.function.* == .Identifier and call.function.Identifier.token.type == .Echo) {
                    try self.render("_print(", .{}); 
                    if (call.arguments.items.len > 0) {
                        try self.compileExpression(call.arguments.items[0]);
                    }
                    try self.render(")", .{});
                } else {
                    // Generic function call
                    try self.render("try ", .{}); // HIAX methods/calls are !void
                    try self.compileExpression(call.function);
                    try self.render("(", .{});
                    for (call.arguments.items, 0..) |arg, i| {
                        try self.compileExpression(arg);
                        if (i < call.arguments.items.len - 1) {
                            try self.render(", ", .{});
                        }
                    }
                    try self.render(")", .{});
                }
            },
            .New => |new_expr| {
                try self.render("(try {s}.init(allocator))", .{new_expr.class_name.value});
            },
            .Dot => |dot_expr| {
                const left = dot_expr.left;
                if (left.* == .This) {
                    try self.render("self.{s}", .{dot_expr.member.value});
                } else {
                    // This is the tricky part: dynamic dot access on a Value
                    // For now, let's assume it's just a regular Zig struct access.
                    // This will fail if the receiver is 'Value'.
                    // To fix this properly, we'd need to cast.
                    // For now, we'll assume the left side is a pointer to a struct.
                    try self.compileExpression(dot_expr.left);
                    try self.render(".{s}", .{dot_expr.member.value});
                }
            },
            .This => |_| {
                try self.render("self", .{});
            },
            .Infix => |infix| {
                if (std.mem.eql(u8, infix.operator, "=")) {
                    // Assignment: a = b
                    try self.compileExpression(infix.left);
                    try self.render(" = Value.from(", .{});
                    try self.compileExpression(infix.right);
                    try self.render(")", .{});
                } else if (std.mem.eql(u8, infix.operator, "+")) {
                    const alloc_name = if (self.is_inside_method) "self.allocator" else "allocator";
                    try self.render("_add({s}, Value.from(", .{alloc_name});
                    try self.compileExpression(infix.left);
                    try self.render("), Value.from(", .{});
                    try self.compileExpression(infix.right);
                    try self.render("))", .{});
                } else {
                    // Comparison/Math: result = Value.from( Value.from(a).int OP Value.from(b).int )
                    try self.render("Value.from(", .{});
                    try self.render("Value.from(", .{});
                    try self.compileExpression(infix.left);
                    try self.render(").int {s} Value.from(", .{infix.operator});
                    try self.compileExpression(infix.right);
                    try self.render(").int)", .{});
                }
            },
            .Boolean => |lit| {
                try self.render("Value.from({})", .{lit.value});
            },
            .If => |if_expr| {
                // Transpile to Zig 'if' block yielding a Value
                try self.render("(if ( (", .{});
                try self.compileExpression(if_expr.condition);
                try self.render(").isTrue()) blk: {{\n", .{});
                
                // If the consequence is a single expression statement, we can yield its value?
                // For HIAX MVP, let's keep it simple.
                for (if_expr.consequence.statements.items) |s| {
                    try self.compileStatementInExpression(s);
                }
                try self.render("    break :blk Value.from(0);\n", .{});
                try self.render("}} else blk: {{\n", .{});
                if (if_expr.alternative) |alt| {
                    for (alt.statements.items) |s| {
                        try self.compileStatementInExpression(s);
                    }
                }
                try self.render("    break :blk Value.from(0);\n", .{});
                try self.render("}})", .{});
            },
            else => {},
        }
    }
};
