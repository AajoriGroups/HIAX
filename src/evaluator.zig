const std = @import("std");
const ast = @import("ast.zig");
const object = @import("object.zig");
const Object = object.Object;
const Environment = @import("environment.zig").Environment;

pub const EvalError = error{
    IdentifierNotFound,
    UnsupportedStatementType,
    UnsupportedExpressionType,
    UserDefinedFunctionsNotImplemented,
    OutOfMemory,
};

pub const Evaluator = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Evaluator {
        return Evaluator{
            .allocator = allocator,
        };
    }

    pub fn eval(self: *Evaluator, node: ast.Node, env: *Environment) EvalError!Object {
        switch (node) {
            .Program => |p| return self.evalProgram(p, env),
            .Statement => |s| return self.evalStatement(s, env),
            .Expression => |e| return self.evalExpression(e, env),
        }
    }

    fn evalProgram(self: *Evaluator, program: *ast.Program, env: *Environment) EvalError!Object {
        var result = Object{ .Null = {} };
        for (program.statements.items) |stmt| {
            const val = try self.evalStatement(stmt, env);
            if (val == .ReturnValue) return val.ReturnValue.*;
            if (val == .Error) return val;
            result = val;
        }
        return result;
    }

    fn evalStatement(self: *Evaluator, stmt: *ast.Statement, env: *Environment) EvalError!Object {
        switch (stmt.*) {
            .Let => |let_stmt| {
                const val = try self.evalExpression(let_stmt.value, env);
                if (val == .Error) return val;
                try env.set(let_stmt.name.value, val);
                return Object{ .Null = {} };
            },
            .Expression => |expr_stmt| return self.evalExpression(expr_stmt.expression, env),
            .Block => |block_stmt| return self.evalBlockStatement(block_stmt, env),
            .Class => |class_stmt| return self.evalClassStatement(class_stmt, env),
            .While => |while_stmt| return self.evalWhileStatement(while_stmt, env),
            .For => |for_stmt| return self.evalForStatement(for_stmt, env),
            else => return Object{ .Error = "Unsupported statement type" },
        }
    }

    fn evalBlockStatement(self: *Evaluator, block: *ast.BlockStatement, env: *Environment) EvalError!Object {
        var result = Object{ .Null = {} };
        for (block.statements.items) |stmt| {
            const val = try self.evalStatement(stmt, env);
            if (val == .ReturnValue or val == .Error) return val;
            result = val;
        }
        return result;
    }

    fn evalExpression(self: *Evaluator, expr: *ast.Expression, env: *Environment) EvalError!Object {
        switch (expr.*) {
            .IntegerLiteral => |lit| return Object{ .Integer = lit.value },
            .StringLiteral => |lit| return Object{ .String = lit.value },
            .Identifier => |ident| {
                if (env.get(ident.value)) |val| return val;
                return Object{ .Error = try std.fmt.allocPrint(self.allocator, "Identifier not found: {s}", .{ident.value}) };
            },
            .Call => |call| return self.evalCallExpression(call, env),
            .New => |new_expr| return self.evalNewExpression(new_expr, env),
            .Dot => |dot_expr| return self.evalDotExpression(dot_expr, env),
            .This => |_| return self.evalThisExpression(env),
            .Infix => |infix| return self.evalInfixExpression(infix, env),
            .If => |if_expr| return self.evalIfExpression(if_expr, env),
            .Function => |func| {
                const obj = Object{ .Function = func };
                // If named function, bind it to current env
                if (func.name.len > 0) {
                     try env.set(func.name, obj);
                }
                return obj;
            },
            else => return Object{ .Error = "Unsupported expression type" },
        }
    }

    fn evalCallExpression(self: *Evaluator, call: *ast.CallExpression, env: *Environment) EvalError!Object {
        // Special case for bilingual 'echo/bol' — check BEFORE evaluating the function
        if (call.function.* == .Identifier) {
            const ident = call.function.Identifier;
            if (ident.token.type == .Echo) {
                if (call.arguments.items.len > 0) {
                    const arg = try self.evalExpression(call.arguments.items[0], env);
                    if (arg == .Error) return arg;
                    const inspected = try arg.inspect(self.allocator);
                    writeStdout(inspected);
                    writeStdout("\n");
                } else {
                    writeStdout("\n");
                }
                return Object{ .Null = {} };
            }
        }

        // Evaluate the function (which could be an identifier or a dot expression)
        const func_obj = try self.evalExpression(call.function, env);
        if (func_obj == .Error) return func_obj;

        // Generic function call (FunctionLiteral or Method)
        switch (func_obj) {
            .Class => return Object{ .Error = "Cannot call a class directly, use 'noya'" },
            .BoundMethod => |bound| return self.applyMethod(bound, call.arguments, env),
            .Function => |func| return self.applyFunction(func, call.arguments, env),
            else => return Object{ .Error = "Not a callable function" },
        }
    }

    fn applyMethod(self: *Evaluator, bound: *object.BoundMethod, args: std.ArrayList(*ast.Expression), env: *Environment) EvalError!Object {
        const method_env = Environment.init(self.allocator, env); // Use current env as outer? Or class env?
        // Bound methods should focus on 'iswa'.
        try method_env.set("__this__", Object{ .Instance = bound.instance });

        // Bind arguments to parameters
        for (bound.method.parameters.items, 0..) |param, i| {
            if (i < args.items.len) {
                const val = try self.evalExpression(args.items[i], env);
                try method_env.set(param.value, val);
            }
        }

        return self.evalBlockStatement(bound.method.body, method_env);
    }

    fn applyFunction(self: *Evaluator, func: *ast.FunctionLiteral, args: std.ArrayList(*ast.Expression), env: *Environment) EvalError!Object {
        const func_env = Environment.init(self.allocator, env);
        
        for (func.parameters.items, 0..) |param, i| {
            if (i < args.items.len) {
                const val = try self.evalExpression(args.items[i], env);
                try func_env.set(param.value, val);
            }
        }

        return self.evalBlockStatement(func.body, func_env);
    }

    fn evalClassStatement(self: *Evaluator, stmt: *ast.ClassStatement, env: *Environment) EvalError!Object {
        const class = self.allocator.create(object.Class) catch return EvalError.OutOfMemory;
        class.name = stmt.name.value;
        class.methods = std.StringHashMap(*ast.FunctionLiteral).init(self.allocator);
        
        for (stmt.methods.items) |method| {
            try class.methods.put(method.name, method);
        }

        const obj = Object{ .Class = class };
        try env.set(stmt.name.value, obj);
        return Object{ .Null = {} };
    }

    fn evalNewExpression(self: *Evaluator, expr: *ast.NewExpression, env: *Environment) EvalError!Object {
        const class_obj = env.get(expr.class_name.value) orelse return Object{ .Error = "Class not found" };
        if (class_obj != .Class) return Object{ .Error = "Not a class" };
        const class = class_obj.Class;

        const instance = self.allocator.create(object.Instance) catch return EvalError.OutOfMemory;
        instance.class = class;
        instance.fields = Environment.init(self.allocator, null);

        const instance_obj = Object{ .Instance = instance };

        // Call 'init' method if exists
        if (class.methods.get("init")) |init_method| {
             const bound = self.allocator.create(object.BoundMethod) catch return EvalError.OutOfMemory;
             bound.instance = instance;
             bound.method = init_method;
             _ = try self.applyMethod(bound, expr.arguments, env);
        }
        
        return instance_obj;
    }

    fn evalDotExpression(self: *Evaluator, expr: *ast.DotExpression, env: *Environment) EvalError!Object {
        const left = try self.evalExpression(expr.left, env);
        if (left == .Error) return left;

        if (left == .Instance) {
            const instance = left.Instance;
            // Check fields first
            if (instance.fields.get(expr.member.value)) |val| return val;
            
            // Check methods
            if (instance.class.methods.get(expr.member.value)) |method| {
                 const bound = self.allocator.create(object.BoundMethod) catch return EvalError.OutOfMemory;
                 bound.instance = instance;
                 bound.method = method;
                 return Object{ .BoundMethod = bound };
            }
        }

        return Object{ .Error = "Member not found" };
    }

    fn evalInfixExpression(self: *Evaluator, expr: *ast.InfixExpression, env: *Environment) EvalError!Object {
        if (std.mem.eql(u8, expr.operator, "=")) {
            // Assignment logic
            const right = try self.evalExpression(expr.right, env);
            if (right == .Error) return right;

            switch (expr.left.*) {
                .Identifier => |ident| {
                    try env.set(ident.value, right);
                    return right;
                },
                .Dot => |dot| {
                    const left_obj = try self.evalExpression(dot.left, env);
                    if (left_obj == .Instance) {
                        try left_obj.Instance.fields.set(dot.member.value, right);
                        return right;
                    }
                    return Object{ .Error = "Cannot assign to non-instance member" };
                },
                else => return Object{ .Error = "Invalid assignment target" },
            }
        }

        const left = try self.evalExpression(expr.left, env);
        if (left == .Error) return left;
        const right = try self.evalExpression(expr.right, env);
        if (right == .Error) return right;

        if (std.mem.eql(u8, expr.operator, "+")) {
             if (left == .String and right == .String) {
                  return Object{ .String = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{left.String, right.String}) };
             }
             if (left == .Integer and right == .Integer) {
                  return Object{ .Integer = left.Integer + right.Integer };
             }
             if (left == .String and right == .Integer) {
                  return Object{ .String = try std.fmt.allocPrint(self.allocator, "{s}{d}", .{left.String, right.Integer}) };
             }
             if (left == .Integer and right == .String) {
                  return Object{ .String = try std.fmt.allocPrint(self.allocator, "{d}{s}", .{left.Integer, right.String}) };
             }
        }

        if (std.mem.eql(u8, expr.operator, "<")) {
             if (left == .Integer and right == .Integer) {
                  return Object{ .Boolean = left.Integer < right.Integer };
             }
        }

        if (std.mem.eql(u8, expr.operator, ">")) {
             if (left == .Integer and right == .Integer) {
                  return Object{ .Boolean = left.Integer > right.Integer };
             }
        }
        
        if (std.mem.eql(u8, expr.operator, "==")) {
             if (left == .Integer and right == .Integer) return Object{ .Boolean = left.Integer == right.Integer };
             if (left == .Boolean and right == .Boolean) return Object{ .Boolean = left.Boolean == right.Boolean };
             return Object{ .Boolean = false }; // Different types or values
        }

        if (std.mem.eql(u8, expr.operator, "!=")) {
             if (left == .Integer and right == .Integer) return Object{ .Boolean = left.Integer != right.Integer };
             if (left == .Boolean and right == .Boolean) return Object{ .Boolean = left.Boolean != right.Boolean };
             return Object{ .Boolean = true }; // Different types are not equal
        }
        
        return Object{ .Error = "Unsupported operator or operand types" };
    }

    fn evalThisExpression(_: *Evaluator, env: *Environment) EvalError!Object {
        // Look for special '__this__' in environment
        if (env.get("__this__")) |val| return val;
        return Object{ .Error = "iswa used outside of class context" };
    }

    fn evalWhileStatement(self: *Evaluator, stmt: *ast.WhileStatement, env: *Environment) EvalError!Object {
        var result = Object{ .Null = {} };
        while (true) {
            const condition = try self.evalExpression(stmt.condition, env);
            if (condition == .Error) return condition;
            
            if (!isTruthy(condition)) break;
            
            result = try self.evalBlockStatement(stmt.body, env);
            if (result == .ReturnValue or result == .Error) return result;
        }
        return result;
    }

    fn evalForStatement(self: *Evaluator, stmt: *ast.ForStatement, env: *Environment) EvalError!Object {
        // Create new scope for loop variables
        // Typically for-loops have their own scope for init variables
        // But HIAX (like JS var or older langs) might keep them in current scope or new scope?
        // Let's use a new scope to be safe and clean.
        const loop_env = Environment.init(self.allocator, env);
        
        // Init
        if (stmt.init) |init_stmt| {
            const val = try self.evalStatement(init_stmt, loop_env);
            if (val == .Error) return val;
        }

        var result = Object{ .Null = {} };

        while (true) {
            // Condition
            if (stmt.condition) |cond| {
                const val = try self.evalExpression(cond, loop_env);
                if (val == .Error) return val;
                if (!isTruthy(val)) break;
            }

            // Body
            result = try self.evalBlockStatement(stmt.body, loop_env);
            if (result == .ReturnValue or result == .Error) return result;

            // Post
            if (stmt.post) |post| {
                 const val = try self.evalStatement(post, loop_env);
                 if (val == .Error) return val;
            }
        }
        return result;
    }

    fn evalIfExpression(self: *Evaluator, expr: *ast.IfExpression, env: *Environment) EvalError!Object {
        const condition = try self.evalExpression(expr.condition, env);
        if (condition == .Error) return condition;

        if (isTruthy(condition)) {
            return self.evalBlockStatement(expr.consequence, env);
        } else if (expr.alternative) |alt| {
            return self.evalBlockStatement(alt, env);
        } else {
            return Object{ .Null = {} };
        }
    }

    fn isTruthy(obj: Object) bool {
        return switch (obj) {
            .Boolean => |val| val,
            .Null => false,
            .Integer => |val| val != 0,
            else => true,
        };
    }

    fn writeStdout(msg: []const u8) void {
        const handle = std.os.windows.kernel32.GetStdHandle(std.os.windows.STD_OUTPUT_HANDLE) orelse return;
        var written: u32 = 0;
        _ = std.os.windows.kernel32.WriteFile(handle, msg.ptr, @intCast(msg.len), &written, null);
    }
};
