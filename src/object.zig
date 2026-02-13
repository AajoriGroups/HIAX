const std = @import("std");

pub const ObjectType = enum {
    Integer,
    String,
    Boolean,
    Null,
    ReturnValue,
    Error,
    Class,
    Instance,
    BoundMethod,
    Function,
};

pub const Object = union(ObjectType) {
    Integer: i64,
    String: []const u8,
    Boolean: bool,
    Null: void,
    ReturnValue: *Object,
    Error: []const u8,
    Class: *Class,
    Instance: *Instance,
    BoundMethod: *BoundMethod,
    Function: *@import("ast.zig").FunctionLiteral,

    pub fn typeName(self: Object) []const u8 {
        return switch (self) {
            .Integer => "INTEGER",
            .String => "STRING",
            .Boolean => "BOOLEAN",
            .Null => "NULL",
            .ReturnValue => "RETURN_VALUE",
            .Error => "ERROR",
            .Class => "CLASS",
            .Instance => "INSTANCE",
            .BoundMethod => "BOUND_METHOD",
            .Function => "FUNCTION",
        };
    }

    pub fn inspect(self: Object, allocator: std.mem.Allocator) ![]const u8 {
        switch (self) {
            .Integer => |val| return try std.fmt.allocPrint(allocator, "{d}", .{val}),
            .String => |val| return try std.fmt.allocPrint(allocator, "{s}", .{val}),
            .Boolean => |val| return if (val) "true" else "false",
            .Null => return "null",
            .ReturnValue => |val| return try val.inspect(allocator),
            .Error => |val| return try std.fmt.allocPrint(allocator, "ERROR: {s}", .{val}),
            .Class => |val| return try std.fmt.allocPrint(allocator, "class {s}", .{val.name}),
            .Instance => |val| return try std.fmt.allocPrint(allocator, "instance of {s}", .{val.class.name}),
            .BoundMethod => |val| return try std.fmt.allocPrint(allocator, "method {s} of {s}", .{val.method.name, val.instance.class.name}),
            .Function => |val| return try std.fmt.allocPrint(allocator, "function {s}", .{val.name}),
        }
    }
};

pub const Class = struct {
    name: []const u8,
    methods: std.StringHashMap(*@import("ast.zig").FunctionLiteral),
    // For simplicity, methods are stored by name
};

pub const Instance = struct {
    class: *Class,
    fields: *(@import("environment.zig").Environment),
};

pub const BoundMethod = struct {
    instance: *Instance,
    method: *@import("ast.zig").FunctionLiteral,
};
