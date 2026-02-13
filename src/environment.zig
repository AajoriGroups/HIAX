const std = @import("std");
const Object = @import("object.zig").Object;

pub const Environment = struct {
    store: std.StringHashMap(Object),
    outer: ?*Environment,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, outer: ?*Environment) *Environment {
        const env = allocator.create(Environment) catch unreachable;
        env.store = std.StringHashMap(Object).init(allocator);
        env.outer = outer;
        env.allocator = allocator;
        return env;
    }

    pub fn get(self: *Environment, name: []const u8) ?Object {
        if (self.store.get(name)) |val| {
            return val;
        }
        if (self.outer) |outer| {
            return outer.get(name);
        }
        return null;
    }

    pub fn set(self: *Environment, name: []const u8, val: Object) !void {
        try self.store.put(name, val);
    }
};
