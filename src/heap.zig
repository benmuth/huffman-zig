const std = @import("std");
const print = std.debug.print;

const HeapError = error{
    EmptyHeap,
};

/// MinHeap
pub fn MinHeap(comptime T: type) type {
    return struct {
        const Self = @This();

        array: std.ArrayList(*T),
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator, initial_cap: u32) !Self {
            var array = try std.ArrayList(*T).initCapacity(allocator, initial_cap);
            // print("init array len: {}\n", .{array.items.len});
            // print("init array cap: {}\n", .{array.capacity});
            // print("array items: {any}\n", .{array.items});
            var ret = Self{ .array = array, .allocator = allocator };
            // print("heap array: {}\n", .{ret});
            return ret;
        }

        pub fn insert(self: *Self, n: *T) void {
            // print("heap array len (insert before append): {}\n", .{self.array.items.len});
            // print("heap array cap (insert before append): {}\n", .{self.array.capacity});
            // print("heap inside insert: {}\n", .{self});
            self.array.appendAssumeCapacity(n);

            // print("heap array len (insert after append): {}\n", .{self.array.items.len});
            self.minHeapifyUp(self.array.items.len - 1);
        }

        fn minHeapifyUp(self: *Self, idx: u64) void {
            var curr_idx = idx;
            // print("current idx: {} \t parent idx: {} \t array items length: {}\n", .{ curr_idx, parent(curr_idx), self.array.items.len });
            while (self.array.items[parent(curr_idx)].count > self.array.items[curr_idx].count) {
                self.swap(parent(curr_idx), curr_idx);
                curr_idx = parent(curr_idx);
            }
        }

        fn minHeapifyDown(self: *Self, idx: u64) void {
            const last = self.array.items.len - 1;
            var l = left(idx);
            var r = right(idx);
            var childToCompare: u64 = 0;

            while (l < last) {
                if (l == last or self.array.items[l].count < self.array.items[r].count) {
                    childToCompare = l;
                } else {
                    childToCompare = r;
                }

                var i = idx;
                if (self.array.items[i].count > self.array.items[childToCompare].count) {
                    self.swap(i, childToCompare);
                    i = childToCompare;
                    l = left(i);
                    r = right(i);
                } else {
                    return;
                }
            }
        }

        pub fn extract(self: *Self) !*T {
            const last = self.array.getLastOrNull();
            var res = self.array.items[0];
            self.array.items[0] = last orelse return HeapError.EmptyHeap;

            self.minHeapifyDown(0);

            self.array.shrinkRetainingCapacity(self.array.items.len - 1);
            return res;
        }

        fn deinit(self: Self) void {
            self.array.deinit();
        }

        fn swap(self: *Self, idx1: u64, idx2: u64) void {
            const tmp = self.array.items[idx1];
            self.array.items[idx1] = self.array.items[idx2];
            self.array.items[idx2] = tmp;
        }
    };
}

const Node = struct {
    left: ?*Node,
    right: ?*Node,
    char: u21, // char represents a Unicode codepoint
    count: u32,
};

test "min heap" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var allocator = arena.allocator();

    const CharHeap = MinHeap(Node);

    var heap: CharHeap = try CharHeap.init(allocator, 4);
    // defer heap.deinit();
    var test_node_1 = Node{ .char = 'a', .count = 10, .left = null, .right = null };
    var test_node_2 = Node{ .char = 'b', .count = 5, .left = null, .right = null };
    var test_node_3 = Node{ .char = 'c', .count = 7, .left = null, .right = null };
    var test_node_4 = Node{ .char = 'd', .count = 12, .left = null, .right = null };
    var test_node_1_ptr = &test_node_1;
    var test_node_2_ptr = &test_node_2;
    var test_node_3_ptr = &test_node_3;
    var test_node_4_ptr = &test_node_4;
    var heap_ptr: *CharHeap = &heap;
    heap_ptr.insert(test_node_1_ptr);
    print("heap array: {any}\n\n", .{heap_ptr.array.items});
    heap_ptr.insert(test_node_2_ptr);
    print("heap array: {any}\n\n", .{heap_ptr.array.items});
    heap_ptr.insert(test_node_3_ptr);
    print("heap array: {any}\n\n", .{heap_ptr.array.items});
    heap_ptr.insert(test_node_4_ptr);
    print("heap array: {any}\n\n", .{heap_ptr.array.items});
    try std.testing.expectEqual(@as(u32, 5), heap.array.items[0].count);
    // try std.testing.expectEqual(@as(u32, 10), heap.array.items[0].count);
    // print("heap: {any}\n", .{heap.array.items});

    var min = try heap.extract();
    try std.testing.expectEqual(@as(u32, 5), min.count);
    try std.testing.expectEqual(@as(u21, 'b'), min.char);
    // try std.testing.expectEqual(@as(u32, 7), heap.array.items[0].count);

    var next = try heap.extract();
    try std.testing.expectEqual(@as(u32, 7), next.count);
    try std.testing.expectEqual(@as(u21, 'c'), next.char);

    var pen = try heap.extract();
    try std.testing.expectEqual(@as(u32, 10), pen.count);
    try std.testing.expectEqual(@as(u21, 'a'), pen.char);

    var last = try heap.extract();
    try std.testing.expectEqual(@as(u32, 12), last.count);
    try std.testing.expectEqual(@as(u21, 'd'), last.char);
    // var next = try heap.extract();
    // try std.testing.expectEqual(@as(u32, 7), next.count);

    // print("heap: {any}\n", .{heap.array.items});
}

// helper functions for heap
fn parent(idx: u64) u64 {
    if (idx == 0) return idx;
    return (idx - 1) / 2;
}

fn left(idx: u64) u64 {
    return 2 * idx + 1;
}

fn right(idx: u64) u64 {
    return 2 * idx + 2;
}
