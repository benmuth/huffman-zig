const std = @import("std");
const print = std.debug.print;

const heap = @import("heap.zig");
const MinHeap = heap.MinHeap;

const CharTableError = error{OutOfSpace};

const sample = @embedFile("sample.txt");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    // var utf8_iterator = (try std.unicode.Utf8View.init(sample_string)).iterator();
    // while (utf8_iterator.nextCodepoint()) |code_point| {
    //     _ = code_point;
    //     // print("type: {}\n", .{@TypeOf(code_point)});
    //     // print("0x{x} is {u} \n", .{ code_point, code_point });
    //     // TODO: add to hash table here
    // }
    var ct = try char_table.init(arena.allocator(), prospectorHash, 4096);
    defer ct.deinit(arena.allocator());
    try ct.add('a');
    try ct.add('b');
    try ct.add('c');

    const head = (try createTree(arena.allocator(), ct)).?;
    print("head count: {}\n", .{head.count});
}

const Node = struct {
    left: ?*Node = null,
    right: ?*Node = null,
    char: u21, // char represents a Unicode codepoint
    count: u32,

    fn traverse(head: *Node) void {
        if (head.left) |left| {
            traverse(left);
        }
        print("current char: {u}\tcurrent count: {d}\n", .{ head.char, head.count });
        if (head.right) |right| {
            traverse(right);
        }
    }
};

const TreeError = error{
    EmptyTable,
};

fn createTree(allocator: std.mem.Allocator, ct: char_table) !?*Node {
    if (ct.distinct_char_count == 0) { // no chars in char table
        return TreeError.EmptyTable;
    }
    if (ct.distinct_char_count == 1) {
        return allocator.create(Node); // TODO: make this return a useful pointer to a node
    }

    // TODO: allocate all nodes at once and create tree in one pass

    const CharHeap = MinHeap(Node);

    var target_cap = ct.distinct_char_count;
    var char_heap: CharHeap = try CharHeap.init(allocator, target_cap);
    // defer heap.deinit();
    // print("heap array len: {}\n", .{heap.array.items.len});
    // print("heap cap: {}\n", .{heap.array.capacity});

    var heap_ptr = &char_heap;
    for (ct.table) |e| {
        if (e.isInitialized()) {
            // print("element: {}:{d}\n\n", .{ e.char, e.count });
            var new_node = try allocator.create(Node);
            new_node.char = e.char;
            new_node.count = e.count;
            new_node.left = null;
            new_node.right = null;
            heap_ptr.insert(new_node);
            // print("heap array: {any}\n\n", .{heap_ptr.array.items});
        }
    }

    var head = try allocator.create(Node);
    // print("heap array: {any}\n", .{heap_ptr.array.items});
    while (char_heap.array.items.len > 1) {
        // print("heap before: {any}\n\n", .{heap_ptr.array.items});
        const left_child = heap_ptr.extract() catch unreachable;
        const right_child = heap_ptr.extract() catch unreachable;

        const sum_counts: u32 = left_child.count + right_child.count;

        const new_node = try allocator.create(Node);

        new_node.* = Node{ .left = left_child, .right = right_child, .char = 0x00, .count = sum_counts };
        head = new_node;
        // print("new node: {any}\n\n", .{new_node});
        heap_ptr.insert(new_node);
    }
    // const ret = Tree{ .head = head, .size = ct.cap };
    return head;
}

// fn freeTree(allocator: std.mem.Allocator, head: ?*Node) void {
//     var not_null_head = head orelse return;

//     print("head stuff: {}\n", .{not_null_head});
//     freeTree(allocator, not_null_head.left);
//     freeTree(allocator, not_null_head.right);
//     allocator.destroy(not_null_head);
// }

test "create tree" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var ct = try char_table.init(arena.allocator(), prospectorHash, 4096);
    // defer ct.deinit(std.testing.allocator);

    try ct.add('a');
    // try ct.add('a');
    try ct.add('b');
    try ct.add('c');

    // for (ct.table) |item| {
    //     if (item.isInitialized()) {
    //         print("ct item: {any}\n", .{item});
    //     }
    // }

    const head = try createTree(arena.allocator(), ct);
    head.?.traverse();
    // defer freeTree(std.testing.allocator, head);

    const node = head.?;
    print("head count: {}\n", .{node.count});
}

// ----------------------------------------
// Counting chars
// ----------------------------------------

/// a char_table is used to count the number of UTF-8 characters in a string
pub const char_table = struct {
    cap: u32,
    distinct_char_count: u32,
    table: []char_table_entry,
    hash_fn: *const fn (char: u21) u32,

    /// hash_fn can be any function that hashes Unicode code points to u32. The char_table
    /// capacity is determined by initial_cap and currently doesn't grow. TODO: make it grow
    pub fn init(allocator: std.mem.Allocator, hash_fn: *const fn (char: u21) u32, initial_cap: u32) !char_table {
        // NOTE: use a std.ArrayList here (like the min heap implementation)
        var table = try allocator.alloc(char_table_entry, initial_cap);
        for (0..table.len) |i| {
            // these values for char and count represent an uninitialized entry
            table[i] = char_table_entry{ .char = 0x00, .count = std.math.maxInt(u32) };
        }

        return char_table{
            .cap = initial_cap,
            .distinct_char_count = 0,
            .table = table,
            .hash_fn = hash_fn,
        };
    }

    pub fn deinit(Self: @This(), allocator: std.mem.Allocator) void {
        allocator.free(Self.table);
    }

    /// add one to the count of char in the table
    pub fn add(Self: *@This(), char: u21) !void {
        const idx = try Self.lookup(char);
        // initialize if needed
        if (Self.table[idx].char == 0x00 and Self.table[idx].count == std.math.maxInt(u32)) {
            Self.table[idx] = char_table_entry{ .char = char, .count = 0 };
            Self.distinct_char_count += 1;
        }
        Self.table[idx].count += 1;
    }

    /// returns the index where the char is located in the backing array. If the char hasn't
    /// been added yet, returns the index of an uninitialized entry.
    fn lookup(Self: @This(), char: u21) !usize {
        const hash = Self.hash_fn(char);
        var candidate_idx = @rem(hash, Self.cap);
        var candidate = Self.table[candidate_idx];
        while (candidate.count != std.math.maxInt(u32) and candidate.char != char) {
            candidate_idx += 1;
            // TODO: use a better heuristic here, like if array is 80% full or something
            if (candidate_idx >= Self.cap) {
                // TODO: grow the array instead of returning error
                return CharTableError.OutOfSpace;
            }
            candidate = Self.table[candidate_idx];
        }
        return candidate_idx;
    }
};

test "basic hash table ops" {
    // var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    // defer arena.deinit();
    // var allocator = arena.allocator();
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var ct = try char_table.init(arena.allocator(), prospectorHash, 4096);
    var utf8_iterator = (try std.unicode.Utf8View.init(sample)).iterator();
    while (utf8_iterator.nextCodepoint()) |code_point| {
        try ct.add(code_point);
    }
    var total_codepoint_count: u32 = 0;
    for (ct.table) |e| {
        if (e.count != std.math.maxInt(u32)) {
            total_codepoint_count += e.count;
            // var s: [4]u8 = undefined;
            // _ = try std.unicode.utf8Encode(e.char, &s);
            // print("char: {s} \t count: {d}\n", .{ s, e.count });
        }
    }
    const expected_count = try std.unicode.utf8CountCodepoints(sample);
    try std.testing.expectEqual(expected_count, total_codepoint_count);
    print("expected: {d}, actual: {d}\n", .{ expected_count, total_codepoint_count });
}

// can I embed this in the char table?
const char_table_entry = struct {
    char: u21,
    count: u32,

    fn isInitialized(Self: @This()) bool {
        return Self.char != 0x00 and Self.count != std.math.maxInt(u32);
    }
};

/// taken from https://nullprogram.com/blog/2018/07/31/
fn prospectorHash(c: u21) u32 {
    var x = @as(u32, c);
    x ^= x >> 16;
    x *%= @as(u32, 0x45d9f3b);
    x ^= x >> 16;
    x *%= @as(u32, 0x45d9f3b);
    x ^= x >> 16;
    return x;
}
