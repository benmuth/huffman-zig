const std = @import("std");
const print = std.debug.print;

const CharTableError = error{OutOfSpace};

const sample_string =
    \\ We'll assume that each character has an associated weight equal to the number of times the character occurs in a file, for example. In the "go go gophers" example, the characters 'g' and 'o' have weight 3, the space has weight 2, and the other characters have weight 1. When compressing a file we'll need to calculate these weights, we'll ignore this step for now and assume that all character weights have been calculated. Huffman's algorithm assumes that we're building a single tree from a group (or forest) of trees. Initially, all the trees have a single node with a character and the character's weight. Trees are combined by picking two trees, and making a new tree from the two trees. This decreases the number of trees by one at each step since two trees are combined into one tree. The algorithm is as follows:
    \\ 
    \\ Begin with a forest of trees. All trees are one node, with the weight of the tree equal to the weight of the character in the node. Characters that occur most frequently have the highest weights. Characters that occur least frequently have the smallest weights.
    \\ Repeat this step until there is only one tree:
    \\ 
    \\ Choose two trees with the smallest weights, call these trees T1 and T2. Create a new tree whose root has a weight equal to the sum of the weights T1 + T2 and whose left subtree is T1 and whose right subtree is T2.
    \\ The single tree left after the previous step is an optimal encoding tree.
    \\ We'll use the string "go go gophers" as an example. Initially we have the forest shown below. The nodes are shown with a weight/count that represents the number of times the node's character occurs.
;

pub fn main() !void {
    const arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var utf8_iterator = (try std.unicode.Utf8View.init(sample_string)).iterator();
    while (utf8_iterator.nextCodepoint()) |code_point| {
        _ = code_point;
        // print("type: {}\n", .{@TypeOf(code_point)});
        // print("0x{x} is {u} \n", .{ code_point, code_point });
        // TODO: add to hash table here
    }

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    // const stdout_file = std.io.getStdOut().writer();
    // var bw = std.io.bufferedWriter(stdout_file);
    // const stdout = bw.writer();

    // try bw.flush(); // don't forget to flush!
}

const node = struct {
    left: *node,
    right: *node,
    char: u21,
    count: u32,
};

fn createTree(allocator: std.mem.Allocator, ct: char_table) ?*node {
    if (ct.table.len == 0) {
        return null;
    }
    if (ct.table.len == 1) {
        return allocator.create(node); // TODO: make this return a useful pointer to a node
    }

    std.assert(std.sort.isSorted(char_table_entry, ct.table, {}, std.sort.asc(char_table_entry)));

    // TODO: allocate all nodes at once and create tree in one pass
    var tree_buf = allocator.alloc(*node, (2 * ct.distinct_char_count - 1)); // can be usize instead of *node?
    for (ct.table, 0..) |e, i| {
        if (e.isInitialized()) {
            const new_node = allocator.create(node);
            new_node.* = node{ .left = null, .right = null, .char = e.char, .count = e.count };
            tree_buf[i] = new_node;
        }
    }

    std.assert(@TypeOf(tree_buf[ct.distinct_char_count + 1]) == undefined); //
    std.assert(@TypeOf(tree_buf[ct.distinct_char_count]) == *node); //

    var i = ct.distinct_char_count;
    _ = i;
    var j = 0;
    while (j < ct.distinct_char_count) {}
}

// ----------------------------------------
// Counting chars
// ----------------------------------------

/// a char_table is used to count the number of UTF-8 characters in a string
pub const char_table = struct {
    len: u32,
    distinct_char_count: u32,
    table: []char_table_entry,
    hash_fn: *const fn (char: u21) u32,

    /// hash_fn can be any function that hashes Unicode code points to u32. The char_table
    /// capacity is determined by initial_cap and currently doesn't grow. TODO: make it grow
    pub fn init(allocator: std.mem.Allocator, hash_fn: *const fn (char: u21) u32, initial_cap: u32) !char_table {
        var table = try allocator.alloc(char_table_entry, initial_cap);
        for (0..table.len) |i| {
            // these values for char and count represent an uninitialized entry
            table[i] = char_table_entry{ .char = 0x00, .count = std.math.maxInt(u32) };
        }

        return char_table{
            .len = initial_cap,
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
        var candidate_idx = @rem(hash, Self.len);
        var candidate = Self.table[candidate_idx];
        while (candidate.count != std.math.maxInt(u32) and candidate.char != char) {
            candidate_idx += 1;
            // TODO: use a better heuristic here, like if array is 80% full or something
            if (candidate_idx >= Self.len) {
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
    var ct = try char_table.init(std.testing.allocator, prospectorHash, 4096);
    defer ct.deinit(std.testing.allocator);
    var utf8_iterator = (try std.unicode.Utf8View.init(sample_string)).iterator();
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
    const expected_count = try std.unicode.utf8CountCodepoints(sample_string);
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

// ----------------------------------------
// Sorting
// ----------------------------------------

fn quickSort(char_counts: []char_table_entry) void {
    if (char_counts.len < 2) {
        return; // already sorted
    }

    partition(char_counts);

    const pivot_idx = char_counts[char_counts.len - 1];
    quickSort(char_counts[0..pivot_idx]);
    quickSort(char_counts[pivot_idx + 1 .. char_counts.len]);
}

fn partition(a: []char_table_entry) void {
    var j: u32 = 0;
    var q: u32 = 0;
    var r = a.len - 1;
    while (j < r) {
        if (a[j].count > a[r].count) {
            j += 1;
            continue;
        }
        const tmp = a[j];
        a[j] = a[q];
        a[q] = tmp;
        q += 1;
    }
    const tmp = a[r];
    a[r] = a[q];
    a[q] = tmp;
}

test "quicksort" {
    // var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    // defer arena.deinit();
    // var allocator = arena.allocator();
    var allocator = std.testing.allocator;
    var entries = try allocator.alloc(char_table_entry, 3);
    defer allocator.free(entries);

    entries[0] = char_table_entry{ .char = 'n', .count = 3 };
    entries[1] = char_table_entry{ .char = 'e', .count = 2 };
    entries[2] = char_table_entry{ .char = 'b', .count = 1 };

    partition(entries);

    for (entries) |e| {
        var s: [4]u8 = undefined;
        _ = try std.unicode.utf8Encode(e.char, &s);
        // print("char: {s} \t count: {d}\n", .{ s, e.count });
        print("char: {s} \t ", .{s});
    }
    print("\n", .{});
}

// ----------------------------------------
// Priority Queue
// ----------------------------------------

// TODO: implement binary heap
