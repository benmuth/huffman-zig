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

    const head = try createTree(arena.allocator(), ct);
    print("head count: {}\n", .{head.count});

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    // const stdout_file = std.io.getStdOut().writer();
    // var bw = std.io.bufferedWriter(stdout_file);
    // const stdout = bw.writer();

    // try bw.flush(); // don't forget to flush!
}

const Node = struct {
    left: ?*Node,
    right: ?*Node,
    char: u21, // char represents a Unicode codepoint
    count: u32,
};

const TreeError = error{
    EmptyTable,
};

fn createTree(allocator: std.mem.Allocator, ct: char_table) !*Node {
    if (ct.distinct_char_count == 0) { // no chars in char table
        return TreeError.EmptyTable;
    }
    if (ct.distinct_char_count == 1) {
        return allocator.create(Node); // TODO: make this return a useful pointer to a node
    }

    // std.assert(std.sort.isSorted(char_table_entry, ct.table, {}, std.sort.asc(char_table_entry)));

    // TODO: allocate all nodes at once and create tree in one pass
    // var tree_buf = allocator.alloc(*node, (2 * ct.distinct_char_count - 1)); // can be usize instead of *node?

    var target_cap = ct.distinct_char_count;
    var heap: MinHeap = try MinHeap.init(allocator, target_cap);
    // defer heap.deinit();
    print("heap array len: {}\n", .{heap.array.items.len});
    print("heap cap: {}\n", .{heap.array.capacity});

    var heap_ptr = &heap;
    for (ct.table) |e| {
        if (e.isInitialized()) {
            var new_node = Node{
                .left = null,
                .right = null,
                .char = e.char,
                .count = e.count,
            };
            print("cap: {}\n", .{heap.array.capacity});
            print("heap: {}\n", .{heap});
            heap_ptr.insert(&new_node);
        }
    }

    var head = try allocator.create(Node);
    // print("heap array: {}\n", .{heap.array});
    while (heap.array.items.len > 1) {
        const left_child = heap_ptr.extract() catch unreachable;
        const right_child = heap_ptr.extract() catch unreachable;

        const sum_counts: u32 = left_child.count + right_child.count;

        const new_node = try allocator.create(Node);

        new_node.* = Node{ .left = left_child, .right = right_child, .char = 0x00, .count = sum_counts };
        head = new_node;
        heap_ptr.insert(new_node);
    }

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
    try ct.add('b');
    try ct.add('c');

    const head = try createTree(arena.allocator(), ct);
    // defer freeTree(std.testing.allocator, head);

    print("head count: {}\n", .{head.count});
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

    const pivot_idx = partition(char_counts);

    // const pivot_idx = char_counts[char_counts.len - 1];
    quickSort(char_counts[0..pivot_idx]);
    quickSort(char_counts[pivot_idx + 1 .. char_counts.len]);
}

fn partition(a: []char_table_entry) u32 {
    // invariants to maintain through iteration:
    // group 'P': just the pivot element, element at [r == a.len-1]
    // group 'L': elements known to be less than pivot, elements from [0,q)
    // group 'G': elements known to be greater than pivot, elements from [q, j)
    // group 'U': unseen elements, elements from [j, r)

    // as iteration progresses group 'G' and 'L' grow, group 'U' shrinks

    // j is our iteration variable
    var j: u32 = 0;
    // the pivot element will eventually be moved to index q
    // during iteration, q points to the leftmost element in group 'G'
    var q: u32 = 0;
    // r points to the pivot, it stays in place during iteration (group 'P')
    var r = a.len - 1;

    // print("pivot index at: {d}\n", .{r});
    // print("pivot element : {}\n", .{a[r]});
    while (j < r) {
        if (a[j].count > a[r].count) {
            // the element is in the right spot (to the right of the eventual
            // pivot, in group 'G'), so look at the next element
            j += 1;
            continue;
        }
        // a[j] <= pivot element, so it should be in group 'L', to the left of
        // q (q is where the pivot element will eventually be)
        // swap a[j] with q (the divider between 'L' and 'G' and
        // increment q so that once again all elements to the left of q
        // are less than the pivot
        const tmp = a[j];
        a[j] = a[q];
        a[q] = tmp;
        q += 1;
        j += 1;
    }
    // after iteration, everything is in place besides p and r, so swap them
    const tmp = a[r];
    a[r] = a[q];
    a[q] = tmp;
    return q;
}

test "quicksort" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var allocator = arena.allocator();

    const rand_seed: u64 = @as(u64, @intCast(std.time.timestamp()));
    var prng = std.rand.DefaultPrng.init(rand_seed);
    const random = prng.random();
    const test_table_size = random.uintLessThan(u8, 20);
    var entries = try allocator.alloc(char_table_entry, test_table_size);
    // defer allocator.free(entries);

    for (entries) |*e| {
        e.count = @as(u32, random.uintLessThan(u10, 1000));
        e.char = random.uintLessThan(u21, 4096);
    }

    // entries[0] = char_table_entry{ .char = 'n', .count = 3 };
    // entries[1] = char_table_entry{ .char = 'e', .count = 2 };
    // entries[2] = char_table_entry{ .char = 'b', .count = 1 };

    quickSort(entries);

    var prev_count: u32 = 0;
    for (entries) |e| {
        var s: [4]u8 = undefined;
        _ = try std.unicode.utf8Encode(e.char, &s);
        try std.testing.expect(e.count >= prev_count);
        prev_count = e.count;
        // print("char: {s} \t count: {d}\n", .{ s, e.count });
        // print("char: {s} \t count: {d}\t", .{ s, e.count });
    }
    print("\n", .{});
}

// ----------------------------------------
// Priority Queue
// ----------------------------------------

const HeapError = error{
    EmptyHeap,
};

/// MinHeap
const MinHeap = struct {
    // TODO: make this generic (for all types with a `count` field)
    // array: std.ArrayList(char_table_entry),
    array: std.ArrayList(*Node),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, initial_cap: u32) !MinHeap {
        var array = try std.ArrayList(*Node).initCapacity(allocator, initial_cap);
        print("init array len: {}\n", .{array.items.len});
        print("init array cap: {}\n", .{array.capacity});
        print("array items: {any}\n", .{array.items});
        var ret = MinHeap{ .array = array, .allocator = allocator };
        // print("heap array: {}\n", .{ret});
        return ret;
    }

    pub fn insert(self: *MinHeap, n: *Node) void {
        print("heap array len (insert before append): {}\n", .{self.array.items.len});
        print("heap array cap (insert before append): {}\n", .{self.array.capacity});
        // print("heap inside insert: {}\n", .{self});
        self.array.appendAssumeCapacity(n);

        // print("heap array len (insert after append): {}\n", .{self.array.items.len});
        self.minHeapifyUp(self.array.items.len - 1);
    }

    fn minHeapifyUp(self: *MinHeap, idx: u64) void {
        var curr_idx = idx;
        print("current idx: {} \t parent idx: {} \t array items length: {}\n", .{ curr_idx, parent(curr_idx), self.array.items.len });
        while (self.array.items[parent(curr_idx)].count > self.array.items[curr_idx].count) {
            self.swap(parent(curr_idx), curr_idx);
            curr_idx = parent(curr_idx);
        }
    }

    fn minHeapifyDown(self: *MinHeap, idx: u64) void {
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

    pub fn extract(self: *MinHeap) !*Node {
        const last = self.array.getLastOrNull();
        var res = self.array.items[0];
        self.array.items[0] = last orelse return HeapError.EmptyHeap;

        self.minHeapifyDown(0);

        self.array.shrinkRetainingCapacity(self.array.items.len - 1);
        return res;
    }

    fn deinit(self: MinHeap) void {
        self.array.deinit();
    }

    fn swap(self: *MinHeap, idx1: u64, idx2: u64) void {
        const tmp = self.array.items[idx1];
        self.array.items[idx1] = self.array.items[idx2];
        self.array.items[idx2] = tmp;
    }
};

test "min heap" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var allocator = arena.allocator();

    var heap: MinHeap = try MinHeap.init(allocator, 4);
    // defer heap.deinit();
    var test_node_1 = Node{ .char = 'a', .count = 10, .left = null, .right = null };
    var test_node_2 = Node{ .char = 'b', .count = 5, .left = null, .right = null };
    var test_node_3 = Node{ .char = 'c', .count = 7, .left = null, .right = null };
    var test_node_4 = Node{ .char = 'd', .count = 12, .left = null, .right = null };
    var test_node_1_ptr = &test_node_1;
    var test_node_2_ptr = &test_node_2;
    var test_node_3_ptr = &test_node_3;
    var test_node_4_ptr = &test_node_4;
    var heap_ptr: *MinHeap = &heap;
    heap_ptr.insert(test_node_1_ptr);
    heap_ptr.insert(test_node_2_ptr);
    heap_ptr.insert(test_node_3_ptr);
    heap_ptr.insert(test_node_4_ptr);
    try std.testing.expectEqual(@as(u32, 5), heap.array.items[0].count);
    // try std.testing.expectEqual(@as(u32, 10), heap.array.items[0].count);
    print("heap: {any}\n", .{heap.array.items});

    var min = try heap.extract();
    print("min: {any}\n", .{min});
    try std.testing.expectEqual(@as(u32, 5), min.count);
    try std.testing.expectEqual(@as(u32, 7), heap.array.items[0].count);
    print("heap: {any}\n", .{heap.array.items});
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
