//!  `(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`

// Author: `(concat user-full-name)` <`(concat user-mail-address)`>
// Created: `(format-time-string "%Y-%m-%d")`

const std = @import("std");

pub fn main() void {
    std.debug.print("Hello, {s}!\n", .{"World"});
}
