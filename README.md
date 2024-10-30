# One-file filesystems

This repository contains a collection of two "filesystems" with different properties for storing one blob of data on a block device.

## OneFFS - One File Filesystem
One-file filesystem is a filesystem for storing a single unnamed file.
It is suitable for storing data that may be lost on updates.
For example a cache of pre-computed cryptographic checksums that are slow to compute, but that we would rather recompute than have stale checksums.

CRC32 checksums are used to ensure integrity of both the header and the data.

## OneFFS-Fail-safe - Fail-safe One File Filesystem

Another one-file filesystem where instead recovery and integrity is prioritized.
This is useful for storing application state and configuration.
If a write fails or is interrupted the old data is recoverable.
The superblock (filesystem header) is duplicated at the beginning and end of the block device and is checked with the cryptographic hash SHA-256.
The data is as well checked with the cryptographic hash SHA-256.

With [mirage-block-partition](https://github.com/reynir/mirage-block-partition/) you can store multiple files with static allocation.
