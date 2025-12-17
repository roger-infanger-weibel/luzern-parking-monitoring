import time
import re
import os

# Mock data generation
def generate_large_file(filename, lines=100000):
    with open(filename, 'w') as f:
        for i in range(lines):
            if i % 100 == 0:
                f.write(f" CALL 'SUB{i}' \n")
            elif i % 150 == 0:
                f.write(f" EXEC PGM=PROG{i} \n")
            else:
                f.write(f" MOVE A TO B \n")

# Method 1: Whole content
def scan_whole_content(filepath, patterns):
    with open(filepath, 'r') as f:
        content = f.read()
        count = 0
        for pat in patterns:
            matches = pat.findall(content)
            count += len(matches)
    return count

# Method 2: Line by line
def scan_line_by_line(filepath, patterns):
    count = 0
    with open(filepath, 'r') as f:
        for line in f:
            for pat in patterns:
                matches = pat.findall(line)
                count += len(matches)
    return count

import mmap

# Method 3: Optimized Bytes + Combined Regex + MMAP
def scan_optimized_bytes(filepath, combined_pattern):
    count = 0
    with open(filepath, 'rb') as f:
        # standard mmap usage
        with mmap.mmap(f.fileno(), 0, access=mmap.ACCESS_READ) as mm:
            matches = combined_pattern.findall(mm)
            count += len(matches)
    return count

# Method 4: Combined Text Regex
def scan_combined_text(filepath, combined_pattern):
    count = 0
    with open(filepath, 'r') as f:
        content = f.read()
        matches = combined_pattern.findall(content)
        count += len(matches)
    return count

def main():
    test_file = "benchmark_test.txt"
    print("Generating test file...")
    generate_large_file(test_file, lines=500000)
    
    # Text patterns
    patterns = [
        re.compile(r'CALL\s+[\'"]([A-Z0-9]+)[\'"]', re.IGNORECASE),
        re.compile(r'EXEC\s+PGM=([A-Z0-9]+)', re.IGNORECASE)
    ]

    # Combined Bytes Pattern
    bytes_pattern = re.compile(
        rb'(?:CALL\s+[\'"]([A-Z0-9]+)[\'"])|(?:EXEC\s+PGM=([A-Z0-9]+))', 
        re.IGNORECASE
    )

    # Combined Text Pattern
    text_pattern = re.compile(
        r'(?:CALL\s+[\'"]([A-Z0-9]+)[\'"])|(?:EXEC\s+PGM=([A-Z0-9]+))', 
        re.IGNORECASE
    )
    
    print("Starting benchmark...")
    
    # Measure Whole Content
    start_time = time.time()
    c1 = scan_whole_content(test_file, patterns)
    t1 = time.time() - start_time
    print(f"Whole File (Split Rx):   {t1:.4f}s (matches: {c1})")
    
    # Measure Line by Line
    start_time = time.time()
    c2 = scan_line_by_line(test_file, patterns)
    t2 = time.time() - start_time
    print(f"Line-by-Line:            {t2:.4f}s (matches: {c2})")

    # Measure Optimized Bytes (Mmap)
    start_time = time.time()
    c3 = scan_optimized_bytes(test_file, bytes_pattern)
    t3 = time.time() - start_time
    print(f"Bytes+Mmap+Combined Rx:  {t3:.4f}s (matches: {c3})")
    
    # Measure Combined Text
    start_time = time.time()
    c4 = scan_combined_text(test_file, text_pattern)
    t4 = time.time() - start_time
    print(f"Whole File (Combined Rx):{t4:.4f}s (matches: {c4})")
    
    print("-" * 30)
    best_time = min(t1, t2, t3, t4)
    if best_time == t1: print("Winner: Whole File (Split Regex)")
    elif best_time == t2: print("Winner: Line-by-Line")
    elif best_time == t3: print("Winner: Bytes + Mmap")
    elif best_time == t4: print("Winner: Whole File (Combined Regex)")

    os.remove(test_file)

if __name__ == "__main__":
    main()
