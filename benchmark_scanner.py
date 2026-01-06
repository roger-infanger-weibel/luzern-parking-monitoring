import time
import re
import os

# Mock data generation
def generate_large_file(filename, lines=100000):
    with open(filename, 'w') as f:
        for i in range(lines):
            if i % 1000 == 0:
                # TRAP: Matches pattern but has SECTION. Should be excluded by Multiline, counted by others.
                f.write(f" BAD-SECTION-{i} SECTION. CALL 'TRAP{i}' \n")
            elif i % 500 == 0:
                f.write(f" A-SECTION-{i} SECTION. \n")
            elif i % 100 == 0:
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

# Method 5: Multiline Regex with Exclusion
def scan_multiline_regex(filepath, multiline_pattern):
    count = 0
    with open(filepath, 'r') as f:
        content = f.read()
        matches = multiline_pattern.findall(content)
        count += len(matches)
    return count

def main():
    test_file = "benchmark_test.txt"
    print("Generating test file...")
    # Reduced size for faster consistent testing, or keep 500k? 500k is fine.
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
    
    # Multiline Pattern with Exclusion
    # Matches lines that DO NOT contain SECTION. but DO contain CALL or EXEC
    # (?m) enables multiline mode (^ matches start of line)
    # ^(?!.*SECTION\.) checks that the line doesn't have SECTION.
    # .* matches content before the keyword
    # Then the keyword pattern
    multiline_pattern = re.compile(
        r'(?m)^(?!.*SECTION\.).*(?:CALL\s+[\'"](?P<call>[A-Z0-9]+)[\'"]|EXEC\s+PGM=(?P<exec>[A-Z0-9]+))',
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
    
    # Measure Multiline Exclusion
    start_time = time.time()
    c5 = scan_multiline_regex(test_file, multiline_pattern)
    t5 = time.time() - start_time
    print(f"Multiline Regex (Excl):  {t5:.4f}s (matches: {c5})")

    print("-" * 30)
    # We only compare best time, but note that counts might differ if exclusions apply
    times = {
        "Whole File (Split Regex)": t1,
        "Line-by-Line": t2,
        "Bytes + Mmap": t3,
        "Whole File (Combined Regex)": t4,
        "Multiline Regex (Excl)": t5
    }
    
    best_name = min(times, key=times.get)
    print(f"Winner: {best_name}")

    os.remove(test_file)

if __name__ == "__main__":
    main()
