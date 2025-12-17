import os
import re

def scan_files(root_dir):
    refs = []
    
    # Define patterns
    # Each pattern is a tuple: (Compiled Regex, Type Label)
    # The regex handles both quoted and unquoted variants where applicable
    patterns = [
        # Matches: PGM='NAME' or PGM=NAME (without requiring EXEC)
        # Group 1: Quoted name, Group 2: Unquoted name
        (re.compile(r'PGM=(?:[\'"]([A-Z0-9\-]+)[\'"]|([A-Z0-9\-]+))', re.IGNORECASE), 'PGM'),
        
        # CALL 'NAME' or CALL NAME
        # Group 1: Quoted name, Group 2: Unquoted name
        (re.compile(r'CALL\s+(?:[\'"]([A-Z0-9\-]+)[\'"]|([A-Z0-9\-]+))', re.IGNORECASE), 'CALL'),
        
        # COPY NAME
        # Group 1: Name
        (re.compile(r'COPY\s+([A-Z0-9\-]+)', re.IGNORECASE), 'COPY')
    ]

    if not os.path.exists(root_dir):
        print(f"Directory not found: {root_dir}")
        return refs

    # Walk through all directories and files
    for root, dirs, files in os.walk(root_dir):
        for filename in files:
            filepath = os.path.join(root, filename)
            try:
                # Attempt to read file; skip binary/encoding errors
                with open(filepath, 'r', encoding='utf-8') as f:
                    for line in f:
                        # Exclude lines containing 'SECTION.' (case-insensitive)
                        if 'SECTION.' in line.upper():
                            continue

                        for pattern, ref_type in patterns:
                            matches = pattern.findall(line)
                            for match in matches:
                                target = ""
                                # regex findall with groups returns tuples.
                                # For our OR logic (?:A|B), we get ('match', '') or ('', 'match').
                                # We need to find the non-empty string.
                                if isinstance(match, tuple):
                                    target = next((m for m in match if m), "")
                                else:
                                    target = match
                                    
                                if target:
                                    refs.append({
                                        'source': filename,
                                        'target': target,
                                        'type': ref_type,
                                        'line': line
                                    })
            except (UnicodeDecodeError, OSError):
                # Skip files that can't be read as text
                continue
                
    return refs

def main():
    # Use the current working directory or specific project part
    # For this specific user request, we scan the whole 'cobol_sample' folder
    base_dir = r"c:\Users\Hp\Antigravity\cobol_sample"
    
    all_refs = scan_files(base_dir)
    
    print("\n" + "="*60)
    print(f"{'SOURCE':<30} | {'TARGET':<20} | {'TYPE':<10}")
    print("-" * 64)
    
    # Sort for cleaner output
    all_refs.sort(key=lambda x: (x['type'], x['source']))
    
    for ref in all_refs:
        print(f"{ref['source']:<30} | {ref['target']:<20} | {ref['type']:<10} | {ref['line']}")

if __name__ == "__main__":
    main()
