import re

# Compile patterns once for performance
# These patterns match the requirements:
# 1. PGM=NAME or PGM='NAME'
# 2. CALL NAME or CALL 'NAME'
# 3. COPY NAME
PATTERNS = [
    # Matches: PGM='NAME' or PGM=NAME
    # Group 1: Quoted name, Group 2: Unquoted name
    (re.compile(r'PGM=(?:[\'"]([A-Z0-9\-]+)[\'"]|([A-Z0-9\-]+))', re.IGNORECASE), 'PGM'),
    
    # Matches: CALL 'NAME' or CALL NAME
    # Group 1: Quoted name, Group 2: Unquoted name
    (re.compile(r'CALL\s+(?:[\'"]([A-Z0-9\-]+)[\'"]|([A-Z0-9\-]+))', re.IGNORECASE), 'CALL'),
    
    # Matches: COPY NAME
    (re.compile(r'COPY\s+([A-Z0-9\-]+)', re.IGNORECASE), 'COPY')
]

def scan_line(line):
    """
    Scans a single string (line) for cross-references.
    
    Args:
        line (str): The line of code to check.
        
    Returns:
        list[dict]: A list of dictionaries containing 'type' and 'target' for each match found.
    """
    # Exclude lines containing 'SECTION.' (case-insensitive)
    if 'SECTION.' in line.upper():
        return []

    found_refs = []
    
    for pattern, ref_type in PATTERNS:
        matches = pattern.findall(line)
        for match in matches:
            target = ""
            # regex findall with groups returns tuples if multiple groups exist
            if isinstance(match, tuple):
                # valid group is the non-empty one
                target = next((m for m in match if m), "")
            else:
                target = match
            
            if target:
                found_refs.append({
                    'type': ref_type,
                    'target': target
                })
                
    return found_refs

# Example usage/Test
if __name__ == "__main__":
    test_lines = [
        "//STEP01 EXEC PGM=MYPROGRAM",
        "//STEP02 EXEC PGM='QUOTEDPGM'",
        "       CALL 'SUBPROG'",
        "       CALL SUBPROG2",
        "       COPY MYCOPYBOOK",
        "       COPY SOMECOPY REPLACING ==X== BY ==Y==.",
        "A-SECTION SECTION.", # Should be ignored even if it matches something (unlikely to match patterns here, but good for test)
        "       MOVE A TO B" # Should find nothing
    ]
    
    print("Testing Line Scanner:")
    print("-" * 20)
    for l in test_lines:
        refs = scan_line(l)
        if refs:
            for r in refs:
                print(f"Line: '{l.strip()}' -> Detected: {r['type']} Reference to '{r['target']}'")
        else:
            print(f"Line: '{l.strip()}' -> No references found")
