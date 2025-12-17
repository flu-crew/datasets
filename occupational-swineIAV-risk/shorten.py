import sys
import re

def extract_strain_names(line):
    return re.findall(r"'([^']+)'", line)

def process_file(input_file, output_file, field_indices):
    with open(input_file, 'r') as infile, open(output_file, 'w') as outfile:
        for line in infile:
            leading_whitespace = line[:len(line) - len(line.lstrip())]  # Capture leading spaces/tabs
            strain_names = extract_strain_names(line)
            
            modified_line = line
            for strain_name in strain_names:
                fields = strain_name.split('|')
                
                if fields[0] == 'REF':
                    adjusted_indices = [0] + [i + 1 for i in field_indices]  # Retain 'REF' and shift indices
                else:
                    adjusted_indices = field_indices
                
                try:
                    new_fields = [fields[i] for i in adjusted_indices]
                    new_strain_name = '|'.join(new_fields)
                    modified_line = modified_line.replace(f"'{strain_name}'", f"'{new_strain_name}'")
                except IndexError:
                    print(f"Skipping strain due to invalid field index: {strain_name}")
            
            outfile.write(modified_line)

if __name__ == "__main__":
    if len(sys.argv) < 4:
        print("Usage: python3 script.py input.tre output.tre field_index1 field_index2 ...")
        sys.exit(1)
    
    input_file = sys.argv[1]
    output_file = sys.argv[2]
    field_indices = [int(i) for i in sys.argv[3:]]
    
    process_file(input_file, output_file, field_indices)