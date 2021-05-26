import sys
import bitstream as bstr

def encode(bit):
  if bit == True:
    return "11111111"

  else:
    return "00000000"

def convert_from_bool(x):
  if x:
    return '1'

  else:
    return '0'

def read_bits(bitstream, size):
  bits = ""    
  for _ in range(size):
    bits += encode(bitstream.read(bool,1)[0])
  return bits

def write_file(bitstream, input_file):
  f = open(input_file[:-4:]+"_byte.edx", "wb")

  byte = ""
  for _ in range(len(bitstream)):
    byte += convert_from_bool(bitstream.read(bool,1)[0])

    if len(byte) == 8:
      byte = int(byte,2).to_bytes(1, 'little')
      f.write(byte)
      byte = ""

  f.close()

def bit_file2byte_file(input_file):
  convert2bool = lambda x: True if x == '1' else False
  bitstream = bstr.BitStream()
  out = bstr.BitStream()

  with open(input_file, "rb") as f:
    byte = f.read(1)
    bitstream.write(byte)
    while byte:
        byte = f.read(1)
        bitstream.write(byte)
  f.close()

  while len(bitstream) > 0:
    bits = read_bits(bitstream, 1)
    out.write(list(map(convert2bool, bits)))

  write_file(out, input_file)

  return 

def main():
  if len(sys.argv) < 2:
    print("Missing image file path parameter!")
    return

  input_file = sys.argv[1]

  bit_file2byte_file(input_file)



if __name__ == "__main__":
  main()