import sys 
import random

def main():
  if len(sys.argv) < 3:
    print("Insuficient arguments!")

  file_name = sys.argv[1]
  pc_side = int(sys.argv[2])
  in_n_points = int(sys.argv[3])

  f = open(file_name, "w")
  f.write(
"""ply
format ascii 1.0
element vertex {n_points:}
property float x
property float y
property float z
end_header\n""".format(n_points = in_n_points))
  
  for i in range(in_n_points):
    x_random,y_random,z_random = [random.randint(0, pc_side-1),
             random.randint(0, pc_side-1),
             random.randint(0, pc_side-1)]
    f.write("{x:} {y:} {z:}\n".format(x = x_random,y = y_random,z = z_random))

  f.close()  

if __name__ == "__main__":
  main()