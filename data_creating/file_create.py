import os
import random
import time


def delete_and_create():
    start = time.perf_counter()
    file_name = 'new_file'
    with open(file_name, "w") as file:
        write = [str(random.randint(i, 1000000)) for i in range(random.randint(3, 10))]
        file.write(str(write))
    end = time.perf_counter()
    with open("out.txt", "a") as file:
        file.write(str(end - start) + " ")


for i in range(50000):
    delete_and_create()
