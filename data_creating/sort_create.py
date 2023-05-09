import numpy as np
import time


def add_new_sorting_data():
    x = np.random.randint(0, 2**32 - 1, 100000)
    start = time.perf_counter_ns()
    x = np.sort(x)
    end = time.perf_counter_ns()
    with open("out.txt", "a") as file:
        file.write(str(end - start) + " ")


for i in range(50000):
    add_new_sorting_data()
