import numpy as np
import time
from sklearn.decomposition import PCA


def add_new_ml_data():
    start = time.perf_counter_ns()
    rng = np.random.RandomState(0)
    n_samples = 50000
    cov = [[3, 3], [3, 4]]
    X = rng.multivariate_normal(mean=[0, 0], cov=cov, size=n_samples)
    pca = PCA(n_components=2).fit(X)
    end = time.perf_counter_ns()
    with open("out.txt", "a") as file:
        file.write(str((end - start) // 100) + " ")


for i in range(45000):
    add_new_ml_data()
