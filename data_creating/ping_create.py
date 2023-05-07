import ipaddress
import random
from pythonping import ping
from joblib import Parallel, delayed

MAX_IPV4 = ipaddress.IPv4Address._ALL_ONES  # 2 ** 32 - 1


def random_ipv4():
    return  ipaddress.IPv4Address._string_from_ip_int(
        random.randint(0, MAX_IPV4)
    )


def ping_random():
    ip = random_ipv4()
    raw_ans = ping(ip, timeout=10, count=1)
    ans = float(str(raw_ans).split()[-2].split('/')[0])
    if 0.01 < ans < 9999.9:
        with open("out.txt", "a") as file:
            file.write(str(ans) + " ")


Parallel(n_jobs=200)(delayed(ping_random)() for i in range(1000000))



