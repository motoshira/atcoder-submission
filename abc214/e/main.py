from heapq import heappop,heappush

def main():
    N = int(input())
    ss = list(map(int,input().split()))
    ts = list(map(int,input().split()))

    h = []

    for i in range(N):
        heappush(h, [ts[i],i]) # [最初に渡される時間, すぬけ君のindex]

    ans = [-1]*N

    while h:
        time, idx = heappop(h)
        if ans[idx] != = -1:
            # 最初に到達
            ans[idx] = time
            time_new = time + ss[idx]
            heappush(h, [time_new,])
