from math import sin, cos, pi, exp, sqrt
from random import uniform


def make_particle(xlims, ylims):
    xlo, xhi = xlims
    ylo, yhi = ylims
    coords = [uniform(xlo, xhi), uniform(ylo, yhi)]
    velocity = [uniform(0, 1) * 0.1, uniform(0, 1) * 0.1]
    return {
        "position": coords,
        "best": coords,
        "velocity": velocity,
        "history": [coords],
    }


def make_swarm(xlims, ylims, nparticles):
    return [make_particle(xlims, ylims) for _ in range(nparticles)]


def best_particle(f, swarm):
    return min(swarm, key=lambda d: f(*d["position"]))


def vec_add(*args):
    return [sum(xs) for xs in zip(*args)]


def vec_sub(v1, v2):
    return [x - y for x, y in zip(v1, v2)]


def update_swarm(swarm, f, cognition, cohesion, inertia):
    global_best = best_particle(f, swarm)

    def update_particle(p):
        xy = p["position"]
        v = p["velocity"]
        b = p["best"]
        h = p["history"]

        new_v = vec_add(
            [x * inertia for x in v],
            [x * cognition * uniform(0, 1) for x in vec_sub(b, xy)],
            [
                x * cohesion * uniform(0, 1)
                for x in vec_sub(global_best["position"], xy)
            ],
        )

        new_xy = vec_add(xy, new_v)
        new_h = h + [new_xy]
        if f(*new_xy) < f(*xy):
            new_b = new_xy
        else:
            new_b = b

        return {
            "position": new_xy,
            "best": new_b,
            "velocity": new_v,
            "history": new_h,
        }

    return [update_particle(p) for p in swarm]


def pso(f, xlims, ylims, nparticles, cognition, cohesion, inertia, niter):
    swarm = make_swarm(xlims, ylims, nparticles)
    for i in range(niter):
        swarm = update_swarm(swarm, f, cognition, cohesion, inertia)
    return best_particle(f, swarm)["best"]


def mlmfunc(x, y):
    return (x - 3.14) ** 2 + (y - 2.72) ** 2 + sin(3 * x + 1.41) + sin(4 * y - 1.73)


def ackley(x, y):
    return (
        -20 * exp(-0.2 * sqrt(0.5 * ((x - 1) ** 2 + (y - 1) ** 2)))
        - exp(0.5 * (cos(2 * pi * x) + cos(2 * pi * y)))
        + exp(1)
        + 20
    )


pso(mlmfunc, [0, 5], [0, 5], 20, 0.01, 0.05, 0.5, 50)
pso(ackley, [-10, 10], [-10, 10], 20, 0.01, 0.05, 0.5, 50)
