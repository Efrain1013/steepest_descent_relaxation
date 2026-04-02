import numpy as np
import argparse
from relax_sd.structure_io.poscar import read_poscar, write_poscar
from relax_sd.core.state import sync_frac_to_cart, sync_cart_to_frac

def random_displace_structure(state, max_step, seed=None):
    rng = np.random.default_rng(seed)
    new_state = state.copy()

    if new_state.positions_cart is None:
        sync_frac_to_cart(new_state)

    n = new_state.positions_cart.shape[0]

    vec = rng.normal(size=(n,3))
    vec /= np.linalg.norm(vec, axis=1)[:,None]

    mag = rng.uniform(0.0, max_step, size=(n,1))
    disp = vec * mag
    new_state.positions_cart += disp
    sync_cart_to_frac(new_state)

    return new_state

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-p", "--poscar", default="POSCAR.vasp", help="The original POSCAR structure")
    parser.add_argument("-o","--output", default="POSCAR_displaced.vasp", help="output filename")
    parser.add_argument("--max-step", type=float, required=True, help="max step allowed")
    parser.add_argument("--seed", type=int, help="seed")

    args = parser.parse_args()

    state = read_poscar(args.poscar)

    new_state = random_displace_structure(state, max_step=args.max_step, seed=args.seed )
    comment = (state.metadata.get("poscar_comment","POSCAR") + f" | displaced max_step={args.max_step} seed={args.seed}" )

    write_poscar(args.output, new_state, comment=comment)

if __name__ == "__main__":
    main()