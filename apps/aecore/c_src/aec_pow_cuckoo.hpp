//-------------------------------------------------------------------
// Copyright (C) 2017, Aeternity Anstalt
// Based on Cuckoo Cycle of John Tromp, a memory-hard proof-of-work
//-------------------------------------------------------------------

struct pow_cuckoo_result {
  u64 key1;
  u64 key2;
  node_t soln[PROOFSIZE];

  pow_cuckoo_result(u64 key1, u64 key2, node_t soln_in[PROOFSIZE]) :
    key1(key1),
    key2(key2) {
    for (int i = 0; i < PROOFSIZE; i++) {
      soln[i] = soln_in[i];
    }
  }
};
