#ifndef SLIDER_SEGMENT_TREE
#define SLIDER_SEGMENT_TREE

#include "slider.h"

#define SEGMENT_TREE_FANOUT 16
#define SEGMENT_TREE_FANOUT_POWER 4

struct segment_tree {
  const void* p_leaves;

  SEXP p_level;
  void** p_p_level;

  SEXP nodes;
  void* p_nodes;

  void* p_state;

  uint64_t n_leaves;
  uint64_t n_levels;
  uint64_t n_nodes;

  void (*state_reset)(void* p_state);
  void (*state_finalize)(void* p_state, void* p_result);

  void* (*nodes_increment)(void* p_nodes);

  void (*aggregate_from_leaves)(const void* p_source, uint64_t begin, uint64_t end, void* p_dest);
  void (*aggregate_from_nodes)(const void* p_source, uint64_t begin, uint64_t end, void* p_dest);
};

#define PROTECT_SEGMENT_TREE(p_tree, p_n) do {  \
  PROTECT((p_tree)->p_level);                   \
  PROTECT((p_tree)->nodes);                     \
  *(p_n) += 2;                                  \
} while(0)


struct segment_tree new_segment_tree(uint64_t n_leaves,
                                     const void* p_leaves,
                                     void* p_state,
                                     void (*state_reset)(void* p_state),
                                     void (*state_finalize)(void* p_state, void* p_result),
                                     void* (*nodes_increment)(void* p_nodes),
                                     SEXP (*nodes_initialize)(uint64_t n),
                                     void (*aggregate_from_leaves)(const void* p_source, uint64_t begin, uint64_t end, void* p_dest),
                                     void (*aggregate_from_nodes)(const void* p_source, uint64_t begin, uint64_t end, void* p_dest));

void segment_tree_aggregate(const struct segment_tree* p_tree,
                            uint64_t begin,
                            uint64_t end,
                            void* p_result);

#endif
