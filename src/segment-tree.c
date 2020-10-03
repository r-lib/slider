#include "segment-tree.h"
#include "utils.h"

static void segment_tree_initialize_levels(struct segment_tree* p_tree);

// [[ include("segment-tree.h") ]]
struct segment_tree new_segment_tree(uint64_t n_leaves,
                                     const void* p_leaves,
                                     void* p_state,
                                     void (*state_reset)(void* p_state),
                                     void (*state_finalize)(void* p_state, void* p_result),
                                     void* (*nodes_increment)(void* p_nodes),
                                     SEXP (*nodes_initialize)(uint64_t n),
                                     void (*aggregate_from_leaves)(const void* p_source, uint64_t begin, uint64_t end, void* p_dest),
                                     void (*aggregate_from_nodes)(const void* p_source, uint64_t begin, uint64_t end, void* p_dest)) {
  struct segment_tree tree;

  tree.n_leaves = n_leaves;
  tree.n_levels = 0;
  tree.n_nodes = 0;

  uint64_t n_level_nodes = n_leaves;
  while (n_level_nodes > 1) {
    n_level_nodes = (uint64_t) ceil((double) n_level_nodes / SEGMENT_TREE_FANOUT);
    tree.n_nodes += n_level_nodes;
    ++tree.n_levels;
  }

  tree.p_leaves = p_leaves;
  tree.p_state = p_state;

  tree.p_level = PROTECT(Rf_allocVector(RAWSXP, tree.n_levels * sizeof(void*)));
  tree.p_p_level = (void**) RAW(tree.p_level);

  tree.nodes = PROTECT(nodes_initialize(tree.n_nodes));
  tree.p_nodes = r_deref(tree.nodes, TYPEOF(tree.nodes));

  tree.state_reset = state_reset;
  tree.state_finalize = state_finalize;
  tree.nodes_increment = nodes_increment;
  tree.aggregate_from_leaves = aggregate_from_leaves;
  tree.aggregate_from_nodes = aggregate_from_nodes;

  segment_tree_initialize_levels(&tree);

  UNPROTECT(2);
  return tree;
}

// -----------------------------------------------------------------------------

static void segment_tree_initialize_levels(struct segment_tree* p_tree) {
  uint64_t n_levels = p_tree->n_levels;

  if (n_levels == 0) {
    return;
  }

  uint64_t n_leaves = p_tree->n_leaves;

  const void* p_leaves = p_tree->p_leaves;
  void* p_dest = p_tree->p_nodes;

  void** p_p_level = p_tree->p_p_level;

  p_p_level[0] = p_dest;
  uint64_t n_nodes_next_source = 0;

  // Handle leaf aggregation
  for (uint64_t i = 0; i < n_leaves; i += SEGMENT_TREE_FANOUT) {
    uint64_t begin = i;
    uint64_t end = min_u64(n_leaves, i + SEGMENT_TREE_FANOUT);

    p_tree->aggregate_from_leaves(p_leaves, begin, end, p_dest);
    p_dest = p_tree->nodes_increment(p_dest);
    ++n_nodes_next_source;
  }

  void* p_source = p_p_level[0];
  uint64_t n_nodes_source = n_nodes_next_source;

  // Handle node aggregation
  for (uint64_t i = 1; i < n_levels; ++i) {
    p_p_level[i] = p_dest;
    n_nodes_next_source = 0;

    for (uint64_t j = 0; j < n_nodes_source; j += SEGMENT_TREE_FANOUT) {
      uint64_t begin = j;
      uint64_t end = min_u64(n_nodes_source, j + SEGMENT_TREE_FANOUT);

      p_tree->aggregate_from_nodes(p_source, begin, end, p_dest);
      p_dest = p_tree->nodes_increment(p_dest);
      ++n_nodes_next_source;
    }

    p_source = p_p_level[i];
    n_nodes_source = n_nodes_next_source;
  }
}

// -----------------------------------------------------------------------------

static void segment_tree_aggregate_level(const void* p_source,
                                         void (*aggregate)(const void* p_source, uint64_t begin, uint64_t end, void* p_dest),
                                         uint64_t* p_begin,
                                         uint64_t* p_end,
                                         void* p_dest,
                                         bool* p_done);

// [[ include("segment-tree.h") ]]
void segment_tree_aggregate(const struct segment_tree* p_tree,
                            uint64_t begin,
                            uint64_t end,
                            void* p_result) {
  bool done = false;

  void* p_state = p_tree->p_state;

  p_tree->state_reset(p_state);

  const void* p_leaves = p_tree->p_leaves;

  // Aggregate leaf level
  segment_tree_aggregate_level(
    p_leaves,
    p_tree->aggregate_from_leaves,
    &begin,
    &end,
    p_state,
    &done
  );

  if (done) {
    p_tree->state_finalize(p_state, p_result);
    return;
  }

  void** p_p_level = p_tree->p_p_level;
  uint64_t n_levels = p_tree->n_levels;

  // Continue aggregation of node levels
  for (uint64_t i = 0; i < n_levels; ++i) {
    const void* p_level = p_p_level[i];

    segment_tree_aggregate_level(
      p_level,
      p_tree->aggregate_from_nodes,
      &begin,
      &end,
      p_state,
      &done
    );

    if (done) {
      break;
    }
  }

  p_tree->state_finalize(p_state, p_result);
  return;
}

static void segment_tree_aggregate_level(const void* p_source,
                                         void (*aggregate)(const void* p_source, uint64_t begin, uint64_t end, void* p_dest),
                                         uint64_t* p_begin,
                                         uint64_t* p_end,
                                         void* p_state,
                                         bool* p_done) {
  uint64_t begin = *p_begin;
  uint64_t end = *p_end;

  // Integer division! Assume fanout is a power of 2 so we can use shifting
  // which is much faster than division
  uint64_t parent_begin = begin >> SEGMENT_TREE_FANOUT_POWER;
  uint64_t parent_end = end >> SEGMENT_TREE_FANOUT_POWER;

  // Same fan group
  if (parent_begin == parent_end) {
    aggregate(p_source, begin, end, p_state);
    *p_done = true;
    return;
  }

  uint64_t group_begin = parent_begin * SEGMENT_TREE_FANOUT;
  uint64_t group_end = parent_end * SEGMENT_TREE_FANOUT;

  if (begin != group_begin) {
    uint64_t stop = group_begin + SEGMENT_TREE_FANOUT;
    aggregate(p_source, begin, stop, p_state);
    parent_begin += 1;
  }

  if (end != group_end) {
    aggregate(p_source, group_end, end, p_state);
  }

  // Update for next level
  *p_begin = parent_begin;
  *p_end = parent_end;
}
