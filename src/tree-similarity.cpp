// Routine only slightly modified from original "main.cc" in
// tree-similarity repo src/command_line, and thus:
// The MIT License (MIT)
// Copyright (c) 2017 Mateusz Pawlik, Nikolaus Augsten, and Daniel Kocher.

#include "cpp11.hpp"

#include <iostream>

#include "tree-similarity/node/node.h"
#include "tree-similarity/label/string_label.h"
#include "tree-similarity/cost_model/unit_cost_model.h"
#include "tree-similarity/parser/bracket_notation_parser.h"
#include "tree-similarity/ted/apted_tree_index.h"

using namespace cpp11;

// Call main tree-similarity algorithm bundled here from
// https://github.com/DatabaseGroup/tree-similarity
// The main "ted" algorithm returns a double (defined in
// src/ted/apted_tree_index.h), so integer tree sizes are also converted to
// double here.
[[cpp11::register]]
writable::doubles cpp_tree_similarity(strings x) {

    using Label = label::StringLabel;
    using CostModelLD = cost_model::UnitCostModelLD<Label>;
    using LabelDictionary = label::LabelDictionary<Label>;

    if (x.size() < 2L) {
        cpp11::stop("tree_similarity requires at least 2 trees\n");
    }

    std::string source_tree_string = r_string(x[0]);
    std::string dest_tree_string = r_string(x[1]);

    parser::BracketNotationParser<Label> bnp;

    if (!bnp.validate_input(source_tree_string)) {
        cpp11::stop("Incorrect format of source tree. Is the number of opening and closing brackets equal?\n");
    }
    const node::Node<Label> source_tree = bnp.parse_single(source_tree_string);

    if (!bnp.validate_input(dest_tree_string)) {
        cpp11::stop("Incorrect format of destination tree. Is the number of opening and closing brackets equal?\n");
    }
    const node::Node<Label> destination_tree = bnp.parse_single(dest_tree_string);

    const double source_tree_size = static_cast<double>(source_tree.get_tree_size());
    const double dest_tree_size = static_cast<double>(destination_tree.get_tree_size());

    LabelDictionary ld;
    CostModelLD ucm(ld);
    ted::APTEDTreeIndex<CostModelLD, node::TreeIndexAPTED> apted_algorithm(ucm);
    node::TreeIndexAPTED ti1;
    node::TreeIndexAPTED ti2;
    node::index_tree(ti1, source_tree, ld, ucm);
    node::index_tree(ti2, destination_tree, ld, ucm);
    const double tree_distance = apted_algorithm.ted(ti1, ti2);

    writable::doubles out(3);
    out [0] = source_tree_size;
    out [1] = dest_tree_size;
    out [2] = tree_distance;

    return out;
}
