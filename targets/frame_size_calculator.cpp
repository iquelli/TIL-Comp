#include "targets/frame_size_calculator.h"
#include ".auto/all_nodes.h" // automatically generated
#include "targets/type_checker.h"

til::frame_size_calculator::~frame_size_calculator() { os().flush(); }

//---------------------------------------------------------------------------

void til::frame_size_calculator::do_nil_node(cdk::nil_node *const node,
                                             int lvl) {
    // EMPTY
}

void til::frame_size_calculator::do_data_node(cdk::data_node *const node,
                                              int lvl) {
    // EMPTY
}

//---------------------------------------------------------------------------

void til::frame_size_calculator::do_sequence_node(
    cdk::sequence_node *const node, int lvl) {
    for (size_t i = 0; i < node->size(); ++i) {
        auto child = node->node(i);
        if (child == nullptr) {
            break;
        }
        child->accept(this, lvl + 2);
    }
}

void til::frame_size_calculator::do_block_node(til::block_node *const node,
                                               int lvl) {
    _symtab.push();
    if (node->declarations()) {
        node->declarations()->accept(this, lvl + 2);
    }
    if (node->instructions()) {
        node->instructions()->accept(this, lvl + 2);
    }
    _symtab.pop();
}

//---------------------------------------------------------------------------

void til::frame_size_calculator::do_integer_node(cdk::integer_node *const node,
                                                 int lvl) {
    // EMPTY
}

void til::frame_size_calculator::do_double_node(cdk::double_node *const node,
                                                int lvl) {
    // EMPTY
}

void til::frame_size_calculator::do_string_node(cdk::string_node *const node,
                                                int lvl) {
    // EMPTY
}

void til::frame_size_calculator::do_null_node(til::null_node *const node,
                                              int lvl) {
    // EMPTY
}

//---------------------------------------------------------------------------

void til::frame_size_calculator::do_unary_minus_node(
    cdk::unary_minus_node *const node, int lvl) {
    // EMPTY
}

void til::frame_size_calculator::do_unary_plus_node(
    cdk::unary_plus_node *const node, int lvl) {
    // EMPTY
}

void til::frame_size_calculator::do_not_node(cdk::not_node *const node,
                                             int lvl) {
    // EMPTY
}

//---------------------------------------------------------------------------

void til::frame_size_calculator::do_add_node(cdk::add_node *const node,
                                             int lvl) {
    // EMPTY
}
void til::frame_size_calculator::do_sub_node(cdk::sub_node *const node,
                                             int lvl) {
    // EMPTY
}
void til::frame_size_calculator::do_mul_node(cdk::mul_node *const node,
                                             int lvl) {
    // EMPTY
}
void til::frame_size_calculator::do_div_node(cdk::div_node *const node,
                                             int lvl) {
    // EMPTY
}
void til::frame_size_calculator::do_mod_node(cdk::mod_node *const node,
                                             int lvl) {
    // EMPTY
}
void til::frame_size_calculator::do_lt_node(cdk::lt_node *const node, int lvl) {
    // EMPTY
}
void til::frame_size_calculator::do_le_node(cdk::le_node *const node, int lvl) {
    // EMPTY
}
void til::frame_size_calculator::do_ge_node(cdk::ge_node *const node, int lvl) {
    // EMPTY
}
void til::frame_size_calculator::do_gt_node(cdk::gt_node *const node, int lvl) {
    // EMPTY
}
void til::frame_size_calculator::do_ne_node(cdk::ne_node *const node, int lvl) {
    // EMPTY
}
void til::frame_size_calculator::do_eq_node(cdk::eq_node *const node, int lvl) {
    // EMPTY
}
void til::frame_size_calculator::do_and_node(cdk::and_node *const node,
                                             int lvl) {
    // EMPTY
}
void til::frame_size_calculator::do_or_node(cdk::or_node *const node, int lvl) {
    // EMPTY
}

//---------------------------------------------------------------------------

void til::frame_size_calculator::do_variable_node(
    cdk::variable_node *const node, int lvl) {
    // EMPTY
}

void til::frame_size_calculator::do_index_node(til::index_node *const node,
                                               int lvl) {
    // EMPTY
}

void til::frame_size_calculator::do_rvalue_node(cdk::rvalue_node *const node,
                                                int lvl) {
    // EMPTY
}

void til::frame_size_calculator::do_assignment_node(
    cdk::assignment_node *const node, int lvl) {
    // EMPTY
}

//---------------------------------------------------------------------------

void til::frame_size_calculator::do_declaration_node(
    til::declaration_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;
    _localsize += node->type()->size();
}

void til::frame_size_calculator::do_function_node(
    til::function_node *const node, int lvl) {
    node->block()->accept(this, lvl + 2);
}

void til::frame_size_calculator::do_function_call_node(
    til::function_call_node *const node, int lvl) {
    // EMPTY
}

void til::frame_size_calculator::do_return_node(til::return_node *const node,
                                                int lvl) {
    // EMPTY
}

//---------------------------------------------------------------------------

void til::frame_size_calculator::do_evaluation_node(
    til::evaluation_node *const node, int lvl) {
    // EMPTY
}

//---------------------------------------------------------------------------

void til::frame_size_calculator::do_print_node(til::print_node *const node,
                                               int lvl) {
    // EMPTY
}

void til::frame_size_calculator::do_read_node(til::read_node *const node,
                                              int lvl) {
    // EMPTY
}

//---------------------------------------------------------------------------

void til::frame_size_calculator::do_if_node(til::if_node *const node, int lvl) {
    node->block()->accept(this, lvl + 2);
}

void til::frame_size_calculator::do_if_else_node(til::if_else_node *const node,
                                                 int lvl) {
    node->thenblock()->accept(this, lvl + 2);
    if (node->elseblock()) {
        node->elseblock()->accept(this, lvl + 2);
    }
}

//---------------------------------------------------------------------------

void til::frame_size_calculator::do_loop_node(til::loop_node *const node,
                                              int lvl) {
    node->block()->accept(this, lvl + 2);
}

void til::frame_size_calculator::do_next_node(til::next_node *const node,
                                              int lvl) {
    // EMPTY
}

void til::frame_size_calculator::do_stop_node(til::stop_node *const node,
                                              int lvl) {
    // EMPTY
}

//---------------------------------------------------------------------------

void til::frame_size_calculator::do_sizeof_node(til::sizeof_node *const node,
                                                int lvl) {
    // EMPTY
}

void til::frame_size_calculator::do_alloc_node(til::alloc_node *const node,
                                               int lvl) {
    // EMPTY
}

void til::frame_size_calculator::do_address_of_node(
    til::address_of_node *const node, int lvl) {
    // EMPTY
}
