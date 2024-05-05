#include "targets/type_checker.h"
#include ".auto/all_nodes.h" // automatically generated
#include <cdk/types/primitive_type.h>
#include <string>

#include "til_parser.tab.h"

#define ASSERT_UNSPEC                                                          \
    {                                                                          \
        if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC))      \
            return;                                                            \
    }

bool til::type_checker::check_compatible_ptr_types(
    std::shared_ptr<cdk::basic_type> t1, std::shared_ptr<cdk::basic_type> t2) {
    auto t1_ptr = t1;
    auto t2_ptr = t2;
    while (t1_ptr->name() == cdk::TYPE_POINTER &&
           t2_ptr->name() == cdk::TYPE_POINTER) {
        t1_ptr = cdk::reference_type::cast(t1_ptr)->referenced();
        t2_ptr = cdk::reference_type::cast(t2_ptr)->referenced();
    }
    return t1_ptr->name() == t2_ptr->name() ||
           t2_ptr->name() == cdk::TYPE_UNSPEC;
}

bool til::type_checker::check_compatible_functional_types(
    std::shared_ptr<cdk::functional_type> t1,
    std::shared_ptr<cdk::functional_type> t2) {
    // the return type must be compatible
    if ((t1->output_length() > 0 && t2->output_length() > 0) &&
        !check_compatible_types(t1->output(0), t2->output(0), false))
        return false;

    // the number of arguments must be the same
    if (t1->input_length() != t2->input_length())
        return false;

    // the types of the arguments must be compatible
    for (size_t i = 0; i < t1->input_length(); i++)
        if (!check_compatible_types(t1->input(i), t2->input(i), false))
            return false;
    return true;
}

bool til::type_checker::check_compatible_types(
    std::shared_ptr<cdk::basic_type> t1, std::shared_ptr<cdk::basic_type> t2,
    bool is_return) {
    const auto t1_name = t1->name();
    const auto t2_name = t2->name();
    switch (t1_name) {
    case cdk::TYPE_INT:
    case cdk::TYPE_DOUBLE:
        if (!(t2_name == cdk::TYPE_DOUBLE || t2_name == cdk::TYPE_INT))
            return false;
        break;
    case cdk::TYPE_STRING:
        if (t2_name != cdk::TYPE_STRING)
            return false;
        break;
    case cdk::TYPE_POINTER:
        if (is_return == (t2_name == cdk::TYPE_POINTER) &&
            !check_compatible_ptr_types(t1, t2))
            return false;
        break;
    case cdk::TYPE_FUNCTIONAL:
        if (!((t2_name == cdk::TYPE_FUNCTIONAL &&
               check_compatible_functional_types(
                   cdk::functional_type::cast(t1),
                   cdk::functional_type::cast(t2))) ||
              (t2_name == cdk::TYPE_POINTER &&
               cdk::reference_type::cast(t2)->referenced() == nullptr)))
            return false;
        break;
    case cdk::TYPE_UNSPEC: // useful for auto cases
        if (t2_name == cdk::TYPE_VOID)
            // auto x = f(), where f calls return void, is not allowed
            return false;
        break;
    default:
        if (t1_name != t2_name)
            return false;
    }
    return true;
}

void til::type_checker::change_type_on_match(cdk::typed_node *const lvalue,
                                             cdk::typed_node *const rvalue) {
    const auto ltype = lvalue->type();
    const auto rtype = rvalue->type();

    if (ltype->name() == cdk::TYPE_UNSPEC &&
        rtype->name() == cdk::TYPE_UNSPEC) {
        // assign default type
        lvalue->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        rvalue->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if ((ltype->name() == cdk::TYPE_POINTER &&
                rtype->name() == cdk::TYPE_POINTER &&
                check_compatible_ptr_types(ltype, rtype)) ||
               (ltype->name() == cdk::TYPE_FUNCTIONAL &&
                rtype->name() == cdk::TYPE_FUNCTIONAL &&
                check_compatible_functional_types(
                    cdk::functional_type::cast(ltype),
                    cdk::functional_type::cast(rtype))) ||
               ((ltype->name() == cdk::TYPE_INT ||
                 ltype->name() == cdk::TYPE_DOUBLE) &&
                rtype->name() == cdk::TYPE_UNSPEC)) {
        rvalue->type(ltype);
    }
}

void til::type_checker::throw_incompatible_types(
    std::shared_ptr<cdk::basic_type> t1, std::shared_ptr<cdk::basic_type> t2) {
    if (check_compatible_types(t1, t2, false))
        return;

    switch (t1->name()) {
    case cdk::TYPE_INT:
    case cdk::TYPE_DOUBLE:
        throw std::string("wrong type (expected double or int)");
    case cdk::TYPE_STRING:
        throw std::string("wrong type (expected string)");
    case cdk::TYPE_POINTER:
        throw std::string("wrong type (expected pointer)");
    case cdk::TYPE_FUNCTIONAL:
        throw std::string("wrong type (expected function)");
    default:
        throw std::string("unknown type");
    }
}

//---------------------------------------------------------------------------

void til::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
    // EMPTY
}

void til::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
    // EMPTY
}

//---------------------------------------------------------------------------

void til::type_checker::do_sequence_node(cdk::sequence_node *const node,
                                         int lvl) {
    for (auto n : node->nodes()) {
        n->accept(this, lvl);
    }
}

void til::type_checker::do_block_node(til::block_node *const node, int lvl) {
    // EMPTY
}

//---------------------------------------------------------------------------

void til::type_checker::do_integer_node(cdk::integer_node *const node,
                                        int lvl) {
    ASSERT_UNSPEC;
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
    ASSERT_UNSPEC;
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}

void til::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
    ASSERT_UNSPEC;
    node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

void til::type_checker::do_null_node(til::null_node *const node, int lvl) {
    ASSERT_UNSPEC;
    node->type(cdk::reference_type::create(
        4, cdk::primitive_type::create(0, cdk::TYPE_VOID)));
}

//---------------------------------------------------------------------------

void til::type_checker::process_unary_expr(
    cdk::unary_operation_node *const node, int lvl, bool acceptDouble) {
    ASSERT_UNSPEC;

    node->argument()->accept(this, lvl + 2);
    if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
        node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (!node->argument()->is_typed(cdk::TYPE_INT) &&
               !(acceptDouble &&
                 node->argument()->is_typed(cdk::TYPE_DOUBLE))) {
        throw std::string("wrong type in argument of unary expression");
    }

    node->type(node->argument()->type());
}

void til::type_checker::do_unary_minus_node(cdk::unary_minus_node *const node,
                                            int lvl) {
    process_unary_expr(node, lvl, true);
}
void til::type_checker::do_unary_plus_node(cdk::unary_plus_node *const node,
                                           int lvl) {
    process_unary_expr(node, lvl, true);
}
void til::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
    process_unary_expr(node, lvl, false);
}

//---------------------------------------------------------------------------

bool til::type_checker::process_binary_expr(
    cdk::binary_operation_node *const node, int lvl) {

    node->left()->accept(this, lvl + 2);
    node->right()->accept(this, lvl + 2);

    // Check types and determine result type
    if (node->left()->is_typed(cdk::TYPE_INT) ||
        node->left()->is_typed(cdk::TYPE_UNSPEC)) {
        if (node->right()->is_typed(cdk::TYPE_INT)) {
            node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        } else if (node->right()->is_typed(cdk::TYPE_DOUBLE)) {
            node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
        } else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
            node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
            node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
            node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        } else {
            return false;
        }
    } else if (node->left()->is_typed(cdk::TYPE_DOUBLE)) {
        if (node->right()->is_typed(cdk::TYPE_DOUBLE) ||
            node->right()->is_typed(cdk::TYPE_INT)) {
            node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
        } else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
            node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
            node->right()->type(
                cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
        } else {
            return false;
        }
    } else {
        return false;
    }

    return true;
}

// Additive Binary

void til::type_checker::process_additive_expr(
    cdk::binary_operation_node *const node, int lvl, bool isSubtraction) {
    ASSERT_UNSPEC;

    if (process_binary_expr(node, lvl)) {
        return;
    }

    if (node->left()->is_typed(cdk::TYPE_POINTER) &&
        node->right()->is_typed(cdk::TYPE_INT)) {
        node->type(node->left()->type());
    } else if (node->left()->is_typed(cdk::TYPE_INT) &&
               node->right()->is_typed(cdk::TYPE_POINTER)) {
        node->type(node->right()->type());
    } else if (isSubtraction && node->left()->is_typed(cdk::TYPE_POINTER) &&
               node->right()->is_typed(cdk::TYPE_POINTER) &&
               check_compatible_ptr_types(node->left()->type(),
                                          node->right()->type())) {
        node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else {
        throw std::string("wrong types in additive binary expression");
    }
}

void til::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
    process_additive_expr(node, lvl, false);
}
void til::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
    process_additive_expr(node, lvl, true);
}

// Multiplicative Binary

void til::type_checker::process_multiplicative_expr(
    cdk::binary_operation_node *const node, int lvl) {
    ASSERT_UNSPEC;

    if (process_binary_expr(node, lvl)) {
        throw std::string("wrong types in multiplicative binary expression");
    }
}

void til::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
    process_multiplicative_expr(node, lvl);
}
void til::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
    process_multiplicative_expr(node, lvl);
}
void til::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
    ASSERT_UNSPEC;

    node->left()->accept(this, lvl + 2);
    node->right()->accept(this, lvl + 2);
    if (!node->left()->is_typed(cdk::TYPE_INT) ||
        !node->right()->is_typed(cdk::TYPE_INT)) {
        throw std::string("wrong type in mod expression");
    }

    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

// Comparison Binary

void til::type_checker::process_comparison_expr(
    cdk::binary_operation_node *const node, int lvl) {
    ASSERT_UNSPEC;

    if (!process_binary_expr(node, lvl)) {
        throw std::string("wrong types in comparison binary expression");
    }

    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
    process_comparison_expr(node, lvl);
}
void til::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
    process_comparison_expr(node, lvl);
}
void til::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
    process_comparison_expr(node, lvl);
}
void til::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
    process_comparison_expr(node, lvl);
}

// Equality Binary

void til::type_checker::process_equality_expr(
    cdk::binary_operation_node *const node, int lvl) {
    ASSERT_UNSPEC;

    node->left()->accept(this, lvl + 2);
    node->right()->accept(this, lvl + 2);

    if (!process_binary_expr(node, lvl) &&
        !check_compatible_ptr_types(node->left()->type(),
                                    node->right()->type())) {
        throw std::string(
            "same type expected on both sides of equality operator");
    }

    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
    process_equality_expr(node, lvl);
}
void til::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
    process_equality_expr(node, lvl);
}

// Logical Binary

void til::type_checker::process_logical_expr(
    cdk::binary_operation_node *const node, int lvl) {
    ASSERT_UNSPEC;

    node->left()->accept(this, lvl + 2);
    if (!node->left()->is_typed(cdk::TYPE_INT)) {
        throw std::string(
            "wrong type in left argument of logical binary expression");
    }

    node->right()->accept(this, lvl + 2);
    if (!node->right()->is_typed(cdk::TYPE_INT)) {
        throw std::string(
            "wrong type in right argument of logical binary expression");
    }

    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
    process_logical_expr(node, lvl);
}
void til::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
    process_logical_expr(node, lvl);
}

//---------------------------------------------------------------------------

void til::type_checker::do_variable_node(cdk::variable_node *const node,
                                         int lvl) {
    ASSERT_UNSPEC;
    const std::string &id = node->name();
    std::shared_ptr<til::symbol> symbol = _symtab.find(id);

    if (symbol != nullptr) {
        node->type(symbol->type());
    } else {
        throw id;
    }
}

void til::type_checker::do_index_node(til::index_node *const node, int lvl) {
    // TODO
    ASSERT_UNSPEC;
    // node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
    ASSERT_UNSPEC;
    try {
        node->lvalue()->accept(this, lvl);
        node->type(node->lvalue()->type());
    } catch (const std::string &id) {
        throw "undeclared variable '" + id + "'";
    }
}

void til::type_checker::do_assignment_node(cdk::assignment_node *const node,
                                           int lvl) {
    ASSERT_UNSPEC;

    node->lvalue()->accept(this, lvl + 2);
    node->rvalue()->accept(this, lvl + 2);

    change_type_on_match(node->lvalue(), node->rvalue());
    const auto lval_type = node->lvalue()->type();
    const auto rval_type = node->rvalue()->type();

    throw_incompatible_types(lval_type, rval_type);
    node->type(lval_type);
}

//---------------------------------------------------------------------------

void til::type_checker::do_declaration_node(til::declaration_node *const node,
                                            int lvl) {
    // TODO
}

void til::type_checker::do_function_node(til::function_node *const node,
                                         int lvl) {
    // EMPTY: type of function_node is already set in its constructor
}

void til::type_checker::do_function_call_node(
    til::function_call_node *const node, int lvl) {
    // TODO
}

void til::type_checker::do_return_node(til::return_node *const node, int lvl) {
    // TODO
}

//---------------------------------------------------------------------------

void til::type_checker::do_evaluation_node(til::evaluation_node *const node,
                                           int lvl) {
    node->argument()->accept(this, lvl + 2);
}

//---------------------------------------------------------------------------

void til::type_checker::do_print_node(til::print_node *const node, int lvl) {
    node->arguments()->accept(this, lvl + 2);
    for (auto *node : node->arguments()->nodes()) {
        const auto &type = (dynamic_cast<cdk::expression_node *>(node))->type();
        if (type->name() != cdk::TYPE_INT && type->name() != cdk::TYPE_DOUBLE &&
            type->name() != cdk::TYPE_STRING) {
            throw std::string("wrong type in argument of print expression");
        }
    }
}

void til::type_checker::do_read_node(til::read_node *const node, int lvl) {
    ASSERT_UNSPEC; // if it already has a value, don't overwrite it

    node->type(cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
}

//---------------------------------------------------------------------------

void til::type_checker::do_if_node(til::if_node *const node, int lvl) {
    node->condition()->accept(this, lvl + 4);
    node->block()->accept(this, lvl + 4);
}

void til::type_checker::do_if_else_node(til::if_else_node *const node,
                                        int lvl) {
    node->condition()->accept(this, lvl + 4);
    node->thenblock()->accept(this, lvl + 4);
    node->elseblock()->accept(this, lvl + 4);
}

//---------------------------------------------------------------------------

void til::type_checker::do_loop_node(til::loop_node *const node, int lvl) {
    node->condition()->accept(this, lvl + 4);
}

void til::type_checker::do_next_node(til::next_node *const node, int lvl) {
    // EMPTY
}

void til::type_checker::do_stop_node(til::stop_node *const node, int lvl) {
    // EMPTY
}

//---------------------------------------------------------------------------

void til::type_checker::do_sizeof_node(til::sizeof_node *const node, int lvl) {
    ASSERT_UNSPEC;

    node->argument()->accept(this, lvl + 2);
    if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
        node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    }

    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_alloc_node(til::alloc_node *const node, int lvl) {
    ASSERT_UNSPEC;

    node->argument()->accept(this, lvl + 2);
    if (!node->argument()->is_typed(cdk::TYPE_INT)) {
        throw std::string("wrong type in argument of alloc expression");
    }

    node->type(cdk::reference_type::create(
        4, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC)));
}

void til::type_checker::do_address_of_node(til::address_of_node *const node,
                                           int lvl) {
    ASSERT_UNSPEC;
    node->lvalue()->accept(this, lvl + 2);
    node->type(cdk::reference_type::create(4, node->lvalue()->type()));
}
