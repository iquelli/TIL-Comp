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
        !check_compatible_types(t1->output(0), t2->output(0))) {
        return false;
    }

    // the number of arguments must be the same
    if (t1->input_length() != t2->input_length()) {
        return false;
    }

    // the types of the arguments must be compatible
    for (size_t i = 0; i < t1->input_length(); ++i) {
        if (!check_compatible_types(t1->input(i), t2->input(i))) {
            return false;
        }
    }
    return true;
}

bool til::type_checker::check_compatible_types(
    std::shared_ptr<cdk::basic_type> t1, std::shared_ptr<cdk::basic_type> t2) {
    const auto t1_name = t1->name();
    const auto t2_name = t2->name();

    if (t1_name == cdk::TYPE_INT || t1_name == cdk::TYPE_DOUBLE) {
        return t2_name == cdk::TYPE_DOUBLE || t2_name == cdk::TYPE_INT;
    } else if (t1_name == cdk::TYPE_STRING) {
        return t2_name == cdk::TYPE_STRING;
    } else if (t1_name == cdk::TYPE_POINTER) {
        return t2_name == cdk::TYPE_POINTER ||
               check_compatible_ptr_types(t1, t2);
    } else if (t1_name == cdk::TYPE_FUNCTIONAL) {
        return (t2_name == cdk::TYPE_FUNCTIONAL &&
                check_compatible_functional_types(
                    cdk::functional_type::cast(t1),
                    cdk::functional_type::cast(t2))) ||
               (t2_name == cdk::TYPE_POINTER &&
                cdk::reference_type::cast(t2)->referenced() == nullptr);
    } else if (t1_name == cdk::TYPE_UNSPEC) { // useful for var cases
        // (var x (f)), where f calls return void, is not allowed
        return t2_name != cdk::TYPE_VOID;
    } else {
        return t1_name == t2_name;
    }
}

void til::type_checker::change_type_on_match(cdk::typed_node *const lvalue,
                                             cdk::typed_node *const rvalue) {
    const auto lval_type = lvalue->type();
    const auto rval_type = rvalue->type();

    if (lval_type->name() == cdk::TYPE_UNSPEC &&
        rval_type->name() == cdk::TYPE_UNSPEC) {
        // assign default type
        lvalue->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        rvalue->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if ((lval_type->name() == cdk::TYPE_POINTER &&
                rval_type->name() == cdk::TYPE_POINTER &&
                check_compatible_ptr_types(lval_type, rval_type)) ||
               (lval_type->name() == cdk::TYPE_FUNCTIONAL &&
                rval_type->name() == cdk::TYPE_FUNCTIONAL &&
                check_compatible_functional_types(
                    cdk::functional_type::cast(lval_type),
                    cdk::functional_type::cast(rval_type))) ||
               ((lval_type->name() == cdk::TYPE_INT ||
                 lval_type->name() == cdk::TYPE_DOUBLE) &&
                rval_type->name() == cdk::TYPE_UNSPEC)) {
        rvalue->type(lval_type);
    }
}

void til::type_checker::throw_incompatible_types(
    std::shared_ptr<cdk::basic_type> t1, std::shared_ptr<cdk::basic_type> t2) {
    if (check_compatible_types(t1, t2)) {
        return;
    }

    if (t1->name() == cdk::TYPE_INT || t1->name() == cdk::TYPE_DOUBLE) {
        throw std::string("wrong type - expected double or int");
    } else if (t1->name() == cdk::TYPE_STRING) {
        throw std::string("wrong type - expected string");
    } else if (t1->name() == cdk::TYPE_POINTER) {
        throw std::string("wrong type - expected pointer");
    } else if (t1->name() == cdk::TYPE_FUNCTIONAL) {
        throw std::string("wrong type - expected function");
    } else {
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

    // TODO: fix read being unspec here
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

    if (!process_binary_expr(node, lvl)) {
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

    // TODO: fix read being unspec here
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

    // TODO: fix read being unspec here
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
        throw std::string("undeclared variable '" + id + "'");
    }
}

void til::type_checker::do_index_node(til::index_node *const node, int lvl) {
    ASSERT_UNSPEC;

    node->ptr()->accept(this, lvl + 2);
    if (!node->ptr()->is_typed(cdk::TYPE_POINTER)) {
        throw std::string("wrong type in ptr of index expression");
    }

    node->index()->accept(this, lvl + 2);
    if (node->index()->is_typed(cdk::TYPE_UNSPEC)) {
        node->index()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (!node->index()->is_typed(cdk::TYPE_INT)) {
        throw std::string("wrong type in index of index expression");
    }

    const auto ptr_ref =
        cdk::reference_type::cast(node->ptr()->type())->referenced();
    node->type(ptr_ref);
}

void til::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
    ASSERT_UNSPEC;
    node->lvalue()->accept(this, lvl);
    node->type(node->lvalue()->type());
}

void til::type_checker::do_assignment_node(cdk::assignment_node *const node,
                                           int lvl) {
    ASSERT_UNSPEC;

    node->lvalue()->accept(this, lvl + 2);
    node->rvalue()->accept(this, lvl + 2);

    // TODO: fix read being unspec here
    change_type_on_match(node->lvalue(), node->rvalue());
    const auto lval_type = node->lvalue()->type();
    const auto rval_type = node->rvalue()->type();

    throw_incompatible_types(lval_type, rval_type);
    node->type(lval_type);
}

//---------------------------------------------------------------------------

void til::type_checker::do_declaration_node(til::declaration_node *const node,
                                            int lvl) {
    // TODO: fix read being unspec here
    const auto &init = node->initializer();
    if (init) {
        init->accept(this, lvl + 2);
        if (node->type()) {
            change_type_on_match(node, init);
            throw_incompatible_types(node->type(), init->type());
            if (node->type()->name() == cdk::TYPE_UNSPEC) {
                node->type(init->type());
            }
        } else {
            node->type(init->type());
        }
    }

    const auto new_symbol =
        til::make_symbol(node->type(), node->identifier(),
                         (bool)node->initializer(), node->qualifier());
    if (!_symtab.insert(node->identifier(), new_symbol)) {
        // in this case, we are redeclaring a variable
        const auto previous_symbol = _symtab.find_local(node->identifier());
        // the redeclared type must be the exact same
        if (previous_symbol->type()->name() != node->type()->name()) {
            throw std::string("cannot redeclare variable '" +
                              node->identifier() + "' with incompatible type");
        }
        _symtab.replace(node->identifier(), new_symbol);
    }
    _parent->set_new_symbol(new_symbol);
}

void til::type_checker::do_function_node(til::function_node *const node,
                                         int lvl) {
    // EMPTY: type of function_node is already set in its constructor
}

void til::type_checker::do_function_call_node(
    til::function_call_node *const node, int lvl) {
    ASSERT_UNSPEC;
    std::vector<std::shared_ptr<cdk::basic_type>> args_types;

    if (node->func()) { // regular call
        node->func()->accept(this, lvl + 2);
        if (!node->func()->is_typed(cdk::TYPE_FUNCTIONAL)) {
            throw std::string("wrong type in function call expression");
        }

        const auto &type = node->func()->type();
        args_types = cdk::functional_type::cast(type)->input()->components();
        node->type(cdk::functional_type::cast(type)->output(0));
    } else { // recursive call (@)
        auto symbol = _symtab.find("@");
        if (!symbol) {
            throw std::string(
                "recursive call not allowed in the current scope");
        }
        const auto &type = symbol->type();
        args_types = cdk::functional_type::cast(type)->input()->components();
        node->type(cdk::functional_type::cast(type)->output(0));
    }

    if (node->arguments()) {
        if (args_types.size() != node->arguments()->size()) {
            throw std::string(
                "wrong number of arguments in function call expression");
        }
        node->arguments()->accept(this, lvl + 2);

        for (size_t i = 0; i < args_types.size(); ++i) {
            // TODO: fix read being unspec here
            const auto &param_type =
                dynamic_cast<cdk::expression_node *>(node->arguments()->node(i))
                    ->type();
            // note that the second condition is to allow passing an int as a
            // double
            if ((args_types[i] == param_type) ||
                (args_types[i]->name() == cdk::TYPE_DOUBLE &&
                 param_type->name() == cdk::TYPE_INT)) {
                continue;
            }
            throw std::string(
                "wrong type in argument of function call expression");
        }
    }
}

void til::type_checker::do_return_node(til::return_node *const node, int lvl) {
    const auto function = _symtab.find("@");
    const auto ret_val = node->retval();
    if (!function) { // we may be in main
        const auto main = _symtab.find("_main");
        if (main) {
            if (!ret_val) {
                throw std::string(
                    "wrong type of return value in main (int expected)");
            }
            ret_val->accept(this, lvl + 2);
            if (!ret_val->is_typed(cdk::TYPE_INT)) {
                throw std::string(
                    "wrong type of return value in main (int expected)");
            }
            return;
        }
        throw std::string("return statement found outside function");
    } else if (!ret_val) {
        return;
    }

    const auto &fun_sym_type = cdk::functional_type::cast(function->type());
    const auto function_output = fun_sym_type->output(0);
    const bool has_output = fun_sym_type->output() != nullptr;
    if (has_output && function_output->name() == cdk::TYPE_VOID) {
        throw std::string("return with a value in void function");
    } else if (!has_output) {
        throw std::string("unknown return type in function");
    }

    // TODO: fix read being unspec here
    ret_val->accept(this, lvl + 2);
    throw_incompatible_types(function_output, ret_val->type());
}

//---------------------------------------------------------------------------

void til::type_checker::do_evaluation_node(til::evaluation_node *const node,
                                           int lvl) {
    node->argument()->accept(this, lvl + 2);
    if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
        node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    }
}

//---------------------------------------------------------------------------

void til::type_checker::do_print_node(til::print_node *const node, int lvl) {
    // TODO: fix read being unspec here
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
    if (node->condition()->is_typed(cdk::TYPE_UNSPEC)) {
        node->condition()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (!node->condition()->is_typed(cdk::TYPE_INT)) {
        throw std::string("wrong type in condition of if instruction");
    }

    node->block()->accept(this, lvl + 4);
}

void til::type_checker::do_if_else_node(til::if_else_node *const node,
                                        int lvl) {
    node->condition()->accept(this, lvl + 4);
    if (node->condition()->is_typed(cdk::TYPE_UNSPEC)) {
        node->condition()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (!node->condition()->is_typed(cdk::TYPE_INT)) {
        throw std::string("wrong type in condition of if else instruction");
    }

    node->thenblock()->accept(this, lvl + 4);
    node->elseblock()->accept(this, lvl + 4);
}

//---------------------------------------------------------------------------

void til::type_checker::do_loop_node(til::loop_node *const node, int lvl) {
    node->condition()->accept(this, lvl + 4);
    if (node->condition()->is_typed(cdk::TYPE_UNSPEC)) {
        node->condition()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (!node->condition()->is_typed(cdk::TYPE_INT)) {
        throw std::string("wrong type in condition of loop instruction");
    }
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
    if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
        node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (!node->argument()->is_typed(cdk::TYPE_INT)) {
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
