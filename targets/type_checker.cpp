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

bool til::type_checker::check_compatible_functional_types(
    std::shared_ptr<cdk::functional_type> t1,
    std::shared_ptr<cdk::functional_type> t2, bool cov) {
    // The number of arguments and outputs must be the same
    if (t1->input_length() != t2->input_length() ||
        t1->output_length() != t2->output_length()) {
        return false;
    }

    // The return type must be compatible
    for (size_t i = 0; i < t1->output_length(); ++i) {
        if (!check_compatible_types(t1->output(i), t2->output(i), cov)) {
            return false;
        }
    }

    // The types of the arguments must be compatible
    for (size_t i = 0; i < t1->input_length(); ++i) {
        if (!check_compatible_types(t2->input(i), t1->input(i), cov)) {
            return false;
        }
    }

    return true;
}

bool til::type_checker::check_compatible_types(
    std::shared_ptr<cdk::basic_type> t1, std::shared_ptr<cdk::basic_type> t2,
    bool cov) {
    const auto t1_name = t1->name();
    const auto t2_name = t2->name();

    if (cov && t1_name == cdk::TYPE_DOUBLE) {
        return t2_name == cdk::TYPE_DOUBLE || t2_name == cdk::TYPE_INT;
    } else if (t1_name == cdk::TYPE_STRING) {
        return t2_name == cdk::TYPE_STRING;
    } else if (t1_name == cdk::TYPE_POINTER) {
        return t2_name == cdk::TYPE_POINTER &&
               (check_compatible_types(
                    cdk::reference_type::cast(t1)->referenced(),
                    cdk::reference_type::cast(t2)->referenced(), false) ||
                cdk::reference_type::cast(t2)->referenced()->name() ==
                    cdk::TYPE_UNSPEC);
    } else if (t1_name == cdk::TYPE_FUNCTIONAL) {
        return t2_name == cdk::TYPE_FUNCTIONAL &&
               check_compatible_functional_types(cdk::functional_type::cast(t1),
                                                 cdk::functional_type::cast(t2),
                                                 cov);
    } else {
        return t1_name == t2_name;
    }
}

void til::type_checker::change_type_on_match(
    std::shared_ptr<cdk::basic_type> lval_type, cdk::typed_node *const rvalue) {
    const auto rval_type = rvalue->type();

    if (lval_type->name() == cdk::TYPE_POINTER &&
        rval_type->name() == cdk::TYPE_POINTER) {
        auto lval_ref = cdk::reference_type::cast(lval_type);
        auto rval_ref = cdk::reference_type::cast(rval_type);
        if (rval_ref->referenced()->name() == cdk::TYPE_UNSPEC ||
            rval_ref->referenced()->name() == cdk::TYPE_VOID ||
            lval_ref->referenced()->name() == cdk::TYPE_VOID) {
            rvalue->type(lval_type);
        }
    }

    if ((lval_type->name() == cdk::TYPE_FUNCTIONAL &&
         rval_type->name() == cdk::TYPE_FUNCTIONAL &&
         check_compatible_functional_types(
             cdk::functional_type::cast(lval_type),
             cdk::functional_type::cast(rval_type), true)) ||
        ((lval_type->name() == cdk::TYPE_INT ||
          lval_type->name() == cdk::TYPE_DOUBLE) &&
         rval_type->name() == cdk::TYPE_UNSPEC)) {
        rvalue->type(lval_type);
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
    for (size_t i = 0; i < node->size(); ++i) {
        node->node(i)->accept(this, lvl);
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
        4, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC)));
}

//---------------------------------------------------------------------------

void til::type_checker::process_unary_expr(
    cdk::unary_operation_node *const node, int lvl, bool accept_doubles) {
    ASSERT_UNSPEC;

    node->argument()->accept(this, lvl);
    if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
        node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (!node->argument()->is_typed(cdk::TYPE_INT) &&
               !(accept_doubles &&
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
    node->left()->accept(this, lvl);
    node->right()->accept(this, lvl);

    // Check types and determine result type
    if (node->left()->is_typed(cdk::TYPE_INT) ||
        node->left()->is_typed(cdk::TYPE_UNSPEC)) {
        if (node->right()->is_typed(cdk::TYPE_INT)) {
            node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        } else if (node->right()->is_typed(cdk::TYPE_DOUBLE)) {
            node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
        } else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
            node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
            node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        } else {
            return false;
        }
        if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
            node->left()->type(node->type());
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

bool til::type_checker::process_binary_integer_expr(
    cdk::binary_operation_node *const node, int lvl) {
    node->left()->accept(this, lvl);
    if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
        node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (!node->left()->is_typed(cdk::TYPE_INT)) {
        return false;
    }

    node->right()->accept(this, lvl);
    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
        node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (!node->right()->is_typed(cdk::TYPE_INT)) {
        return false;
    }

    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    return true;
}

// Additive Binary

void til::type_checker::process_additive_expr(
    cdk::binary_operation_node *const node, int lvl, bool is_subtraction) {
    ASSERT_UNSPEC;

    if (process_binary_expr(node, lvl)) {
        return;
    }

    if (node->left()->is_typed(cdk::TYPE_POINTER) &&
        (node->right()->is_typed(cdk::TYPE_INT) ||
         node->right()->is_typed(cdk::TYPE_UNSPEC))) {
        node->type(node->left()->type());
        node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if ((node->left()->is_typed(cdk::TYPE_INT) ||
                node->left()->is_typed(cdk::TYPE_UNSPEC)) &&
               node->right()->is_typed(cdk::TYPE_POINTER)) {
        node->type(node->right()->type());
        node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (is_subtraction && node->left()->is_typed(cdk::TYPE_POINTER) &&
               node->right()->is_typed(cdk::TYPE_POINTER) &&
               check_compatible_types(node->left()->type(),
                                      node->right()->type(), false)) {
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
    if (!process_binary_integer_expr(node, lvl)) {
        throw std::string("wrong type in mod expression");
    }
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

    node->left()->accept(this, lvl);
    node->right()->accept(this, lvl);
    if (!(process_binary_expr(node, lvl) ||
          (node->left()->is_typed(cdk::TYPE_POINTER) &&
           node->right()->is_typed(cdk::TYPE_POINTER) &&
           check_compatible_types(node->left()->type(), node->right()->type(),
                                  false)))) {
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

void til::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
    ASSERT_UNSPEC;
    if (!process_binary_integer_expr(node, lvl)) {
        throw std::string("wrong type in and expression");
    }
}
void til::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
    ASSERT_UNSPEC;
    if (!process_binary_integer_expr(node, lvl)) {
        throw std::string("wrong type in or expression");
    }
}

//---------------------------------------------------------------------------

void til::type_checker::do_variable_node(cdk::variable_node *const node,
                                         int lvl) {
    ASSERT_UNSPEC;
    const std::string &id = node->name();
    std::shared_ptr<til::symbol> symbol = _symtab.find(id);

    if (symbol == nullptr) {
        throw std::string("undeclared variable '" + id + "'");
    }
    node->type(symbol->type());
}

void til::type_checker::do_index_node(til::index_node *const node, int lvl) {
    ASSERT_UNSPEC;

    node->ptr()->accept(this, lvl);
    if (!node->ptr()->is_typed(cdk::TYPE_POINTER)) {
        throw std::string("wrong type in ptr of index expression");
    }

    node->index()->accept(this, lvl);
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

    node->lvalue()->accept(this, lvl);
    node->rvalue()->accept(this, lvl);
    change_type_on_match(node->lvalue()->type(), node->rvalue());

    const auto lval_type = node->lvalue()->type();
    const auto rval_type = node->rvalue()->type();
    if (!check_compatible_types(lval_type, rval_type, true)) {
        throw std::string("wrong types in assignment expression");
    }
    node->type(lval_type);
}

//---------------------------------------------------------------------------

void til::type_checker::do_declaration_node(til::declaration_node *const node,
                                            int lvl) {
    const auto &init = node->initializer();
    if (init) {
        init->accept(this, lvl);
        if (node->type()) { // non-var
            change_type_on_match(node->type(), init);
            if (!check_compatible_types(node->type(), init->type(), true)) {
                throw std::string("wrong type on right side of declaration");
            }
        } else { // var
            if (init->is_typed(cdk::TYPE_UNSPEC)) {
                // assign default type
                init->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
            } else if (init->is_typed(cdk::TYPE_VOID)) {
                // (var x (f)), where f calls return void, is not allowed
                throw std::string("cannot declare a void variable");
            } else if (init->is_typed(cdk::TYPE_POINTER)) {
                auto ref = cdk::reference_type::cast(init->type());
                if (ref->referenced()->name() == cdk::TYPE_UNSPEC) {
                    init->type(cdk::reference_type::create(
                        4, cdk::primitive_type::create(4, cdk::TYPE_INT)));
                }
            }
            node->type(init->type());
        }
    }

    if (node->qualifier() == tEXTERNAL &&
        !node->is_typed(cdk::TYPE_FUNCTIONAL)) {
        throw std::string("non-function '" + node->identifier() +
                          "' declared as external");
    }

    const auto new_symbol =
        til::make_symbol(node->type(), node->identifier(),
                         (bool)node->initializer(), node->qualifier());
    if (!_symtab.insert(node->identifier(), new_symbol)) {
        const auto prev_symbol = _symtab.find(node->identifier());
        if (prev_symbol->qualifier() != tFORWARD ||
            !check_compatible_types(prev_symbol->type(), node->type(), false)) {
            throw std::string("redeclaration of variable '" +
                              node->identifier() + "'");
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
        node->func()->accept(this, lvl);
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

    if (args_types.size() != node->arguments()->size()) {
        throw std::string(
            "wrong number of arguments in function call expression");
    }
    node->arguments()->accept(this, lvl);
    for (size_t i = 0; i < args_types.size(); ++i) {
        const auto &arg =
            dynamic_cast<cdk::expression_node *>(node->arguments()->node(i));
        change_type_on_match(args_types[i], arg);
        if (!check_compatible_types(args_types[i], arg->type(), true)) {
            throw std::string("wrong type in argument " +
                              std::to_string(i + 1) +
                              " of function call expression");
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
            ret_val->accept(this, lvl);
            if (!ret_val->is_typed(cdk::TYPE_INT)) {
                throw std::string(
                    "wrong type of return value in main (int expected)");
            }
            return;
        }
        throw std::string("return statement found outside function");
    }

    const auto &fun_sym_type = cdk::functional_type::cast(function->type());
    const auto function_output = fun_sym_type->output(0);
    if (!ret_val) {
        if (fun_sym_type->output() &&
            function_output->name() != cdk::TYPE_VOID) {
            throw std::string("no return with a value in non-void function");
        }
        return;
    }
    if (fun_sym_type->output() && function_output->name() == cdk::TYPE_VOID) {
        throw std::string("return with a value in void function");
    } else if (!fun_sym_type->output()) {
        throw std::string("unknown return type in function");
    }

    ret_val->accept(this, lvl);
    change_type_on_match(function_output, ret_val);
    if (!check_compatible_types(function_output, ret_val->type(), true)) {
        throw std::string("wrong type in return value of return expression");
    }
}

//---------------------------------------------------------------------------

void til::type_checker::do_evaluation_node(til::evaluation_node *const node,
                                           int lvl) {
    node->argument()->accept(this, lvl);
    if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
        node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    }
}

//---------------------------------------------------------------------------

void til::type_checker::do_print_node(til::print_node *const node, int lvl) {
    for (size_t i = 0; i < node->arguments()->size(); ++i) {
        auto child =
            dynamic_cast<cdk::expression_node *>(node->arguments()->node(i));
        child->accept(this, lvl);
        if (child->is_typed(cdk::TYPE_UNSPEC)) {
            child->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        } else if (!child->is_typed(cdk::TYPE_INT) &&
                   !child->is_typed(cdk::TYPE_DOUBLE) &&
                   !child->is_typed(cdk::TYPE_STRING)) {
            throw std::string("wrong type for argument number " +
                              std::to_string(i + 1) + " of print instruction");
        }
    }
}

void til::type_checker::do_read_node(til::read_node *const node, int lvl) {
    ASSERT_UNSPEC; // if it already has a value, don't overwrite it

    node->type(cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
}

//---------------------------------------------------------------------------

void til::type_checker::do_if_node(til::if_node *const node, int lvl) {
    node->condition()->accept(this, lvl);
    if (node->condition()->is_typed(cdk::TYPE_UNSPEC)) {
        node->condition()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (!node->condition()->is_typed(cdk::TYPE_INT)) {
        throw std::string("wrong type in condition of if instruction");
    }

    node->block()->accept(this, lvl);
}

void til::type_checker::do_if_else_node(til::if_else_node *const node,
                                        int lvl) {
    node->condition()->accept(this, lvl);
    if (node->condition()->is_typed(cdk::TYPE_UNSPEC)) {
        node->condition()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (!node->condition()->is_typed(cdk::TYPE_INT)) {
        throw std::string("wrong type in condition of if else instruction");
    }

    node->thenblock()->accept(this, lvl);
    node->elseblock()->accept(this, lvl);
}

//---------------------------------------------------------------------------

void til::type_checker::do_loop_node(til::loop_node *const node, int lvl) {
    node->condition()->accept(this, lvl);
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

    node->argument()->accept(this, lvl);
    if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
        node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    }

    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_alloc_node(til::alloc_node *const node, int lvl) {
    ASSERT_UNSPEC;

    node->argument()->accept(this, lvl);
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
    node->lvalue()->accept(this, lvl);
    if (node->lvalue()->is_typed(cdk::TYPE_POINTER)) {
        auto ref = cdk::reference_type::cast(node->lvalue()->type());
        if (ref->referenced()->name() == cdk::TYPE_VOID) {
            // void!!! is the same as void!! and void!
            node->type(node->lvalue()->type());
            return;
        }
    }
    node->type(cdk::reference_type::create(4, node->lvalue()->type()));
}
