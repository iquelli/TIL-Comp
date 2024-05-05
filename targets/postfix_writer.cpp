#include "targets/postfix_writer.h"
#include ".auto/all_nodes.h" // all_nodes.h is automatically generated
#include "targets/frame_size_calculator.h"
#include "targets/type_checker.h"
#include <sstream>
#include <string>

#include "til_parser.tab.h"

//---------------------------------------------------------------------------

void til::postfix_writer::do_nil_node(cdk::nil_node *const node, int lvl) {
    // EMPTY
}

void til::postfix_writer::do_data_node(cdk::data_node *const node, int lvl) {
    // EMPTY
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_sequence_node(cdk::sequence_node *const node,
                                           int lvl) {
    for (size_t i = 0; i < node->size(); ++i) {
        node->node(i)->accept(this, lvl);
    }
}

void til::postfix_writer::do_block_node(til::block_node *const node, int lvl) {
    _symtab.push(); // for variables in the block

    if (node->declarations()) {
        node->declarations()->accept(this, lvl + 2);
    }

    _lastBlockInstrSeen = false;
    for (size_t i = 0; i < node->instructions()->size(); ++i) {
        if (_lastBlockInstrSeen) {
            error(node->lineno(), "unreachable code");
            return;
        }
        node->instructions()->node(i)->accept(this, lvl + 2);
    }
    _lastBlockInstrSeen = false;

    _symtab.pop();
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_integer_node(cdk::integer_node *const node,
                                          int lvl) {
    if (_inFunctionBody) {
        _pf.INT(node->value());
    } else {
        _pf.SINT(node->value());
    }
}

void til::postfix_writer::do_double_node(cdk::double_node *const node,
                                         int lvl) {
    if (_inFunctionBody) {
        _pf.DOUBLE(node->value());
    } else {
        _pf.SDOUBLE(node->value());
    }
}

void til::postfix_writer::do_string_node(cdk::string_node *const node,
                                         int lvl) {
    const auto lbl = mklbl(++_lbl);

    /* generate the string */
    _pf.RODATA();               // strings are DATA readonly
    _pf.ALIGN();                // make sure we are aligned
    _pf.LABEL(lbl);             // give the string a name
    _pf.SSTRING(node->value()); // output string characters

    if (_inFunctionBody) {
        // local variable initializer
        _pf.TEXT(_functionLabels.back()); // return to the TEXT segment
        _pf.ADDR(lbl);                    // the string to be printed
    } else {
        // global variable initializer
        _pf.DATA();     // return to the DATA segment
        _pf.SADDR(lbl); // the string to be printed
    }
}

void til::postfix_writer::do_null_node(til::null_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    // 0 in the stack for null
    if (_inFunctionBody) {
        _pf.INT(0);
    } else {
        _pf.SINT(0);
    }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_unary_minus_node(cdk::unary_minus_node *const node,
                                              int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    node->argument()->accept(this, lvl + 2); // determine the value
    _pf.NEG();                               // 2-complement
}

void til::postfix_writer::do_unary_plus_node(cdk::unary_plus_node *const node,
                                             int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    node->argument()->accept(this, lvl + 2); // determine the value
}

void til::postfix_writer::do_not_node(cdk::not_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    // compare the value of the node with false
    node->argument()->accept(this, lvl + 2);
    _pf.INT(0);
    // check if the two values on the stack are the same
    _pf.EQ();
}

//---------------------------------------------------------------------------

// Additive Binary

void til::postfix_writer::process_additive_expr(
    cdk::binary_operation_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    node->left()->accept(this, lvl + 2);
    if (node->is_typed(cdk::TYPE_DOUBLE) &&
        !node->left()->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.I2D();
    } else if (node->is_typed(cdk::TYPE_POINTER) &&
               !node->left()->is_typed(cdk::TYPE_POINTER)) {
        const auto ref_right =
            cdk::reference_type::cast(node->right()->type())->referenced();
        // void size should be 1 for pointer arithmetic
        _pf.INT(std::max(1, static_cast<int>(ref_right->size())));
        _pf.MUL();
    }

    node->right()->accept(this, lvl + 2);
    if (node->is_typed(cdk::TYPE_DOUBLE) &&
        !node->right()->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.I2D();
    } else if (node->is_typed(cdk::TYPE_POINTER) &&
               !node->right()->is_typed(cdk::TYPE_POINTER)) {
        const auto ref_left =
            cdk::reference_type::cast(node->left()->type())->referenced();
        // void size should be 1 for pointer arithmetic
        _pf.INT(std::max(1, static_cast<int>(ref_left->size())));
        _pf.MUL();
    }
}

void til::postfix_writer::do_add_node(cdk::add_node *const node, int lvl) {
    process_additive_expr(node, lvl);

    if (!node->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.ADD();
    } else {
        _pf.DADD();
    }
}
void til::postfix_writer::do_sub_node(cdk::sub_node *const node, int lvl) {
    process_additive_expr(node, lvl);

    if (node->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.DSUB();
        return;
    }

    _pf.SUB();

    // if it's a pointer we need to a special case
    if (node->left()->is_typed(cdk::TYPE_POINTER) &&
        node->right()->is_typed(cdk::TYPE_POINTER) &&
        cdk::reference_type::cast(node->left()->type())->referenced()->name() !=
            cdk::TYPE_VOID) {
        _pf.INT(cdk::reference_type::cast(node->left()->type())
                    ->referenced()
                    ->size());
        _pf.DIV();
    }
}

// Multiplicative Binary

void til::postfix_writer::process_multiplicative_expr(
    cdk::binary_operation_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    node->left()->accept(this, lvl + 2);
    if (node->is_typed(cdk::TYPE_DOUBLE) &&
        !node->left()->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.I2D();
    }

    node->right()->accept(this, lvl + 2);
    if (node->is_typed(cdk::TYPE_DOUBLE) &&
        !node->right()->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.I2D();
    }
}

void til::postfix_writer::do_mul_node(cdk::mul_node *const node, int lvl) {
    process_multiplicative_expr(node, lvl);

    if (!node->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.MUL();
    } else {
        _pf.DMUL();
    }
}
void til::postfix_writer::do_div_node(cdk::div_node *const node, int lvl) {
    process_multiplicative_expr(node, lvl);

    if (!node->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.DIV();
    } else {
        _pf.DDIV();
    }
}
void til::postfix_writer::do_mod_node(cdk::mod_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    node->left()->accept(this, lvl);
    node->right()->accept(this, lvl);
    _pf.MOD();
}

// Comparison Binary

void til::postfix_writer::process_comparison_expr(
    cdk::binary_operation_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    node->left()->accept(this, lvl + 2);
    if (!node->left()->is_typed(cdk::TYPE_DOUBLE) &&
        node->right()->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.I2D();
    }

    node->right()->accept(this, lvl + 2);
    if (!node->right()->is_typed(cdk::TYPE_DOUBLE) &&
        node->left()->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.I2D();
    }

    if (node->left()->is_typed(cdk::TYPE_DOUBLE) ||
        node->right()->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.DCMP();
        _pf.INT(0);
    }
}

void til::postfix_writer::do_lt_node(cdk::lt_node *const node, int lvl) {
    process_comparison_expr(node, lvl);
    _pf.LT();
}
void til::postfix_writer::do_le_node(cdk::le_node *const node, int lvl) {
    process_comparison_expr(node, lvl);
    _pf.LE();
}
void til::postfix_writer::do_ge_node(cdk::ge_node *const node, int lvl) {
    process_comparison_expr(node, lvl);
    _pf.GE();
}
void til::postfix_writer::do_gt_node(cdk::gt_node *const node, int lvl) {
    process_comparison_expr(node, lvl);
    _pf.GT();
}
void til::postfix_writer::do_ne_node(cdk::ne_node *const node, int lvl) {
    process_comparison_expr(node, lvl);
    _pf.NE();
}
void til::postfix_writer::do_eq_node(cdk::eq_node *const node, int lvl) {
    process_comparison_expr(node, lvl);
    _pf.EQ();
}

// Logical Binary

void til::postfix_writer::do_and_node(cdk::and_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    const auto lbl = mklbl(++_lbl);

    node->left()->accept(this, lvl + 2);
    _pf.DUP32();
    _pf.JZ(lbl);

    node->right()->accept(this, lvl + 2);
    _pf.AND();
    _pf.ALIGN();
    _pf.LABEL(lbl);
}
void til::postfix_writer::do_or_node(cdk::or_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    const auto lbl = mklbl(++_lbl);

    node->left()->accept(this, lvl + 2);
    _pf.DUP32();
    _pf.JNZ(lbl);

    node->right()->accept(this, lvl + 2);
    _pf.OR();
    _pf.ALIGN();
    _pf.LABEL(lbl);
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_variable_node(cdk::variable_node *const node,
                                           int lvl) {
    // TODO
    ASSERT_SAFE_EXPRESSIONS;
    // // simplified generation: all variables are global
    // _pf.ADDR(node->name());
}

void til::postfix_writer::do_index_node(til::index_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    node->ptr()->accept(this, lvl);
    node->index()->accept(this, lvl);
    _pf.INT(node->type()->size()); // type size
    _pf.MUL();                     // type size * index
    _pf.ADD();                     // ptr + (type size * index)
}

void til::postfix_writer::do_rvalue_node(cdk::rvalue_node *const node,
                                         int lvl) {
    // TODO
    ASSERT_SAFE_EXPRESSIONS;
    // node->lvalue()->accept(this, lvl);
    // _pf.LDINT(); // depends on type size
}

void til::postfix_writer::do_assignment_node(cdk::assignment_node *const node,
                                             int lvl) {
    // TODO
    ASSERT_SAFE_EXPRESSIONS;
    // node->rvalue()->accept(this, lvl); // determine the new value
    // _pf.DUP32();
    // if (new_symbol() == nullptr) {
    //     node->lvalue()->accept(this, lvl); // where to store the value
    // } else {
    //     _pf.DATA();  // variables are all global and live in DATA
    //     _pf.ALIGN(); // make sure we are aligned
    //     _pf.LABEL(new_symbol()->name()); // name variable location
    //     reset_new_symbol();
    //     _pf.SINT(0);                       // initialize it to 0 (zero)
    //     _pf.TEXT();                        // return to the TEXT segment
    //     node->lvalue()->accept(this, lvl); // DAVID: bah!
    // }
    // _pf.STINT(); // store the value at address
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_declaration_node(til::declaration_node *const node,
                                              int lvl) {
    // TODO
    ASSERT_SAFE_EXPRESSIONS;
}

void til::postfix_writer::do_function_node(til::function_node *const node,
                                           int lvl) {
    // TODO
    // Note that Simple doesn't have functions. Thus, it doesn't need
    // a function node. However, it must start in the main function.
    // The ProgramNode (representing the whole program) doubles as a
    // main function node.

    // generate the main function (RTS mandates that its name be "_main")
    // _pf.TEXT();
    // _pf.ALIGN();
    // _pf.GLOBAL("_main", _pf.FUNC());
    // _pf.LABEL("_main");
    // _pf.ENTER(0); // Simple doesn't implement local variables

    // node->statements()->accept(this, lvl);

    // end the main function
    // _pf.INT(0);
    // _pf.STFVAL32();
    // _pf.LEAVE();
    // _pf.RET();

    // these are just a few library function imports
    // _pf.EXTERN("readi");
    // _pf.EXTERN("printi");
    // _pf.EXTERN("prints");
    // _pf.EXTERN("println");
}

void til::postfix_writer::do_function_call_node(
    til::function_call_node *const node, int lvl) {
    // TODO
    ASSERT_SAFE_EXPRESSIONS;
}

void til::postfix_writer::do_return_node(til::return_node *const node,
                                         int lvl) {
    // TODO
    ASSERT_SAFE_EXPRESSIONS;
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_evaluation_node(til::evaluation_node *const node,
                                             int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    node->argument()->accept(this, lvl + 2);     // determine the value
    _pf.TRASH(node->argument()->type()->size()); // delete it
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_print_node(til::print_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    for (size_t i = 0; i < node->arguments()->size(); ++i) {
        auto child =
            dynamic_cast<cdk::expression_node *>(node->arguments()->node(i));

        child->accept(this, lvl); // determine the value to print
        if (child->is_typed(cdk::TYPE_INT)) {
            _functionsToDeclare.insert("printi");
            _pf.CALL("printi");
            _pf.TRASH(4); // delete the printed value
        } else if (child->is_typed(cdk::TYPE_DOUBLE)) {
            _functionsToDeclare.insert("printd");
            _pf.CALL("printd");
            _pf.TRASH(8); // delete the printed value
        } else if (child->is_typed(cdk::TYPE_STRING)) {
            _functionsToDeclare.insert("prints");
            _pf.CALL("prints");
            _pf.TRASH(4); // delete the printed value's address
        } else {
            error(node->lineno(), "print expression cannot print unknown type");
        }
    }

    if (node->newline()) {
        _functionsToDeclare.insert("println");
        _pf.CALL("println");
    }
}

void til::postfix_writer::do_read_node(til::read_node *const node, int lvl) {
    // TODO
    ASSERT_SAFE_EXPRESSIONS;
    // _pf.CALL("readi");
    // _pf.LDFVAL32();
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_if_node(til::if_node *const node, int lvl) {
    // TODO
    ASSERT_SAFE_EXPRESSIONS;
    // int lbl1;
    // node->condition()->accept(this, lvl);
    // _pf.JZ(mklbl(lbl1 = ++_lbl));
    // node->block()->accept(this, lvl + 2);
    // _pf.LABEL(mklbl(lbl1));
}

void til::postfix_writer::do_if_else_node(til::if_else_node *const node,
                                          int lvl) {
    // TODO
    ASSERT_SAFE_EXPRESSIONS;
    // int lbl1, lbl2;
    // node->condition()->accept(this, lvl);
    // _pf.JZ(mklbl(lbl1 = ++_lbl));
    // node->thenblock()->accept(this, lvl + 2);
    // _pf.JMP(mklbl(lbl2 = ++_lbl));
    // _pf.LABEL(mklbl(lbl1));
    // node->elseblock()->accept(this, lvl + 2);
    // _pf.LABEL(mklbl(lbl1 = lbl2));
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_loop_node(til::loop_node *const node, int lvl) {
    // TODO
    ASSERT_SAFE_EXPRESSIONS;
    // int lbl1, lbl2;
    // _pf.LABEL(mklbl(lbl1 = ++_lbl));
    // node->condition()->accept(this, lvl);
    // _pf.JZ(mklbl(lbl2 = ++_lbl));
    // node->block()->accept(this, lvl + 2);
    // _pf.JMP(mklbl(lbl1));
    // _pf.LABEL(mklbl(lbl2));
}

void til::postfix_writer::do_next_node(til::next_node *const node, int lvl) {
    // TODO
}

void til::postfix_writer::do_stop_node(til::stop_node *const node, int lvl) {
    // TODO
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_sizeof_node(til::sizeof_node *const node,
                                         int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    if (_inFunctionBody) {
        _pf.INT(node->argument()->type()->size());
    } else {
        _pf.SINT(node->argument()->type()->size());
    }
}

void til::postfix_writer::do_alloc_node(til::alloc_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    node->argument()->accept(this, lvl);
    _pf.INT(cdk::reference_type::cast(node->type())
                ->referenced()
                ->size()); // type size
    _pf.MUL();             // type size * argument
    _pf.ALLOC();           // allocate space for the array
    _pf.SP();              // pushes the array's address
}

void til::postfix_writer::do_address_of_node(til::address_of_node *const node,
                                             int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    node->lvalue()->accept(this, lvl + 2);
}
