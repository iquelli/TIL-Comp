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
        node->declarations()->accept(this, lvl);
    }

    _lastBlockInstrSeen = false;
    for (size_t i = 0; i < node->instructions()->size(); ++i) {
        if (_lastBlockInstrSeen) {
            error(node->lineno(), "unreachable code after final instruction");
            return;
        }
        node->instructions()->node(i)->accept(this, lvl);
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

    node->argument()->accept(this, lvl);    // determine the value
    if (node->is_typed(cdk::TYPE_DOUBLE)) { // 2-complement
        _pf.DNEG();
    } else {
        _pf.NEG();
    }
}

void til::postfix_writer::do_unary_plus_node(cdk::unary_plus_node *const node,
                                             int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    node->argument()->accept(this, lvl); // determine the value
}

void til::postfix_writer::do_not_node(cdk::not_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    // compare the value of the node with false
    node->argument()->accept(this, lvl);
    _pf.INT(0);
    _pf.EQ(); // check if the two values on the stack are the same
}

//---------------------------------------------------------------------------

// Additive Binary

void til::postfix_writer::process_additive_expr(
    cdk::binary_operation_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    node->left()->accept(this, lvl);
    if (node->is_typed(cdk::TYPE_DOUBLE) &&
        node->left()->is_typed(cdk::TYPE_INT)) {
        _pf.I2D();
    } else if (node->is_typed(cdk::TYPE_POINTER) &&
               !node->left()->is_typed(cdk::TYPE_POINTER)) {
        const auto ref_right =
            cdk::reference_type::cast(node->right()->type())->referenced();
        _pf.INT(std::max(1, static_cast<int>(ref_right->size())));
        _pf.MUL();
    }

    node->right()->accept(this, lvl);
    if (node->is_typed(cdk::TYPE_DOUBLE) &&
        node->right()->is_typed(cdk::TYPE_INT)) {
        _pf.I2D();
    } else if (node->is_typed(cdk::TYPE_POINTER) &&
               !node->right()->is_typed(cdk::TYPE_POINTER)) {
        const auto ref_left =
            cdk::reference_type::cast(node->left()->type())->referenced();
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

    if (!node->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.SUB();
    } else {
        _pf.DSUB();
    }

    // if both are pointers we need a special case
    if (node->left()->is_typed(cdk::TYPE_POINTER) &&
        node->right()->is_typed(cdk::TYPE_POINTER)) {
        const auto ref_left =
            cdk::reference_type::cast(node->left()->type())->referenced();
        _pf.INT(std::max(1, static_cast<int>(ref_left->size())));
        _pf.DIV();
    }
}

// Multiplicative Binary

void til::postfix_writer::process_multiplicative_expr(
    cdk::binary_operation_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    node->left()->accept(this, lvl);
    if (node->is_typed(cdk::TYPE_DOUBLE) &&
        node->left()->is_typed(cdk::TYPE_INT)) {
        _pf.I2D();
    }

    node->right()->accept(this, lvl);
    if (node->is_typed(cdk::TYPE_DOUBLE) &&
        node->right()->is_typed(cdk::TYPE_INT)) {
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

    node->left()->accept(this, lvl);
    if (node->is_typed(cdk::TYPE_DOUBLE) &&
        node->left()->is_typed(cdk::TYPE_INT)) {
        _pf.I2D();
    }

    node->right()->accept(this, lvl);
    if (node->is_typed(cdk::TYPE_DOUBLE) &&
        node->right()->is_typed(cdk::TYPE_INT)) {
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

    node->left()->accept(this, lvl);
    _pf.DUP32();
    _pf.JZ(lbl); // and short-circuit

    node->right()->accept(this, lvl);
    _pf.AND();
    _pf.ALIGN();
    _pf.LABEL(lbl);
}
void til::postfix_writer::do_or_node(cdk::or_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    const auto lbl = mklbl(++_lbl);

    node->left()->accept(this, lvl);
    _pf.DUP32();
    _pf.JNZ(lbl); // or short-circuit

    node->right()->accept(this, lvl);
    _pf.OR();
    _pf.ALIGN();
    _pf.LABEL(lbl);
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_variable_node(cdk::variable_node *const node,
                                           int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    const auto symbol = _symtab.find(node->name());

    // a symbol may be external, global or local
    if (symbol->qualifier() == tEXTERNAL) {
        _currentExternalLabel = symbol->name();
    } else if (symbol->is_global()) {
        _pf.ADDR(symbol->name());
    } else {
        _pf.LOCAL(symbol->offset());
    }
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
    ASSERT_SAFE_EXPRESSIONS;

    node->lvalue()->accept(this, lvl);
    if (!_currentExternalLabel.empty()) {
        return; // external method, so we call it by their label
    }
    if (!node->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.LDINT(); // integers, pointers, strings, functionals
    } else {
        _pf.LDDOUBLE();
    }
}

void til::postfix_writer::do_assignment_node(cdk::assignment_node *const node,
                                             int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    node->rvalue()->accept(this, lvl);
    if (!node->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.DUP32();
    } else {
        if (node->rvalue()->is_typed(cdk::TYPE_INT)) {
            _pf.I2D();
        }
        _pf.DUP64();
    }

    node->lvalue()->accept(this, lvl); // get address to store the val
    if (!node->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.STINT();
    } else {
        _pf.STDOUBLE();
    }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_declaration_node(til::declaration_node *const node,
                                              int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    int offset = 0;                              // 0 is global
    const auto type_size = node->type()->size(); // bytes
    if (_inFunctionArgs) {
        offset = _offset;
        _offset += type_size;
    } else if (_inFunctionBody) {
        _offset -= type_size;
        offset = _offset;
    }

    const auto symbol = new_symbol();
    if (symbol) {
        reset_new_symbol();
        symbol->set_offset(offset);
    }

    if (node->initializer()) {
        if (_inFunctionBody) {
            process_local_var_init(symbol, node->initializer(), lvl);
        } else {
            process_global_var_init(symbol, node->initializer(), lvl);
        }
        _symbolsToDeclare.erase(symbol->name());
    } else if (!_inFunctionArgs && !_inFunctionBody) {
        _symbolsToDeclare.insert(symbol->name());
    }
}
void til::postfix_writer::process_local_var_init(
    std::shared_ptr<til::symbol> symbol,
    cdk::expression_node *const initializer, int lvl) {
    initializer->accept(this, lvl);
    if (symbol->type()->name() == cdk::TYPE_DOUBLE) {
        if (initializer->is_typed(cdk::TYPE_INT)) {
            _pf.I2D();
        }
        _pf.LOCAL(symbol->offset());
        _pf.STDOUBLE();
    } else {
        _pf.LOCAL(symbol->offset());
        _pf.STINT();
    }
}
void til::postfix_writer::process_global_var_init(
    std::shared_ptr<til::symbol> symbol,
    cdk::expression_node *const initializer, int lvl) {
    if (!dynamic_cast<cdk::integer_node *>(initializer) &&
        !dynamic_cast<cdk::double_node *>(initializer) &&
        !dynamic_cast<cdk::string_node *>(initializer) &&
        !dynamic_cast<til::null_node *>(initializer) &&
        !dynamic_cast<til::function_node *>(initializer)) {
        error(initializer->lineno(),
              "non-literal initializer for global variable '" + symbol->name() +
                  "'");
        return;
    }

    if (symbol->type()->name() == cdk::TYPE_INT ||
        symbol->type()->name() == cdk::TYPE_STRING ||
        symbol->type()->name() == cdk::TYPE_POINTER) {
        _pf.DATA();
        _pf.ALIGN();
        _pf.LABEL(symbol->name());
        initializer->accept(this, lvl);
    } else if (symbol->type()->name() == cdk::TYPE_DOUBLE) {
        _pf.DATA();
        _pf.ALIGN();
        _pf.LABEL(symbol->name());
        const cdk::integer_node *dclini;
        cdk::double_node *ddi;
        if (initializer->type()->name() == cdk::TYPE_INT) {
            // essentially cast the int into a double
            dclini = dynamic_cast<const cdk::integer_node *>(initializer);
            ddi = new cdk::double_node(dclini->lineno(), dclini->value());
            ddi->accept(this, lvl);
        } else if (initializer->type()->name() == cdk::TYPE_DOUBLE) {
            initializer->accept(this, lvl);
        }
    } else if (symbol->type()->name() == cdk::TYPE_FUNCTIONAL) {
        _functions.push_back(symbol);
        initializer->accept(this, lvl);
        _pf.DATA();
        _pf.ALIGN();
        if (symbol->qualifier() == tPUBLIC) {
            _pf.GLOBAL(symbol->name(), _pf.OBJ());
        }
        _pf.LABEL(symbol->name());
        _pf.SADDR(_functionLabels.back());
    }
}

void til::postfix_writer::do_function_node(til::function_node *const node,
                                           int lvl) {
    node->main() ? process_main_function(node, lvl)
                 : process_normal_function(node, lvl);
}
void til::postfix_writer::process_main_function(til::function_node *const node,
                                                int lvl) {
    for (auto symbol_name : _symbolsToDeclare) {
        const auto symbol = _symtab.find(symbol_name);
        if (symbol->qualifier() == tEXTERNAL ||
            symbol->qualifier() == tFORWARD) {
            _functionsToDeclare.insert(symbol_name);
            continue;
        }
        _pf.BSS();
        _pf.ALIGN();
        _pf.LABEL(symbol_name);
        _pf.SALLOC(symbol->type()->size());
    }

    const auto main = til::make_symbol(node->type(), "_main", 0, tPRIVATE);
    _symtab.insert(main->name(), main);
    _functions.push_back(main);
    _functionLabels.push_back("_main");

    _symtab.push(); // new context for the main function
    _pf.TEXT("_main");
    _pf.ALIGN();
    _pf.GLOBAL("_main", _pf.FUNC());
    _pf.LABEL("_main");

    // calculate the stack size for local variables
    frame_size_calculator fsc(_compiler, _symtab);
    node->accept(&fsc, lvl);
    _pf.ENTER(fsc.localsize());

    _inFunctionBody = true;
    node->block()->accept(this, lvl);
    _inFunctionBody = false;

    _symtab.pop(); // leave the context

    _functionLabels.pop_back();
    _functions.pop_back();
    if (!_mainReturnSeen) { // in case of no return instruction
        _pf.INT(0);
        _pf.STFVAL32();
    }
    _pf.LEAVE();
    _pf.RET();

    for (auto external_function : _functionsToDeclare) {
        _pf.EXTERN(external_function);
    }
}
void til::postfix_writer::process_normal_function(
    til::function_node *const node, int lvl) {
    _symtab.push(); // arguments context
    auto function = til::make_symbol(node->type(), "@", 0, tPRIVATE);
    if (!_symtab.insert(function->name(), function)) {
        _symtab.replace(function->name(), function);
    }
    _functions.push_back(function);
    const auto function_label = mklbl(++_lbl);
    _functionLabels.push_back(function_label);

    const auto previous_offset = _offset;
    _offset = 8;

    if (node->arguments()) {
        _inFunctionArgs = true;
        for (size_t i = 0; i < node->arguments()->size(); ++i) {
            node->arguments()->node(i)->accept(this, lvl);
        }
        _inFunctionArgs = false;
    }

    _pf.TEXT(function_label);
    _pf.ALIGN();
    _pf.LABEL(function_label);

    // calculate the stack size for local variables
    frame_size_calculator fsc(_compiler, _symtab);
    node->accept(&fsc, lvl);
    _pf.ENTER(fsc.localsize());

    _offset = 0; // reset offset
    auto prev_in_function_body = _inFunctionBody;
    _inFunctionBody = true;
    if (node->block()) {
        node->block()->accept(this, lvl);
    }
    _inFunctionBody = prev_in_function_body;
    _symtab.pop(); // leave arguments context
    _offset = previous_offset;

    if (function) {
        _functions.pop_back();
    }
    _pf.LEAVE();
    _pf.RET();

    if (_inFunctionBody) {
        _functionLabels.pop_back();
        _pf.TEXT(_functionLabels.back());
        _pf.ADDR(function_label);
    }
}

void til::postfix_writer::do_function_call_node(
    til::function_call_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    std::vector<std::shared_ptr<cdk::basic_type>> arg_types;
    if (node->func()) {
        arg_types = cdk::functional_type::cast(node->func()->type())
                        ->input()
                        ->components();
    } else { // recursive call (@)
        auto deepest_function = _functions.back();
        arg_types = cdk::functional_type::cast(deepest_function->type())
                        ->input()
                        ->components();
    }

    size_t args_bytes = 0;
    if (node->arguments()) {
        for (int i = node->arguments()->size() - 1; i >= 0; --i) {
            auto arg = dynamic_cast<cdk::expression_node *>(
                node->arguments()->node(i));
            arg->accept(this, lvl);
            if (arg_types[i]->name() == cdk::TYPE_DOUBLE &&
                arg->type()->name() == cdk::TYPE_INT) {
                // extra 4 bytes in case we pass an integer for an argument of
                // type double
                args_bytes += 4;
                _pf.I2D();
            }
            args_bytes += arg->type()->size();
        }
    }

    // 3 cases -> non-recursive call, external call or recursive call
    if (!node->func()) { // recursive calls
        _pf.CALL(_functionLabels.back());
    } else { // non-recursive calls
        _currentExternalLabel.clear();
        node->func()->accept(this, lvl);
        if (_currentExternalLabel.empty()) { // it is a non-external call
            _pf.BRANCH();
        } else { // external call
            _pf.CALL(_currentExternalLabel);
        }
    }

    // clear the bytes held by the arguments that are no longer needed
    if (args_bytes > 0) {
        _pf.TRASH(args_bytes);
    }

    if (node->type()->name() == cdk::TYPE_INT) {
        if (_currentExternalLabel.empty()) {
            // because every non-main function returns double, we need to
            // reconvert it back to int
            _pf.LDFVAL64();
            _pf.D2I();
        } else {
            _pf.LDFVAL32(); // external methods already return int
        }
    } else if (node->type()->name() == cdk::TYPE_STRING ||
               node->type()->name() == cdk::TYPE_POINTER ||
               node->type()->name() == cdk::TYPE_FUNCTIONAL) {
        _pf.LDFVAL32();
    } else if (node->type()->name() == cdk::TYPE_DOUBLE) {
        _pf.LDFVAL64();
    }

    _currentExternalLabel.clear();
}

void til::postfix_writer::do_return_node(til::return_node *const node,
                                         int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    const auto func_out_type_name =
        cdk::functional_type::cast(_functions.back()->type())
            ->output(0)
            ->name();

    if (func_out_type_name != cdk::TYPE_VOID) {
        node->retval()->accept(this, lvl);
        if (func_out_type_name == cdk::TYPE_INT) {
            if (_functions.back()->is_main()) {
                // in the case of the main function we have to return an int
                _mainReturnSeen = true;
                _pf.STFVAL32();
            } else {
                // to allow covariance, we return a double from non-main
                // functions
                _pf.I2D();
                _pf.STFVAL64();
            }
        } else if (func_out_type_name == cdk::TYPE_STRING ||
                   func_out_type_name == cdk::TYPE_POINTER ||
                   func_out_type_name == cdk::TYPE_FUNCTIONAL) {
            _pf.STFVAL32(); // remove 4 bytes from the stack
        } else if (func_out_type_name == cdk::TYPE_DOUBLE) {
            if (!node->retval()->is_typed(cdk::TYPE_DOUBLE)) {
                _pf.I2D();
            }
            _pf.STFVAL64(); // remove 8 bytes from the stack
        }
    }

    _lastBlockInstrSeen = true;
    _pf.LEAVE();
    _pf.RET();
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_evaluation_node(til::evaluation_node *const node,
                                             int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    node->argument()->accept(this, lvl); // determine the value
    if (node->argument()->type()->size() > 0) {
        _pf.TRASH(node->argument()->type()->size()); // delete it
    }
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
        }
    }

    if (node->newline()) {
        _functionsToDeclare.insert("println");
        _pf.CALL("println");
    }
}

void til::postfix_writer::do_read_node(til::read_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    if (node->is_typed(cdk::TYPE_INT) || node->is_typed(cdk::TYPE_UNSPEC)) {
        _functionsToDeclare.insert("readi");
        _pf.CALL("readi");
        _pf.LDFVAL32();
    } else if (node->is_typed(cdk::TYPE_DOUBLE)) {
        _functionsToDeclare.insert("readd");
        _pf.CALL("readd");
        _pf.LDFVAL64();
    }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_if_node(til::if_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    int lbl;

    node->condition()->accept(this, lvl);
    _pf.JZ(mklbl(lbl = ++_lbl));

    node->block()->accept(this, lvl);
    // in case it's not a block_node, but a single instruction
    _lastBlockInstrSeen = false;

    _pf.ALIGN();
    _pf.LABEL(mklbl(lbl));
}

void til::postfix_writer::do_if_else_node(til::if_else_node *const node,
                                          int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    int lbl1, lbl2;

    node->condition()->accept(this, lvl);
    _pf.JZ(mklbl(lbl1 = ++_lbl));

    node->thenblock()->accept(this, lvl);
    // in case it's not a block_node, but a single instruction
    _lastBlockInstrSeen = false;

    _pf.JMP(mklbl(lbl2 = ++_lbl));
    _pf.ALIGN();
    _pf.LABEL(mklbl(lbl1));

    node->elseblock()->accept(this, lvl);
    // in case it's not a block_node, but a single instruction
    _lastBlockInstrSeen = false;

    _pf.ALIGN();
    _pf.LABEL(mklbl(lbl1 = lbl2));
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_loop_node(til::loop_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    int condLbl, endLbl;

    _pf.ALIGN();
    _pf.LABEL(mklbl(condLbl = ++_lbl)); // label for loop condition
    node->condition()->accept(this, lvl);
    _pf.JZ(mklbl(endLbl = ++_lbl)); // jump to the exit if false

    _loopCond.push_back(condLbl); // the deepest loop condition label
    _loopEnd.push_back(endLbl);   // the deepest loop end label

    node->block()->accept(this, lvl);
    // in case it's not a block_node, but a single instruction
    _lastBlockInstrSeen = false;

    _loopCond.pop_back();
    _loopEnd.pop_back();

    _pf.JMP(mklbl(condLbl)); // repeat
    _pf.ALIGN();
    _pf.LABEL(mklbl(endLbl)); // label for end of loop
}

void til::postfix_writer::do_next_node(til::next_node *const node, int lvl) {
    const auto loopLabels = _loopCond.size();
    if (loopLabels == 0) {
        error(node->lineno(), "next node found outside a loop block");
        return;
    }

    const size_t nextNesting = (size_t)node->nesting();
    if (nextNesting < 1 || nextNesting > loopLabels) {
        error(node->lineno(), "invalid next nesting");
        return;
    }

    _lastBlockInstrSeen = true;
    const auto loopCondLbl = _loopCond[loopLabels - nextNesting];
    _pf.JMP(mklbl(loopCondLbl));
}

void til::postfix_writer::do_stop_node(til::stop_node *const node, int lvl) {
    const auto loopLabels = _loopCond.size();
    if (loopLabels == 0) {
        error(node->lineno(), "stop node found outside a loop block");
        return;
    }

    const size_t stopNesting = (size_t)node->nesting();
    if (stopNesting < 1 || stopNesting > loopLabels) {
        error(node->lineno(), "invalid stop nesting");
        return;
    }

    _lastBlockInstrSeen = true;
    const auto loopEndLbl = _loopEnd[loopLabels - stopNesting];
    _pf.JMP(mklbl(loopEndLbl));
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

    node->lvalue()->accept(this, lvl);
}
