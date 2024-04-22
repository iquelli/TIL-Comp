#ifndef __TIL_AST_FUNCTION_NODE_H__
#define __TIL_AST_FUNCTION_NODE_H__

#include "block_node.h"
#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/typed_node.h>
#include <cdk/types/basic_type.h>
#include <cdk/types/functional_type.h>
#include <cdk/types/primitive_type.h>
#include <memory>
#include <vector>

namespace til {

/**
 * Class for describing function nodes.
 */
class function_node : public cdk::expression_node {
    cdk::sequence_node *_arguments;
    til::block_node *_block;
    bool _main;

  public:
    function_node(int lineno, std::shared_ptr<cdk::basic_type> type_return,
                  cdk::sequence_node *arguments, til::block_node *block)
        : cdk::expression_node(lineno), _arguments(arguments), _block(block),
          _main(false) {
        // Get the argument types from the argument sequence
        std::vector<std::shared_ptr<cdk::basic_type>> type_arguments;
        for (size_t i = 0; i < arguments->size(); ++i) {
            auto *argument =
                dynamic_cast<cdk::typed_node *>(arguments->node(i));
            type_arguments.push_back(argument->type());
        }

        type(cdk::functional_type::create(type_arguments, type_return));
    }

    /**
     * Constructor for the main function.
     */
    function_node(int lineno, til::block_node *block)
        : cdk::expression_node(lineno),
          _arguments(new cdk::sequence_node(lineno)), _block(block),
          _main(true) {
        type(cdk::functional_type::create(
            cdk::primitive_type::create(4, cdk::TYPE_INT)));
    }

    cdk::sequence_node *arguments() { return _arguments; }

    til::block_node *block() { return _block; }

    bool main() { return _main; }

    void accept(basic_ast_visitor *sp, int level) {
        sp->do_function_node(this, level);
    }
};

} // til

#endif
