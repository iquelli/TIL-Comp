#ifndef __TIL_TARGETS_XML_WRITER_H__
#define __TIL_TARGETS_XML_WRITER_H__

#include "targets/basic_ast_visitor.h"
#include <cdk/ast/basic_node.h>
#include <cdk/types/types.h>

#include "til_parser.tab.h"

namespace til {

/**
 * Print nodes as XML elements to the output stream.
 */
class xml_writer : public basic_ast_visitor {
    cdk::symbol_table<til::symbol> &_symtab;

  public:
    xml_writer(std::shared_ptr<cdk::compiler> compiler,
               cdk::symbol_table<til::symbol> &symtab)
        : basic_ast_visitor(compiler), _symtab(symtab) {}

  public:
    ~xml_writer() { os().flush(); }

  private:
    void openTag(const std::string &tag, int lvl) {
        os() << std::string(lvl, ' ') + "<" + tag + ">" << std::endl;
    }
    void openTag(const cdk::basic_node *node, int lvl) {
        openTag(node->label(), lvl);
    }
    void closeTag(const std::string &tag, int lvl) {
        os() << std::string(lvl, ' ') + "</" + tag + ">" << std::endl;
    }
    void closeTag(const cdk::basic_node *node, int lvl) {
        closeTag(node->label(), lvl);
    }

  protected:
    void do_binary_operation(cdk::binary_operation_node *const node, int lvl);
    void do_unary_operation(cdk::unary_operation_node *const node, int lvl);
    template <typename T>
    void process_literal(cdk::literal_node<T> *const node, int lvl) {
        os() << std::string(lvl, ' ') << "<" << node->label() << ">"
             << node->value() << "</" << node->label() << ">" << std::endl;
    }
    inline const char *qualifier_name(int qualifier) {
        switch (qualifier) {
        case tFORWARD:
            return "forward";
        case tEXTERNAL:
            return "external";
        case tPUBLIC:
            return "public";
        default:
            return "unknown";
        };
    }
    inline std::string type_name(std::shared_ptr<cdk::basic_type> type) {
        if (type->name() == cdk::TYPE_VOID) {
            return "void";
        }
        if (type->name() == cdk::TYPE_INT) {
            return "int";
        }
        if (type->name() == cdk::TYPE_DOUBLE) {
            return "double";
        }
        if (type->name() == cdk::TYPE_STRING) {
            return "string";
        }
        if (type->name() == cdk::TYPE_UNSPEC) {
            return "unspec";
        }
        if (type->name() == cdk::TYPE_POINTER) {
            auto ptr = std::dynamic_pointer_cast<cdk::reference_type>(type);
            return type_name(ptr->referenced()) + '!';
        }
        if (type->name() == cdk::TYPE_FUNCTIONAL) {
            auto ptr = std::dynamic_pointer_cast<cdk::functional_type>(type);
            std::string typ = "(" + type_name(ptr->output(0));
            if (ptr->input_length() != 0) {
                typ += " (";
                for (size_t i = 0; i < ptr->input_length(); ++i) {
                    typ += type_name(ptr->input(i));
                    if (i != ptr->input_length() - 1) {
                        typ += " ";
                    }
                }
                typ += ")";
            }
            typ += ")";
            return typ;
        }
        return "unknown";
    }

  public:
    // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h" // automatically generated
#undef __IN_VISITOR_HEADER__
    // do not edit these lines: end
};

} // til

#endif
