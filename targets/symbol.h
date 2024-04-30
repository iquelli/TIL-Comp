#ifndef __TIL_TARGETS_SYMBOL_H__
#define __TIL_TARGETS_SYMBOL_H__

#include <cdk/types/basic_type.h>
#include <memory>
#include <string>

namespace til {

class symbol {
    std::shared_ptr<cdk::basic_type> _type;
    std::string _name;
    long _value; // hack!
    int _qualifier;
    int _offset = 0; // 0 means global

  public:
    symbol(std::shared_ptr<cdk::basic_type> type, const std::string &name,
           long value, int qualifier)
        : _type(type), _name(name), _value(value), _qualifier(qualifier) {}

    virtual ~symbol() {
        // EMPTY
    }

    std::shared_ptr<cdk::basic_type> type() const { return _type; }
    bool is_typed(cdk::typename_type name) const {
        return _type->name() == name;
    }
    const std::string &name() const { return _name; }
    long value() const { return _value; }
    long value(long v) { return _value = v; }
    int qualifier() const { return _qualifier; }
    int offset() const { return _offset; }
    void set_offset(int offset) { _offset = offset; }

    bool is_main() const { return name() == "_main"; }
    bool is_global() const { return _offset == 0; }
};

inline auto make_symbol(std::shared_ptr<cdk::basic_type> type,
                        const std::string &name, long value, int qualifier) {
    return std::make_shared<symbol>(type, name, value, qualifier);
}

} // til

#endif
