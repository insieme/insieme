#include "insieme/core/parser3/detail/driver.hpp"
#include "insieme/core/parser3/detail/nodes.hpp"

// this last one is generated and the path will be provided to the command
#include "inspire_parser.hpp"

namespace insieme{
namespace core{
namespace parser3{
namespace detail{


inspire_driver::inspire_driver (const std::string &f, NodeKeeper& nk)
  : scanner( new scanner_string(this, f)),  nodeKeeper(nk), file("no-file"), str(f), result(nullptr)
{
}

inspire_driver::~inspire_driver ()
{
    delete scanner;
}

int inspire_driver::parse ()
{
  scanner->scan_begin ();
  inspire_parser parser (*this);
  int res = parser.parse ();
  scanner->scan_end ();
  return res;
}

void inspire_driver::error (const location& l, const std::string& m)
{
  std::cerr << l << ": " << m << std::endl;
  //int lineb = l.begin.line;
  //int linee = l.end.line;
  int colb = l.begin.column;
  int cole = l.end.column;

  //TODO: multiline?

  std::cerr << "  => " << str << std::endl;
  std::cerr << "     ";
  for (int i =0; i < colb-1; ++i) std::cerr << " ";
  std::cerr << "^";
  for (int i =0; i < cole - colb; ++i) std::cerr << "~";
  std::cerr << std::endl;
}

void inspire_driver::error (const std::string& m)
{
  std::cerr << m << std::endl;
}

} // namespace detail
} // namespace parser3
} // namespace core
} // namespace insieme
