//===- Rust IR Dialect registration in MLIR ------------------------------===//
//
// Copyright 2019 The MLIR Authors.
// Copyright 2019 KTH Royal Institute of Technology.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// =============================================================================
//
// Defines the types of the Rust dialect.
//
//===----------------------------------------------------------------------===//

#include "Rust/Types.h"
#include "Rust/Rust.h"
#include "Rust/RustPrinterStream.h"
#include <llvm/Support/raw_ostream.h>
#include <mlir/IR/DialectImplementation.h>
#include <mlir/IR/StandardTypes.h>

using namespace mlir;
using namespace rust;
using namespace types;

namespace rust {
namespace types {

//===----------------------------------------------------------------------===//
// RustType
//===----------------------------------------------------------------------===//

static std::string getTypeString(Type t) {
  switch (t.getKind()) {
  case types::RUST_STRUCT:
    return t.cast<RustStructType>().getRustType();
  case types::RUST_TUPLE:
    return t.cast<RustTupleType>().getRustType();
  case types::RUST_TENSOR:
    return t.cast<RustTensorType>().getRustType();
  case types::RUST_TYPE: {
    RustType rt = t.cast<RustType>();
    return rt.getRustType().str();
  }
  default:
    return "<unsupported type>";
  }
}

struct RustTypeStorage : public TypeStorage {
  RustTypeStorage(std::string type) : rustType(type) {}

  std::string rustType;

  using KeyTy = std::string;

  bool operator==(const KeyTy &key) const { return key == KeyTy(rustType); }

  static llvm::hash_code hashKey(const KeyTy &key) {
    return llvm::hash_combine(key);
  }

  static RustTypeStorage *construct(TypeStorageAllocator &allocator,
                                    const KeyTy &key) {
    return new (allocator.allocate<RustTypeStorage>()) RustTypeStorage(key);
  }

  raw_ostream &printAsRust(raw_ostream &os) const {
    os << rustType;
    return os;
  }
};

RustType RustType::get(MLIRContext *context, StringRef type) {
  return Base::get(context, RUST_TYPE, type);
}

StringRef RustType::getRustType() const { return getImpl()->rustType; }

void RustType::print(DialectAsmPrinter &os) const { os << getRustType(); }

raw_ostream &RustType::printAsRust(raw_ostream &os) const {
  return getImpl()->printAsRust(os);
}

bool RustType::isBool() const { return getRustType().equals("bool"); }

RustType RustType::getFloatTy(RustDialect *dialect) { return dialect->floatTy; }

RustType RustType::getDoubleTy(RustDialect *dialect) {
  return dialect->doubleTy;
}

RustType RustType::getIntegerTy(RustDialect *dialect, IntegerType ty) {
  switch (ty.getWidth()) {
  case 1:
    return dialect->boolTy;
  case 8:
    return ty.isUnsigned() ? dialect->u8Ty : dialect->i8Ty;
  case 16:
    return ty.isUnsigned() ? dialect->u16Ty : dialect->i16Ty;
  case 32:
    return ty.isUnsigned() ? dialect->u32Ty : dialect->i32Ty;
  case 64:
    return ty.isUnsigned() ? dialect->u64Ty : dialect->i64Ty;
  default:
    return emitError(UnknownLoc::get(dialect->getContext()), "unhandled type"),
           nullptr;
  }
}

//===----------------------------------------------------------------------===//
// RustStructType
//===----------------------------------------------------------------------===//

struct RustStructTypeStorage : public TypeStorage {
  RustStructTypeStorage(ArrayRef<RustStructType::StructFieldTy> fields,
                        unsigned id)
      : structFields(fields.begin(), fields.end()), id(id) {}

  SmallVector<RustStructType::StructFieldTy, 4> structFields;
  unsigned id;

  using KeyTy = ArrayRef<RustStructType::StructFieldTy>;

  bool operator==(const KeyTy &key) const { return key == KeyTy(structFields); }

  static llvm::hash_code hashKey(const KeyTy &key) {
    return llvm::hash_combine(key);
  }

  static RustStructTypeStorage *construct(TypeStorageAllocator &allocator,
                                          const KeyTy &key) {
    return new (allocator.allocate<RustStructTypeStorage>())
        RustStructTypeStorage(key, idCounter++);
  }

  RustPrinterStream &printAsRust(RustPrinterStream &os) const;
  raw_ostream &printAsRustNamedType(raw_ostream &os) const;
  void print(DialectAsmPrinter &os) const { os << getRustType(); }
  StringRef getFieldName(unsigned idx) const;

  std::string getRustType() const;
  unsigned getStructTypeId() const;

  void emitNestedTypedefs(rust::RustPrinterStream &os) const;

private:
  static unsigned idCounter;
};

unsigned RustStructTypeStorage::idCounter = 0;

RustStructType RustStructType::get(RustDialect *dialect,
                                   ArrayRef<StructFieldTy> fields) {
  mlir::MLIRContext *ctx = fields.front().second.getContext();
  return Base::get(ctx, rust::types::RUST_STRUCT, fields);
}

void RustStructType::print(DialectAsmPrinter &os) const {
  getImpl()->print(os);
}

RustPrinterStream &RustStructType::printAsRust(RustPrinterStream &os) const {
  return getImpl()->printAsRust(os);
}

raw_ostream &RustStructType::printAsRustNamedType(raw_ostream &os) const {
  return getImpl()->printAsRustNamedType(os);
}

StringRef RustStructType::getFieldName(unsigned idx) const {
  return getImpl()->getFieldName(idx);
}

StringRef RustStructTypeStorage::getFieldName(unsigned idx) const {
  return structFields[idx].first.getValue();
}

std::string RustStructType::getRustType() const {
  return getImpl()->getRustType();
}

unsigned RustStructTypeStorage::getStructTypeId() const { return id; }

unsigned RustStructType::getStructTypeId() const {
  return getImpl()->getStructTypeId();
}
std::string RustStructTypeStorage::getRustType() const {
  std::string str;
  llvm::raw_string_ostream s(str);

  s << "ArcStruct" << id;
  return s.str();
}

RustPrinterStream &
RustStructTypeStorage::printAsRust(RustPrinterStream &ps) const {

  llvm::raw_ostream &os = ps.getNamedTypesStream();
  // First ensure that any structs used by this struct are defined
  emitNestedTypedefs(ps);

  os << "pub struct ";
  printAsRustNamedType(os) << "Value {\n  ";

  for (unsigned i = 0; i < structFields.size(); i++) {
    if (i != 0)
      os << ",\n  ";
    os << structFields[i].first.getValue() << " : ";
    os << getTypeString(structFields[i].second);
  }
  os << "\n}\n";
  os << "type ";
  printAsRustNamedType(os) << " = Rc<";
  printAsRustNamedType(os) << "Value>;\n";
  ps.registerDirective("rc-import", "use std::rc::Rc;\n");
  return ps;
}

void RustStructType::emitNestedTypedefs(rust::RustPrinterStream &ps) const {
  return getImpl()->emitNestedTypedefs(ps);
}

void RustStructTypeStorage::emitNestedTypedefs(
    rust::RustPrinterStream &ps) const {
  // First ensure that any structs used by this tuple are defined
  for (unsigned i = 0; i < structFields.size(); i++)
    if (structFields[i].second.isa<RustStructType>())
      ps.writeStructDefiniton(structFields[i].second.cast<RustStructType>());
    else if (structFields[i].second.isa<RustTupleType>())
      structFields[i].second.cast<RustTupleType>().emitNestedTypedefs(ps);
}

raw_ostream &
RustStructTypeStorage::printAsRustNamedType(raw_ostream &os) const {

  os << "ArcStruct" << id;
  return os;
}

//===----------------------------------------------------------------------===//
// RustTensorType
//===----------------------------------------------------------------------===//

struct RustTensorTypeStorage : public TypeStorage {
  using KeyTy = std::pair<Type, ArrayRef<int64_t>>;

  RustTensorTypeStorage(KeyTy key)
      : elementTy(key.first), dimensions(key.second.begin(), key.second.end()) {
  }

  Type elementTy;
  SmallVector<int64_t, 3> dimensions;

  bool operator==(const KeyTy &key) const {
    return key == KeyTy(elementTy, dimensions);
  }

  static llvm::hash_code hashKey(const KeyTy &key) {
    return llvm::hash_combine(key);
  }

  static RustTensorTypeStorage *construct(TypeStorageAllocator &allocator,
                                          const KeyTy &key) {
    return new (allocator.allocate<RustTensorTypeStorage>())
        RustTensorTypeStorage(key);
  }

  RustPrinterStream &printAsRust(RustPrinterStream &os) const;
  void print(DialectAsmPrinter &os) const { os << getRustType(); }

  std::string getRustType() const;
};

RustTensorType RustTensorType::get(RustDialect *dialect, Type elementTy,
                                   ArrayRef<int64_t> dimensions) {
  mlir::MLIRContext *ctx = elementTy.getContext();
  return Base::get(ctx, rust::types::RUST_TENSOR, elementTy, dimensions);
}

void RustTensorType::print(DialectAsmPrinter &os) const {
  getImpl()->print(os);
}

RustPrinterStream &RustTensorType::printAsRust(RustPrinterStream &os) const {
  return getImpl()->printAsRust(os);
}

std::string RustTensorType::getRustType() const {
  return getImpl()->getRustType();
}

std::string RustTensorTypeStorage::getRustType() const {
  std::string str;
  llvm::raw_string_ostream s(str);

  s << "Rc<Array<" << getTypeString(elementTy) << ", Dim<[Ix; "
    << dimensions.size() << "]>>>";
  return s.str();
}

RustPrinterStream &
RustTensorTypeStorage::printAsRust(RustPrinterStream &ps) const {
  ps.registerDirective("rc-import", "use std::rc::Rc;\n");
  ps.registerDirective("ndarray-import", "use ndarray::{Array,Dim,Ix};\n");
  ps.registerDependency("ndarray", CrateVersions::ndarray);
  ps << getRustType();
  return ps;
}

//===----------------------------------------------------------------------===//
// RustTupleType
//===----------------------------------------------------------------------===//

struct RustTupleTypeStorage : public TypeStorage {
  RustTupleTypeStorage(ArrayRef<Type> fields)
      : tupleFields(fields.begin(), fields.end()) {}

  SmallVector<Type, 4> tupleFields;

  using KeyTy = ArrayRef<Type>;

  bool operator==(const KeyTy &key) const { return key == KeyTy(tupleFields); }

  static llvm::hash_code hashKey(const KeyTy &key) {
    return llvm::hash_combine(key);
  }

  static RustTupleTypeStorage *construct(TypeStorageAllocator &allocator,
                                         const KeyTy &key) {
    return new (allocator.allocate<RustTupleTypeStorage>())
        RustTupleTypeStorage(key);
  }

  RustPrinterStream &printAsRust(RustPrinterStream &os) const;
  void print(DialectAsmPrinter &os) const { os << getRustType(); }

  std::string getRustType() const;

  void emitNestedTypedefs(rust::RustPrinterStream &ps) const;
};

RustTupleType RustTupleType::get(RustDialect *dialect, ArrayRef<Type> fields) {
  mlir::MLIRContext *ctx = fields.front().getContext();
  return Base::get(ctx, rust::types::RUST_TUPLE, fields);
}

void RustTupleType::print(DialectAsmPrinter &os) const { getImpl()->print(os); }

RustPrinterStream &RustTupleType::printAsRust(RustPrinterStream &os) const {
  return getImpl()->printAsRust(os);
}

std::string RustTupleType::getRustType() const {
  return getImpl()->getRustType();
}

std::string RustTupleTypeStorage::getRustType() const {
  std::string str;
  llvm::raw_string_ostream s(str);

  s << "Rc<(";
  for (unsigned i = 0; i < tupleFields.size(); i++)
    s << getTypeString(tupleFields[i]) << ", ";
  s << ")>";
  return s.str();
}

RustPrinterStream &
RustTupleTypeStorage::printAsRust(RustPrinterStream &ps) const {
  emitNestedTypedefs(ps);

  ps.registerDirective("rc-import", "use std::rc::Rc;\n");
  ps << getRustType();
  return ps;
}

void RustTupleType::emitNestedTypedefs(rust::RustPrinterStream &ps) const {
  return getImpl()->emitNestedTypedefs(ps);
}

void RustTupleTypeStorage::emitNestedTypedefs(
    rust::RustPrinterStream &ps) const {
  // First ensure that any structs used by this tuple are defined
  for (unsigned i = 0; i < tupleFields.size(); i++)
    if (tupleFields[i].isa<RustStructType>())
      ps.writeStructDefiniton(tupleFields[i].cast<RustStructType>());
    else if (tupleFields[i].isa<RustTupleType>())
      tupleFields[i].cast<RustTupleType>().emitNestedTypedefs(ps);
}

} // namespace types
} // namespace rust
