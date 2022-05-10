// XFAIL: *
// RUN: arc-mlir-rust-test %t %s -rustinclude %s.rust-tests
// RUN: arc-mlir-rust-test %t-roundtrip-scf %s -rustinclude %s.rust-tests -canonicalize -remove-scf -canonicalize -to-scf -canonicalize

module @arctorustinttensorarith {
func.func @addi_tensor2x2xui8(%a : tensor<2x2xui8>, %b : tensor<2x2xui8>) -> tensor<2x2xui8> {
  %c = arc.addi %a, %b : tensor<2x2xui8>
  return %c : tensor<2x2xui8>
}

func.func @subi_tensor2x2xui8(%a : tensor<2x2xui8>, %b : tensor<2x2xui8>) -> tensor<2x2xui8> {
  %c = arc.subi %a, %b : tensor<2x2xui8>
  return %c : tensor<2x2xui8>
}

func.func @muli_tensor2x2xui8(%a : tensor<2x2xui8>, %b : tensor<2x2xui8>) -> tensor<2x2xui8> {
  %c = arc.muli %a, %b : tensor<2x2xui8>
  return %c : tensor<2x2xui8>
}

func.func @divi_tensor2x2xui8(%a : tensor<2x2xui8>, %b : tensor<2x2xui8>) -> tensor<2x2xui8> {
  %c = arc.divi %a, %b : tensor<2x2xui8>
  return %c : tensor<2x2xui8>
}

func.func @remi_tensor2x2xui8(%a : tensor<2x2xui8>, %b : tensor<2x2xui8>) -> tensor<2x2xui8> {
  %c = arc.remi %a, %b : tensor<2x2xui8>
  return %c : tensor<2x2xui8>
}

func.func @addi_tensor2x2xui16(%a : tensor<2x2xui16>, %b : tensor<2x2xui16>) -> tensor<2x2xui16> {
  %c = arc.addi %a, %b : tensor<2x2xui16>
  return %c : tensor<2x2xui16>
}

func.func @subi_tensor2x2xui16(%a : tensor<2x2xui16>, %b : tensor<2x2xui16>) -> tensor<2x2xui16> {
  %c = arc.subi %a, %b : tensor<2x2xui16>
  return %c : tensor<2x2xui16>
}

func.func @muli_tensor2x2xui16(%a : tensor<2x2xui16>, %b : tensor<2x2xui16>) -> tensor<2x2xui16> {
  %c = arc.muli %a, %b : tensor<2x2xui16>
  return %c : tensor<2x2xui16>
}

func.func @divi_tensor2x2xui16(%a : tensor<2x2xui16>, %b : tensor<2x2xui16>) -> tensor<2x2xui16> {
  %c = arc.divi %a, %b : tensor<2x2xui16>
  return %c : tensor<2x2xui16>
}

func.func @remi_tensor2x2xui16(%a : tensor<2x2xui16>, %b : tensor<2x2xui16>) -> tensor<2x2xui16> {
  %c = arc.remi %a, %b : tensor<2x2xui16>
  return %c : tensor<2x2xui16>
}

func.func @addi_tensor2x2xui32(%a : tensor<2x2xui32>, %b : tensor<2x2xui32>) -> tensor<2x2xui32> {
  %c = arc.addi %a, %b : tensor<2x2xui32>
  return %c : tensor<2x2xui32>
}

func.func @subi_tensor2x2xui32(%a : tensor<2x2xui32>, %b : tensor<2x2xui32>) -> tensor<2x2xui32> {
  %c = arc.subi %a, %b : tensor<2x2xui32>
  return %c : tensor<2x2xui32>
}

func.func @muli_tensor2x2xui32(%a : tensor<2x2xui32>, %b : tensor<2x2xui32>) -> tensor<2x2xui32> {
  %c = arc.muli %a, %b : tensor<2x2xui32>
  return %c : tensor<2x2xui32>
}

func.func @divi_tensor2x2xui32(%a : tensor<2x2xui32>, %b : tensor<2x2xui32>) -> tensor<2x2xui32> {
  %c = arc.divi %a, %b : tensor<2x2xui32>
  return %c : tensor<2x2xui32>
}

func.func @remi_tensor2x2xui32(%a : tensor<2x2xui32>, %b : tensor<2x2xui32>) -> tensor<2x2xui32> {
  %c = arc.remi %a, %b : tensor<2x2xui32>
  return %c : tensor<2x2xui32>
}

func.func @addi_tensor2x2xui64(%a : tensor<2x2xui64>, %b : tensor<2x2xui64>) -> tensor<2x2xui64> {
  %c = arc.addi %a, %b : tensor<2x2xui64>
  return %c : tensor<2x2xui64>
}

func.func @subi_tensor2x2xui64(%a : tensor<2x2xui64>, %b : tensor<2x2xui64>) -> tensor<2x2xui64> {
  %c = arc.subi %a, %b : tensor<2x2xui64>
  return %c : tensor<2x2xui64>
}

func.func @muli_tensor2x2xui64(%a : tensor<2x2xui64>, %b : tensor<2x2xui64>) -> tensor<2x2xui64> {
  %c = arc.muli %a, %b : tensor<2x2xui64>
  return %c : tensor<2x2xui64>
}

func.func @divi_tensor2x2xui64(%a : tensor<2x2xui64>, %b : tensor<2x2xui64>) -> tensor<2x2xui64> {
  %c = arc.divi %a, %b : tensor<2x2xui64>
  return %c : tensor<2x2xui64>
}

func.func @remi_tensor2x2xui64(%a : tensor<2x2xui64>, %b : tensor<2x2xui64>) -> tensor<2x2xui64> {
  %c = arc.remi %a, %b : tensor<2x2xui64>
  return %c : tensor<2x2xui64>
}

func.func @addi_tensor2x2xsi8(%a : tensor<2x2xsi8>, %b : tensor<2x2xsi8>) -> tensor<2x2xsi8> {
  %c = arc.addi %a, %b : tensor<2x2xsi8>
  return %c : tensor<2x2xsi8>
}

func.func @subi_tensor2x2xsi8(%a : tensor<2x2xsi8>, %b : tensor<2x2xsi8>) -> tensor<2x2xsi8> {
  %c = arc.subi %a, %b : tensor<2x2xsi8>
  return %c : tensor<2x2xsi8>
}

func.func @muli_tensor2x2xsi8(%a : tensor<2x2xsi8>, %b : tensor<2x2xsi8>) -> tensor<2x2xsi8> {
  %c = arc.muli %a, %b : tensor<2x2xsi8>
  return %c : tensor<2x2xsi8>
}

func.func @divi_tensor2x2xsi8(%a : tensor<2x2xsi8>, %b : tensor<2x2xsi8>) -> tensor<2x2xsi8> {
  %c = arc.divi %a, %b : tensor<2x2xsi8>
  return %c : tensor<2x2xsi8>
}

func.func @remi_tensor2x2xsi8(%a : tensor<2x2xsi8>, %b : tensor<2x2xsi8>) -> tensor<2x2xsi8> {
  %c = arc.remi %a, %b : tensor<2x2xsi8>
  return %c : tensor<2x2xsi8>
}

func.func @addi_tensor2x2xsi16(%a : tensor<2x2xsi16>, %b : tensor<2x2xsi16>) -> tensor<2x2xsi16> {
  %c = arc.addi %a, %b : tensor<2x2xsi16>
  return %c : tensor<2x2xsi16>
}

func.func @subi_tensor2x2xsi16(%a : tensor<2x2xsi16>, %b : tensor<2x2xsi16>) -> tensor<2x2xsi16> {
  %c = arc.subi %a, %b : tensor<2x2xsi16>
  return %c : tensor<2x2xsi16>
}

func.func @muli_tensor2x2xsi16(%a : tensor<2x2xsi16>, %b : tensor<2x2xsi16>) -> tensor<2x2xsi16> {
  %c = arc.muli %a, %b : tensor<2x2xsi16>
  return %c : tensor<2x2xsi16>
}

func.func @divi_tensor2x2xsi16(%a : tensor<2x2xsi16>, %b : tensor<2x2xsi16>) -> tensor<2x2xsi16> {
  %c = arc.divi %a, %b : tensor<2x2xsi16>
  return %c : tensor<2x2xsi16>
}

func.func @remi_tensor2x2xsi16(%a : tensor<2x2xsi16>, %b : tensor<2x2xsi16>) -> tensor<2x2xsi16> {
  %c = arc.remi %a, %b : tensor<2x2xsi16>
  return %c : tensor<2x2xsi16>
}

func.func @addi_tensor2x2xsi32(%a : tensor<2x2xsi32>, %b : tensor<2x2xsi32>) -> tensor<2x2xsi32> {
  %c = arc.addi %a, %b : tensor<2x2xsi32>
  return %c : tensor<2x2xsi32>
}

func.func @subi_tensor2x2xsi32(%a : tensor<2x2xsi32>, %b : tensor<2x2xsi32>) -> tensor<2x2xsi32> {
  %c = arc.subi %a, %b : tensor<2x2xsi32>
  return %c : tensor<2x2xsi32>
}

func.func @muli_tensor2x2xsi32(%a : tensor<2x2xsi32>, %b : tensor<2x2xsi32>) -> tensor<2x2xsi32> {
  %c = arc.muli %a, %b : tensor<2x2xsi32>
  return %c : tensor<2x2xsi32>
}

func.func @divi_tensor2x2xsi32(%a : tensor<2x2xsi32>, %b : tensor<2x2xsi32>) -> tensor<2x2xsi32> {
  %c = arc.divi %a, %b : tensor<2x2xsi32>
  return %c : tensor<2x2xsi32>
}

func.func @remi_tensor2x2xsi32(%a : tensor<2x2xsi32>, %b : tensor<2x2xsi32>) -> tensor<2x2xsi32> {
  %c = arc.remi %a, %b : tensor<2x2xsi32>
  return %c : tensor<2x2xsi32>
}

func.func @addi_tensor2x2xsi64(%a : tensor<2x2xsi64>, %b : tensor<2x2xsi64>) -> tensor<2x2xsi64> {
  %c = arc.addi %a, %b : tensor<2x2xsi64>
  return %c : tensor<2x2xsi64>
}

func.func @subi_tensor2x2xsi64(%a : tensor<2x2xsi64>, %b : tensor<2x2xsi64>) -> tensor<2x2xsi64> {
  %c = arc.subi %a, %b : tensor<2x2xsi64>
  return %c : tensor<2x2xsi64>
}

func.func @muli_tensor2x2xsi64(%a : tensor<2x2xsi64>, %b : tensor<2x2xsi64>) -> tensor<2x2xsi64> {
  %c = arc.muli %a, %b : tensor<2x2xsi64>
  return %c : tensor<2x2xsi64>
}

func.func @divi_tensor2x2xsi64(%a : tensor<2x2xsi64>, %b : tensor<2x2xsi64>) -> tensor<2x2xsi64> {
  %c = arc.divi %a, %b : tensor<2x2xsi64>
  return %c : tensor<2x2xsi64>
}

func.func @remi_tensor2x2xsi64(%a : tensor<2x2xsi64>, %b : tensor<2x2xsi64>) -> tensor<2x2xsi64> {
  %c = arc.remi %a, %b : tensor<2x2xsi64>
  return %c : tensor<2x2xsi64>
}

}
