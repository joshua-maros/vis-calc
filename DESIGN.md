struct Number
- start out as f64 wrapper.

enum Value
- Scalar(Number)
- Vector {
    sort: VectorSort,
    values: Vec<Value>,
}
- Matrix { height: usize, values: Vec<Value>}

enum VectorSort
- PlainVector
- ComplexNumber
- Quaternion
- Octonion
- Multivector(usize)

struct DomainError {

}

trait Expression
- fn operands(&self) -> Vec<&dyn Expression>
- fn compute(&self, operands: Vec<Value>) -> Result<Value, DomainError>