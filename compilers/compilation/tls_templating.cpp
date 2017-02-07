/*
 * Check found when TBB was ported over MPC.
 * Up to the version 5.2.0 (without cherry-picked patchs), this
 * bug is reproducible with the following code.
 * It does not support thread_local variables in template definitions.
 * Here, it uses struct, but it probably fail with any type of 
 * container : litteral types, classes,...
 */

#include <iostream>

struct Heavy { Heavy(){} };

template <typename T>
struct A {
  virtual void foo() { v; }
  static thread_local Heavy v;
};

template <typename T>
thread_local Heavy A<T>::v;

struct E {};
struct F {};

int main() {
  A<E> foo;
  A<F> bar;
  bar.v;
}
