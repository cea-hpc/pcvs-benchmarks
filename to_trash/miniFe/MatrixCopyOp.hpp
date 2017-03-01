/*
//@HEADER
// ************************************************************************
// 
//               HPCCG: Simple Conjugate Gradient Benchmark Code
//                 Copyright (2006) Sandia Corporation
// 
// Under terms of Contract DE-AC04-94AL85000, there is a non-exclusive
// license for use of this work by or on behalf of the U.S. Government.
// 
// This library is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation; either version 2.1 of the
// License, or (at your option) any later version.
//  
// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//  
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// Questions? Contact Michael A. Heroux (maherou@sandia.gov) 
// 
// ************************************************************************
//@HEADER
*/

#ifndef _MatrixCopyOp_hpp_
#define _MatrixCopyOp_hpp_

template<typename MatrixType>
struct MatrixCopyOp {
  typedef typename MatrixType::GlobalOrdinalType GlobalOrdinalType;
  typedef typename MatrixType::LocalOrdinalType LocalOrdinalType;
  typedef typename MatrixType::ScalarType ScalarType;

  const GlobalOrdinalType* src_rows;
  const LocalOrdinalType*  src_rowoffsets;
  const GlobalOrdinalType* src_cols;
  const ScalarType*        src_coefs;

  GlobalOrdinalType* dest_rows;
  LocalOrdinalType*  dest_rowoffsets;
  GlobalOrdinalType* dest_cols;
  ScalarType*        dest_coefs;
  int n;

  void operator()(int i) {
    dest_rows[i] = src_rows[i];
    dest_rowoffsets[i] = src_rowoffsets[i];
    for(int j=src_rowoffsets[i]; j<src_rowoffsets[i+1]; ++j) {
      dest_cols[j] = src_cols[j];
      dest_coefs[j] = src_coefs[j];
    }
  }
};

#endif
