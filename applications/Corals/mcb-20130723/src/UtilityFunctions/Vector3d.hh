//----------------------------------*-C++-*----------------------------------//
// Copyright 2009 Lawrence Livermore National Security, LLC
// All rights reserved.
//---------------------------------------------------------------------------//

// This work performed under the auspices of the U.S. Department of Energy by
// Lawrence Livermore National Laboratory under Contract DE-AC52-07NA27344

//  DISCLAIMER
//  This work was prepared as an account of work sponsored by an agency of the
//  United States Government. Neither the United States Government nor the
//  Lawrence Livermore National Security, LLC, nor any of their employees,
//  makes any warranty, express or implied, including the warranties of
//  merchantability and fitness for a particular purpose, or assumes any
//  legal liability or responsibility for the accuracy, completeness, or
//  usefulness of any information, apparatus, product, or process disclosed,
//  or represents that its use would not infringe privately owned rights.

/**********************************************************************
 *
 * Purpose:
 *          This file contains the interface to the Vector3d class.
 *          This class is used to define a 3d point in space relative
 *          to some origin of a coordinate system.
 * 
 * Methods:
 *
 *    Constructors:
 *          Vector3d()      - Intializes all data members to 0
 *          Vector3d(x,y,z) - Initializes data members to passed in
 *                            values
 *    Destructors:
 *          Vector3d~()     - Currently a no-op.
 *
 *    Vector3d + Vector3d   - Adds 2 vectors and returns the new 
 *                            vector
 *    Vector3d - Vector3d   - Subtracts 2 vectors and returns the new 
 *                            vector
 *    Vector3d * double     - Multiplies a scalar with each member of 
 *                            the vector and returns the new vector
 *    double   * Vector3d   - Multiplies a scalar with each member of 
 *                            the vector and returns the new vector
 *    Vector3d / double     - Divides each member of the vector by a
 *                            scalar and returns the new vector
 *    - Vector3d            - returns negative of operand vector
 *    Vector3d = Vector3d   - Assigns vector to vector on left hand 
 *                            side
 *    Vector3d += Vector3d  - Adds 2 vectors and stores the result
 *                            in the left hand side
 *    Vector3d -= Vector3d  - Subtracts 2 vectors and stores the result
 *                            in the left hand side
 *    Vector3d *= double    - Multiplies a scalar with the vector and 
 *                            stores the result in the left hand side
 *    Vector3d /= double    - Divides the vector by the scalar and 
 *                            stores the result in the left hand side
 *    GetX()                - Returns the value of the x member
 *    GetY()                - Returns the value of the y member
 *    GetZ()                - Returns the value of the z member
 *    SetX(double)          - Sets the value of the x member to double
 *    SetY(double)          - Sets the value of the y member to double
 *    SetZ(double)          - Sets the value of the z member to double
 *    Zero()                - Zeros out the vector
 *    dot(Vector3d)         - Returns the dot product of 2 vectors
 *    Vector3d cross(Vector3d) - Returns Vector3d cross product of
 *                               this with argument
 *    magnitude(Vector3d)    - Returns the magnitude of a vector
 *    magnitude2(Vector3d)   - Returns the magnitude squared of a vector
 *
 * Gotchas:
 *          This class must have ALL member functions inlined.  This
 *          is necessary because we use the array template class for
 *          this class, and we write out these arrays to the restart
 *          dumps, and the C code.  We CAN NOT have function pointers
 *          appearing in the class because the C code expects only
 *          the members x y z to appear.  This also forces us to
 *          not allow this class to be derived from any class, 
 *          including the Base class, because a placeholder is put in
 *          the class for the derived entries.
 *
 *          This class must stay consistant with the type vector3d
 *          in the C code. 
 *
 * Modified:    9 July 98 by PAKA for cross casting & construction
 *
 *              Fri Aug 28 18:27:32 PDT 1998 by Katherine Price
 *                      - reorganized file and removed 3d -> 2d casts
 *
 *********************************************************************/

#ifndef VECTOR3D_HH
#define VECTOR3D_HH

#include <cmath>
#include <iostream>
#include <float.h>

#include "ASSERT.hh"

namespace Vector3d_namespace
{

class Vector3d {
public:

   typedef double    ScalarType;
   typedef double    InnerProductType;

   static int numberOfDimensions() { return 3; }
   static Vector3d zero() { return Vector3d(0.0, 0.0, 0.0); }
   static Vector3d one() { return Vector3d(1.0, 1.0, 1.0); }
   static Vector3d epsilon() { return Vector3d(FLT_MIN, FLT_MIN, FLT_MIN); }

   Vector3d(); // default constructor -- might need this for OpenMP
   explicit     Vector3d(const double val); // no conversions allowed
   Vector3d(const double xval, const double yval,
            const double zval);
   Vector3d(const Vector3d &vector3d);
   ~Vector3d();

   // Return the unit vector in the direction of this vector.
   Vector3d     unitVector() const;

   Vector3d     operator+(const Vector3d &vector3d) const;
   Vector3d     operator-(const Vector3d &vector3d) const;
   Vector3d     operator*(const double scalar) const;
   Vector3d     operator/(const double scalar) const;
   Vector3d     operator-() const;
    
   Vector3d&    operator=(const Vector3d &vector3d);
   Vector3d&    operator+=(const Vector3d &vector3d);
   Vector3d&    operator-=(const Vector3d &vector3d);
   Vector3d&    operator*=(const double scalar);
   Vector3d&    operator/=(const double scalar);

   int          compare(const Vector3d &vector3d) const;
   
   bool         operator==(const Vector3d &vector3d) const;
   bool         operator!=(const Vector3d &vector3d) const;
   bool         operator< (const Vector3d &vector3d) const;
   bool         operator<=(const Vector3d &vector3d) const;
   bool         operator> (const Vector3d &vector3d) const;
   bool         operator>=(const Vector3d &vector3d) const;

   Vector3d     cross(const Vector3d &vector3d) const;
   double       dot(const Vector3d &vector3d) const;
   double       magnitude() const;
   double       magnitude2() const;

   void         Zero();

   void         SetX(const double xval);
   void         SetY(const double yval);
   void         SetZ(const double zval);

   double       GetX() const;
   double       GetY() const;
   double       GetZ() const;

   double&              operator[](const unsigned index);
   const double&        operator[](const unsigned index) const;
   double&              operator()(const unsigned index) { return (*this)[index]; }
   const double&        operator()(const unsigned index) const { return (*this)[index]; }

   friend Vector3d      operator*(const double scalar,
                                  const Vector3d& vector3d);
   friend std::ostream& operator<<(std::ostream& os,
                                   const Vector3d& vector3d);

private:
   double x;
   double y;
   double z;
};

//---------------------------------------------------------------------
// Vector3d::Vector3d(val)
//---------------------------------------------------------------------
inline
Vector3d::Vector3d() :
   x(0.0), y(0.0), z(0.0)
{}

//---------------------------------------------------------------------
// Vector3d::Vector3d(val)
//---------------------------------------------------------------------
inline
Vector3d::Vector3d(const double val) :
   x(val), y(val), z(val)
{}

//---------------------------------------------------------------------
// Vector3d::Vector3d(xval, yval, zval)
//---------------------------------------------------------------------
inline
Vector3d::Vector3d(const double xval, const double yval, const double zval) :
   x(xval), y(yval), z(zval)
{}   

//---------------------------------------------------------------------
// Vector3d::Vector3d(vector3d)
//---------------------------------------------------------------------
inline
Vector3d::Vector3d(const Vector3d &vector3d)
{
   x = vector3d.x; y = vector3d.y; z = vector3d.z;
} 
  
//---------------------------------------------------------------------
// Vector3d::~Vector3d()
//---------------------------------------------------------------------
inline
Vector3d::~Vector3d()
{}

//---------------------------------------------------------------------
// Vector3d::unitVector()
//---------------------------------------------------------------------
inline
Vector3d
Vector3d::unitVector() const
{
   const double mag = magnitude();
   if (mag > 1.0e-20) {
      return (*this)/mag;
  } else {
     return Vector3d(1.0, 0.0, 0.0);
  }
}

//---------------------------------------------------------------------
// Vector3d::operator+(vector3d)
//---------------------------------------------------------------------
inline Vector3d
Vector3d::operator+(const Vector3d &vector3d) const
{
   return Vector3d(x + vector3d.x, y + vector3d.y, z + vector3d.z);
}  

//---------------------------------------------------------------------
// Vector3d::operator-(vector3d)
//---------------------------------------------------------------------
inline Vector3d
Vector3d::operator-(const Vector3d &vector3d) const
{
   return Vector3d(x - vector3d.x, y - vector3d.y, z - vector3d.z);
}  

//---------------------------------------------------------------------
// Vector3d::operator*(scalar)
//---------------------------------------------------------------------
inline Vector3d
Vector3d::operator*(const double scalar) const
{
   return Vector3d(x*scalar, y*scalar, z*scalar);
}  

//---------------------------------------------------------------------
// Vector3d::operator/(scalar)
//---------------------------------------------------------------------
inline Vector3d
Vector3d::operator/(const double scalar) const
{
   ASSERT(scalar != 0);
   return Vector3d(x/scalar, y/scalar, z/scalar);
}

//---------------------------------------------------------------------
// Vector3d::operator-()
//---------------------------------------------------------------------
inline Vector3d
Vector3d::operator-() const
{
   return Vector3d(-x, -y, -z);
}

//---------------------------------------------------------------------
// Vector3d::operator=()
//---------------------------------------------------------------------
inline Vector3d&
Vector3d::operator=(const Vector3d &vector3d)
{
   x = vector3d.x; y = vector3d.y; z = vector3d.z;
   return *this;
}  

//---------------------------------------------------------------------
// Vector3d::operator+=(vector3d)
//---------------------------------------------------------------------
inline Vector3d&
Vector3d::operator+=(const Vector3d &vector3d)
{
   x += vector3d.x; y += vector3d.y; z += vector3d.z;
   return *this;
}  

//---------------------------------------------------------------------
// Vector3d::operator-=(vector3d)
//---------------------------------------------------------------------
inline Vector3d&
Vector3d::operator-=(const Vector3d &vector3d)
{
   x -= vector3d.x; y -= vector3d.y; z -= vector3d.z;;
   return *this;
}  

//---------------------------------------------------------------------
// Vector3d::operator*=(scalar)
//---------------------------------------------------------------------
inline Vector3d&
Vector3d::operator*=(const double scalar)
{
   x *= scalar; y *= scalar; z *= scalar;;
   return *this;
}
   
//---------------------------------------------------------------------
// Vector3d::operator*=(scalar)
//---------------------------------------------------------------------
inline Vector3d&
Vector3d::operator/=(const double scalar)
{
   ASSERT(scalar != 0);
   x /= scalar; y /= scalar; z /= scalar;
   return *this;
}

//---------------------------------------------------------------------
// Vector3d::compare(vector3d)
//---------------------------------------------------------------------
inline int
Vector3d::compare(const Vector3d &vector3d) const
{
   // Return -1 if this < vector3d, 1 if this > vector3d,
   // or 0 if this == vector3d.
   return (x < vector3d.x ? -1 : x > vector3d.x ? 1 :
           y < vector3d.y ? -1 : y > vector3d.y ? 1 :
           z < vector3d.z ? -1 : z > vector3d.z ? 1 :
           0);
}

//---------------------------------------------------------------------
// Vector3d::operator==(vector3d)
//---------------------------------------------------------------------
inline bool
Vector3d::operator==(const Vector3d &vector3d) const
{
   return (x == vector3d.x && y == vector3d.y && z == vector3d.z);
}

//---------------------------------------------------------------------
// Vector3d::operator!=(vector3d)
//---------------------------------------------------------------------
inline bool
Vector3d::operator!=(const Vector3d &vector3d) const
{
   return !(*this == vector3d);
}

//---------------------------------------------------------------------
// Vector3d::operator< (vector3d)
//---------------------------------------------------------------------
inline bool
Vector3d::operator< (const Vector3d &vector3d) const
{
   return (compare(vector3d) < 0);
}
   
//---------------------------------------------------------------------
// Vector3d::operator<=(vector3d)
//---------------------------------------------------------------------
inline bool
Vector3d::operator<=(const Vector3d &vector3d) const
{
   return (compare(vector3d) <= 0);
}
   
//---------------------------------------------------------------------
// Vector3d::operator> (vector3d)
//---------------------------------------------------------------------
inline bool
Vector3d::operator> (const Vector3d &vector3d) const
{
   return (compare(vector3d) > 0);
}
   
//---------------------------------------------------------------------
// Vector3d::operator>=(vector3d)
//---------------------------------------------------------------------
inline bool
Vector3d::operator>=(const Vector3d &vector3d) const
{
   return (compare(vector3d) >= 0);
}
   
//---------------------------------------------------------------------
// Vector3d::cross(vector3d)
//---------------------------------------------------------------------
inline Vector3d
Vector3d::cross(const Vector3d &vector3d) const
{
   return Vector3d(y*vector3d.z - z*vector3d.y,
                   z*vector3d.x - x*vector3d.z,
                   x*vector3d.y - y*vector3d.x);
}  

//---------------------------------------------------------------------
// Vector3d::dot(vector3d)
//---------------------------------------------------------------------
inline double
Vector3d::dot(const Vector3d &vector3d) const
{
   return x*vector3d.x + y*vector3d.y + z*vector3d.z;
}  
   
//---------------------------------------------------------------------
// Vector3d::magnitude()
//---------------------------------------------------------------------
inline double
Vector3d::magnitude() const
{
   return std::sqrt(dot(*this));
}
   
//---------------------------------------------------------------------
// Vector3d::magnitude2()
//---------------------------------------------------------------------
inline double
Vector3d::magnitude2() const
{
   return dot(*this);
}

//---------------------------------------------------------------------
// Vector3d::Zero()
//---------------------------------------------------------------------
inline void
Vector3d::Zero()
{
   x = 0.0; y = 0.0; z = 0.0;
}  
   
//---------------------------------------------------------------------
// Vector3d::SetX(xval)
//---------------------------------------------------------------------
inline void
Vector3d::SetX(const double xval)
{
   x = xval;
}
   
//---------------------------------------------------------------------
// Vector3d::SetY(yval)
//---------------------------------------------------------------------
inline void
Vector3d::SetY(const double yval)
{
   y = yval;
}
   
//---------------------------------------------------------------------
// Vector3d::SetZ(zval)
//---------------------------------------------------------------------
inline void
Vector3d::SetZ(const double zval)
{
   z = zval;
}

//---------------------------------------------------------------------
// Vector3d::GetX()
//---------------------------------------------------------------------
inline double
Vector3d::GetX() const
{
   return x;
}

//---------------------------------------------------------------------
// Vector3d::GetY()
//---------------------------------------------------------------------
inline double
Vector3d::GetY() const
{
   return y;
}

//---------------------------------------------------------------------
// Vector3d::GetZ()
//---------------------------------------------------------------------
inline double
Vector3d::GetZ() const
{
   return z;
}
   
//---------------------------------------------------------------------
// Vector3d::operator[](index)
//---------------------------------------------------------------------
inline double&
Vector3d::operator[](const unsigned index)
{
   ASSERT(index < 3);

   return (index == 0 ? x : index == 1 ? y : z);
}

//---------------------------------------------------------------------
// Vector3d::operator[](index)
//---------------------------------------------------------------------
inline const double&
Vector3d::operator[](const unsigned index) const
{
   ASSERT(index < 3);

   return (index == 0 ? x : index == 1 ? y : z);
}

//---------------------------------------------------------------------
// operator*(scalar, vector3d)
//---------------------------------------------------------------------
inline Vector3d
operator*(const double scalar, const Vector3d &vector3d)
{
   return Vector3d(scalar*vector3d.x, scalar*vector3d.y, scalar*vector3d.z);
}

//---------------------------------------------------------------------
// operator<<(os, vector3d)
//---------------------------------------------------------------------
inline std::ostream&
operator<<(std::ostream& os, const Vector3d& vector3d)
{
   os << "(" << vector3d.x << ", " << vector3d.y << ", " << vector3d.z << ")";
   return os;
}

} // end namespace IMC_namespace

#endif

//-----------------------------------------------------------------------//
//                              end of Vector3d.hh
//-----------------------------------------------------------------------//



