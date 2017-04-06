#ifndef CHARSTAR8_HH
#define CHARSTAR8_HH
#include <iostream>

typedef struct {

char data[8];

} CharStar8;

inline std::ostream&
operator<<(std::ostream& os, const CharStar8& cs8){
  for (int i = 0; i < 8; ++i){
      os << cs8.data[i] ;
  }
  return os;
}

#endif
