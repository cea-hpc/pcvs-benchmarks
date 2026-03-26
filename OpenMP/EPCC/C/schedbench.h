/****************************************************************************
*                                                                           *
*             OpenMP MicroBenchmark Suite - Version 4.0                     *
*                                                                           *
*                            produced by                                    *
*                                                                           *
*                             Mark Bull                                     *
*                                                                           *
*                                at                                         *
*                                                                           *
*                   EPCC, University of Edinburgh                           *
*                                                                           *
*                    email: m.bull@epcc.ed.ac.uk                            *
*                                                                           *
*                                                                           *
*      This version copyright (c) The University of Edinburgh, 2023.        *
*                                                                           *
*                                                                           *
*  Licensed under the Apache License, Version 2.0 (the "License");          *
*  you may not use this file except in compliance with the License.         *
*  You may obtain a copy of the License at                                  *
*                                                                           *
*      http://www.apache.org/licenses/LICENSE-2.0                           *
*                                                                           *
*  Unless required by applicable law or agreed to in writing, software      *
*  distributed under the License is distributed on an "AS IS" BASIS,        *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *
*  See the License for the specific language governing permissions and      *
*  limitations under the License.                                           *
*                                                                           *
****************************************************************************/

#ifndef SCHEDBENCH_H
#define SCHEDBENCH_H

void refer(void);

void teststatic(void);

void teststaticmono(void);


void teststaticn(void);

void teststaticnmono(void);


void testdynamicn(void);

void testdynamicnmono(void);

void testdynamicnnonmono(void);

void testguidedn(void);

void testguidednmono(void);

void testguidednnonmono(void);

void testruntime(void);

void testruntimemono(void);


void taskloopn(void);

#endif //SCHEDBENCH_H
