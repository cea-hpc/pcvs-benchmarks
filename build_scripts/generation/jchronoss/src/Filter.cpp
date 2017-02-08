/****************************************************************************/
/*                                                                          */
/*                         Copyright or (C) or Copr.                        */
/*       Commissariat a l'Energie Atomique et aux Energies Alternatives     */
/*                                                                          */
/* Version : 2.0                                                            */
/* Date    : Tue Jul 22 13:28:10 CEST 2014                                  */
/* Ref ID  : IDDN.FR.001.160040.000.S.P.2015.000.10800                      */
/* Author  : Julien Adam <julien.adam@cea.fr>                               */
/*           Marc Perache <marc.perache@cea.fr>                             */
/*                                                                          */
/* This file is part of JCHRONOSS software.                                 */
/*                                                                          */
/* This software is governed by the CeCILL-C license under French law and   */
/* abiding by the rules of distribution of free software.  You can  use,    */
/* modify and/or redistribute the software under the terms of the CeCILL-C  */
/* license as circulated by CEA, CNRS and INRIA at the following URL        */
/* "http://www.cecill.info".                                                */
/*                                                                          */
/* As a counterpart to the access to the source code and  rights to copy,   */
/* modify and redistribute granted by the license, users are provided only  */
/* with a limited warranty  and the software's author,  the holder of the   */
/* economic rights,  and the successive licensors  have only  limited       */
/* liability.                                                               */
/*                                                                          */
/* In this respect, the user's attention is drawn to the risks associated   */
/* with loading,  using,  modifying and/or developing or reproducing the    */
/* software by the user in light of its specific status of free software,   */
/* that may mean  that it is complicated to manipulate,  and  that  also    */
/* therefore means  that it is reserved for developers  and  experienced    */
/* professionals having in-depth computer knowledge. Users are therefore    */
/* encouraged to load and test the software's suitability as regards their  */
/* requirements in conditions enabling the security of their systems and/or */
/* data to be ensured and,  more generally, to use and operate it in the    */
/* same conditions as regards security.                                     */
/*                                                                          */
/* The fact that you are presently reading this means that you have had     */
/* knowledge of the CeCILL-C license and that you accept its terms.         */
/*                                                                          */
/****************************************************************************/

#include "Filter.h"
using namespace std;

Filter::Filter() : 
	type(WHATEVER)
{}

Filter::Filter ( const Filter& other ) :
	type(other.type), rules(other.rules)
{}

Filter::~Filter() {
	for(vector<FilterRule*>::iterator it = rules.begin(); it != rules.end();){
		safeFree(*it);
		it = rules.erase(it);
	}
}

bool Filter::accept ( std::string name ) const {
	bool valid = false;
	if(type == WHATEVER) return true;
	
	//check if name matchs with at list on rule
	for(size_t i=0; i < rules.size() && !valid; i++){
		valid = (name.find(rules[i]->getName()) != string::npos);
	}
	
	if(type == BLACKLIST) valid = !valid;
	
	return valid;
}
void Filter::addRule ( FilterRule* rule ) {
	assert(rule != NULL);
	rules.push_back(rule);
}

bool Filter::fillFromFile ( std::string fileName, FilterMode mode ) {
	FilterRule* current;
	
	std::ifstream file(fileName.c_str(), ios::in);
	if(!file){
		printWarning("Unable to find the filter file "+fileName+" ! This filter will be ignored !");
		return false;
	}
	
	current = new FilterRule;
	assert(current != NULL);
	while(file >> *current){	
		addRule(current);
		current = new FilterRule;
		assert(current != NULL);
	}
	safeFree(current);
	current = NULL;
	
	type = mode;
	file.close();
	return true;
}

void Filter::display() const {
	vector<FilterRule*>::const_iterator cur;
	string stype = (type == WHITELIST) ? "WHITELIST" : "BLACKLIST";
	cout << "\t- Mode = " << stype << endl;
	for(cur = rules.begin(); cur != rules.end(); cur++)
		cout << "\t\t- "<< (*cur)->getName() << endl;
}

