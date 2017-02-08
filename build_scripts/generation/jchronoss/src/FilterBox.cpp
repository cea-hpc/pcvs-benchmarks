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

#include "FilterBox.h"
using namespace std;

FilterBox::FilterBox() : nbFilters(0) {}

FilterBox::FilterBox ( const FilterBox& other ) {
	nbFilters = other.nbFilters;
	filters = other.filters;
}

FilterBox::~FilterBox() {
	for(vector<Filter*>::iterator it = filters.begin(); it != filters.end();){
		safeFree(*it);
		it = filters.erase(it);
	}
	nbFilters=0;
}

bool FilterBox::accept ( std::string name ) const {
	bool valid = true;
	vector<Filter*>::const_iterator current = filters.begin();

	while(valid && current != filters.end()){
		valid = (*current)->accept(name);
		current++;
	}
	return valid;
}
bool FilterBox::addFilter ( FileManager* file, FilterMode mode ) {
	Filter* new_filter = new Filter;
	bool ok;
	
	assert(file != NULL);
	assert(new_filter != NULL);
	
	ok = new_filter->fillFromFile(file->toString(), mode);
	if(ok){
		nbFilters++;
		filters.push_back(new_filter);
	} else {
		safeFree(new_filter);
		new_filter = NULL;
	}
	
	return ok;	
}

void FilterBox::display() const {
	vector<Filter*>::const_iterator current;
	cout << "Filters:\n";
	for(current = filters.begin(); current != filters.end(); current++)
		(*current)->display();
	
}

