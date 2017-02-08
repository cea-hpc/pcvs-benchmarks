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

#ifndef FILTERBOX_H
#define FILTERBOX_H

#include "utils.h"
#include "Filter.h"
#include "FileManager.h"

///A filterBox gathers all filters (blacklist, whitelist) into one object
/**
 * A filterbox is msotly used like an array with a list and a size.
 */
class FilterBox {
private:
	/************** MEMBERS **************/
	int nbFilters;                ///< number of filters of application
	std::vector<Filter*> filters; ///< list of filters
	
public:
	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	///standard constructor
	FilterBox();
	///constructor by copy
	FilterBox ( const FilterBox& other );
	/// add a filter to the application
	/**
	 * add the given file as a new filter in database
	 * \param[in] file the file to upload
	 * \param[in] mode mode to apply to this filter
	 * \return <b>True</b> if succeded
	 * \return <b>False</b> otherwise
	 */
	bool addFilter(FileManager* file, FilterMode mode);
	/// virtual destructor to unset FilterBox
	virtual ~FilterBox();
	
	/****** CONST ******/
	///check if a job is a valid one
	/**
	 * Check if the job matches in the whole database of filters.
	 * \param[in] name the job name
	 * \return <b>True</b> if job is valid
	 * \return <b>False</b> otherwise
	 */
	bool accept(std::string name) const;
	/// display a filterBox (used to debug)
	void display() const;
};

#endif // FILTERBOX_H
