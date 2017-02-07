/****************************************************************************/
/*                                                                          */
/*                         Copyright or (C) or Copr.                        */
/*       Commissariat a l'Energie Atomique et aux Energies Alternatives     */
/*                                                                          */
/* Version : 1.2                                                            */
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

#ifndef FILTER_H
#define FILTER_H

#include "utils.h"
#include "FilterRule.h"

/// Enum of the filter entity mode
typedef enum eFilterMode {
	WHITELIST = 0, ///< current filter is considered as a whitelist
	BLACKLIST = 1, ///< current filter is considered as a blacklist
	WHATEVER = 3   ///< current filter have no effect (default)
} FilterMode;

/// Represents the entity group applied to jobs selection
/**
 * A filter is constituted with Filter items list (FilterRule) and a mode
 * applied to this list. The filter can be inclusive, exclusive or with no
 * effect. This list is generated from an input file given by confguration
 */
class Filter {
private:
	/************** MEMBERS **************/
	FilterMode type;                ///< mode to apply to current filter
	std::vector<FilterRule*> rules; ///< list of rules to apply
	
public:
	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	///standard constructor
	Filter();
	/// constructor by copy
	Filter ( const Filter& other );
	/// add a Filter item to the filter
	/**
	 * Help to construct the filter.
	 * \param[in] rule the new rule to insert
	 */
	void addRule(FilterRule* rule);
	///load filter items from a given file
	/**
	 * insert each line of the file as a rule in the current filter.
	 * Alos, apply the mode to fhe filter
	 * \param[in] fileName the file path used as a filter
	 * \param[in] mode The mode to apply to the filter
	 * \return <b>False</b> if file cannot be opened
	 * \return <b>True</b> otherwise
	 */
	bool fillFromFile(std::string fileName, FilterMode mode);
	/// virtual destructor to unset a filter
	virtual ~Filter();
	
	/****** CONST ******/
	///check if given job is valid according to the current filter
	/**
	 * We iterate on each rule and check if a rule matches with given job.
	 * According to mode, if it's whitelist, the job is kept, otherwise it's
	 * rejected.
	 * \param[in] name the job name to match with filter
	 * \return <b>True</b> if job need to be kept
	 * \return <b>False</b> otherwise
	 */
	bool accept(std::string name) const;
	/// display the current filter (used to debug)
	void display() const;
};

#endif // FILTER_H
