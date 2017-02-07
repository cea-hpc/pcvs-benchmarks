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

#ifndef FILTERRULE_H
#define FILTERRULE_H

#include "utils.h"

/// represents a most little unit of filtering in database
/**
 * A filter rule is the struct which allows to match a job with a constraint.
 * Here, a filter rule is constituted like Java class identification:
 * package.subpackage.subpackage.jobName. This path is re-constructed from job
 * path and stored in a rule. To ensure a job validity, we compare job path to the rule.
 */
class FilterRule {
private:
	/************** MEMBERS **************/
	std::string name;   ///< the path chain used to compare
	
public:
	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	///standard constructor
	explicit FilterRule();
	///Create a rule from a given string
	/**
	 * \param[in] rule the rule containing a job path
	 */
	FilterRule(std::string rule);
	/// read a rule from file
	/**
	 * \param[in] cin the file where read (need to be open)
	 * \param[in] obj the rule  to fill
	 * \return stream reference to concatenate instructions
	 */
	friend std::istream& operator>>(std::istream& cin, FilterRule& obj);
	/// set current rule with given name
	/**
	 * \param[in] name job path to set the current filter rule
	 */
	void setName(std::string name);
	
	/****** CONST ******/
	///rule getter
	/**
	 * \return the job path
	 */
	std::string getName() const;
};


#endif // FILTERRULE_H
