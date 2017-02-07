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

#ifndef JOBCONSTRAINT_H
#define JOBCONSTRAINT_H

#include "utils.h"

/// represents a job constraint applied on a job
/**
 * A Job constraint is a rule which is applied to some jobs. Jobs with same constraints
 * are grouped together and some actions can be started on this jobs group specifically
 * (e.g. compilations running, etc...)
 */
class JobConstraint {
private:
	/************** MEMBERS **************/
	std::string name;        ///< constraint name
	
public:
	/************** STATICS **************/
	/****** CONST ******/
	static const std::string COMPILATION_TAG; ///< defined known constraint : compilation
	static const std::string FAST_TAG; ///< defined known constraint : fast jobs
	static const std::string ALONE_TAG; ///< defined known constraint : alone tags
	
	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	//construct a JobConstraint with a given name
	/**
	 * \param[in] name constraint name
	 */
	JobConstraint( std::string name );
	/****** CONST ******/
	/// check if current constraint belongs to the group defined by the parameter
	/**
	 * \param[in] group the group name used to check constraint matching
	 * \return <b>True</b> if constraint belongs to the same as given id
	 * \return <b>False</b> otherwise
	 */
	bool isBelongingTo(std::string group) const;
	/// operaator overloading: JobConstraint comparison
	/**
	 * check if two constraints are the same
	 * \param[in] other the constraint to compare with
	 * \return <b>True</b> if they are the same
	 * \return <b>False</b> otherwise
	 */
	bool operator==(const JobConstraint& other) const;
	/// get constraint name
	/**
	 * \return a string containing the constraint name
	 */
	std::string getName() const;
	/// print a jobConstraint on the screen
	void display() const;
	/// default destructor
	virtual ~JobConstraint();
};

#endif // JOBCONSTRAINT_H
