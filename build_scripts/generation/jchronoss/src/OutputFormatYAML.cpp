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

#include "Configuration.h"
#include "OutputFormatYAML.h"

using namespace std;

OutputFormatYAML::OutputFormatYAML(Configuration *config) : OutputFormat(config)
{
	flux << std::fixed << std::setprecision(2);
}

OutputFormatYAML::~OutputFormatYAML()
{}

std::string OutputFormatYAML::getName()
{
	return "YAML";
}

std::string OutputFormatYAML::getExt()
{
	return ".yml";
}

bool OutputFormatYAML::isEmpty()
{
	return stringify().size() == 0;
}

void OutputFormatYAML::appendDisabled(std::string group, std::string name, std::string command, std::string data, double time)
{
	this->group = group;
	flux	<< "\t- " << name << ":" << endl
		<< "\t\t- status  : Disabled" << endl
		<< "\t\t- command : \"" << command << "\"" << endl
		<< "\t\t- time    : " << time << endl;
	/* we choose to not include logs in this format */
	UNUSED(data);
}
void OutputFormatYAML::appendError(std::string group, std::string name, std::string command, std::string data, double time)
{
	this->group = group;
	flux	<< "\t- " << name << ":" << endl
		<< "\t\t- status  : Error" << endl
		<< "\t\t- command : \"" << command << "\"" << endl
		<< "\t\t- time    : " << time << endl;
	/* we choose to not include logs in this format */
	UNUSED(data);
}

void OutputFormatYAML::appendFailure(std::string group, std::string name, std::string command, std::string data, double time)
{
	this->group = group;
	flux	<< "\t- " << name << ":" << endl
		<< "\t\t- status  : Failure" << endl
		<< "\t\t- command : \"" << command << "\"" << endl
		<< "\t\t- time    : " << time << endl;
	/* we choose to not include logs in this format */
	UNUSED(data);
}
void OutputFormatYAML::appendSkipped(std::string group, std::string name, std::string command, std::string data, double time)
{
	this->group = group;
	flux	<< "\t- " << name << ":" << endl
		<< "\t\t- status  : Skipped" << endl
		<< "\t\t- command : \"" << command << "\"" << endl
		<< "\t\t- time    : " << time << endl;
	/* we choose to not include logs in this format */
	UNUSED(data);
}

void OutputFormatYAML::appendSuccess(std::string group, std::string name, std::string command, std::string data, double time)
{
	this->group = group;
	flux	<< "\t- " << name << ":" << endl
		<< "\t\t- status  : Success" << endl
		<< "\t\t- command : \"" << command << "\"" << endl
		<< "\t\t- time    : " << time << endl;

	/* we choose to not include logs in this format */
	UNUSED(data);
}

void OutputFormatYAML::appendHeader(size_t err, size_t fail, size_t skip, size_t succ, double time)
{
	const std::string& temp = flux.str();
	flux.seekp(0);

	flux 	<< "package    : " << this->group << endl
	 	<< "total_time : " << time << endl
		<< "failures   : " << fail << endl
		<< "errors     : " << err << endl
		<< "skipped    : " << skip << endl
		<< "success    : " << succ << endl
		<< "testsuite  : " << endl
		<< temp;

}

void OutputFormatYAML::appendFooter()
{
}

std::string OutputFormatYAML::stringify()
{
	return flux.str();
}

void OutputFormatYAML::clear()
{
	flux.str("");
	flux.clear();
}
