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

#include "Configuration.h"
#include "OutputFormatJUnit.h"
OutputFormatJUnit::OutputFormatJUnit(Configuration *config) : OutputFormat(config)
{
	flux << std::fixed << std::setprecision(2);
}

OutputFormatJUnit::~OutputFormatJUnit()
{}

std::string OutputFormatJUnit::getName()
{
	return "JUNIT";
}

std::string OutputFormatJUnit::getExt()
{
	return "";
}

bool OutputFormatJUnit::isEmpty()
{
	return stringify().size() == 0;
}

void OutputFormatJUnit::appendDisabled(std::string group, std::string name, std::string command, std::string data, double time)
{
	this->group = group;
	flux 	<< "\t<testcase classname=\"" << group <<"\" name=\""<< name << "\" time=\"" << time << "\" >\n"
		<< "\t\t<disabled message=\"" << command << "\" type=\"ValidityError\"/>\n";

	if(config->job().getLogLevel() == LOG_ALL || config->job().getLogLevel() == LOG_ONLY_FAILED)
		flux << "\t\t<system-out>" << data << "\n\t\t</system-out>\n";

	flux	<< "\t</testcase>\n"; 
}

void OutputFormatJUnit::appendError(std::string group, std::string name, std::string command, std::string data, double time)
{
	this->group = group;
	flux 	<< "\t<testcase classname=\"" << group <<"\" name=\""<< name << "\" time=\"" << time << "\" >\n"
		<< "\t\t<error message=\"" << command << "\" type=\"EnvironmentError\"/>\n";

	if(config->job().getLogLevel() == LOG_ALL || config->job().getLogLevel() == LOG_ONLY_FAILED)
		flux << "\t\t<system-out>" << data << "\n\t\t</system-out>\n";

	flux	<< "\t</testcase>\n"; 
}

void OutputFormatJUnit::appendFailure(std::string group, std::string name, std::string command, std::string data, double time)
{
	this->group = group;
	flux 	<< "\t<testcase classname=\"" << group <<"\" name=\""<< name << "\" time=\"" << time << "\" >\n"
		<< "\t\t<failure message=\"" << command << "\" type=\"ExecutionError\"/>\n";

	if(config->job().getLogLevel() == LOG_ALL || config->job().getLogLevel() == LOG_ONLY_FAILED)
		flux << "\t\t<system-out>" << data << "\n\t\t</system-out>\n";
	flux	<< "\t</testcase>\n"; 
}
void OutputFormatJUnit::appendSkipped(std::string group, std::string name, std::string command, std::string data, double time)
{
	this->group = group;
	flux 	<< "\t<testcase classname=\"" << group <<"\" name=\""<< name << "\" time=\"" << time << "\" >\n"
		<< "\t\t<skipped message=\"" << command <<"\" />\n";

	if(config->job().getLogLevel() == LOG_ALL || config->job().getLogLevel() == LOG_ONLY_FAILED)
		flux << "\t\t<system-out>" << data << "\n\t\t</system-out>\n";
	flux	<< "\t</testcase>\n"; 
}

void OutputFormatJUnit::appendSuccess(std::string group, std::string name, std::string command, std::string data, double time)
{
	this->group = group;
	flux 	<< "\t<testcase classname=\"" << group <<"\" name=\""<< name << "\" time=\"" << time << "\" >\n"
		<< "\t\t<success message=\"" << command << "\" />\n";

	if(config->job().getLogLevel() == LOG_ALL || config->job().getLogLevel() == LOG_ONLY_SUCCESS)
		flux << "\t\t<system-out>" << data << "\n\t\t</system-out>\n";

	flux	<< "\t</testcase>\n"; 
}

void OutputFormatJUnit::appendHeader(size_t err, size_t fail, size_t skip, size_t succ, double time)
{
	const std::string& temp = flux.str();
	flux.seekp(0);

	flux 	<< "<?xml version=\"1.0\"?>\n"
		<< "<testsuite name=\""<< this->group << "\" failures=\""<< fail << "\" success=\""<< succ << "\" errors=\""<< err << "\" skipped=\""<< skip <<  "\" time=\"" << time << "\">\n"
		<< temp;

}

void OutputFormatJUnit::appendFooter()
{
	flux 	<< "</testsuite>\n";
}

std::string OutputFormatJUnit::stringify()
{
	return flux.str();
}

void OutputFormatJUnit::clear()
{
	flux.str("");
	flux.clear();
}
