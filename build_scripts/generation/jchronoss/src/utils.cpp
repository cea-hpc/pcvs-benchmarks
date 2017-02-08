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

#include "utils.h"

using namespace std;

/************* GLOBAL VARS ***********/
///store timers in order to time measurement
static double previous_date = 0.0;
std::string type = "";
double getCurrentDate() {
	double time=0.0;
	struct timeval tic;

	gettimeofday(&tic, NULL);

	time = (double)tic.tv_sec+ (double)(tic.tv_usec/1000000.0);
	return time;
}

void printLine ( string prefix, string format, ... ) {
	va_list args;
	int cursor=0;
	char output[1024];

	cursor += sprintf(output, "%s", prefix.c_str());
	va_start(args, format);

	cursor += vsprintf(output+cursor, format.c_str(), args);
	cursor += sprintf(output+cursor, "%s", (char*) COLOR_NORM );

	va_end(args);
	if(write(1, output, cursor) != cursor)
	{
		printError("Unable to write (printLine routine)", JE_UNKNOWN);
	}
}

int safeRead(FILE* fd, void * buf, int size) {
	int total_count=0, current_count=0;
	
	/* reading while not all the characters are been read */
	while(total_count < size){
		current_count=fread((char*)buf+total_count, sizeof(char), size - total_count, fd);
		/* if there are nothing to read anymore */
		if(current_count == 0){
			break;
			/* error case */
		}else if( current_count < 0){
			/* if signal interruption, reading continuation */
			if(errno == EINTR) continue;
			/* aborting if others errors */
			else{
				printf("Error reading %p %d/%d\n", buf, total_count, size);
				exit(JE_UNKNOWN);
			}
		}
		total_count += current_count;
	}
	return total_count;
}

bool matchWith (std::string chain,std::string prefix ) {
	return (chain.compare(0, prefix.size(), prefix, 0, prefix.size()) == 0);
}

std::string* splitArgumentPrefix ( std::string chain, std::string prefix ) {
	chain.erase(0, prefix.size());
	return new string(chain);
}

void HTMLEncoding(string& src){
	string buffer;
	buffer.reserve(src.size());
	for(std::string::size_type i = 0 ; i != src.size(); i++)
	{
		switch(src[i]){
			case '&':  buffer.append("&amp;"); break;
			case '\"': buffer.append("&quot;"); break;
			case '\'': buffer.append("&apos;"); break;
			case '<':  buffer.append("&lt;"); break;
			case '>':  buffer.append("&gt;"); break;
// 			case 'é':  buffer.append("&eacute;"); break;
// 			case 'è':  buffer.append("&egrave;"); break;
// 			case 'à':  buffer.append("&agrave;"); break;
			default :  buffer.append(&src[i], 1); break;
		}
	}
	
	src.swap(buffer);
}

void measureTimeInterval(bool init, std::string info){
	ostringstream flux;
	if(init){
		previous_date = getCurrentDate();
	} else {
		double res = getCurrentDate() - previous_date;
		flux << "Measure " << info << " = " << std::setprecision(5) << res << " sec";
		printInfo(flux.str());
	}
}

size_t hash_fn(string toHash){
	size_t value = 0;
	for(size_t i=0; i<toHash.size(); i++)
		value += (int)toHash[i]*i;
	return value;
}

void printHeader() {
	cout << COLOR_NRUN "+--------------------------------------------------------------+" COLOR_NORM << endl;
	cout << COLOR_NRUN "|   " COLOR_INFO "STATUS" COLOR_NORM "    |   " COLOR_INFO "ID  /  SZ" COLOR_NORM "   | " COLOR_INFO "LEFT" COLOR_NORM " |    " COLOR_INFO "TIME" COLOR_NORM "    |    " COLOR_INFO "NAME" COLOR_NORM "    |" COLOR_NORM << endl;
	cout << COLOR_NRUN "+--------------------------------------------------------------+" COLOR_NORM << endl;
}

void banner(){
#ifdef ENABLE_COLOR
	cout	<< COLOR_FAIL "    ___ " COLOR_URUN " _____  _   _ " COLOR_PASS "______  _____  _   _  _____  _____        \n"
		<< COLOR_FAIL "   |_  |" COLOR_URUN "/  __ \\| | | |" COLOR_PASS "| ___ \\|  _  || \\ | ||  _  |/  ___|       \n"
		<< COLOR_FAIL "     | |" COLOR_URUN "| /  \\/| |_| |" COLOR_PASS "| |_/ /| | | ||  \\| || | | |\\ `--.        \n"
		<< COLOR_FAIL "     | |" COLOR_URUN "| |    |  _  |" COLOR_PASS "|    / | | | || . ` || | | | `--. \\_____  \n"
		<< COLOR_FAIL " /\\__/ /" COLOR_URUN "| \\__/\\| | | |" COLOR_PASS "| |\\ \\ \\ \\_/ /| |\\  |\\ \\_/ //\\__/ /  ___| \n"
		<< COLOR_FAIL " \\____/ " COLOR_URUN " \\____/\\_| |_/" COLOR_PASS "\\_| \\_| \\___/ \\_| \\_/ \\___/ \\____/\\ `--.  \n"
		<< COLOR_FAIL "        " COLOR_URUN "              " COLOR_PASS "                                   `--. \\ \n"
		<< COLOR_FAIL "        " COLOR_URUN "              " COLOR_PASS "                                  /\\__/ / \n"
		<< COLOR_FAIL "        " COLOR_URUN "              " COLOR_PASS "                                  \\____/  \n"
		<< COLOR_NORM "\n";

#else
	cout	<<"    ___  _____  _   _ ______  _____  _   _  _____  _____        \n"
		<<"   |_  |/  __ \\| | | || ___ \\|  _  || \\ | ||  _  |/  ___|       \n"
		<<"     | || /  \\/| |_| || |_/ /| | | ||  \\| || | | |\\ `--.        \n"
		<<"     | || |    |  _  ||    / | | | || . ` || | | | `--. \\_____  \n"
		<<" /\\__/ /| \\__/\\| | | || |\\ \\ \\ \\_/ /| |\\  |\\ \\_/ //\\__/ /  ___| \n"
		<<" \\____/  \\____/\\_| |_/\\_| \\_| \\___/ \\_| \\_/ \\___/ \\____/\\ `--.  \n"
		<<"                                                         `--. \\ \n"
		<<"                                                        /\\__/ / \n"
		<<"                                                        \\____/  \n"
		<<"\n";
#endif
}

string convertDate(double elapsed){
	stringstream flux;
	int seconds = elapsed;
	int minutes = seconds / 60;
	int hours = minutes / 60;
	int days = hours / 24;
	int weeks = days / 7;	
	if(weeks > 0.0) flux << weeks << " week(s), ";
	if(days > 0.0) flux << (days%7) << " day(s), ";
	if(hours > 0.0) flux << (hours%24) << " hour(s), ";
	if(minutes > 0.0) flux << (minutes%60) << " minute(s), ";
	if(seconds > 0.0) flux << (seconds%60) << " second(s)";
	if(flux.str().empty())
		flux << "< 1 sec";
	return flux.str();
}

string& replace(std::string& str, std::string from, std::string to)
{
	size_t start = 0;
	while((start = str.find(from, start)) != std::string::npos)
	{
		if(start > 0 && str[start-1] != '\\')
		{
			str.replace(start, from.size(), to);
			start += to.size(); // to avoid replacing patterns if to is a substring of "from"
		}
		else
		{
			start++;
		}
	}
	return str;
}
