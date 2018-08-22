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


#ifndef FILE_H
#define FILE_H

#include "utils.h"

/// Defines differents opening mode for files
/**
 * This enum gathers differents opening mode for read/write info file
 */
typedef enum eFileMode{
	INPUT_ACCESS = 0,     ///< Open file in INPUT mode
	OUTPUT_ACCESS = 1,    ///< Open file in OUTPUT mode
	INPUT_ACCESS_BIN = 2, ///< open file in INPUT mode but the content is in binary mode
	OUTPUT_ACCESS_BIN = 3 ///< open file in OUTPUT mode but the content is in binary mode
} FileMode;

/// Defines how to use file in application
/**
 * instead of use a file descriptor and manipulate file paths and others options, this object
 * gathers all these information. Some operator overloading are provided to use a FileManager as
 * a file stream. Thus, FileManager using is transparent
 */
class FileManager {
	/************** MEMBERS **************/
	std::string* name;     ///< base name of the file to use
	std::string* path;     ///< directory path of the file to use
	std::fstream* stream;  ///< the stream focused on this file
	FileMode mode;         ///< the mode of opening \see eFileMode
	
	/************** STATICS **************/
	static size_t newFileID; ///< define an unique id for the current file
public:	
	/************** STATICS **************/
	/// check if given file name is created on file system
	/**
	 * \param[in] file file to check
	 * \return <b>True</b> if file exists
	 * \return <b>fALSE</B> otherwise
	 */
	static bool isCreated(std::string* file);
	/// check if given file name is created on file system (from a const file pointer)
	/**
	 * \param[in] file file to check
	 * \return <b>True</b> if file exists
	 * \return <b>fALSE</B> otherwise
	 */
	static bool isCreated(const std::string* file);
	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	///standard constructor
	explicit FileManager();
	///construct a FileManager
	/**
	 * This constructor build a operational FileManager!
	 * \warning The constructor did NOT open the file !! 
	 * \see open
	 * \param[in] chain the file path to open (absolute path)
	 * \param[in] mode the opening mode as depicted in FileMode enum
	 */
	FileManager(std::string* chain, FileMode mode);
	/// open a file
	/**
	 * This function opens the file in file Manager and check it's well opened
	 * \return <b>True</b> if stream is well-opened
	 * \return <b>False</b> otherwise
	 */
	bool open();
	/// close a file
	/**
	 * This function closes the file in file Manager and check it's well closed
	 * \return <b>True</b> if stream is well-closed
	 * \return <b>False</b> otherwise
	 */
	bool close();
	///write data into stream
	/**
	 * This function calls fstream::write() funtion. it's used write an object
	 * directly into the file pointed by File Manager. In order to do this, our 
	 * function is called with a const char *, representing the object cast and the
	 * object size. This function is used to write into file from binary mode
	 * \param[in] obj the block to write into file
	 * \param[in] size the block's size
	 */
	void write(const char* obj, long unsigned int size);
	/// read data into stream
	/**
	 * This function reads the stream to set an string (the data flow) with the 
	 * content of the file.
	 * \param[in] arg1 the object we fill from the file
	 * \param[in] arg2 the chain's length to read
	 * \return <b>True</b> if reading is well-done (no eof() or others)..
	 * \return <b>False</b> otherwise
	 */
	bool read(std::string* arg1, long unsigned int arg2 );
	/// read data into stream
	/**
	 * Due to some issues to directly cast into char*, this function has the same
	 * behaviour than read(string*)
	 * \see read(std::string* arg1, long unsigned int arg2 )
	 * \param[in] arg1 the object we fill
	 * \param[in] arg2 the object length to read
	 * \return <b>True</b> if reading is well-done (no eof() or others)..
	 * \return <b>False</b> otherwise
	 */ 
	bool read(char* arg1, long unsigned int arg2 );
	/// Operator overloading to write a string into a file
	/**
	 * This function overloads << operator to directly write a chain into the file
	 * without getters
	 * \param[in] chain the string to write
	 * \return a reference to himself in order to support operator concatenation
	 */
	FileManager& operator<<(std::string chain);
	/// virtual destructor to unset the File Manager
	virtual ~FileManager();

	/****** CONST ******/
	/// get the file path the file manager is based on
	/**
	 * \return The complete string from file path
	 */
	std::string toString() const;
	/// checker if file is open
	/**
	 * \return <b>True</b> if file is open
	 * \return <b>False</b> otherwise
	 */
	bool isOpen() const;
	/// get the directory name for the current file
	/**
	 * \return the string representing the path to the last directory to find the file
	 */
	std::string getPath() const;
	/// get the file base name for the current file
	/**
	 * \return the string representing the base name with incremental indice
	 */
	std::string getBaseName() const;
	/// get the file name (without path) for current file
	/**
	 * \return the string representing the file name
	 */
	std::string getName() const;
};
#endif // FILE_H
