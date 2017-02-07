#!/bin/sh

NEW_TEST_DIR=.

generate_c_tests(){
	LIST=`ls ${NEW_TEST_DIR}/c`
	FILES=""

	for FILE in $LIST; do
        	./template_parser_c.pl --test ${NEW_TEST_DIR}/c/${FILE} C/${FILE}
		FILES="${FILES} ${FILE}"
	done
	echo "LIST OF FILES:"
	echo "--------------"
	echo ${FILES}
}


generate_fortran_tests(){
        LIST=`ls ${NEW_TEST_DIR}/fortran`
        FILES=""

        for FILE in $LIST; do
		./template_parser_fortran.pl --test ${NEW_TEST_DIR}/fortran/${FILE} FORTRAN/${FILE}

		mv FORTRAN/${FILE} FORTRAN/${FILE}.old
		sed 's/END PROGRAM/END SUBROUTINE/g' FORTRAN/${FILE}.old \
			| sed 's/PROGRAM [a-zA-Z0-9_]*/SUBROUTINE mpc_user_main/g' > FORTRAN/${FILE}
		rm -f FORTRAN/${FILE}.old
		FILES="${FILES} ${FILE}"
        done

	echo "LIST OF FILES:"
	echo "--------------"
	echo ${FILES}
}

# generate_c_tests
 generate_fortran_tests
