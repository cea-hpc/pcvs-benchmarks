#!/bin/sh

#display root tag
echo "<?xml version='1.0'?>"
echo "<global>"

#find xml results
file_list=$(find $SOURCES_LIST -iname 'output.xml')

#open detail balis
echo "	<details>"

#setup defaults
total_test_cases=0
total_test_failures=0
total_test_errors=0

#loop on files
for file in ${file_list}
do
	#remove ./ from path
	file=$(echo $file | sed -e 's#^./##g')

	#calc htmlfilename
	htmlfile=$(echo $file | sed -e 's#/#-#g' -e 's/.xml/.html/g')

	#compute
	cnt_test_cases=$(grep -c '<testcase ' ${file})
	cnt_test_failures=$(grep -c '<failure ' ${file})
	cnt_test_errors=$(grep -c '<error ' ${file})
	cnt_test_success=$(( ${cnt_test_cases} - ${cnt_test_errors} - ${cnt_test_failures} ))
	test_dir=$(dirname ${file})

	#check final status
	if [ "${cnt_test_errors}" != "0" ]; then
		test_status='error'
	elif [ "${cnt_test_failures}" != "0" ]; then
		test_status='failed'
	else
		test_status='success'
	fi

	#display summary for this file
	echo "		<testdir>"
	echo "			<name>${test_dir}</name>"
	echo "			<tests>${cnt_test_cases}</tests>"
	echo "			<failures>${cnt_test_failures}</failures>"
	echo "			<errors>${cnt_test_errors}</errors>"
	echo "			<success>${cnt_test_success}</success>"
	echo "			<status>${test_status}</status>"
	echo "			<htmlfile>${htmlfile}</htmlfile>"
	echo "		</testdir>"

	#update global counters
	total_test_cases=$(( ${total_test_cases} + ${cnt_test_cases} ))
	total_test_failures=$(( ${total_test_failures} + ${cnt_test_failures} ))
	total_test_errors=$(( ${total_test_errors} + ${cnt_test_errors} ))
done

#close detail balis
echo "	</details>"

#compute success
total_test_success=$(( ${total_test_cases} - ${total_test_failures} - ${total_test_errors} ))

#check final status
if [ "${total_test_errors}" != "0" ]; then
	final_status='error'
elif [ "${total_test_failures}" != "0" ]; then
	final_status='failed'
else
	final_status='success'
fi

#print summary
echo "	<total>"
echo "		<tests>${total_test_cases}</tests>"
echo "		<success>${total_test_success}</success>"
echo "		<failures>${total_test_failures}</failures>"
echo "		<errors>${total_test_errors}</errors>"
echo "		<status>${final_status}</status>"
echo "	</total>"

#close root tag
echo "</global>"
