#!/bin/bash
#
MPIMETA_PATH="${MPIMETA-$PWD}"
CHECK_INSTALL="${CHECK_INSTALL-yes}"
LIST_ONLY="${LIST_ONLY-no}"

err()
{
	>&2 echo "$@"
	exit 1
}

safe_exec()
{
	echo "-> $@"
	eval "$@" || err "Failed to complete $@"
}

pcvs_bankname=$1
test -z "$pcvs_bankname" && err "You must provide a target PCVS bank"

test -z "$(which spack)" && err "Spack should be in PATH"
test -z "$(which pcvs)" && err "PCVS should be in PATH"
spack_repo=$(spack location -r)
pcvs_bank=$(pcvs bank show $pcvs_bankname -p)

test ! -e "$spack_repo" && err "Failed to locate the Spack repo"
test ! -e "$pcvs_bank" && err "Failed to locate the PCVS bank"

# create cache dir
test ! -e "$pcvs_bank/cache" && mkdir -p $pcvs_bank/cache

echo "Update Spack builtin repo"
safe_exec git -C $spack_repo pull > /dev/null 2>&1
echo "Load Spack Shell integration"
. $spack_repo/share/spack/setup-env.sh


list_versions_for_pkg()
{
	spack info $runtime | grep -oE "\s+([0-9]\..*+)\s+http.*" | awk '{print $1}' | sort -u
}

is_missing_spec()
{
	target_file=$pcvs_bank/cache/$1
	test -e $target_file && return 1
	return 0
}

mark_spec()
{
	target_file=$pcvs_bank/cache/$1
	touch $target_file
}


echo "Browse runtimes"
for runtime in openmpi intel-mpi mpich mvapich2; do
	versions=$(list_versions_for_pkg $runtime)
	mkdir -p $pcvs_bank/cache/$runtime
	echo "$runtime $versions"
	for v in $versions; do
		# check if already stored
		if is_missing_spec $runtime/$v; then
			echo "==> $runtime/$v is missing"
			test "$LIST_ONLY" = "yes" && continue
			
			profile="mpi"
			#dedicated IntelMPI profile to change compiler name mapping
			if test "$runtime" = "intel-mpi"; then
				profile=intelmpi
				spack_line="$runtime@$v%intel"
			else
				spack_line="$runtime@$v"
			fi
			
			if test "$CHECK_INSTALL" = "yes"; then
				spack location -i $spack_line > /dev/null  2>&1
				if test "$?" -ne 0; then
					safe_exec spack install $spack_line
				fi
			fi

			# comment/uncomment to prepare the env
			# if uncommented, will only build spack recipes
			#continue
			#
			if test "$runtime" = "intel-mpi"; then
				safe_exec spack load intel-oneapi-compilers
			fi

			safe_exec spack load $spack_line
			#safe_exec pcvs run -p $profile mpicheck:${MPIMETA_PATH}/tests/functions -b mpimeta@$runtime -m \"$runtime $v\"
			safe_exec pcvs run -p $profile mpicheck:${MPIMETA_PATH}/tests/functions -m \"$runtime $v\"
			test -d /archives && safe_exec cp .pcvs-build/pcvsrun_*.tar.gz /archives
			mark_spec $runtime/$v
		fi
	done
done
echo "Completed."


