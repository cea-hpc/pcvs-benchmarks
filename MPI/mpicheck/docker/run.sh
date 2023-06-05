#!/bin/bash
#
MPIMETA_PATH="${MPIMETA-$PWD}"

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

safe_exec git -C $spack_repo pull
. $spack_repo/share/spack/setup-env.sh

for runtime in openmpi intel-mpi mpich; do
	versions=$(spack info $runtime | grep -oE "\s+([0-9\.]+)\s+http.*" | awk '{print $1}' | sort -n)
	runtime_checkdir=$pcvs_bank/cache/$runtime
	test ! -e $runtime_checkdir && mkdir -p $runtime_checkdir
	for v in $versions; do
		version_checkfile=$runtime_checkdir/$v

		# check if already stored
		if test ! -e $version_checkfile; then
			
			profile="mpi"
			#dedicated IntelMPI profile to change compiler name mapping
			if test "$runtime" = "intel-mpi"; then
				profile=intelmpi
				spack_line="$runtime@$v%intel"
			else
				spack_line="$runtime@$v"
			fi

			echo "Build new version: $runtime@$v"
			spack location -i $spack_line > /dev/null  2>&1
			if test "$?" -ne 0; then
				safe_exec spack install $spack_line
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
			safe_exec touch $version_checkfile
		fi
	done
done

