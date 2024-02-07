#! /bin/bash

test -d ./config || mkdir ./config

tests_sos_dir=modules/tests-sos

script_dir="$(cd "$(dirname "$0")" && pwd)"
current_dir="$(pwd)"

# Check if the script is being run from within the root SOS source directory
if [ "$script_dir" != "$current_dir" ]; then
    echo "Error: Please run ./autogen.sh from the top-level directory of SOS."
    exit 1
fi

if [ ! -d "${tests_sos_dir}" ]; then
    echo "Error: the tests-sos submodule directory ${tests_sos_dir} does not exist"
    exit 1
elif [ ! -f "${tests_sos_dir}/test/Makefile.am" ] ||
     [ ! -f "${tests_sos_dir}/test/apps/Makefile.am" ] ||
     [ ! -f "${tests_sos_dir}/test/include/Makefile.am" ] ||
     [ ! -f "${tests_sos_dir}/test/performance/Makefile.am" ] ||
     [ ! -f "${tests_sos_dir}/test/shmemx/Makefile.am" ] ||
     [ ! -f "${tests_sos_dir}/test/spec-example/Makefile.am" ] ||
     [ ! -f "${tests_sos_dir}/test/unit/Makefile.am" ]; then
    echo "Error: test submodule (${tests_sos_dir}) contents are missing."
    echo "Please run the following command to download the SOS tests:"
    echo "   git submodule update --init"
    exit 1
else
    if [ -d "$current_dir/test" ] && [ ! -z "$(ls -A "$current_dir/test")" ]; then
        echo "The ./test directory already contains files."
        read -p "Do you want to overwrite them? (y/n): " overwrite
        if [ "$overwrite" != "y" ]; then
            echo "Copying tests-sos submodule aborted. Continuing..."
        fi
    fi
    cp -r "${tests_sos_dir}/test" "$current_dir"
    if [ $? -eq 0 ]; then
        echo "tests-sos submodule copied successfully."
    else
        echo "Error: tests-sos submodule copy failed."
        exit 1;
    fi
fi

FILES=./man/*.1
echo -n "dist_man1_MANS =" > ./man/Makefile.am
for f in $FILES
do
  echo -n " $(basename $f)" >> ./man/Makefile.am
done

FILES=./man/*.3
echo -e -n "\ndist_man3_MANS =" >> ./man/Makefile.am
for f in $FILES
do
  echo -n " $(basename $f)" >> ./man/Makefile.am
done

echo -e "\n" >> ./man/Makefile.am

autoreconf -vif
