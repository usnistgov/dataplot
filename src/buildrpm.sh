#!/bin/sh
# This is a shell archive (produced by GNU sharutils 4.13.3).
# To extract the files from this archive, save it to some FILE, remove
# everything before the `#!/bin/sh' line above, then type `sh FILE'.
#
lock_dir=_sh04928
# Made on 2020-03-04 14:26 EST by <heckert@h048122.nist.gov>.
# Source directory was `/home/heckert/dataplot/git/dataplot/src'.
#
# Existing files will *not* be overwritten, unless `-c' is specified.
#
# This shar contains:
# length mode       name
# ------ ---------- ------------------------------------------
#    474 -rw-r--r-- buildrpm.txt
#
MD5SUM=${MD5SUM-md5sum}
f=`${MD5SUM} --version | egrep '^md5sum .*(core|text)utils'`
test -n "${f}" && md5check=true || md5check=false
${md5check} || \
  echo 'Note: not verifying md5sums.  Consider installing GNU coreutils.'
if test "X$1" = "X-c"
then keep_file=''
else keep_file=true
fi
echo=echo
save_IFS="${IFS}"
IFS="${IFS}:"
gettext_dir=
locale_dir=
set_echo=false

for dir in $PATH
do
  if test -f $dir/gettext \
     && ($dir/gettext --version >/dev/null 2>&1)
  then
    case `$dir/gettext --version 2>&1 | sed 1q` in
      *GNU*) gettext_dir=$dir
      set_echo=true
      break ;;
    esac
  fi
done

if ${set_echo}
then
  set_echo=false
  for dir in $PATH
  do
    if test -f $dir/shar \
       && ($dir/shar --print-text-domain-dir >/dev/null 2>&1)
    then
      locale_dir=`$dir/shar --print-text-domain-dir`
      set_echo=true
      break
    fi
  done

  if ${set_echo}
  then
    TEXTDOMAINDIR=$locale_dir
    export TEXTDOMAINDIR
    TEXTDOMAIN=sharutils
    export TEXTDOMAIN
    echo="$gettext_dir/gettext -s"
  fi
fi
IFS="$save_IFS"
if (echo "testing\c"; echo 1,2,3) | grep c >/dev/null
then if (echo -n test; echo 1,2,3) | grep n >/dev/null
     then shar_n= shar_c='
'
     else shar_n=-n shar_c= ; fi
else shar_n= shar_c='\c' ; fi
f=shar-touch.$$
st1=200112312359.59
st2=123123592001.59
st2tr=123123592001.5 # old SysV 14-char limit
st3=1231235901

if   touch -am -t ${st1} ${f} >/dev/null 2>&1 && \
     test ! -f ${st1} && test -f ${f}; then
  shar_touch='touch -am -t $1$2$3$4$5$6.$7 "$8"'

elif touch -am ${st2} ${f} >/dev/null 2>&1 && \
     test ! -f ${st2} && test ! -f ${st2tr} && test -f ${f}; then
  shar_touch='touch -am $3$4$5$6$1$2.$7 "$8"'

elif touch -am ${st3} ${f} >/dev/null 2>&1 && \
     test ! -f ${st3} && test -f ${f}; then
  shar_touch='touch -am $3$4$5$6$2 "$8"'

else
  shar_touch=:
  echo
  ${echo} 'WARNING: not restoring timestamps.  Consider getting and
installing GNU `touch'\'', distributed in GNU coreutils...'
  echo
fi
rm -f ${st1} ${st2} ${st2tr} ${st3} ${f}
#
if test ! -d ${lock_dir} ; then :
else ${echo} "lock directory ${lock_dir} exists"
     exit 1
fi
if mkdir ${lock_dir}
then ${echo} "x - created lock directory ${lock_dir}."
else ${echo} "x - failed to create lock directory ${lock_dir}."
     exit 1
fi
# ============= buildrpm.txt ==============
if test -n "${keep_file}" && test -f 'buildrpm.txt'
then
${echo} "x - SKIPPING buildrpm.txt (file already exists)"

else
${echo} "x - extracting buildrpm.txt (text)"
  sed 's/^X//' << 'SHAR_EOF' > 'buildrpm.txt' &&
type dnf >&/dev/null && instpkg=dnf || instpkg=yum
sudo $instpkg install git $instpkg-utils rpm-build rpmdevtools
git clone https://github.com/usnistgov/dataplot.git
spec=dataplot/src/dataplot.spec
version=$(awk '/^Version/ {print $NF}' $spec)
# create ~/rpmbuild/ tree
rpmdev-setuptree
tar -czf $(rpm --eval %{_sourcedir})/dataplot-$version.tar.gz dataplot
sudo yum-builddep $spec  # 66 packages
rpmbuild -ba $spec
find $(rpm --eval %{_topdir}) -name '*.rpm' -mtime -1 -ls
SHAR_EOF
  (set 20 20 03 04 14 26 24 'buildrpm.txt'
   eval "${shar_touch}") && \
  chmod 0644 'buildrpm.txt'
if test $? -ne 0
then ${echo} "restore of buildrpm.txt failed"
fi
  if ${md5check}
  then (
       ${MD5SUM} -c >/dev/null 2>&1 || ${echo} 'buildrpm.txt': 'MD5 check failed'
       ) << \SHAR_EOF
d75b6b9806a4d4b047f57b78be42d064  buildrpm.txt
SHAR_EOF

else
test `LC_ALL=C wc -c < 'buildrpm.txt'` -ne 474 && \
  ${echo} "restoration warning:  size of 'buildrpm.txt' is not 474"
  fi
fi
if rm -fr ${lock_dir}
then ${echo} "x - removed lock directory ${lock_dir}."
else ${echo} "x - failed to remove lock directory ${lock_dir}."
     exit 1
fi
exit 0
