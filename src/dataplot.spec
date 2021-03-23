Name:		dataplot
Version:	20210323
Release:	1%{?dist}
Summary:	Scientific visualization, statistical analysis, and non-linear modeling
Group:		Applications/Engineering
License:	Public Domain
URL:		http://www.itl.nist.gov/div898/software/dataplot/
#Source:		ftp://ftp.nist.gov/pub/dataplot/unix/dataplot-%{version}.tar.gz
#Source:		dataplot-%{version}.tar.gz
Source:		https://www.itl.nist.gov/div898/software/dataplot/ftp/unix/dataplot-%{version}.tar.gz
BuildRequires:	gcc-gfortran
BuildRequires:	gd-devel, libpng, libjpeg, zlib, freetype, readline-devel, cairo-devel, xclip



%define dplibdir %{_libdir}/dataplot

%description
Dataplot is a tool for scientific visualization, statistical analysis,
and non-linear modeling.  This build uses IBIT=64 and IPREC=64-64 defaults,
leading to compiler flags: -fdefault-real-8 -fdefault-double-8.


%prep
#%setup -q -c -a 1 -a 2
# tarball unpacks into unversioned "dataplot", so use -n
%setup -q -n dataplot


%build
# source will emit lots of warnings when -Wall is used, so brute-force remove it, but leave the rest.
# 2020/05/21: Dataplot source has been upgraded so that -Wall will not emit a lot of warnings
export FFLAGS=$(sed 's/\s-Werror=format-security\s/ /g' <<<" %{optflags} ")
#export FFLAGS=$(sed 's/\s-Wall\s/ /g' <<<" %{optflags} ")
# below leaves all in but excludes some of the less serious warnings
#export FFLAGS=" %{optflags} -Wno-unused-variable -Wno-unused-but-set-variable -Wno-maybe-uninitialized -Wno-unused-dummy-argument -Wno-unused-label"

# Fedora 29 compiles with -Werror=format-security which is an error without -Wformat (i.e., without -Wall).
#export CFLAGS=$(sed 's/\s-Wall\s/ -Wformat /g' <<< "%{optflags} ")
export CFLAGS="%{optflags} -Wno-unused-variable -Wno-unused-but-set-variable "

make -C src DPLIBDIR=%{dplibdir}   %{?_smp_mflags}


%install
rm -rf $RPM_BUILD_ROOT
make -C src install  DPLIBDIR=$RPM_BUILD_ROOT%{dplibdir}  DESTDIR=$RPM_BUILD_ROOT BINDIR=$RPM_BUILD_ROOT%{_bindir}

echo removing zero-length files:
find $RPM_BUILD_ROOT%{dplibdir} -empty -exec rm -v {} +
echo


echo removing hidden files:
find $RPM_BUILD_ROOT%{dplibdir} -name '.??*' -exec rm -v {} +
echo


%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
#%doc %{dplibdir}/frmenus/interp/README.DOC
%doc README INSTALL
/usr/bin/*
%{dplibdir}/*


%changelog
* Fri Apr  5 2019 Chris Schanzle <schanzle@nist.gov> - 20190409-1
- massive overhaul / reorg, hopefully for the better.

* Sun Oct 30 2011 Chris Schanzle <schanzle@nist.gov> - 20090821-1
- cleanup, and makefile mods for new files
- can run upstream build script if necessary to provide sample build

* Tue Feb 26 2008 Chris Schanzle <schanzle@nist.gov> - 2006.12-3
- rpmlint clean, basic checks OK.

* Tue Feb 26 2008 Chris Schanzle <schanzle@nist.gov> - 2006.12-2
- rpmlint still reports many zero-length files, hidden files/dirs, non-executable scripts

* Mon Feb 25 2008 Chris Schanzle <schanzle@nist.gov> - 2006.12-2
- cleanups, buildrequires to build under mock, permissions.

* Wed Feb 21 2007 Chris Schanzle <schanzle@nist.gov> - 2006.12-1
- Initial package
