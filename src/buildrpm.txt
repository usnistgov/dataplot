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
