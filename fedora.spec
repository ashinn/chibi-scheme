Summary:                 A small-footprint Scheme for use as a C Extension Language
Name:                    chibi-scheme
Version:                 0.4
Release:                 1%{?dist}


Source0:                 http://chibi-scheme.googlecode.com/files/chibi-scheme-0.4.tgz
Patch1:                  chibi-scheme.Makefile.patch
Group:                   Development/Tools
License:                 BSD
URL:                     http://code.google.com/p/chibi-scheme/
BuildRoot:               %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
# BuildRequires:


%description
Chibi-Scheme is a very small library intended for use as an extension
and scripting language in C programs.  In addition to support for
lightweight VM-based threads, each VM itself runs in an isolated heap
allowing multiple VMs to run simultaneously in different OS threads.

%prep
%setup -q -n %{name}-%{version}
%patch1

%build
%{__make} PREFIX=%{_prefix} DESTDIR=%{RPM_BUILD_ROOT}  LIBDIR=%{_libdir} SOLIBDIR=%{_libdir} MODDIR=%{_datarootdir}/chibi-scheme doc all

%install
rm -rf $RPM_BUILD_ROOT

mkdir -p ${RPM_BUILD_ROOT}
%{__make} PREFIX=%{_prefix} DESTDIR=${RPM_BUILD_ROOT} LIBDIR=%{_libdir} SOLIBDIR=%{_libdir} LDFLAGS="-C ${RPM_BUILD_ROOT}%{_sysconfdir}/ld.so.conf.d" MODDIR=%{_datarootdir}/chibi-scheme  install

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{_bindir}/chibi-scheme
%{_datarootdir}/chibi-scheme
%{_datarootdir}/man
%{_libdir}/libchibi-scheme.so


%package devel
Summary:                 Development files for the %{name} package.
%description devel
This package contains development and include
files for %{name} package.

%files devel
%defattr(-,root,root,-)
%{_includedir}

%changelog
* Sat May 28 2011 Alex Shinn <alexshinn[AT]gmail.com> - 0.4
* Wed Apr 22 2011 Rajesh Krishnan <devel[AT]krishnan.cc> - 0.3
- Initial release
